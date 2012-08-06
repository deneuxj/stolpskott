module CleverRake.StolpSkott.Referee

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils.CoopMultiTasking

open Match

let wallDistance = 9.15f<m>

let refereeTask (env : Environment) timeFactor (getMatchState : unit -> MatchState) (setMatchState : MatchState -> unit) (reportScored : Team.TeamSide -> unit) (kickerReady : IEvent<_>) (ballKicked : IEvent<_>) =
    let watch = env.NewStopwatch()
    let scoreA = ref 0
    let scoreB = ref 0

    let manageMatchTime state =
        // Start/pause the watch
        match state with
        | { ball = { inPlay = Ball.LiveBall } } -> watch.Start()
        | { ball = { inPlay = Ball.DeadBall _ } } -> watch.Stop()

        // Transitions to the next period
        // TODO: Injury time and extra time.
        match state with
        | { ball = { inPlay = Ball.LiveBall } ; period = Match.FirstHalf } when watch.ElapsedSeconds * timeFactor / 60.0f > 45.0f ->
            watch.Stop()
            watch.Reset()
            { state with
                ball = { state.ball with inPlay = Ball.KickOff (Ball.WaitWhistle, Team.TeamB) }
                period = Match.SecondHalf }
        | { ball = { inPlay = Ball.LiveBall } ; period = Match.SecondHalf } when watch.ElapsedSeconds * timeFactor / 60.0f > 45.0f ->
            watch.Stop()
            { state with period = Match.MatchOver }
        | _ -> state

    let (|InUpperGoal|InLowerGoal|OutLeft|OutRight|OutUpper|OutLower|Other|) (state : MatchState) =
        if state.ball.pos.X > -Physics.goalWidth / 2.0f &&
           state.ball.pos.X < Physics.goalWidth / 2.0f &&
           state.ball.pos.Z < Physics.goalHeight then
            if state.ball.pos.Y > state.pitch.length / 2.0f + Ball.ballRadius then
                InUpperGoal
            elif state.ball.pos.Y < -state.pitch.length / 2.0f - Ball.ballRadius then
                InLowerGoal
            else
                Other
        elif state.ball.pos.X > state.pitch.width / 2.0f + Ball.ballRadius then
            OutRight
        elif state.ball.pos.X < -state.pitch.width / 2.0f - Ball.ballRadius then
            OutLeft
        elif state.ball.pos.Y > state.pitch.length / 2.0f + Ball.ballRadius then
            OutUpper
        elif state.ball.pos.Y < -state.pitch.length / 2.0f - Ball.ballRadius then
            OutLower
        else
            Other

    let switchToDeadBall (state : MatchState) =
        let otherTeam =
            match state.ball.lastTouchedBy with
            | None -> Team.TeamA
            | Some Team.TeamA -> Team.TeamB
            | Some Team.TeamB -> Team.TeamA

        match state.ball.inPlay with
        | Ball.DeadBall _-> state
        | Ball.LiveBall ->
            match state with
            | OutRight ->
                { state with ball = { state.ball with inPlay = Ball.ThrowIn(otherTeam, Ball.Right, state.ball.pos.Y) } }
            
            | OutLeft ->
                { state with ball = { state.ball with inPlay = Ball.ThrowIn(otherTeam, Ball.Left, state.ball.pos.Y) } }
            
            | InUpperGoal | InLowerGoal ->
                let scoringTeam =
                    if (state.ball.pos.Y > 0.0f<m>) = (isTeamAttackingUp Team.TeamA state.period) then
                        Team.TeamA
                    else
                        Team.TeamB
                
                reportScored scoringTeam
                match scoringTeam with
                | Team.TeamA -> scoreA := !scoreA + 1
                | Team.TeamB -> scoreB := !scoreB + 1
                
                { state with
                    ball = { state.ball with
                                inPlay = Ball.KickOff(Ball.WaitWhistle, Team.otherSide scoringTeam) }
                }

            | OutUpper | OutLower ->
                let homeTeam =
                    if (state.ball.pos.Y > 0.0f<m>) = (isTeamAttackingUp Team.TeamA state.period) then
                        Team.TeamB
                    else
                        Team.TeamA

                let pitchSide =
                    if state.ball.pos.X > 0.0f<m> then Ball.Right else Ball.Left

                if otherTeam <> homeTeam then
                    { state with
                        ball = { state.ball with
                                    inPlay = Ball.CornerKick(Ball.WaitWhistle, otherTeam, pitchSide) }
                    }
                else
                    { state with
                        ball = { state.ball with
                                    inPlay = Ball.KickIn(homeTeam, pitchSide) }
                    }

            | Other ->
                state

    let engagementWhistle = env.NewChannel()
    let engagement killed =
        task {
            while not !killed do
                // Wait for a dead ball that requires a whistle before the ball is kicked.
                do! env.WaitUntil <|
                    fun () ->
                        !killed 
                        ||
                        match getMatchState().ball.inPlay with
                        | Ball.CornerKick(Ball.WaitWhistle, _, _)
                        | Ball.KickOff(Ball.WaitWhistle, _)
                        | Ball.Penalty(Ball.WaitWhistle, _) ->
                            true
                        | _ -> false

                // Small delay to avoid false detection of immobile players
                do! env.Wait(0.5f)

                // Give some time to players to move into position, 15s at most.
                do! env.WaitUnless(15.0f, fun () ->
                        !killed
                        ||
                        let state = getMatchState()
                        let teamAReady =
                            state.teamA.onPitch
                            |> Array.exists(fun player -> player.speed > 0.0f<m/s>)
                            |> not
                        let teamBReady =
                            state.teamB.onPitch
                            |> Array.exists(fun player -> player.speed > 0.0f<m/s>)
                            |> not
                        teamAReady && teamBReady
                        )

                // Wait for the conditions required for the whistle to be fulfilled.
                do! env.WaitUntil <|
                    fun () ->
                        !killed
                        ||
                        let state = getMatchState()
                        match state.ball.inPlay with
                        | Ball.CornerKick(_, owner, _)
                        | Ball.KickOff(_, owner) ->
                            let opponents =
                                match owner with
                                | Team.TeamA -> state.teamB.onPitch
                                | Team.TeamB -> state.teamA.onPitch
                            let distancesOk =
                                opponents
                                |> Array.exists (fun player ->
                                    TypedVector2<m>(state.ball.pos.X, state.ball.pos.Y) - player.pos
                                    |> TypedVector.len2
                                        < wallDistance)
                                |> not
                            distancesOk

                        | Ball.Penalty(_, owner) ->
                            let opponents, owners =
                                match owner with
                                | Team.TeamA -> state.teamB.onPitch, state.teamA.onPitch
                                | Team.TeamB -> state.teamA.onPitch, state.teamB.onPitch
                            let distancesOpponentsOk =
                                opponents
                                |> Array.exists (fun player ->
                                    TypedVector2<m>(state.ball.pos.X, state.ball.pos.Y) - player.pos
                                    |> TypedVector.len2
                                        < wallDistance)
                                |> not
                            let ownersCloseToBall =
                                owners
                                |> Array.filter (fun player ->
                                    TypedVector2<m>(state.ball.pos.X, state.ball.pos.Y) - player.pos
                                    |> TypedVector.len2
                                        < wallDistance)
                            distancesOpponentsOk && Array.length ownersCloseToBall = 1

                        | _ -> failwith "Unexpected case"
                
                // Blow the whistle.
                if not <| !killed then
                    do! engagementWhistle.Send()

                // Wait for a live ball.
                if not <| !killed then
                    do! env.WaitUntil <|
                        fun () ->
                            match getMatchState().ball.inPlay with
                            | Ball.LiveBall _ -> true
                            | _ -> false
        }

    let makeBallLive = env.NewChannel()
    let switchToLiveBall killed =
        task {
            while not !killed do
                // Wait for a dead ball
                do! env.WaitUntil <|
                    fun () ->
                        !killed
                        ||
                        match getMatchState().ball.inPlay with
                        | Ball.DeadBall _ -> true
                        | _ -> false

                do! env.AwaitEvent kickerReady

                // Wait for the ball to lay still
                do! env.WaitUntil <|
                    fun() ->
                        !killed
                        ||
                        getMatchState().ball.speed = TypedVector3<m/s>.Zero

                // Wait for the ball to be kicked
                let! kickerTeam = env.AwaitEvent(ballKicked)

                // The ball is now alive
                if not <| !killed then
                    do! makeBallLive.Send()
        }

    let rec main killed =
        task {            
            match getMatchState().period with
            | Match.MatchOver -> return ()
            | _ ->
                let state = getMatchState()
                let state = manageMatchTime state
                let state = switchToDeadBall state
                match getMatchState().ball.inPlay, state.ball.inPlay with
                | Ball.LiveBall, Ball.DeadBall _ ->
                    printfn "DEAD BALL"
                | _ -> ()
                let state = { state with periodTime = 1.0f<s> * watch.ElapsedSeconds }
                let! inPlay =
                    task {
                        if not <| engagementWhistle.IsEmpty() then
                            let! _ = engagementWhistle.Receive()
                            printfn "WHISTLE!"
                            return
                                match state.ball.inPlay with
                                | Ball.CornerKick(_, teamSide, pitchSide) -> Ball.CornerKick(Ball.CanKick, teamSide, pitchSide)
                                | Ball.KickOff(_, side) -> Ball.KickOff(Ball.CanKick, side)
                                | Ball.Penalty(_, side) -> Ball.Penalty(Ball.CanKick, side)
                                | _ -> state.ball.inPlay
                        elif not <| makeBallLive.IsEmpty() then
                            let! _ = makeBallLive.Receive()
                            printfn "BALL LIVE"
                            return Ball.InPlay
                        else
                            return state.ball.inPlay
                    }
                let state = { state with ball = { state.ball with inPlay = inPlay } }
                setMatchState(state)
                do! env.WaitNextFrame()
                return! main killed
        }

    task {
        let main = env.Spawn main
        let whistler = env.Spawn engagement
        let ballReviver = env.Spawn switchToLiveBall
        do! env.WaitUntil (fun () -> main.IsDead)
        whistler.Kill()
        ballReviver.Kill()
        do! env.WaitUntil (fun () -> whistler.IsDead && ballReviver.IsDead)
        return !scoreA, !scoreB
    }