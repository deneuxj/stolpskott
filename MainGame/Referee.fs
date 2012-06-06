module CleverRake.StolpSkott.Referee

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils.CoopMultiTasking

open Match

let wallDistance = 9.15f<m>

let refereeTask (env : Environment) timeFactor (getMatchState : unit -> MatchState) (setMatchState : MatchState -> unit) (reportScored : Team.TeamSide -> unit) =
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

    let whoTouchedTheBallLast state player =
        match state with
        | { ball = { inPlay = Ball.DeadBall _ } } -> player
        | { ball = { inPlay = Ball.LiveBall } ; teamA = teamA ; teamB = teamB } ->
            let touched (team : Team.TeamSide) players =
                players
                |> Array.tryFind (fun player ->
                    match Physics.collideBallWithPlayer (team, player) state.ball with
                    | Physics.Free -> false
                    | _ -> true)
            match touched Team.TeamA teamA.onPitch with
            | None ->
                match touched Team.TeamB teamB.onPitch with
                | None -> None
                | Some player -> Some(Team.TeamB, player)
            | Some player ->
                Some(Team.TeamA, player)

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

    let switchToDeadBall (state : MatchState) toucher =
        let otherTeam = function
        | None -> Team.TeamA
        | Some (Team.TeamA, _) -> Team.TeamB
        | Some (Team.TeamB, _) -> Team.TeamA

        match state.ball.inPlay with
        | Ball.DeadBall _-> state
        | Ball.LiveBall ->
            match state with
            | OutRight ->
                { state with ball = { state.ball with inPlay = Ball.ThrowIn(otherTeam toucher, Ball.Right, state.ball.pos.Y) } }
            
            | OutLeft ->
                { state with ball = { state.ball with inPlay = Ball.ThrowIn(otherTeam toucher, Ball.Left, state.ball.pos.Y) } }
            
            | InUpperGoal | InLowerGoal ->
                let scoringTeam =
                    if state.ball.pos.Y > 0.0f<m> && isTeamAttackingUp Team.TeamA state.period then
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
                    if state.ball.pos.Y > 0.0f<m> && isTeamAttackingUp Team.TeamA state.period then
                        Team.TeamB
                    else
                        Team.TeamA

                let pitchSide =
                    if state.ball.pos.X > 0.0f<m> then Ball.Right else Ball.Left

                match toucher, homeTeam with
                | Some (Team.TeamA, _), Team.TeamA
                | Some (Team.TeamB, _), Team.TeamB ->
                    { state with
                        ball = { state.ball with
                                    inPlay = Ball.CornerKick(Ball.WaitWhistle, Team.otherSide homeTeam, pitchSide) }
                    }
                | _ ->
                    { state with
                        ball = { state.ball with
                                    inPlay = Ball.KickIn(homeTeam, pitchSide) }
                    }

            | Other ->
                state

    let engagementWhistle = ref false
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
                    engagementWhistle := true
                    do! env.WaitUntil (fun () -> not !engagementWhistle)
        }

    let makeBallLive = ref false
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

                // Wait for the ball to lay still at the kick position
                do! env.WaitUntil <|
                    fun () ->
                        !killed
                        ||
                        match getMatchState().ball with
                        | { inPlay = Ball.CornerKick(Ball.CanKick, _, _) ; speed = speed }
                        | { inPlay = Ball.KickIn(_, _) ; speed = speed }
                        | { inPlay = Ball.KickOff(Ball.CanKick, _) ; speed = speed }
                        | { inPlay = Ball.Penalty(Ball.CanKick, _) ; speed = speed }
                            when speed = TypedVector3<m/s>.Zero ->
                            true
                        | { inPlay = Ball.FreeKick(_, kickPos) ; speed = speed ; pos = pos }
                            when speed = TypedVector3<m/s>.Zero && pos.X = kickPos.X && pos.Y = kickPos.Y ->
                            true
                        | _ -> false

                // Wait for the ball to move
                do! env.WaitUntil <|
                    fun() ->
                        !killed
                        ||
                        getMatchState().ball.speed <> TypedVector3<m/s>.Zero

                // The ball is now alive
                if not <| !killed then
                    makeBallLive := true
                    do! env.WaitUntil(fun () -> not !makeBallLive)
        }

    let rec main toucher killed =
        task {            
            match getMatchState().period with
            | Match.MatchOver -> return ()
            | _ ->
                let state = getMatchState()
                let toucher = whoTouchedTheBallLast state toucher
                let state = manageMatchTime state
                let state = switchToDeadBall state toucher
                match getMatchState().ball.inPlay, state.ball.inPlay with
                | Ball.LiveBall, Ball.DeadBall _ ->
                    printfn "DEAD BALL"
                | _ -> ()
                let state = { state with periodTime = 1.0f<s> * watch.ElapsedSeconds }
                let inPlay =
                    if !engagementWhistle then
                        engagementWhistle := false
                        printfn "WHISTLE!"
                        match state.ball.inPlay with
                        | Ball.CornerKick(_, teamSide, pitchSide) -> Ball.CornerKick(Ball.CanKick, teamSide, pitchSide)
                        | Ball.KickOff(_, side) -> Ball.KickOff(Ball.CanKick, side)
                        | Ball.Penalty(_, side) -> Ball.Penalty(Ball.CanKick, side)
                        | _ -> state.ball.inPlay
                    elif !makeBallLive then
                        printfn "BALL NOW ALIVE!"
                        Ball.InPlay
                    else
                        state.ball.inPlay
                let state = { state with ball = { state.ball with inPlay = inPlay } }
                setMatchState(state)
                do! env.WaitNextFrame()
                return! main toucher killed
        }

    task {
        let main = env.Spawn (main None)
        let whistler = env.Spawn engagement
        let ballReviver = env.Spawn switchToLiveBall
        do! env.WaitUntil (fun () -> main.IsDead)
        whistler.Kill()
        ballReviver.Kill()
        do! env.WaitUntil (fun () -> whistler.IsDead && ballReviver.IsDead)
        return !scoreA, !scoreB
    }