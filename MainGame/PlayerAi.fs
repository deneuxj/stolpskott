module CleverRake.StolpSkott.PlayerAi

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils.CoopMultiTasking

open Units

type AiPlayerObjective =
    | RunningToBall
    | RunningWithBallTo of TypedVector2<m>
    | FollowingTactic
    | RunningTo of TypedVector2<m> * TypedVector2<1>
    | PassingTo of int
    | CrossingTo of TypedVector2<m>
    | ShootingAtGoal


let assignObjectives (env : Environment) formation assign side (getMatchState : unit -> Match.MatchState) =
    let getTeam() =
        match side with
        | Team.TeamA -> getMatchState().teamA
        | Team.TeamB -> getMatchState().teamB
        
    let attackUp() = Match.isTeamAttackingUp side (getMatchState().period)
    let getRelPos pos = Tactics.getRelPos (getMatchState().pitch) (attackUp()) pos
    let getAbsPos pos = Tactics.getAbsPos (getMatchState().pitch) (attackUp()) pos
    let getAbsDir dir = Tactics.getAbsDir (attackUp()) dir
    let absUp() = TypedVector2<1>(0.0f, 1.0f) |> getAbsDir

    let waitUntilBallInPlay =
        task {
            return! env.WaitUntil <|
                fun () ->
                    match getMatchState().ball.inPlay with
                    | Ball.InPlay -> true
                    | _ -> false
        }

    let waitUntilBallEngaged =
        task {
            return! env.WaitUntil <|
                fun () ->
                    match getMatchState().ball.inPlay with
                    | Ball.CornerKick(Ball.CanKick, _, _)
                    | Ball.KickOff(Ball.CanKick, _)
                    | Ball.Penalty(Ball.CanKick, _) ->
                        true
                    | _ -> false
        }

    let prepareForKickOff =
        task {
            // Goal keeper goes to the goal
            (getAbsPos { x = 0.0f ; y = -1.0f }, absUp()) |> RunningTo |> assign 0

            // Field players place themselves according to the formation            
            let destinations =
                formation
                |> List.map (Tactics.transform 0.8f 0.25f 0.0f -0.25f)
                |> List.map getAbsPos
                // Field players stand out of the circle.
                |> List.map (fun v ->
                    let dist = v.Length
                    if dist < 9.15f<m> then
                        let v = 1.0f / dist * v
                        9.15f<m> * v
                    else
                        v)
                |> List.map (fun dest -> (dest, absUp()))
                |> Array.ofList

            // If the ball is ours, the two players closest to the ball go to it
            let ballIsOurs =
                match getMatchState().ball.inPlay with
                | Ball.KickOff(_, owner) when owner = side -> true
                | _ -> false

            if ballIsOurs then
                let _, ((player0, _), (player1, _)) =
                    destinations
                    |> Array.fold (fun (i, ((i0, d0), (i1, d1) as x)) (v, _) ->
                        let d = v.Length
                        let matches =
                            if d < d0 then
                                ((i, d), (i0, d0))
                            elif d < d1 then
                                ((i0, d0), (i, d))
                            else
                                x
                        (i + 1, matches)) (0, ((-1, 1000.0f<m>), (-1, 1000.0f<m>)))
                 
                if player0 < 0 || player0 >= destinations.Length ||
                    player1 < 0 || player1 >= destinations.Length then
                    failwith "Could not find two players to kick off the ball"

                destinations.[player0] <- TypedVector2<m>(-2.0f<m>, 0.0f<m>), absUp()
                destinations.[player1] <- TypedVector2<m>(2.0f<m>, 0.0f<m>), absUp()
            
                destinations
                |> Array.iteri(fun i v -> RunningTo v |> assign i)

                do! waitUntilBallEngaged

                PassingTo player1 |> assign player0

                do! waitUntilBallInPlay
                do! env.WaitUntil <|
                    fun () ->
                        let team = getTeam()
                        if player0 > 0 && player0 < team.onPitch.Length then
                            let player0 = team.onPitch.[player0]
                            match player0 with
                            | { activity = Player.Passing } -> true
                            | _ -> false
                        else
                            true
                
                FollowingTactic |> assign player0
            else
                destinations
                |> Array.iteri(fun i v -> RunningTo v |> assign i)

                do! waitUntilBallInPlay
        }

    let normalPlay =
        task {            
            return! env.WaitNextFrame()
        }

    let defendCorner =
        task {
            return ()
        }

    let kickCorner =
        task {
            return ()
        }

    let defendThrowIn =
        task {
            return ()
        }

    let throwIn =
        task {
            return ()
        }

    let defendFreeKick =
        task {
            return ()
        }

    let freeKick =
        task {
            return ()
        }

    let defendPenalty =
        task {
            return ()
        }

    let kickPenalty =
        task {
            return ()
        }

    let celebrate =
        task {
            return ()
        }

    let matchIsOver() =
        match getMatchState().period with
        | Match.MatchOver -> true
        | _ -> false

    let rec main() =
        task {
            match getMatchState() with
            | { period = Match.MatchOver } -> return ()
            | { ball = { inPlay = Ball.KickOff _ } } ->
                do! prepareForKickOff
                return! main()
            | _ ->
                do! normalPlay
                return! main()
        }

    main()


let actPlayerOnObjective side (matchState : Match.MatchState) objective (playerState : Player.State) =
    let ball = matchState.ball
    let team =
        match side with
        | Team.TeamA -> matchState.teamA.onPitch
        | Team.TeamB -> matchState.teamB.onPitch

    let ballPos2 = TypedVector2<m>(ball.pos.X, ball.pos.Y)
    let distToBall pos =
        pos - ballPos2
        |> TypedVector.len2

    let runToPos destination =
        let dir = destination - playerState.pos
        let dist = dir.Length
        if dist > 0.1f<m> then
            let dir = 1.0f / dist * dir
            { playerState with direction = dir; activity = Player.Standing; speed = Player.getRunSpeed playerState }
        else
            { playerState with activity = Player.Standing; speed = 0.0f<m/s> }

    match objective, playerState.activity with
    | _, Player.Fallen _
    | _, Player.Jumping _
    | _, Player.KeeperDive _
    | _, Player.Kicking _
    | _, Player.Tackling _ ->
        // Player unavailable, activity cannot be changed
        playerState
    | RunningTo (dest, dir), _ ->
        let newState = runToPos dest
        match newState with
        | { speed = 0.0f<m/s> } -> { newState with direction = dir }
        | _ -> newState
    | RunningToBall, _ ->
        runToPos ballPos2
    | PassingTo other, Player.Standing ->
        match distToBall playerState.pos with
        | x when x < Physics.controlMaxDistance ->
            { playerState with activity = Player.Trapping }
        | _ ->
            runToPos ballPos2
    | PassingTo other, Player.Trapping ->
        let dir =
            if other >= 0 && other < team.Length then
                team.[other].pos - playerState.pos
                |> TypedVector.tryNormalize2
            else None

        match dir with
        | None ->
            { playerState with activity = Player.Passing }
        | Some d ->
            { playerState with direction = d ; activity = Player.Passing }
    | _, Player.Passing ->
        { playerState with activity = Player.Standing }
    | _, _ ->
        playerState


(*
let assignObjectives side (gameState0 : Match.MatchState) (gameState1 : Match.MatchState) =
    let team0, team1 =
        match side with
        | Team.TeamA -> gameState0.teamA, gameState1.teamA
        | Team.TeamB -> gameState0.teamB, gameState1.teamB

    let attackUp = Match.isTeamAttackingUp side gameState1.period

    let getRelPos = Tactics.getRelPos gameState1.pitch attackUp

    let timeToBall player =
        let speed = Player.getRunSpeed player
        let vx = gameState1.ball.speed.X
        let vy = gameState1.ball.speed.Y
        let px = gameState1.ball.pos.X - player.pos.X
        let py = gameState1.ball.pos.Y - player.pos.Y

        let A = vx * vx + vy * vy - speed * speed
        let B = 2.0f * px * vx + 2.0f * py * vy
        let C = px * px + py * py

        let delta = B * B - 4.0f * A * C
        let t0, t1 =
            let inf = 1.0f<s> * System.Single.PositiveInfinity 
            match delta with
            | x when float32 x > 0.0f ->
                let x' = sqrt x
                let r1 = (-B - x')/(2.0f * A)
                let r2 = (-B + x')/(2.0f * A)                
                r1, r2
            | x when float32 x = 0.0f ->
                let r = -B / (2.0f * A)
                r, inf
            | _ -> inf, inf

        min t0 t1

    match gameState0.ball.inPlay, gameState1.ball.inPlay with
    | _, Ball.DeadBall _ ->
        team1.onPitch
        |> Array.map (fun _ -> FollowingTactic)
    
    | Ball.DeadBall _, Ball.LiveBall ->
        let runToBall, _ =
            team1.onPitch
            |> Seq.mapi (fun i player -> (i, timeToBall player))
            |> Seq.minBy snd

        team1.onPitch
        |> Array.mapi (fun i _ -> if i = runToBall then RunningToBall else FollowingTactic)

    | Ball.LiveBall, Ball.LiveBall ->
        let goalPos =
            match attackUp with
            | true -> TypedVector2<m>(gameState1.pitch.length / 2.0f, gameState1.pitch.width / 2.0f)
            | false -> TypedVector2<m>(gameState1.pitch.length / 2.0f, -gameState1.pitch.width / 2.0f)

        let distToBall =
            let pos2 = TypedVector2<m>(gameState1.ball.pos.X, gameState1.ball.pos.Y)
            fun pos ->
                TypedVector.len2 (pos2 - pos)

        let closestToBall, _ =
            team1.onPitch
            |> Seq.mapi (fun i player -> (i, timeToBall player))
            |> Seq.minBy snd

        let decidePass =
            let valueOfAttackingPlayer (player : Player.State) =        
                let timeToGoal =
                    TypedVector.len2(player.pos - goalPos) / Player.getRunSpeed player

                let distToBall = distToBall player.pos

                let distToBallScore =
                    let bestDist = 25.0f<m>
                    let span = 10.0f<m>
                    max 0.0f (1.0f - abs (distToBall - bestDist) / span)
        
                let timeToGoalScore =
                    let maxTime = 10.0f<s>
                    max 0.0f ((maxTime - timeToGoal) / maxTime)

                distToBallScore + timeToGoalScore

            fun pos ->
                let passTo, _ =
                    team1.onPitch
                    |> Seq.mapi (fun i player -> (i, valueOfAttackingPlayer player))
                    |> Seq.maxBy snd
                let targetPos = team1.onPitch.[passTo].pos
                if TypedVector.len2(pos - targetPos) > 30.0f<m> then
                    CrossingTo targetPos
                else
                    PassingTo passTo
            
        team1.onPitch
        |> Array.mapi (fun i player ->
            if i = closestToBall then
                if distToBall player.pos < 0.1f<m> then                    
                    let bestPass = decidePass player.pos
                    let targetPos =
                        match bestPass with
                        | PassingTo idx -> team1.onPitch.[idx].pos
                        | CrossingTo x -> x
                        | _ -> failwith "Unhandled pass type"
                    
                    let (|ShouldShoot|ShouldRun|ShouldPass|) (playerPos : TypedVector2<m>, targetPos : TypedVector2<m>) =
                        if TypedVector.len2 (goalPos - playerPos) < 20.0f<m> then
                            ShouldShoot
                        elif attackUp && targetPos.Y < playerPos.Y then
                            ShouldRun
                        elif not attackUp && targetPos.Y > playerPos.Y then
                            ShouldRun
                        else ShouldPass

                    match (player.pos, targetPos) with
                    | ShouldPass -> bestPass
                    | ShouldShoot -> ShootingAtGoal
                    | ShouldRun ->
                        let towardsGoal = goalPos - player.pos
                        let distToGoal = towardsGoal.Length
                        RunningWithBallTo (player.pos + 10.0f<m> / distToGoal * towardsGoal)
                        
                else
                    RunningToBall
            else
                FollowingTactic)
*)