module CleverRake.StolpSkott.PlayerAi

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

type AiPlayerObjective =
    | RunningToBall
    | RunningWithBallTo of TypedVector2<m>
    | FollowingTactic
    | RunningTo of TypedVector2<m>
    | PassingTo of int
    | CrossingTo of TypedVector2<m>
    | ShootingAtGoal

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
