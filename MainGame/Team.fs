module CleverRake.StolpSkott.Team

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units
open Microsoft.Xna.Framework

type TeamSide = TeamA | TeamB

type MatchPeriod =
    | FirstHalf
    | FirstHalfExtra
    | SecondHalf
    | SecondHalfExtra
    | TieFirstHalf
    | TieSecondHalf
    | TieExtra

let isTeamAttackingUp side period =
    let inverted =
        match period with
        | FirstHalf
        | FirstHalfExtra
        | TieFirstHalf -> false
        | _ -> true

    match inverted, side with
    | true, TeamB
    | false, TeamA -> true
    | _ -> false

// x and y live in (-1, +1)
// -1 for y is the home of team A, +1 the home of team B.
type PitchRelPos = { x : float32; y : float32 }

let transform sx sy tx ty pos =
    { x = pos.x * sx + tx
      y = pos.y * sy + ty }

let penaltyBoxWidth = 40.3f<m>
let penaltyBoxHeight = 16.5f<m>
let goalBoxWidth = penaltyBoxWidth - 22.0f<m>
let goalBoxHeight = 5.5f<m>

type PitchTraits =
    { width : float32<m>
      length : float32<m>
    }

let boundBall pitch (ball : Ball.State) =
    let speed =
        if abs ball.pos.X > 5.0f<m> + pitch.width / 2.0f ||
           abs ball.pos.Y > 5.0f<m> + pitch.length / 2.0f then
            TypedVector3<m/s>(0.0f<_>, 0.0f<_>, ball.speed.Z)
        else
            ball.speed

    { ball with speed = speed }

type GameState =
    { teamA : Team
      teamB : Team
      ball : Ball.State
      pitch : PitchTraits
      period : MatchPeriod
      periodTime : float32<s>
    }

and Team =
    { onPitch : Player.State[]
      tactics : GameState -> PitchRelPos[]
    }

let getRelPos pitch isTeamAttackingUp (v : TypedVector2<m>) =
    let x = v.X / pitch.width
    let y = v.Y / pitch.length
    let k = if not isTeamAttackingUp then -1.0f else 1.0f
    let v = TypedVector.scale2(k, TypedVector2<1>(x, y))
    { x = v.X; y = v.Y }

let formation442 =
    [ (-1.0f, -0.5f); (-0.33f, -1.0f); (0.33f, -1.0f); (1.0f, -0.5f)
      (-1.0f, 0.0f); (-0.33f, 0.0f); (0.33f, 0.0f); (1.0f, 0.0f)
      (-0.33f, 1.0f); (0.33f, 1.0f)
    ]
    |> List.map (fun (x, y) -> { x = x; y = y })

let tactics formation side game =
    let getRelPos = getRelPos game.pitch (isTeamAttackingUp side game.period)
    let ballPos = TypedVector2(game.ball.pos.X, game.ball.pos.Y) |> getRelPos
    let isDefending = 
        match side, game.ball.inPlay with
        | TeamA, Ball.DeadBallTeamB
        | TeamB, Ball.DeadBallTeamA
        | TeamA, Ball.TrappedByKeeperB
        | TeamB, Ball.TrappedByKeeperA
        | _, Ball.InPlay when ballPos.y < 0.0f -> true
        | _ -> false

    let (|LeftCorridor|CenterCorridor|RightCorridor|) ballPos =
        if ballPos.x < -0.33f then LeftCorridor
        elif ballPos.x > 0.33f then RightCorridor
        else CenterCorridor

    let tx, sx = 
        // Horizontal spread factor: tight when defending.
        let sx_factor =
            if isDefending then 0.5f else 0.8f

        // Limit spread to avoid having players fall off the pitch
        let max_sx =
            match ballPos with
            | LeftCorridor | RightCorridor -> 0.66f
            | CenterCorridor -> 1.0f

        let tx =
            match ballPos with
            | LeftCorridor -> -0.33f
            | RightCorridor -> 0.33f
            | CenterCorridor -> 0.0f

        tx, sx_factor * max_sx

    let ty, sy =
        match side, game.ball.inPlay with
        | _, Ball.InPlay
        | _, Ball.OutOfPitch ->
            let baseY =
                // defending close to our goal. Back line in line with the ball
                if ballPos.y < -0.5f then ballPos.y
                // defensive midfield play. Back line is fixed.
                elif ballPos.y < 0.0f then -0.5f
                // offensive midfield play. Back line moves up to the middle.
                elif ballPos.y < 0.5f then ballPos.y - 0.5f
                // offensive play, close to the opponent's goal. Back line stays on the middle line.
                else 0.0f

            let topY =
                // Attackers follow the opponent's back line.
                let opponents =
                    match side with
                    | TeamA -> game.teamB
                    | TeamB -> game.teamA
                opponents.onPitch
                |> Seq.map (fun player -> player.pos |> getRelPos)
                |> Seq.map (fun { y = y } -> y )
                |> Seq.max
            (baseY + topY) / 2.0f, (topY - baseY) / 2.0f

        // Trapped by own keeper
        | TeamA, Ball.TrappedByKeeperA
        | TeamB, Ball.TrappedByKeeperB ->
            0.0f, 0.5f

        // Trapped by the opponent's keeper
        | TeamA, Ball.TrappedByKeeperB
        | TeamB, Ball.TrappedByKeeperA ->
            0.0f, 0.5f

        // Dead ball, our team.
        | TeamA, Ball.DeadBallTeamA
        | TeamB, Ball.DeadBallTeamB ->
            let baseY, topY =
                // Free kick, penalty or corner in the opponent's side
                if ballPos.y > 0.0f then
                    0.0f, 0.9f
                else // from within our side.
                    ballPos.y, ballPos.y + 0.9f
            (baseY + topY) / 2.0f, (topY - baseY) / 2.0f

        // Dead ball, opponents
        | TeamB, Ball.DeadBallTeamA
        | TeamA, Ball.DeadBallTeamB ->
            let baseY, topY =
                // Corner or free kick close to the line
                if ballPos.y < 0.1f then
                    0.0f, 1.0f
                elif ballPos.y < 0.0f then
                    0.1f, 1.0f
                else
                    ballPos.y - 0.9f, ballPos.y
            (baseY + topY) / 2.0f, (topY - baseY) / 2.0f

    formation
    |> List.map (fun { x = x; y = y } -> { x = tx + sx * x; y = ty + sy * y })

type AiPlayerObjective =
    | RunningToBall
    | RunningWithBallTo of TypedVector2<m>
    | FollowingTactic
    | RunningTo of TypedVector2<m>
    | PassingTo of int
    | CrossingTo of TypedVector2<m>
    | ShootingAtGoal

let assignObjectives side gameState0 gameState1 =
    let team0, team1 =
        match side with
        | TeamA -> gameState0.teamA, gameState1.teamA
        | TeamB -> gameState0.teamB, gameState1.teamB

    let attackUp = isTeamAttackingUp side gameState1.period

    let getRelPos = getRelPos gameState1.pitch attackUp

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

    let (|DeadBall|LiveBall|) = function
        | Ball.InPlay -> LiveBall
        | Ball.DeadBallTeamA
        | Ball.DeadBallTeamB
        | Ball.OutOfPitch
        | Ball.TrappedByKeeperA
        | Ball.TrappedByKeeperB -> DeadBall

    match gameState0.ball.inPlay, gameState1.ball.inPlay with
    | _, DeadBall ->
        team1.onPitch
        |> Array.map (fun _ -> FollowingTactic)
    
    | DeadBall, LiveBall ->
        let runToBall, _ =
            team1.onPitch
            |> Seq.mapi (fun i player -> (i, timeToBall player))
            |> Seq.minBy snd

        team1.onPitch
        |> Array.mapi (fun i _ -> if i = runToBall then RunningToBall else FollowingTactic)

    | LiveBall, LiveBall ->
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
                        