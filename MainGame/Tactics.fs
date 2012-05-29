module CleverRake.StolpSkott.Tactics

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

// x and y live in (-1, +1)
// -1 for y is the home of team A, +1 the home of team B.
type PitchRelPos = { x : float32; y : float32 }

let getRelPos (pitch : Pitch.PitchTraits) isTeamAttackingUp (v : TypedVector2<m>) =
    let x = v.X / pitch.width
    let y = v.Y / pitch.length
    let k = if not isTeamAttackingUp then -1.0f else 1.0f
    let v = TypedVector.scale2(k, TypedVector2<1>(x, y))
    { x = v.X; y = v.Y }

let transform sx sy tx ty pos =
    { x = pos.x * sx + tx
      y = pos.y * sy + ty }

let formation442 =
    [ (-1.0f, -0.5f); (-0.33f, -1.0f); (0.33f, -1.0f); (1.0f, -0.5f)
      (-1.0f, 0.0f); (-0.33f, 0.0f); (0.33f, 0.0f); (1.0f, 0.0f)
      (-0.33f, 1.0f); (0.33f, 1.0f)
    ]
    |> List.map (fun (x, y) -> { x = x; y = y })

let tactics formation side (game : Match.MatchState) =
    let getRelPos = getRelPos game.pitch (Match.isTeamAttackingUp side game.period)
    let ballPos = TypedVector2(game.ball.pos.X, game.ball.pos.Y) |> getRelPos
    let isDefending = 
        match side, game.ball.inPlay with
        | Team.TeamA, Ball.DeadBall (Some Team.TeamB)
        | Team.TeamB, Ball.DeadBall (Some Team.TeamA)
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
        | _, Ball.DeadBall None
        | _, Ball.LiveBall ->
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
                    | Team.TeamA -> game.teamB
                    | Team.TeamB -> game.teamA
                opponents.onPitch
                |> Seq.map (fun player -> player.pos |> getRelPos)
                |> Seq.map (fun { y = y } -> y )
                |> Seq.max
            (baseY + topY) / 2.0f, (topY - baseY) / 2.0f

        // Trapped by own keeper
        | Team.TeamA, Ball.TrappedByKeeper Team.TeamA
        | Team.TeamB, Ball.TrappedByKeeper Team.TeamB ->
            0.0f, 0.5f

        // Trapped by the opponent's keeper
        | Team.TeamA, Ball.TrappedByKeeper Team.TeamB
        | Team.TeamB, Ball.TrappedByKeeper Team.TeamA ->
            0.0f, 0.5f

        // Dead ball, our team.
        | Team.TeamA, Ball.DeadBall (Some Team.TeamA)
        | Team.TeamB, Ball.DeadBall (Some Team.TeamB) ->
            let baseY, topY =
                // Free kick, penalty or corner in the opponent's side
                if ballPos.y > 0.0f then
                    0.0f, 0.9f
                else // from within our side.
                    ballPos.y, ballPos.y + 0.9f
            (baseY + topY) / 2.0f, (topY - baseY) / 2.0f

        // Dead ball, opponents
        | Team.TeamB, Ball.DeadBall (Some Team.TeamA)
        | Team.TeamA, Ball.DeadBall (Some Team.TeamB) ->
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
