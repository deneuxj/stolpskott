module CleverRake.StolpSkott.Tactics

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

// x and y live in (-1, +1)
// -1 for y is the home of team A, +1 the home of team B.
type PitchRelPos = { x : float32; y : float32 }

let getRelPos (pitch : Pitch.PitchTraits) isTeamAttackingUp (v : TypedVector2<m>) =
    let x = 2.0f * v.X / pitch.width
    let y = 2.0f * v.Y / pitch.length
    let k = if not isTeamAttackingUp then -1.0f else 1.0f
    let v = TypedVector.scale2(k, TypedVector2<1>(x, y))
    { x = v.X; y = v.Y }

let getAbsPos (pitch : Pitch.PitchTraits) isTeamAttackingUp pos =
    let x = 0.5f * pos.x * pitch.width
    let y = 0.5f * pos.y * pitch.length
    let k = if not isTeamAttackingUp then -1.0f else 1.0f
    k * TypedVector2<m>(x, y)

let getAbsDir isTeamAttackingUp (dir : TypedVector2<1>) =
    if isTeamAttackingUp then
        dir
    else
        -1.0f * dir

let getRelDir = getAbsDir

let transform sx sy tx ty pos =
    { x = pos.x * sx + tx
      y = pos.y * sy + ty }

let formation442 =
    [ (-1.0f, -0.5f); (-0.33f, -1.0f); (0.33f, -1.0f); (1.0f, -0.5f)
      (-1.0f, 0.0f); (-0.33f, 0.0f); (0.33f, 0.0f); (1.0f, 0.0f)
      (-0.33f, 1.0f); (0.33f, 1.0f)
    ]
    |> List.map (fun (x, y) -> { x = x; y = y })

let getThrowInFormation side y formation team (game : Match.MatchState) =    
    let x =
        match side with
        | Ball.Left -> -game.pitch.length / 2.0f
        | Ball.Right -> game.pitch.length / 2.0f

    let relPos = getRelPos game.pitch (Match.isTeamAttackingUp team game.period) (TypedVector2<m>(x, y))
    let baseY = max -1.0f (relPos.y - 0.5f)
    let topY = max 1.0f (relPos.y + 0.5f)
    let sy = (topY - baseY) / 2.0f
    let ty = (topY + baseY) / 2.0f
    let sx = 0.5f
    let tx =
        if relPos.x > 0.0f then
            0.5f
        else
            -0.5f

    formation
    |> List.map (transform sx sy tx ty)

let getCornerDefenseFormation formation team (game : Match.MatchState) =
    let sx = 0.5f
    let tx = 0.5f
    let sy = 0.5f
    let ty = -0.5f
    
    formation
    |> List.map (transform sx sy tx ty)

let getCornerAttackFormation formation team (game : Match.MatchState) =
    let sx = 0.5f
    let tx = 0.5f
    let sy = 0.5f
    let ty = 0.5f
    
    formation
    |> List.map (transform sx sy tx ty)

let getPlayFormation formation team (game : Match.MatchState) =
    let isAttackingUp = Match.isTeamAttackingUp team game.period
    let ballPos2 = TypedVector2<m>(game.ball.pos.X, game.ball.pos.Y)
    let ballRelPos = ballPos2 |> getRelPos game.pitch isAttackingUp

    let baseY, topY =
        if ballRelPos.y < -0.8f then
            ballRelPos.y, 0.0f
        elif ballRelPos.y < 0.5f then
            max (ballRelPos.y - 0.5f) -0.8f, min (ballRelPos.y + 0.5f) 0.5f
        else
            0.0f, ballRelPos.y
    
    let sx = 0.5f
    let tx =
        if ballRelPos.x < -0.33f then
            -sx
        elif ballRelPos.x > 0.33f then
            sx
        else
            0.0f

    let sy = (topY - baseY) / 2.0f
    let ty = (topY + baseY) / 2.0f

    formation
    |> List.map (transform sx sy tx ty)

let getKickInFormation formation team (game : Match.MatchState) =
    let sx = 0.5f
    let tx = 0.5f
    let sy = 0.5f
    let ty = 0.0f
    
    formation
    |> List.map (transform sx sy tx ty)
