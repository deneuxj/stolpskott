module CleverRake.StolpSkott.Rendering

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils
open CleverRake.StolpSkott.Units

[<Measure>] type px // Pixels

type Resources =
    {
        grassLight : Texture2D
        grassDark : Texture2D
        goalUpper : Texture2D
        goalLower : Texture2D
        ball : Texture2D
        ballShadow : Texture2D
        playerSprites : Texture2D
        playerShadows : Texture2D
        whiteLine : Texture2D
    }

let ratio = 2048.0f<px> / 80.0f<m>

let worldToScreen
    (screenWidth : float32<px>, screenHeight : float32<px>)
    (viewWidth : float32<m>, viewHeight : float32<m>)
    (viewX : float32<m>, viewY : float32<m>)
    (x : float32<m>, y : float32<m>) : float32<px> * float32<px> =

    let x' = ((x - viewX) / viewWidth + 0.5f) * screenWidth
    let y' = (-(y - viewY) / viewHeight + 0.5f) * screenHeight
    (x', y')

let inline cmp (o1 : #System.IComparable<'T>) (o2 : #System.IComparable<'T>) =
    o1.CompareTo(o2)

type SpriteType =
    | Ball of Ball.State
    | Player of Player.State * Team.TeamSide
    | GoalUpper
    | GoalLower
with
    static member Compare(this, other) =
        match this, other with
        | Ball b1, Ball b2 -> cmp b1.pos.Y b2.pos.Y
        | Ball ball, Player(player, _) ->
            if player.traits.length < ball.pos.Z then
                -1
            else
                cmp player.pos.Y ball.pos.Y
        | Ball ball, GoalUpper
        | Ball ball, GoalLower ->
            if ball.pos.Z < Physics.goalHeight then
                -1
            elif ball.pos.Z = Physics.goalHeight then
                0
            else
                +1
        | Player(player1, _), Player(player2, _) ->
            cmp player2.pos.Y player1.pos.Y
        | Player _, GoalUpper -> -1
        | Player _, GoalLower -> +1
        | Player _, Ball _ ->
            SpriteType.Compare(other, this)
        | GoalUpper, Ball _
        | GoalLower, Ball _  ->
            SpriteType.Compare(other, this)
        | GoalUpper, Player _ -> +1
        | GoalLower, Player _ -> -1
        | GoalUpper, GoalUpper -> 0
        | GoalUpper, GoalLower -> -1
        | GoalLower, GoalLower -> 0
        | GoalLower, GoalUpper -> +1

let renderGrass (sb : SpriteBatch) (viewWidth : float32<m>, viewHeight : float32<m>) (lightGrass : Texture2D) (darkGrass : Texture2D) (viewX : float32<m>, viewY : float32<m>) =
    let darkStripHeight = 1.0f<px> * float32 darkGrass.Height / ratio
    let darkStripWidth = 1.0f<px> * float32 darkGrass.Width / ratio
    let lightStripHeight = 1.0f<px> * float32 lightGrass.Height / ratio
    let lightStripWidth = 1.0f<px> * float32 lightGrass.Width / ratio

    let viewLower = viewY - 0.5f * viewHeight
    let viewUpper = viewY + 0.5f * viewHeight

    let upperStrips =
        Seq.unfold (fun (y1, stripIsDark) ->
            let y0, y1 = y1, y1 + (if not stripIsDark then darkStripHeight else lightStripHeight)
            if y0 > viewUpper then
                None
            else
                let strip = (y0, y1, not stripIsDark)
                Some (strip, (y1, not stripIsDark)))
            (0.0f<m>, true)

    let lowerStrips =
        Seq.unfold (fun (y0, stripIsDark) ->
            let y0, y1 = y0 - (if not stripIsDark then darkStripHeight else lightStripHeight), y0
            if y1 < viewLower then
                None
            else
                let strip = (y0, y1, not stripIsDark)
                Some (strip, (y0, not stripIsDark)))
            (0.0f<m>, false)

    let worldToScreen = worldToScreen (ratio * viewWidth, ratio * viewHeight) (viewWidth, viewHeight) (viewX, viewY)

    Seq.append upperStrips lowerStrips
    |> Seq.iter (fun (y0, y1, stripIsDark) ->
        let x, y =
            if stripIsDark then
                (-darkStripWidth * 0.5f, y1)
            else
                (-lightStripWidth * 0.5f, y1)
            |> worldToScreen

        sb.Draw((if stripIsDark then darkGrass else lightGrass), Vector2(x / 1.0f<px>, y / 1.0f<px>), Color.White))


let renderLines
    (sb : SpriteBatch)
    (viewWidth : float32<m>, viewHeight : float32<m>)
    (whiteLine : Texture2D)
    (pitch : Pitch.PitchTraits)
    (viewX : float32<m>, viewY : float32<m>) =

    let worldToScreen = worldToScreen (ratio * viewWidth, ratio * viewHeight) (viewWidth, viewHeight) (viewX, viewY)
    let lineHalfWidth = 0.5f * 0.14f<m>
    let drawHLine (x0, y0) (x1) =
        let x0, x1 = min x0 x1, max x0 x1
        let x0', y0' = worldToScreen (x0 - lineHalfWidth, y0 - lineHalfWidth)
        let x1', y1' = worldToScreen (x1 + lineHalfWidth, y0 + lineHalfWidth)
        sb.Draw(whiteLine, Rectangle(int x0', int y1', int (x1' - x0'), int (y0' - y1')), Color.White)

    let drawVLine (x0, y0) (y1) =
        let y0, y1 = min y0 y1, max y0 y1
        let x0', y0' = worldToScreen (x0 - lineHalfWidth, y0 - lineHalfWidth)
        let x1', y1' = worldToScreen (x0 + lineHalfWidth, y1 + lineHalfWidth)
        sb.Draw(whiteLine, Rectangle(int x0', int y1', int (x1' - x0'), int (y0' - y1')), Color.White)

    // Outer lines, middle line.
    let x0 = -pitch.width / 2.0f
    let x1 = -x0
    let y0 = -pitch.length / 2.0f
    let y1 = -y0

    drawHLine (x0, y0) x1
    drawHLine (x0, 0.0f<m>) x1
    drawHLine (x0, y1) x1

    drawVLine (x0, y0) y1
    drawVLine (x1, y0) y1

    // upper penalty box
    let x0 = -Pitch.penaltyBoxWidth / 2.0f
    let x1 = -x0
    let y0 = pitch.length / 2.0f
    let y1 = y0 - Pitch.penaltyBoxHeight

    drawVLine (x0, y0) y1
    drawVLine (x1, y0) y1
    drawHLine (x0, y1) x1

    // lower penalty box
    let y0 = -pitch.length / 2.0f
    let y1 = y0 + Pitch.penaltyBoxHeight

    drawVLine (x0, y0) y1
    drawVLine (x1, y0) y1
    drawHLine (x0, y1) x1

    // upper six-yards box
    let x0 = -Pitch.goalBoxWidth / 2.0f
    let x1 = -x0
    let y0 = pitch.length / 2.0f
    let y1 = y0 - Pitch.goalBoxHeight

    drawVLine (x0, y0) y1
    drawVLine (x1, y0) y1
    drawHLine (x0, y1) x1

    // lower six-yards box
    let x0 = -Pitch.goalBoxWidth / 2.0f
    let x1 = -x0
    let y0 = -pitch.length / 2.0f
    let y1 = y0 + Pitch.goalBoxHeight

    drawVLine (x0, y0) y1
    drawVLine (x1, y0) y1
    drawHLine (x0, y1) x1

let sin45m = sin (0.5f * MathHelper.PiOver4)
let sin45M = sin (1.5f * MathHelper.PiOver4)
let sin45 = sin (MathHelper.PiOver4)
let discretizeTrigo x =
    let x' = abs x
    let sx =
        if x' > sin45M then
            1.0f
        elif x' > sin45m then
            sin45
        else
            0.0f
    if x < 0.0f then
        -sx
    else
        sx
    
let runningFrames = [| 0 .. 3 |]
let tacklingFrames = Array.concat [[| 3 .. 8 |] ; [| 8;8;8;8;8 |]]
let jumpingFrames = Array.concat [[|3|] ; [| 9 .. 13 |]]

let getFrame frames (kf : float32<kf>) =
    let num = Array.length frames
    let idx =
        kf * (1.0f + float32 num)
        |> int
        |> min (num - 1)
        |> max 0
    frames.[idx]

let renderSprites (sb : SpriteBatch) (viewWidth, viewHeight) ball playerSprites goalUpper goalLower (pitch : Pitch.PitchTraits) (viewX, viewY) sprites =
    let worldToScreen = worldToScreen (ratio * viewWidth, ratio * viewHeight) (viewWidth, viewHeight) (viewX, viewY)
    
    for s in sprites do
        match s with
        | Ball b ->
            let scale = 1.0f + b.pos.Z / 2.5f<m>
            let scaledRadius = scale * Ball.ballRadius
            let x = b.pos.X - scaledRadius
            let y = b.pos.Y + scaledRadius
            let x, y = worldToScreen (x, y)
            let w = 2.0f * scaledRadius * ratio |> int
            sb.Draw(ball, Rectangle(int x, int y, w, w), Color.White)

        | Player(player, side) ->
            let x = player.pos.X
            let y = player.pos.Y
            let playerRadius = 0.3f<m>
            let x, y = worldToScreen (x, y)
            let w = 2.0f * playerRadius * ratio |> int
            let sw = 32
            let sx, sy =
                match player.activity with
                | Player.Standing _ ->
                    sw * runningFrames.[player.runningFrame], 0
                | Player.Tackling(kf, _) ->
                    let frame = getFrame tacklingFrames kf
                    sw * frame, 0
                | Player.Jumping kf ->
                    let frame = getFrame jumpingFrames kf
                    sw * frame, 0
                | _ -> 0, 0
            let dx = player.direction.X |> discretizeTrigo
            let dy = player.direction.Y |> discretizeTrigo
            let angle = atan2 dx dy
            sb.Draw(playerSprites, Rectangle(int x, int y, w, w), System.Nullable(Rectangle(sx, sy, sw, sw)), Color.White, angle, Vector2(16.0f, 16.0f), SpriteEffects.None, 0.0f)

        | GoalUpper ->
            let x, y = (-Physics.goalWidth / 2.0f, pitch.length / 2.0f) |> worldToScreen
            sb.Draw(goalUpper, Vector2(x / 1.0f<px>, (y - 64.0f<px>) / 1.0f<px>), Color.White)

        | GoalLower ->
            let x, y = (-Physics.goalWidth / 2.0f, -pitch.length / 2.0f) |> worldToScreen
            sb.Draw(goalLower, Vector2(x / 1.0f<px>, (y - 33.0f<px>) / 1.0f<px>), Color.White)


let testRender(gd : GraphicsDevice, sb : SpriteBatch, darkGrass, lightGrass, line, ball, player, goalUpper, goalLower, pitch, allPlayers, ballState : Ball.State, (x, y)) =
    let viewSize =
        let viewHeight = (1.0f<px> * float32 gd.Viewport.Height) / ratio
        let viewWidth = (1.0f<px> * float32 gd.Viewport.Width) / ratio
        (viewWidth, viewHeight)
    
    //let x, y = ballState.pos.X, ballState.pos.Y

    sb.Begin(SpriteSortMode.Immediate, BlendState.AlphaBlend)
    try
        renderGrass sb viewSize darkGrass lightGrass (x, y)
        renderLines sb viewSize line pitch (x, y)
        let sprites =
            [| yield GoalUpper
               yield GoalLower
               for (side, playerState) in allPlayers do
                yield Player(playerState, side)
               yield Ball(ballState) |]
        Array.sortInPlaceWith (fun this other -> SpriteType.Compare(this, other)) sprites
        renderSprites sb viewSize ball player goalUpper goalLower pitch (x, y) sprites
    finally
        sb.End()


let render renderPlayerShadows renderBallShadow renderGoalShadows (sb : SpriteBatch) viewSize resources (state : Match.MatchState) : unit =
    let viewPos = 
        let viewX = state.ball.pos.X |> max (-state.pitch.width * 0.5f) |> min (state.pitch.width * 0.5f)
        let viewY = state.ball.pos.Y |> max (-state.pitch.length * 0.5f) |> min (state.pitch.length * 0.5f)
        (viewX, viewY)

    let sprites =
        [|
            for player in state.teamA.onPitch do
                yield Player(player, Team.TeamA)
            for player in state.teamB.onPitch do
                yield Player(player, Team.TeamB)
            yield GoalUpper
            yield GoalLower
            yield Ball(state.ball)
        |]

    Array.sortInPlaceWith (fun this other -> SpriteType.Compare(this, other)) sprites

    try
        sb.Begin()
        renderGrass sb viewSize resources.grassLight resources.grassDark viewPos
        renderLines sb viewSize resources.whiteLine state.pitch viewPos
        renderPlayerShadows sb viewSize resources.playerShadows viewPos (Array.concat [state.teamA.onPitch ; state.teamB.onPitch])
        renderBallShadow sb viewSize resources.ballShadow viewPos state.ball.pos
        //renderGoalShadows sb viewSize resources.goalShadows viewPos
        renderSprites sb viewSize resources.ball resources.playerSprites resources.goalUpper resources.goalLower state.pitch viewPos sprites
    finally
        sb.End()