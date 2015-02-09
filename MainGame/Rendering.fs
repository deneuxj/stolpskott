module CleverRake.StolpSkott.Rendering

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Audio
open Microsoft.Xna.Framework
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils
open CleverRake.StolpSkott.Units
open CleverRake.XnaUtils.Pi

// Pixels
[<Measure>] type px

// Some animations are available at 3 angles, depending on the direction the
// character is facing: up, down and right.
// Animations in the left direction use a mirrored version of the animations to
// the right direction.
// Animations in the diagonal directions use up and down rotated
// +/- 45 degrees.
type StripDirection =
    | Up
    | Down
    | Right

// Textures, sound effects... used in render functions.
type Resources =
    {
        grassLight : Texture2D
        grassDark : Texture2D
        goalUpper : Texture2D
        goalLower : Texture2D
        ball : Texture2D
        ballShadow : Texture2D
        playerRunUp : Texture2D
        playerRunDown: Texture2D
        playerRunRight: Texture2D
        playerTackle: Texture2D
        playerJumpUp : Texture2D
        playerJumpDown : Texture2D
        playerJumpRight : Texture2D
        keeperDiveUp : Texture2D
        keeperDiveDown : Texture2D
        whiteLine : Texture2D
        ballKick : SoundEffect
        colorChange : Effect;
    }

// Ratio pixels/world length units
let ratio = 2048.0f<px> / 80.0f<m>

// Convert world coordinates to screen coordinates
let worldToScreen
    (screenWidth : float32<px>, screenHeight : float32<px>)
    (viewWidth : float32<m>, viewHeight : float32<m>)
    (viewX : float32<m>, viewY : float32<m>)
    (x : float32<m>, y : float32<m>) : float32<px> * float32<px> =

    let x' = ((x - viewX) / viewWidth + 0.5f) * screenWidth
    let y' = (-(y - viewY) / viewHeight + 0.5f) * screenHeight
    (x', y')

// Shortcut to compare IComparable(T) instances for a given T.
let inline cmp (o1 : #System.IComparable<'T>) (o2 : #System.IComparable<'T>) =
    o1.CompareTo(o2)

// Type used to compute the order in which sprites are rendered.
type SpriteType =
    | Ball of Ball.State
    | Player of Player.State * Team.TeamSide
    | GoalUpper
    | GoalLower
with
    // Sorting of sprite types to avoid overdraw bugs.
    // We draw objects that are high on the screen first
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

// Render the grass strips
let renderGrass
    (sb : SpriteBatch)
    (viewWidth : float32<m>, viewHeight : float32<m>)
    (lightGrass : Texture2D)
    (darkGrass : Texture2D)
    (viewX : float32<m>, viewY : float32<m>) =

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

// Render the lines using provided functions.
// This is used both for the pitch and the radar.
let renderPitchLines drawHLine drawVLine (pitch : Pitch.PitchTraits) =
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

// Render the pitch lines
let renderLines
    (sb : SpriteBatch)
    (viewWidth : float32<m>, viewHeight : float32<m>)
    (whiteLine : Texture2D)
    (pitch : Pitch.PitchTraits)
    (viewX : float32<m>, viewY : float32<m>) =

    let worldToScreen = worldToScreen (ratio * viewWidth, ratio * viewHeight) (viewWidth, viewHeight) (viewX, viewY)
    let lineHalfWidth = 0.5f * 0.14f<m>
    let brightness = 0.9f
    let color = new Color(brightness, brightness, brightness, 0.5f)
    let drawHLine (x0, y0) (x1) =
        let x0, x1 = min x0 x1, max x0 x1
        let x0', y0' = worldToScreen (x0 - lineHalfWidth, y0 - lineHalfWidth)
        let x1', y1' = worldToScreen (x1 + lineHalfWidth, y0 + lineHalfWidth)
        sb.Draw(whiteLine, Rectangle(int x0', int y1', int (x1' - x0'), int (y0' - y1')), color)

    let drawVLine (x0, y0) (y1) =
        let y0, y1 = min y0 y1, max y0 y1
        let x0', y0' = worldToScreen (x0 - lineHalfWidth, y0 - lineHalfWidth)
        let x1', y1' = worldToScreen (x0 + lineHalfWidth, y1 + lineHalfWidth)
        sb.Draw(whiteLine, Rectangle(int x0', int y1', int (x1' - x0'), int (y0' - y1')), color)

    renderPitchLines drawHLine drawVLine pitch

// Render the transparent radar
let renderRadar
    (sb : SpriteBatch)
    (radarX : float32<px>, radarY : float32<px>)
    (radarWidth : float32<px>, radarHeight : float32<px>)
    (whiteLine : Texture2D)
    (pitch : Pitch.PitchTraits)
    (teamA : TypedVector2<m>[])
    (teamB : TypedVector2<m>[])
    (ball : TypedVector3<m>) =

    let worldToScreen (x, y) =
        let x' = radarWidth * (1.0f + 2.0f * (x / pitch.width))
        let y' = radarHeight * (1.0f - 2.0f * (y / pitch.length))
        x', y'

    let alpha = 1.0f
    let color = new Color(0.0f, 0.0f, 0.0f, alpha)
    let drawHLine (x0, y0) (x1) =
        let x0, x1 = min x0 x1, max x0 x1
        let x0', y' = worldToScreen (x0, y0)
        let x1', _ = worldToScreen (x1, y0)
        sb.Draw(whiteLine, Rectangle(int x0', int y', int (x1' - x0'), 1), color)

    let drawVLine (x0, y0) (y1) =
        let y0, y1 = min y0 y1, max y0 y1
        let x', y0' = worldToScreen (x0, y0)
        let _, y1' = worldToScreen (x0, y1)
        sb.Draw(whiteLine, Rectangle(int x', int y1', 1, int (y0' - y1')), color)

    renderPitchLines drawHLine drawVLine pitch

    let renderTeam color (players : TypedVector2<m>[]) =
        for pos in players do
            let x', y' = worldToScreen (pos.X, pos.Y)
            sb.Draw(whiteLine, Rectangle(int x' - 1, int y' - 1, 3, 3), color)

    renderTeam (Color(0.0f, 0.0f, 1.0f, alpha)) teamA 
    renderTeam (Color(1.0f, 0.0f, 0.0f, alpha)) teamB 

    let x', y' = worldToScreen (ball.X, ball.Y)
    sb.Draw(whiteLine, Rectangle(int x', int y', 2, 2), Color(1.0f, 1.0f, 1.0f, alpha))


let sin45m = sin (0.5f * MathHelper.PiOver4)
let sin45M = sin (1.5f * MathHelper.PiOver4)
let sin45 = sin (MathHelper.PiOver4)
// Discretize an angle in 45-degree steps.
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

/// Get the strip, rotation and horizontal mirror for a given direction.
let getSpriteEffects (x, y) =
    let (|Pos|Neg|Zero|) x =
        if x = 0.0f then
            Zero
        elif x < 0.0f then
            Neg
        else
            Pos

    match (discretizeTrigo x, discretizeTrigo y) with
    | Pos, Zero -> Right, 0.0f, SpriteEffects.None
    | Neg, Zero -> Right, 0.0f, SpriteEffects.FlipHorizontally
    | Zero, Pos -> Up, 0.0f, SpriteEffects.None
    | Zero, Neg -> Down, 0.0f, SpriteEffects.None
    | Pos, Pos -> Up, (π/4.0f), SpriteEffects.None
    | Pos, Neg -> Down, -(π/4.0f), SpriteEffects.FlipHorizontally
    | Neg, Pos -> Up, (π/4.0f), SpriteEffects.FlipHorizontally
    | Neg, Neg -> Down, (π/4.0f), SpriteEffects.None
    | Zero, Zero -> Up, 0.0f, SpriteEffects.None

// Identify the sprite strip based on the activity of a player.
let (|Running|Jumping|Tackling|Diving|) =
    function
    | Player.Crossing
    | Player.Kicking _
    | Player.Passing
    | Player.Standing
    | Player.Trapping
    | Player.Stumbling _ -> Running
    | Player.Jumping _ -> Jumping
    | Player.Tackling _ -> Tackling
    | Player.KeeperDive _ -> Diving
    | Player.Fallen _ -> Tackling
    
// Distance from spine to outer point of the shoulder    
let playerRadius = 0.32f<m>

// Render the ball, the players and the goals.
let renderSprites
    (sb : SpriteBatch)
    (viewWidth, viewHeight)
    (resources: Resources)
    (period : Match.MatchPeriod)
    (pitch : Pitch.PitchTraits)
    (viewX, viewY)
    sprites =
    let worldToScreen = worldToScreen (ratio * viewWidth, ratio * viewHeight) (viewWidth, viewHeight) (viewX, viewY)
    
    // Vertical displacement due to the slightly tipped view angle
    // The lower goal sprite is drawed in such a way that the length of a post
    // takes half the height of the sprite.
    let elevation = 1.0f<px> * (float32 <| resources.goalLower.Height / 2) / Physics.goalHeight

    for s in sprites do
        match s with
        | Ball b ->
            let scale = 1.0f + b.pos.Z / 2.5f<m>
            let scaledRadius = scale * Ball.ballRadius
            let x = b.pos.X - scaledRadius
            let y = b.pos.Y + scaledRadius
            let x, y = worldToScreen (x, y)
            let w = 2.0f * scaledRadius * ratio |> int
            try
                sb.Begin()
                sb.Draw(resources.ball, Rectangle(int x, int (y - elevation * b.pos.Z), w, w), Color.White)
            finally
                sb.End()

        | Player(player, side) ->
            let x = player.pos.X
            let y = player.pos.Y
            let x, y = worldToScreen (x, y)
            let w = 2.0f * playerRadius * ratio |> int
            let stripDirection, angle, effect =
                match player.activity, getSpriteEffects (player.direction.X, player.direction.Y) with
                | Diving, (_, _, fx) -> StripDirection.Right, 0.0f, fx
                | _, x -> x
            let strip =
                match player.activity, stripDirection with
                | Running, StripDirection.Right -> resources.playerRunRight
                | Running, StripDirection.Down -> resources.playerRunDown
                | Running, StripDirection.Up -> resources.playerRunUp
                | Jumping, StripDirection.Right -> resources.playerJumpRight
                | Jumping, StripDirection.Down -> resources.playerJumpDown
                | Jumping, StripDirection.Up -> resources.playerJumpUp
                | Tackling, _ -> resources.playerTackle
                | Diving, _ when Match.isTeamAttackingUp side period -> resources.keeperDiveUp
                | Diving, _ -> resources.keeperDiveDown
            let sw = strip.Height
            let sx =
                player.runningFrame * float32 strip.Width
                |> fun x -> x / (float32 sw)
                |> truncate
                |> (*) (float32 sw)
                |> int
                |> min (strip.Width - sw)
            try
                let effect = resources.colorChange
                let shirtColor, shortColor =
                    match player.isKeeper, side with
                    | true, _ -> Color.LightPink, Color.DarkGray
                    | false, Team.TeamA -> Color.Blue, Color.DarkBlue
                    | false, Team.TeamB -> Color.Red, Color.DarkRed
                effect.Parameters.["OldColor"].SetValue(Color(0, 0, 255, 255).ToVector4())
                effect.Parameters.["NewColor"].SetValue(shirtColor.ToVector4())
                effect.Parameters.["OldColor2"].SetValue(Color(0, 0, 113, 255).ToVector4())
                effect.Parameters.["NewColor2"].SetValue(shortColor.ToVector4())
                effect.Parameters.["SpriteSheet"].SetValue(strip)
                sb.Begin(SpriteSortMode.Immediate, BlendState.AlphaBlend, SamplerState.PointClamp, DepthStencilState.Default, RasterizerState.CullNone, resources.colorChange)
                let center = float32 sw / 2.0f
                sb.Draw(strip, Rectangle(int x, int y, w, w), System.Nullable(Rectangle(sx, 0, sw, sw)), Color.White, angle, Vector2(center, center), SpriteEffects.None, 0.0f)
            finally
                sb.End()
        | GoalUpper ->
            let x, y = (-Physics.goalWidth / 2.0f, pitch.length / 2.0f) |> worldToScreen
            try
                sb.Begin()
                sb.Draw(resources.goalUpper, Vector2(x / 1.0f<px>, (y - 64.0f<px>) / 1.0f<px>), Color.White)
            finally
                sb.End()

        | GoalLower ->
            let x, y = (-Physics.goalWidth / 2.0f, -pitch.length / 2.0f) |> worldToScreen
            try
                sb.Begin()
                sb.Draw(resources.goalLower, Vector2(x / 1.0f<px>, (y - 33.0f<px>) / 1.0f<px>), Color.White)
            finally
                sb.End()

// Render the shadow of the ball
let renderBallShadow (sb : SpriteBatch) (viewWidth, viewHeight) shadow (viewX, viewY) (ballPos : TypedVector3<m>) =
    let worldToScreen = worldToScreen (ratio * viewWidth, ratio * viewHeight) (viewWidth, viewHeight) (viewX, viewY)
    
    let x = ballPos.X - Ball.ballRadius
    let y = ballPos.Y + Ball.ballRadius
    let x, y = worldToScreen (x, y)
    let w = 2.0f * Ball.ballRadius * ratio |> int

    let color = new Color(0.1f, 0.1f, 0.1f, 0.9f)

    sb.Draw(shadow, Rectangle(int x, int y, w, w), color)

// Render the lines under the players that are currently under human control
let renderHighlights (sb : SpriteBatch) (viewWidth, viewHeight) line highlights (viewX, viewY) =
    let worldToScreen = worldToScreen (ratio * viewWidth, ratio * viewHeight) (viewWidth, viewHeight) (viewX, viewY)
    let color = Color.Yellow
    
    for x, y in highlights do
        let x = x - playerRadius
        let y = y - playerRadius
        let x, y = worldToScreen (x, y)
        let w = 2.0f * playerRadius * ratio |> int

        sb.Draw(line, Rectangle(int x, int y, w, 2), color)
    
// Render everything.
let render (sb : SpriteBatch) viewSize resources (state : Match.MatchState) : unit =
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
        renderBallShadow sb viewSize resources.ballShadow viewPos state.ball.pos
    finally
        sb.End()
    renderSprites sb viewSize resources state.period state.pitch viewPos sprites
