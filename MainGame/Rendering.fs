﻿module CleverRake.StolpSkott.Rendering

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils

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
        goalShadows : Texture2D
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

type SpriteType =
    | Ball of float32<m>
    | Player of Player.State * Team.TeamSide
    | GoalUpper
    | GoalLower

type PositionedSprite =
    { spriteType : SpriteType
      x : float32<m>
      y : float32<m> }

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
            (0.0f<m>, true)

    let worldToScreen = worldToScreen (ratio * viewWidth, ratio * viewHeight) (viewWidth, viewHeight) (viewX, viewY)
    Seq.append upperStrips lowerStrips
    |> Seq.iter (fun (y0, y1, stripIsDark) ->
        let x, y =
            if stripIsDark then
                (-darkStripWidth * 0.5f, y0)
            else
                (-lightStripWidth * 0.5f, y0)
            |> worldToScreen

        sb.Draw((if stripIsDark then darkGrass else lightGrass), Vector2(x / 1.0f<px>, y / 1.0f<px>), Color.White))


let renderLines
    (sb : SpriteBatch)
    (viewWidth : float32<m>, viewHeight : float32<m>)
    (whiteLine : Texture2D)
    (pitch : Team.PitchTraits)
    (viewX : float32<m>, viewY : float32<m>) =

    let worldToScreen = worldToScreen (ratio * viewWidth, ratio * viewHeight) (viewWidth, viewHeight) (viewX, viewY)
    let lineHalfWidth = 0.5f * 0.1f<m>
    let drawHLine (x0, y0) (x1) =
        let x0, x1 = min x0 x1, max x0 x1
        let x0', y0' = worldToScreen (x0 - lineHalfWidth, y0 - lineHalfWidth)
        let x1', y1' = worldToScreen (x1 + lineHalfWidth, y0 + lineHalfWidth)
        sb.Draw(whiteLine, Rectangle(int x0', int y0', int (x1' - x0'), int (y1' - y0')), Color.White)

    let drawVLine (x0, y0) (y1) =
        let y0, y1 = min y0 y1, max y0 y1
        let x0', y0' = worldToScreen (x0 - lineHalfWidth, y0 - lineHalfWidth)
        let x1', y1' = worldToScreen (x0 + lineHalfWidth, y1 + lineHalfWidth)
        sb.Draw(whiteLine, Rectangle(int x0', int y0', int (x1' - x0'), int (y1' - y0')), Color.White)

    let x0 = -pitch.width / 2.0f
    let x1 = -x0
    let y0 = -pitch.length / 2.0f
    let y1 = -y0

    drawHLine (x0, y0) x1
    drawHLine (x0, 0.0f<m>) x1
    drawHLine (x0, y1) x1

    drawVLine (x0, y0) y1
    drawVLine (x1, y0) y1

    // TODO: penalty boxes, six-yards boxes


let render renderLines renderPlayerShadows renderBallShadow renderGoalShadows renderSprites (sb : SpriteBatch) viewSize resources (state : Team.GameState) : unit =
    let viewPos = 
        let viewX = state.ball.pos.X |> max (-state.pitch.width * 0.5f) |> min (state.pitch.width * 0.5f)
        let viewY = state.ball.pos.Y |> max (-state.pitch.length * 0.5f) |> min (state.pitch.length * 0.5f)
        (viewX, viewY)

    let sprites =
        seq {
            for player in state.teamA.onPitch do
                yield { x = player.pos.X ; y = player.pos.Y; spriteType = Player(player, Team.TeamA) }
            for player in state.teamB.onPitch do
                yield { x = player.pos.X ; y = player.pos.Y; spriteType = Player(player, Team.TeamB) }
            yield { x = 0.0f<m> ; y = state.pitch.length * 0.5f ; spriteType = GoalUpper }
            yield { x = 0.0f<m> ; y = -state.pitch.length * 0.5f ; spriteType = GoalLower }
            yield { x = state.ball.pos.X ; y = state.ball.pos.Y ; spriteType = Ball(state.ball.pos.Z) }
        }
        |> Seq.sortBy (function { x = x ; y = y } -> (y, x))

    try
        sb.Begin()
        renderGrass sb viewSize resources.grassLight resources.grassDark viewPos
        renderLines sb viewSize resources.whiteLine state.pitch viewPos
        renderPlayerShadows sb viewSize resources.playerShadows viewPos (Array.concat [state.teamA.onPitch ; state.teamB.onPitch])
        renderBallShadow sb viewSize resources.ballShadow viewPos state.ball.pos
        renderGoalShadows sb viewSize resources.goalShadows viewPos
        renderSprites sb viewSize resources.ball resources.playerSprites resources.goalUpper resources.goalLower viewPos sprites
    finally
        sb.End()