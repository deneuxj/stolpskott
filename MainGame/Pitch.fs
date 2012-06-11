module CleverRake.StolpSkott.Pitch

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units
open Microsoft.Xna.Framework

let penaltyBoxWidth = 40.3f<m>
let penaltyBoxHeight = 16.5f<m>
let goalBoxWidth = penaltyBoxWidth - 22.0f<m>
let goalBoxHeight = 5.5f<m>
let pitchRestitution = 0.5f
let pitchDrag = 1.0f</s>

type PitchTraits =
    { width : float32<m>
      length : float32<m>
    }

let boundBall (pitch : PitchTraits) (ball : Ball.State) =
    let speed =
        if abs ball.pos.X > 5.0f<m> + pitch.width / 2.0f ||
           abs ball.pos.Y > 5.0f<m> + pitch.length / 2.0f then
            TypedVector3<m/s>(0.0f<_>, 0.0f<_>, ball.speed.Z)
        else
            ball.speed

    { ball with speed = speed }
