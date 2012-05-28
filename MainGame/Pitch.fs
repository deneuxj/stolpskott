module CleverRake.StolpSkott.Pitch

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units
open Microsoft.Xna.Framework

let penaltyBoxWidth = 40.3f<m>
let penaltyBoxHeight = 16.5f<m>
let goalBoxWidth = penaltyBoxWidth - 22.0f<m>
let goalBoxHeight = 5.5f<m>
let pitchRestitution = 0.5f
let pitchDrag = 0.5f</s>

type PitchTraits =
    { width : float32<m>
      length : float32<m>
    }