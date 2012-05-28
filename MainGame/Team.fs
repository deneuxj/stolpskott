module CleverRake.StolpSkott.Team

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units
open Microsoft.Xna.Framework

type Team =
    { onPitch : Player.State[]
      substitutes : Player.Traits list
    }
