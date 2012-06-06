module CleverRake.StolpSkott.Team

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units
open Microsoft.Xna.Framework

type TeamSide = TeamA | TeamB

let otherSide = function
    | TeamA -> TeamB
    | TeamB -> TeamA

type Team =
    { onPitch : Player.State[]
      substitutes : Player.Traits list
    }
