module CleverRake.StolpSkott.Ball

open Microsoft.Xna.Framework
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils
open Team

type PitchSide = Left | Right

type InPlay =
    | InPlay
    | CornerKick of TeamSide * PitchSide
    | KickIn of TeamSide * PitchSide
    | KickOff of TeamSide
    | ThrowIn of TeamSide * PitchSide * float32<m>
    | Penalty of TeamSide
    | FreeKick of TeamSide * TypedVector2<m>
    | TrappedByKeeper of TeamSide

let (|DeadBall|LiveBall|) = function
    | InPlay -> LiveBall
    | KickIn (team, _)
    | KickOff (team)
    | ThrowIn (team, _, _)
    | Penalty (team)
    | FreeKick (team, _)
    | TrappedByKeeper team
    | CornerKick (team, _) -> DeadBall (Some team)

type State =
    { pos : TypedVector3<m>
      speed : TypedVector3<m/s>
      inPlay : InPlay
    }

let ballRadius = 0.12f<m>
let ballMass = 0.4<kg>