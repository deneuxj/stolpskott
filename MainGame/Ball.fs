module CleverRake.StolpSkott.Ball

open Microsoft.Xna.Framework
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils
open Team

type InPlay =
    | InPlay
    | DeadBall of TeamSide
    | OutOfPitch
    | TrappedByKeeper of TeamSide

type State =
    { pos : TypedVector3<m>
      speed : TypedVector3<m/s>
      inPlay : InPlay
    }

let ballRadius = 0.12f<m>
let ballMass = 0.4<kg>