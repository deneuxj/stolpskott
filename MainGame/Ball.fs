module CleverRake.StolpSkott.Ball

open Microsoft.Xna.Framework
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils

type InPlay =
    | InPlay
    | DeadBallTeamA
    | DeadBallTeamB
    | OutOfPitch
    | TrappedByKeeperA
    | TrappedByKeeperB

type State =
    { pos : TypedVector3<m>
      speed : TypedVector3<m/s>
      inPlay : InPlay
    }

let ballRadius = 0.12f<m>
let ballMass = 0.4<kg>