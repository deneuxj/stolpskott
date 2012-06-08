module CleverRake.StolpSkott.Ball

open Microsoft.Xna.Framework
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils
open Team

type PitchSide = Left | Right

type Engagement = CanKick | WaitWhistle

type InPlay =
    | InPlay
    | CornerKick of Engagement * TeamSide * PitchSide
    | KickIn of TeamSide * PitchSide
    | KickOff of Engagement * TeamSide
    | ThrowIn of TeamSide * PitchSide * float32<m>
    | Penalty of Engagement * TeamSide
    | FreeKick of TeamSide * TypedVector2<m>
    | TrappedByKeeper of TeamSide

let (|DeadBall|LiveBall|) = function
    | InPlay -> LiveBall
    | KickIn (team, _)
    | KickOff (_, team)
    | ThrowIn (team, _, _)
    | Penalty (_, team)
    | FreeKick (team, _)
    | TrappedByKeeper team
    | CornerKick (_, team, _) -> DeadBall (Some team)

(*
let (|Constrained|PhysicsControlled|) = function
    | CornerKick (CanKick, _, _)
    | Penalty (CanKick, _)
    | KickOff (CanKick, _)
    | InPlay -> PhysicsControlled
    | CornerKick (WaitWhistle, _, _)
    | Penalty (WaitWhistle, _)
    | KickOff (WaitWhistle, _)
    | KickIn _
    | ThrowIn _
    | FreeKick _
    | TrappedByKeeper _ -> Constrained
*)

type State =
    { pos : TypedVector3<m>
      speed : TypedVector3<m/s>
      inPlay : InPlay
    }

let ballRadius = 0.12f<m>
let ballMass = 0.4<kg>