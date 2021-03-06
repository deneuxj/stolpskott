﻿module CleverRake.StolpSkott.Ball

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
    | KickOff (_, team)
    | Penalty (_, team)
    | CornerKick (_, team, _)
    | KickIn (team, _)
    | ThrowIn (team, _, _)
    | FreeKick (team, _)
    | TrappedByKeeper team -> DeadBall team

type State =
    { pos : TypedVector3<m>
      speed : TypedVector3<m/s>
      inPlay : InPlay
      lastTouchedBy : TeamSide option
    }

let ballRadius = 0.12f<m>
let ballMass = 0.4<kg>