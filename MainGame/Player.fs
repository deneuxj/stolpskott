module CleverRake.StolpSkott.Player

open Microsoft.Xna.Framework
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils
open CleverRake.StolpSkott.Units

type Activity =
    | Standing
    | Jumping of float32<kf>
    | Tackling of float32<kf> * bool // animation key, has touched the ball
    | Trapping
    | Kicking of float32<kf>
    | Fallen of float32<kf>
    | KeeperDive of float32<kf>

type Traits =
    { speed : float32<m/s>
      stamina : float32<sta>
      strength : float32<st>
      length : float32<m>
      ballControl : float32<bc> }

let jumpHeight = 0.75f<m>
let maxRunSpeed = 7.0f<m/s>

type State =
    { pos : TypedVector2<m>
      direction : TypedVector2<1>
      speed : float32<m/s>
      activity : Activity
      traits : Traits
      isKeeper : bool
      health : float32<he>
      condition : float32<sta> }

let getRunSpeed { traits = { speed = speed }; health = health; condition = condition } =
    speed * health * condition / 1.0f<he sta>