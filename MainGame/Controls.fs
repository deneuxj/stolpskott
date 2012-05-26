module CleverRake.StolpSkott.Controls

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

open CleverRake.StolpSkott.Player
open CleverRake.StolpSkott.Units

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

type ControlActivity =
    | Running
    | Kicking
    | TrappingBall
    | Passing
    | Jumping
    | Tackling
    | SwitchingPlayer


type Configuration =
    { direction : GamePadState -> float32 * float32
      trap : GamePadState -> bool
      kick : GamePadState -> bool
    }

let updateControl config prePad pad hasBallControl isBallHigh (activity, player : Player.State) =
    let dir, speed =
        let x, y = config.direction pad
        let v = TypedVector2<1>(x, y)
        match v.Length with
        | x when x > 0.1f -> 1.0f / x * v, Player.getRunSpeed player
        | _ -> player.direction, 0.0f<m/s>

    match activity, player.activity with
    | SwitchingPlayer, _ ->
        activity, player
    
    | Running, Standing ->
        if config.trap pad then
            TrappingBall, { player with activity = Trapping ; direction = dir ; speed = 0.0f<m/s> }
        elif config.kick pad && not <| config.kick prePad then
            match hasBallControl, isBallHigh with
            | _, true -> Jumping, { player with activity = Player.Jumping 0.0f<kf> }
            | true, false -> Kicking, { player with activity = Player.Kicking 0.0f<kf> }
            | false, false -> Tackling, { player with activity = Player.Tackling(0.0f<kf>, false) }
        else
            Running, { player with activity = Standing ; direction = dir ; speed = speed }
    
    | _, Standing ->
        Running, { player with activity = Standing ; direction = dir ; speed = speed }
    
    | _, Player.Jumping _ ->
        Jumping, player

    | _, Player.Tackling _ ->
        Tackling, player

    | TrappingBall, Player.Trapping ->
        if not <| config.trap pad then
            Passing, player
        else
            TrappingBall, { player with direction = dir }

    | Passing, Player.Trapping ->
        Running, { player with activity = Standing }

    | _, Player.Trapping ->
        TrappingBall, player

    | _, Player.Kicking _ ->
        Kicking, player

    | _, Player.Fallen _ ->
        Running, player

    | _, Player.KeeperDive _ ->
        failwith "updateControl() not meant to control goal keepers"
