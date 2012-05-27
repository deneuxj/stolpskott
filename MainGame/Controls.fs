module CleverRake.StolpSkott.Controls

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

open CleverRake.StolpSkott.Player
open CleverRake.StolpSkott.Units

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

type Configuration =
    { direction : GamePadState -> float32 * float32
      trap : GamePadState -> bool
      kick : GamePadState -> bool
    }

let updateControl config dt prePad pad hasBallControl isBallHigh (player : Player.State) =
    let dir, speed =
        let x, y = config.direction pad
        let v = TypedVector2<1>(x, y)
        match v.Length with
        | x when x > 0.1f -> 1.0f / x * v, Player.getRunSpeed player
        | _ -> player.direction, 0.0f<m/s>

    match player.activity with
    | Standing ->
        if config.trap pad then
            { player with activity = Trapping ; direction = dir ; speed = 0.0f<m/s> }
        elif config.kick pad && not <| config.kick prePad then
            match hasBallControl, isBallHigh with
            | _, true -> { player with activity = Player.Jumping 0.0f<kf> }
            | true, false -> { player with activity = Player.Kicking(0.0f<kf>) }
            | false, false -> { player with activity = Player.Tackling(0.0f<kf>, false) }
        else
            { player with activity = Standing ; direction = dir ; speed = speed }
        
    | Player.Jumping _ ->
        player

    | Player.Tackling _ ->
        player

    | Player.Trapping ->
        if config.trap pad then
            { player with direction = dir }
        else
            { player with activity = Passing }

    | Player.Passing ->
        { player with activity = Standing }

    | Player.Kicking(kf) ->
        player

    | Player.Fallen _ ->
        player

    | Player.KeeperDive _ ->
        failwith "updateControl() not meant to control goal keepers"
