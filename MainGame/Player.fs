﻿module CleverRake.StolpSkott.Player

open Microsoft.Xna.Framework
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils
open CleverRake.StolpSkott.Units

type Activity =
    | Standing
    | Jumping of float32<kf>
    | Tackling of float32<kf> * bool // animation key, has touched the ball
    | Trapping
    | Passing
    | Crossing
    | Kicking of float32<kf>
    | Fallen of float32<kf>
    | KeeperDive of float32<kf>
    | Stumbling of float32<s>

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
      travelled : float32<m> // To decide the frame to use when rendering running players
      runningFrame : float32 // 0.0 -> 1.0, completion of the running animation
      activity : Activity
      traits : Traits
      isKeeper : bool
      health : float32<he>
      condition : float32<sta> }

let getRunSpeed { traits = { speed = speed }; health = health; condition = condition } =
    speed * health * condition / 1.0f<he sta>

let jumpingLength = 2.0f<s>
let tacklingLength = 2.0f<s>
let kickingLength = 0.2f<s>
let fallenLength = 1.0f<s>
let keeperDiveLength = 0.75f<s>
let stumbleTimeAfterKick = 0.5f<s>

let updateKeyFrame (dt : float32<s>) player =
    let activity =
        match player.activity with
        | Standing -> Standing
        | Trapping -> Trapping
        | Passing -> Stumbling stumbleTimeAfterKick
        | Crossing -> Stumbling stumbleTimeAfterKick
        | Jumping kf ->
            let kf = kf + 1.0f<kf> * dt / jumpingLength
            if kf < 1.0f<kf> then 
                Jumping kf
            else
                Standing
        | Tackling (kf, touchedBall) ->
            let kf = kf + 1.0f<kf> * dt / tacklingLength
            if kf < 1.0f<kf> then
                Tackling(kf, touchedBall)
            else
                Standing
        | Kicking kf ->
            let kf = kf + 1.0f<kf> * dt / kickingLength 
            if kf < 1.0f<kf> then
                Kicking kf
            else
                Standing
        | Fallen kf ->
            let kf = kf + 1.0f<kf> * dt / fallenLength
            if kf < 1.0f<kf> then
                Fallen kf
            else
                Standing
        | KeeperDive kf ->
            let kf = kf + 1.0f<kf> * dt / keeperDiveLength
            if kf < 1.0f<kf> then
                KeeperDive kf
            else
                Standing
        | Stumbling timeLeft ->
            if timeLeft < 0.0f<s> then
                Standing
            else
                Stumbling (timeLeft - dt)

    { player with activity = activity }

let updatePlayer (dt : float32<s>) player =
    let runningAnimationDistance = 2.0f<m>
    let player = updateKeyFrame dt player
    let pos = player.pos + dt * player.speed * player.direction
    let travelled = player.travelled + (pos - player.pos).Length
    let travelled =
        if travelled > runningAnimationDistance then
            travelled - runningAnimationDistance
        else
            travelled
    let frame = travelled / runningAnimationDistance
    let speed =
        match player.activity with
        | Trapping
        | Passing
        | Stumbling _
        | Fallen _ -> 0.0f<m/s>

        | Standing
        | Crossing
        | Jumping _
        | Kicking _ -> player.speed

        | Tackling(kf, _) ->
            if kf < 0.3f<kf> then
                1.1f * getRunSpeed player
            elif kf < 0.7f<kf> then
                getRunSpeed player
            else
                1.0f<m/s> * MathHelper.Lerp(float32 <| getRunSpeed player, 0.0f, (kf - 0.7f<kf>) / 0.3f<kf>)

        | KeeperDive kf ->
            if kf > 0.7f<kf> then
                2.0f * getRunSpeed player
            elif kf > 0.3f<kf> then
                1.5f * getRunSpeed player
            else
                1.5f * (kf / 0.3f<kf>) * getRunSpeed player

    { player with pos = pos ; speed = speed ; travelled = travelled ; runningFrame = frame }
