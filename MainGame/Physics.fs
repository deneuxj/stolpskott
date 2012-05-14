﻿module CleverRake.StolpSkott.Physics

open Microsoft.Xna.Framework
open CleverRake.StolpSkott
open CleverRake.StolpSkott.Units
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils

open Ball

// Collision of a light moving object with a heavy object
// normal: Normal to the surface of the heavy object
// speed: Speed of the moving object relative to the heavy object
// r: Restitution
let collideLightWithHeavy (r : float32<1>) (normal : TypedVector3<1>) (speed : TypedVector3<m/s>) : TypedVector3<m/s> =
    let projSpeed = TypedVector.dot3(speed, normal)
    TypedVector.scale3(-projSpeed * r, normal)

let vector3Of2 (v2 : TypedVector2<'M>) : TypedVector3<'M>=
    TypedVector3<'M>(v2.X, v2.Y, 0.0f<_>)

type BallImpulse<'PlayerId> =
    | Free
    | BouncedOffPlayer of 'PlayerId * TypedVector3<m/s> // Bounced off a player, who will have a chance to initiate a kick within some small amount of time
    | Bounced of TypedVector3<m/s> // Bounced off something or some player who will not be able to initiate a kick.
    | Header of TypedVector3<m/s>  // Bounced off a player that is currently jumping
    | Pushed of 'PlayerId * TypedVector3<m/s>  // Bounced off a player that has control over the ball
    | Trapped of 'PlayerId  // Trapped by a player.

let (|SomeImpulse|_|) =
    function
    | Bounced imp
    | BouncedOffPlayer(_, imp)
    | Pushed(_, imp)
    | Header imp -> Some imp
    | Trapped _ -> Some TypedVector3<m/s>.Zero
    | Free -> None

let controlMaxDistance = 0.5f<m> // Beyond this distance, balls don't collide with players.
let headerSpeed = 1.0f<m/s> // Speed modifier for headers
let optimalKeeperKeyframe = 0.5f<kf> // The keyframe at which a keeper manages to catch the ball
let keeperCaughtThreshold = 0.05f<kf> // Half-width of the interval in which keepers catch balls
let keeperBounceRestitution = 0.5f // Bounciness of the keeper-ball collisions when the keeper fails to catch the ball
let playerBounceRestitution = 0.8f // Bounciness of player-ball collisions
let playerTackleRestitution = 0.5f // Bounciness of collisions between tackling players and the ball
let maxBallControlSpeed = 2.0f<m/s> // Maximum speed relative to the player under which control is achieved
let pushSpeedFactor = 1.0f // Affects how far players push the ball when they have control over it

let collideBallWithPlayer (playerId, player : Player.State) ball =
    let ballSpeed2d = TypedVector2<m/s>(ball.speed.X, ball.speed.Y)
    let ballPos2d = TypedVector2<m>(ball.pos.X, ball.pos.Y)

    let relPos = ballPos2d - player.pos
    let relSpeed = ballSpeed2d - player.speed * player.direction

    match TypedVector.dot2(relPos, relSpeed) with
    | x when x < 0.0f<m^2/s> ->
        let jump =
            match player.activity with
            | Player.Jumping _ -> Player.jumpHeight
            | _ -> 0.0f<m>

        if ball.pos.Y - Ball.ballRadius <= player.traits.length + jump then
            let dist = relPos.Length
            if dist < controlMaxDistance then
                match player.activity with
                | Player.Trapping ->
                    Trapped(playerId)
                | Player.Fallen _ ->
                    Free
                | Player.Jumping _ ->
                    Header(headerSpeed * vector3Of2 player.direction)
                | Player.Kicking _ ->
                    Bounced(collideLightWithHeavy playerBounceRestitution (1.0f / dist * vector3Of2 relPos) (vector3Of2 relSpeed))
                | Player.Tackling _ ->
                    BouncedOffPlayer(playerId, collideLightWithHeavy playerTackleRestitution (1.0f / dist * vector3Of2 relPos) (vector3Of2 relSpeed))

                | Player.KeeperDive keyframe ->
                    if abs(keyframe - optimalKeeperKeyframe) < keeperCaughtThreshold then
                        Trapped(playerId)
                    else
                        Bounced(collideLightWithHeavy keeperBounceRestitution (1.0f / dist * vector3Of2 relPos) (vector3Of2 relSpeed))

                | Player.Standing _ ->
                    let canControl =
                        player.speed > 0.0f<m/s> &&
                        TypedVector.dot2(player.direction, relPos) < 0.0f<m> &&
                        relSpeed.Length < maxBallControlSpeed * player.traits.ballControl

                    if canControl then                        
                        Pushed(playerId, player.speed * pushSpeedFactor * (2.0f - player.traits.ballControl) * (vector3Of2 player.direction))
                    else
                        BouncedOffPlayer(playerId, collideLightWithHeavy playerBounceRestitution (1.0f / dist * vector3Of2 relPos) (vector3Of2 relSpeed))
            else
                Free
        else
            Free
    | _ -> Free


let collidePlayersWithBall ball players =
    let impulse =
        players
        |> Seq.fold (fun impulse player ->
            match impulse, collideBallWithPlayer player ball with
            // Free is the neutral element of impulse combination.
            | Free, impulse
            | impulse, Free -> impulse

            // All combinations of non-free impulses result in uncontrolled bounces
            | SomeImpulse imp0, SomeImpulse imp1 -> Bounced (imp0 + imp1)
            
            | _ -> failwith "Unexpected impulse combination" // Should not be reachable
            ) Free

    impulse

let goalWidth = 7.32f<m>
let goalHeight = 2.44f<m>
let goalPostRadius = 0.07f<m>

let collideGoalWithBall ball (goalCenter : TypedVector2<m>) =
    let relPos = TypedVector2<m>(ball.pos.X, ball.pos.Y) - goalCenter

    let collideWithPost x =
        let postRelPos = relPos - TypedVector2<m>(x, 0.0f<m>)
        let dist = postRelPos.Length
        let normal = 1.0f / dist * vector3Of2 postRelPos
        let collides =
            dist < Ball.ballRadius + goalPostRadius &&
            ball.pos.Z < goalHeight

        if collides then
            collideLightWithHeavy 1.0f normal ball.speed
        else
            TypedVector3.Zero

    let collideWithBar =
        let postRelPos = TypedVector2<m>(ball.pos.Y, ball.pos.Z) - TypedVector2<m>(0.0f<m>, goalHeight)
        let dist = postRelPos.Length
        let normal = 1.0f / dist * TypedVector3<m>(0.0f<m>, postRelPos.X, postRelPos.Y)

        let collides =
            dist < Ball.ballRadius + goalPostRadius &&
            abs(relPos.X) < goalWidth / 2.0f

        if collides then
            collideLightWithHeavy 1.0f normal ball.speed
        else
            TypedVector3.Zero

    collideWithPost (-goalWidth / 2.0f) +
    collideWithPost (goalWidth / 2.0f) +
    collideWithBar


let gravity = TypedVector3<m/s^2>(0.0f<_>, -9.8f<_>, 0.0f<_>)
let pitchRestitution = 0.5f
let pitchDrag = 0.5f</s>

type InfluenceFunc<'PlayerId> = InfluenceFunc of ('PlayerId -> TypedVector3<m/s> * InfluenceFunc<'PlayerId>)

let updateBall goalCenters (dt : float32<s>) (InfluenceFunc getInfluence as influence) players ball =
    let impulse = collidePlayersWithBall ball players
    let speed, influence =
        match impulse with
        | Free ->
            ball.speed + dt * gravity, influence
        | Bounced imp ->
            ball.speed + imp + dt * gravity, influence
        | BouncedOffPlayer(player, imp) ->
            let imp', (influence : InfluenceFunc<_>) = getInfluence player
            ball.speed + imp + imp' + dt * gravity, influence
        | Pushed(_, imp)
        | Header(imp) ->
            imp, influence
        | Trapped _ ->
            TypedVector3.Zero, influence

    let pos = ball.pos + dt * speed

    // Bouncing on the pitch
    let posZ, speedZ, drag =
        match pos.Z - Ball.ballRadius with
        | h when h < 0.0f<m> ->
            -h * pitchRestitution + Ball.ballRadius, -pitchRestitution * speed.Z, pitchDrag
        | _ ->
            pos.Z, speed.Z, 0.0f</s>

    let dragAccel = pitchDrag * TypedVector3<m/s>(speed.X, speed.Y, 0.0f<_>)

    let ball =
        { ball with
            pos = TypedVector3(pos.X, pos.Y, posZ)
            speed = TypedVector3(speed.X, speed.Y, speedZ) - dt * dragAccel }

    // Bouncing against goal posts
    let goalImpulses =
        goalCenters
        |> Seq.sumBy (collideGoalWithBall ball)

    ball = { ball with speed = ball.speed + goalImpulses }