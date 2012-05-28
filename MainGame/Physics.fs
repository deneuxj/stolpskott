module CleverRake.StolpSkott.Physics

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
    | Bounced of TypedVector3<m/s> // Bounced off an obstacle (goal post...)
    | BouncedOffPlayer of 'PlayerId * TypedVector3<m/s> // Bounced off a player
    | Pushed of 'PlayerId * TypedVector3<m/s> // Bounced off a player that has control over the ball
    | Trapped of 'PlayerId  // Trapped by a player.
    | Kicked of 'PlayerId * TypedVector3<m/s> // Kicked by a player

let (|SomeImpulse|_|) =
    function
    | Bounced imp
    | BouncedOffPlayer(_, imp)
    | Pushed(_, imp) -> Some imp
    | Kicked(_, imp) -> Some imp
    | Trapped _ -> Some TypedVector3<m/s>.Zero
    | Free -> None

let controlMaxDistance = 0.5f<m> // Beyond this distance, balls don't collide with players.
let kickMaxDistance = 0.8f<m> // Distance from the ball within which a player can kick it.
let pushedDistance = 0.3f<m>
let headerSpeed = 1.0f<m/s> // Speed modifier for headers
let optimalKeeperKeyframe = 0.5f<kf> // The keyframe at which a keeper manages to catch the ball
let keeperCaughtThreshold = 0.05f<kf> // Half-width of the interval in which keepers catch balls
let keeperBounceRestitution = 0.5f // Bounciness of the keeper-ball collisions when the keeper fails to catch the ball
let playerBounceRestitution = 0.8f // Bounciness of player-ball collisions
let playerTackleRestitution = 0.5f // Bounciness of collisions between tackling players and the ball
let maxBallControlSpeed = 20.0f<m/s> // Maximum speed relative to the player under which control is achieved
let minPushSpeedFactor = 1.25f // Affects how far good players push the ball when they have control over it
let maxPushSpeedFactor = 1.5f // Affects how far poor players push the ball when they have control over it
let hardKickElevation = 0.2f // Controls how fast the ball goes up upon leaving the foot of the kicker
let hardKickSpeed = 40.0f<m/s> // How fast the ball goes forward upon leaving the foot of the kicker
let softKickElevation = 0.1f
let softKickSpeed = 20.0f<m/s>
let crossKickSpeed = 25.0f<m/s>
let crossKickElevation = 0.3f

let makeKick (kickElevation : float32<1>) (kickSpeed : float32<m/s>) (ballControl : float32<bc>) (direction : TypedVector2<1>) =
    let kickHeight =
        kickElevation * MathHelper.Lerp(2.0f, 1.0f, ballControl)
    let dir = TypedVector3<1>(direction.X, direction.Y, kickHeight) |> TypedVector.normalize3
    kickSpeed * dir

let collideBallWithPlayer (playerId, player : Player.State) ball =
    let ballSpeed2d = TypedVector2<m/s>(ball.speed.X, ball.speed.Y)
    let ballPos2d = TypedVector2<m>(ball.pos.X, ball.pos.Y)

    let relPos = ballPos2d - player.pos
    let relSpeed = ballSpeed2d - player.speed * player.direction

    let isBallGoingTowardsPlayer =
        TypedVector.dot2(relPos, relSpeed) < 0.0f<m^2/s>

    let jump =
        match player.activity with
        | Player.Jumping _ -> Player.jumpHeight
        | _ -> 0.0f<m>

    if ball.pos.Z - Ball.ballRadius <= player.traits.length + jump then
        let dist = relPos.Length

        match player.activity with
        | Player.Trapping ->
            if dist < 2.0f * controlMaxDistance then
                Trapped(playerId)
            else
                Free

        | Player.Passing ->
            if dist < controlMaxDistance then
                let kick = makeKick softKickElevation softKickSpeed player.traits.ballControl player.direction
                Kicked(playerId, kick)
            else
                Free

        | Player.Crossing ->
            if dist < controlMaxDistance then
                let kick = makeKick crossKickElevation crossKickSpeed player.traits.ballControl player.direction
                Kicked(playerId, kick)
            else
                Free

        | Player.Fallen _ ->
            Free

        | Player.Jumping _ ->
            if isBallGoingTowardsPlayer && dist < controlMaxDistance then
                BouncedOffPlayer(playerId, headerSpeed * vector3Of2 player.direction)
            else
                Free

        | Player.Kicking (kf) ->
            if isBallGoingTowardsPlayer && dist < kickMaxDistance then
                let controlModifier = MathHelper.Lerp(1.0f, 0.5f, (dist - pushedDistance) / (kickMaxDistance - pushedDistance))
                let kick = makeKick hardKickElevation hardKickSpeed (controlModifier * player.traits.ballControl) player.direction
                Kicked(playerId, kick)
            else
                Free
                
        | Player.Tackling _ ->
            if isBallGoingTowardsPlayer && dist < controlMaxDistance then
                BouncedOffPlayer(playerId, collideLightWithHeavy playerTackleRestitution (1.0f / dist * vector3Of2 relPos) (vector3Of2 relSpeed))
            else
                Free

        | Player.KeeperDive keyframe ->
            if abs(keyframe - optimalKeeperKeyframe) < keeperCaughtThreshold && dist < controlMaxDistance then
                Trapped(playerId)
            elif isBallGoingTowardsPlayer && dist < controlMaxDistance then
                BouncedOffPlayer(playerId, collideLightWithHeavy keeperBounceRestitution (1.0f / dist * vector3Of2 relPos) (vector3Of2 relSpeed))
            else
                Free

        | Player.Standing _ ->
            let canControl =
                player.speed > 0.0f<m/s> &&
                TypedVector.dot2(player.direction, relPos) > 0.0f<m> &&
                relSpeed.Length < maxBallControlSpeed * MathHelper.Lerp(0.8f, 1.0f, player.traits.ballControl)

            if canControl && isBallGoingTowardsPlayer && dist < pushedDistance then
                let factor = MathHelper.Lerp(maxPushSpeedFactor, minPushSpeedFactor, player.traits.ballControl)
                Pushed(playerId, player.speed * factor * (vector3Of2 player.direction))
            else
                Free
    else
        Free


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
let goalPostRestitution = 1.0f

let collideGoalWithBall ball (goalCenter : TypedVector2<m>) =
    let relPos = ball.pos - TypedVector3<m>(goalCenter.X, goalCenter.Y, 0.0f<_>)

    let collideWithPost x =
        let postRelPos = relPos - TypedVector3<m>(x, 0.0f<_>, 0.0f<_>)
        let dist = postRelPos.Length
        let normal = (1.0f / dist) * postRelPos
        let collides =
            dist < Ball.ballRadius + goalPostRadius &&
            ball.pos.Z < goalHeight

        if collides then
            collideLightWithHeavy goalPostRestitution normal ball.speed
        else
            TypedVector3.Zero

    let collideWithBar =
        let postRelPos = TypedVector3<m>(0.0f<m>, ball.pos.Y, ball.pos.Z) - TypedVector3<m>(0.0f<m>, goalCenter.Y, goalHeight)
        let dist = postRelPos.Length
        let normal = (1.0f / dist) * postRelPos

        let collides =
            dist < Ball.ballRadius + goalPostRadius &&
            abs(relPos.X) < goalWidth / 2.0f

        if collides then
            collideLightWithHeavy goalPostRestitution normal ball.speed
        else
            TypedVector3.Zero

    collideWithPost (-goalWidth / 2.0f) +
    collideWithPost (goalWidth / 2.0f) +
    collideWithBar


let gravity = TypedVector3<m/s^2>(0.0f<_>, 0.0f<_>, -9.8f<_>)
let pitchRestitution = 0.5f
let pitchDrag = 0.5f</s>

let updateBall goalCenters (dt : float32<s>) players ball =
    let impulse = collidePlayersWithBall ball players
    
    match impulse with
    | Free -> ()
    | _ -> printfn "%A" impulse

    let speed =
        match impulse with
        | Free ->
            ball.speed + dt * gravity
        | Bounced imp ->
            ball.speed + imp + dt * gravity
        | BouncedOffPlayer(player, imp) ->
            ball.speed + imp + dt * gravity
        | Pushed(_, imp)
        | Kicked(_, imp) ->
            imp
        | Trapped _ ->
            TypedVector3.Zero

    let pos = ball.pos + dt * speed

    // Bouncing on the pitch
    let posZ, speedZ, drag =
        match pos.Z - Ball.ballRadius with
        | h when h < 0.0f<m> ->
            Ball.ballRadius,
            -pitchRestitution * speed.Z,
            pitchDrag
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

    { ball with speed = ball.speed + goalImpulses }, impulse
