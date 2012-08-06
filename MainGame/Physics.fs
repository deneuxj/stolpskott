module CleverRake.StolpSkott.Physics

open Microsoft.Xna.Framework
open CleverRake.StolpSkott
open CleverRake.StolpSkott.Units
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils

open Ball


type Sphere = Sphere of TypedVector3<m> * float32<m> * TypedVector3<m/s>
type Rectangle = Rectangle of TypedVector3<m> * float32<m> * float32<m> * TypedVector3<1> * TypedVector3<m/s>
type Cylinder = Cylinder of TypedVector3<m> * float32<m> * float32<m> * TypedVector3<1> * TypedVector3<m/s>


let checkCollisionCylinderVsSphere
    (Cylinder(pos1, radius1, length, axis, vel1))
    (Sphere(pos2, radius2, vel2)) =

    let axis = length * axis
    let cast (x : float32<'M>) = LanguagePrimitives.FloatWithMeasure<'M> (float x)
    let tup (v : TypedVector3<'M>) = (cast v.X, cast v.Y, cast v.Z)
    match Collisions.CylinderSphere.computeCollisionTimes
            (tup pos1)
            (tup axis)
            (cast radius1)
            (tup vel1)
            (tup pos2)
            (cast radius2)
            (tup vel2) with
    | [] -> None
    | t :: _ -> Some (1.0f<s> * float32 t)


let checkCollisionRectangleVsSphere
    (Rectangle(pos1, width, length, normal, vel1))
    (Sphere(pos2, radius, vel2)) =

    let relSpeed = vel2 - vel1
    let relPos = pos2 - pos1
    let pn = TypedVector.dot3(relPos, normal)
    let v = relSpeed
    let vn = TypedVector.dot3(v, normal)
    let t = (radius - pn ) / vn
    if t >= 0.0f<s> then
        let relPos' = pos2 + t * vel2 - pos1
        let up, right =
            let up = TypedVector3<1>(Vector3.UnitY)
            let right = TypedVector.cross3(up, normal)
            match TypedVector.tryNormalize3 right with
            | Some r -> TypedVector.cross3(normal, r), r
            | None ->
                let right = TypedVector3<1>(Vector3.UnitX)
                let up = TypedVector.cross3(normal, right) |> TypedVector.normalize3
                up, TypedVector.cross3(up, normal)
        let x = TypedVector.dot3(relPos', right)
        let y = TypedVector.dot3(relPos', up)
        if abs x < width && abs y < length then
            Some t
        else
            None
    else
        None


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

let controlMaxDistance = 1.0f<m> // Beyond this distance, players can't trap the ball
let kickMaxDistance = 1.6f<m> // Distance from the ball within which a player can kick it.
let pushedDistance = 0.3f<m> // Distance before the ball is pushed by a player.
let headerSpeed = 1.0f<m/s> // Speed modifier for headers
let optimalKeeperKeyframe = 0.5f<kf> // The keyframe at which a keeper manages to catch the ball
let keeperCaughtThreshold = 0.05f<kf> // Half-width of the interval in which keepers catch balls
let keeperBounceRestitution = 1.5f // Bounciness of the keeper-ball collisions when the keeper fails to catch the ball
let playerBounceRestitution = 1.8f // Bounciness of player-ball collisions
let playerTackleRestitution = 1.5f // Bounciness of collisions between tackling players and the ball
let maxBallControlSpeed = 20.0f<m/s> // Maximum speed relative to the player under which control is achieved
let minPushSpeedFactor = 1.25f // Affects how far good players push the ball when they have control over it
let maxPushSpeedFactor = 1.5f // Affects how far poor players push the ball when they have control over it
let hardKickElevation = 0.1f // Controls how fast the ball goes up upon leaving the foot of the kicker
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

let collideBallWithPlayer dt (playerId, player : Player.State) ball =
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
            if dist < controlMaxDistance then
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

        | Player.Stumbling _                
        | Player.Tackling _ ->
            let f =
                match player.activity with
                | Player.Tackling _ -> 2.0f
                | _ -> 1.0f

            if isBallGoingTowardsPlayer && dist < f * controlMaxDistance then
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
            // Picture the player pushing a rake in from of him.
            // The ball is pushed if the ball hits the rake.
            let rakeCollision =
                let rake =
                    let dir = vector3Of2 player.direction
                    let rakeWidth = 1.0f<m>
                    let rakeHeight = 1.0f<m>
                    let rakePos =
                        TypedVector3<m>(0.0f<m>, 0.0f<m>, rakeHeight / 2.0f) +
                        pushedDistance * dir +
                        vector3Of2 player.pos
                    Rectangle(rakePos,
                              rakeWidth,
                              rakeHeight,
                              dir,
                              player.speed * dir)
                let sphere =
                    Sphere(ball.pos, ballRadius, ball.speed)
                checkCollisionRectangleVsSphere rake sphere

            match rakeCollision with
            | Some x when x < dt ->
                let factor = MathHelper.Lerp(maxPushSpeedFactor, minPushSpeedFactor, player.traits.ballControl)
                Pushed(playerId, player.speed * factor * (vector3Of2 player.direction))
            | _ ->
                Free
    else
        Free


let collidePlayersWithBall dt ball players =
    let impulse =
        players
        |> Seq.fold (fun impulse player ->
            match impulse, collideBallWithPlayer dt player ball with
            // Free is the neutral element of impulse combination.
            | Free, impulse
            | impulse, Free -> impulse

            // All combinations of non-free impulses result in uncontrolled bounces
            | SomeImpulse imp0, SomeImpulse imp1 -> Bounced (imp0 + imp1)
            
            | _ -> failwith "Unexpected impulse combination" // Should not be reachable
            ) Free

    impulse

type Goal = UpperGoal | LowerGoal
let goalWidth = 7.32f<m>
let goalHeight = 2.44f<m>
let goalPostRadius = 0.07f<m>
let goalPostRestitution = 2.0f
let goalNetDepth = 1.3f<m>
let goalNetRestitution = 1.2f

let collideGoalNetWithBall dt (pitch : Pitch.PitchTraits) goal ball =
    let sphere = Sphere(ball.pos, ballRadius, ball.speed)
    let goalCenter =
        let y = pitch.length * match goal with UpperGoal -> 0.5f | LowerGoal -> -0.5f
        TypedVector2<m>(0.0f<m>, y)

    let normal =
        match goal with
        | UpperGoal -> TypedVector3<1>(0.0f, -1.0f, 0.0f)
        | LowerGoal -> TypedVector3<1>(0.0f, 1.0f, 0.0f)

    let backNet =
        Rectangle(
            TypedVector3<m>(goalCenter.X, goalCenter.Y, goalHeight / 2.0f) - goalNetDepth * normal,
            goalWidth / 2.0f,
            goalHeight / 2.0f,
            normal,
            TypedVector3<m/s>.Zero)

    let leftNet =
        Rectangle(
            TypedVector3<m>(goalCenter.X - goalWidth / 2.0f, goalCenter.Y, goalHeight / 2.0f) - goalNetDepth * normal,
            goalNetDepth / 2.0f,
            goalHeight / 2.0f,
            TypedVector3<1>(1.0f, 0.0f, 0.0f),
            TypedVector3<m/s>.Zero)

    let rightNet =
        Rectangle(
            TypedVector3<m>(goalCenter.X + goalWidth / 2.0f, goalCenter.Y, goalHeight / 2.0f) - goalNetDepth * normal,
            goalNetDepth / 2.0f,
            goalHeight / 2.0f,
            TypedVector3<1>(-1.0f, 0.0f, 0.0f),
            TypedVector3<m/s>.Zero)

    let topNet =
        Rectangle(
            TypedVector3<m>(goalCenter.X, goalCenter.Y, goalHeight) - goalNetDepth * normal,
            goalWidth / 2.0f,
            goalNetDepth / 2.0f,
            TypedVector3<1>(0.0f, 0.0f, -1.0f),
            TypedVector3<m/s>.Zero)

    let computeImpulse (rect, normal) =
        match checkCollisionRectangleVsSphere rect sphere with
        | Some s when s < dt ->
            collideLightWithHeavy goalNetRestitution normal ball.speed
        | _ ->
            TypedVector3<m/s>.Zero

    [|
        (backNet, normal)
        (leftNet, TypedVector3<1>(1.0f, 0.0f, 0.0f))
        (rightNet, TypedVector3<1>(-1.0f, 0.0f, 0.0f))
        (topNet, TypedVector3<1>(0.0f, 0.0f, -1.0f))
    |]
    |> Array.fold (fun impulse net -> let imp = computeImpulse net in imp + impulse) TypedVector3<m/s>.Zero


let collideGoalPostsWithBall dt (pitch : Pitch.PitchTraits) goal ball =
    let sphere = Sphere(ball.pos, ballRadius, ball.speed)
    let goalCenter =
        let y = pitch.length * match goal with UpperGoal -> 0.5f | LowerGoal -> -0.5f
        TypedVector2<m>(0.0f<m>, y)

    let crossBar = Cylinder(
                    TypedVector3(goalCenter.X - goalWidth/2.0f, goalCenter.Y, goalHeight),
                    goalPostRadius,
                    goalWidth,
                    TypedVector3(1.0f, 0.0f, 0.0f),
                    TypedVector3.Zero)
    let leftPost = Cylinder(
                    TypedVector3(goalCenter.X - goalWidth/2.0f, goalCenter.Y, 0.0f<m>),
                    goalPostRadius,
                    goalHeight,
                    TypedVector3(0.0f, 0.0f, 1.0f),
                    TypedVector3.Zero)
    let rightPost = Cylinder(
                        TypedVector3(goalCenter.X + goalWidth/2.0f, goalCenter.Y, 0.0f<m>),
                        goalPostRadius,
                        goalHeight,
                        TypedVector3(0.0f, 0.0f, 1.0f),
                        TypedVector3.Zero)

    let impulseCrossBar =
        match checkCollisionCylinderVsSphere crossBar sphere with
        | Some s when s < dt ->
            let normal =
                TypedVector3(
                    0.0f<m>,
                    goalCenter.Y - ball.pos.Y,
                    goalHeight - ball.pos.Z
                )
                |> TypedVector.normalize3
            collideLightWithHeavy goalPostRestitution normal ball.speed
        | _ ->
            TypedVector3.Zero

    let impulseLeftPost =
        match checkCollisionCylinderVsSphere leftPost sphere with
        | Some s when s < dt ->
            let normal =
                TypedVector3(
                    goalCenter.X - goalWidth / 2.0f - ball.pos.X,
                    goalCenter.Y - ball.pos.Y,
                    0.0f<m>
                )
                |> TypedVector.normalize3
            collideLightWithHeavy goalPostRestitution normal ball.speed
        | _ ->
            TypedVector3.Zero

    let impulseRightPost =
        match checkCollisionCylinderVsSphere leftPost sphere with
        | Some s when s < dt ->
            let normal =
                TypedVector3(
                    goalCenter.X + goalWidth / 2.0f - ball.pos.X,
                    goalCenter.Y - ball.pos.Y,
                    0.0f<m>
                )
                |> TypedVector.normalize3
            collideLightWithHeavy goalPostRestitution normal ball.speed
        | _ ->
            TypedVector3.Zero

    impulseCrossBar + impulseLeftPost + impulseRightPost


let gravity = TypedVector3<m/s^2>(0.0f<_>, 0.0f<_>, -9.8f<_>)
let airDrag = 0.5f</s>

let updateBall pitch (dt : float32<s>) players ball =
    let impulse = collidePlayersWithBall dt ball players
    
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
    let posZ, speedZ =
        match pos.Z - Ball.ballRadius with
        | h when h < 0.0f<m> ->
            Ball.ballRadius,
            -Pitch.pitchRestitution * speed.Z
        | _ ->
            pos.Z, speed.Z

    let drag =
        if pos.Z - Ball.ballRadius < 0.1f<m> then
            Pitch.pitchDrag
        else
            airDrag

    let dragAccel = drag * TypedVector3<m/s>(speed.X, speed.Y, speed.Z)

    let ball =
        { ball with
            pos = TypedVector3(pos.X, pos.Y, posZ)
            speed = TypedVector3(speed.X, speed.Y, speedZ) - dt * dragAccel }

    // Bouncing against goal posts and net
    let goalImpulses =
        [ UpperGoal ; LowerGoal ]
        |> Seq.sumBy (fun goal -> collideGoalPostsWithBall dt pitch goal ball + collideGoalNetWithBall dt pitch goal ball)

    { ball with speed = ball.speed + goalImpulses }, impulse
