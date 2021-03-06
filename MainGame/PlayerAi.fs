﻿module CleverRake.StolpSkott.PlayerAi

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils.CoopMultiTasking

open Units

type AiPlayerObjective =
    | RunningToBall
    | RunningWithBallTo of TypedVector2<m>
    | FollowingTactic
    | RunningTo of TypedVector2<m> * TypedVector2<1>
    | PassingTo of int
    | CrossingTo of TypedVector2<m>
    | ShootingAtGoal of TypedVector2<m>


type KeeperControl =
    | KeeperGrabbed
    | KeeperReleased


let assignObjectives (env : Environment) formation assign side (getMatchState : unit -> Match.MatchState) (kickerReady : Event<_>) =
    let getTeam() =
        match side with
        | Team.TeamA -> getMatchState().teamA
        | Team.TeamB -> getMatchState().teamB

    let getOpponents() =
        match side with
        | Team.TeamA -> getMatchState().teamB
        | Team.TeamB -> getMatchState().teamA

    let getBall() =
        getMatchState().ball
                
    let attackUp() = Match.isTeamAttackingUp side (getMatchState().period)
    let getRelPos pos = Tactics.getRelPos (getMatchState().pitch) (attackUp()) pos
    let getRelY pos = pos |> getRelPos |> fun { y = y } -> y
    let getAbsPos pos = Tactics.getAbsPos (getMatchState().pitch) (attackUp()) pos
    let getAbsDir dir = Tactics.getAbsDir (attackUp()) dir
    let absUp() = TypedVector2<1>(0.0f, 1.0f) |> getAbsDir

    let waitUntilBallInPlay =
        task {
            return! env.WaitUntil <|
                fun () ->
                    match getMatchState().ball.inPlay with
                    | Ball.InPlay -> true
                    | _ -> false
        }

    let waitUntilBallEngaged =
        task {
            return! env.WaitUntil <|
                fun () ->
                    match getMatchState().ball.inPlay with
                    | Ball.CornerKick(Ball.CanKick, _, _)
                    | Ball.KickOff(Ball.CanKick, _)
                    | Ball.Penalty(Ball.CanKick, _) ->
                        true
                    | _ -> false
        }

    let pickTwoClosest target positions =
        let far = 1.0f<m> * System.Single.PositiveInfinity
        let _, ((player0, _), (player1, _)) =
            positions
            |> Array.fold (fun (i, ((i0, d0), (i1, d1) as x)) v ->
                let d = target - v |> TypedVector.len2
                let matches =
                    if d < d0 then
                        ((i, d), (i0, d0))
                    elif d < d1 then
                        ((i0, d0), (i, d))
                    else
                        x
                (i + 1, matches)) (0, ((-1, far), (-1, far)))

        (player0, player1)
                
    let prepareForKickOff =
        task {
            // Goal keeper goes to the goal
            (getAbsPos { x = 0.0f ; y = -1.0f }, absUp()) |> RunningTo |> assign 0

            // Field players place themselves according to the formation            
            let destinations =
                formation
                |> List.map (Tactics.transform 0.8f 0.25f 0.0f -0.3f)
                |> List.map getAbsPos
                // Field players stand out of the circle.
                |> List.map (fun v ->
                    let dist = v.Length
                    if dist < 9.15f<m> then
                        let v = 1.0f / dist * v
                        11.0f<m> * v
                    else
                        v)
                |> List.map (fun dest -> (dest, absUp()))
                |> Array.ofList

            // If the ball is ours, the two players closest to the ball go to it
            let ballIsOurs =
                match getMatchState().ball.inPlay with
                | Ball.KickOff(_, owner) when owner = side -> true
                | _ -> false

            if ballIsOurs then
                // Pick two players to do the kick-off.
                // The first kicks the ball, the second catches it.
                let player0, player1 =
                    destinations
                    |> Array.map fst
                    |> pickTwoClosest (TypedVector2<m>.Zero)
                 
                if player0 < 0 || player0 >= destinations.Length ||
                    player1 < 0 || player1 >= destinations.Length then
                    failwith "Could not find two players to kick off the ball"

                destinations.[player0] <- TypedVector2<m>(-2.0f<m>, 0.0f<m>), absUp()
                destinations.[player1] <- TypedVector2<m>(7.0f<m>, if attackUp() then -7.0f<m> else 7.0f<m>), absUp()
            
                destinations
                |> Array.iteri(fun i v -> RunningTo v |> assign (i + 1))

                do! waitUntilBallEngaged
                kickerReady.Trigger()

                // Order the first player to pass the ball to the second.
                PassingTo (player1 + 1) |> assign (player0 + 1)

                // Wait until the first player has passed the ball.
                do! waitUntilBallInPlay
                
                // All players follow the tactic.
                getTeam().onPitch
                |> Array.iteri(fun i _ -> FollowingTactic |> assign i)
            else
                destinations
                |> Array.iteri(fun i v -> RunningTo v |> assign (i + 1))

                do! waitUntilBallInPlay
        }

    let waitKick maxTime player =
        task {
            let completed = ref false
            let timer = env.Spawn (fun killed -> env.WaitUnless(maxTime / 1.0f<s>, fun () -> !killed))
            let cond = env.Spawn (fun killed ->
                task {
                    do! env.WaitUntil <|
                            fun () ->
                                !killed
                                ||
                                match getTeam().onPitch.[player] with
                                | { activity = Player.Crossing }
                                | { activity = Player.Passing }
                                | { activity = Player.Kicking _ } -> false
                                | _ -> true

                    do! env.WaitUntil <|
                            fun () ->
                                !killed
                                ||
                                match getTeam().onPitch.[player] with
                                | { activity = Player.Crossing }
                                | { activity = Player.Passing }
                                | { activity = Player.Kicking _ } -> true
                                | _ -> false

                    completed := not !killed
                    return ()
                })

            do! env.WaitUntil <| fun () -> timer.IsDead || cond.IsDead
            timer.Kill()
            cond.Kill()
            return !completed
        }

    let waitMove maxTime player =
        task {
            return! env.WaitUnless(
                        maxTime / 1.0f<s>,
                        fun () ->
                            getTeam().onPitch.[player].speed > 0.0f<m/s>)
        }

    let waitStill maxTime player =
        task {
            return! env.WaitUnless(
                        maxTime / 1.0f<s>,
                        fun () ->
                            getTeam().onPitch.[player].speed = 0.0f<m/s>)
        }

    let getClearUpDirection (dir : TypedVector2<1>) =
        let x =
            if dir.X > 0.7f then 1.0f
            elif dir.X < -0.7f then -1.0f
            else 0.0f

        let y =
            let up = absUp()
            if up.Y * dir.Y >= 0.0f then up.Y
            else 0.0f

        TypedVector2<1>(x, y)
        |> TypedVector.normalize2

    // Notify the keeper task to stop issuing objectives to the keeper
    let keeperControl = env.NewChannel<KeeperControl>()
    let grabKeeper order = task {
        do! keeperControl.Send(KeeperGrabbed)
        let keeper = 0 in order |> assign keeper
        return ()
    }
    let releaseKeeper = task { return! keeperControl.Send(KeeperReleased) }
    let keeper killed =
        let ballInBox() =
            let state = getMatchState()
            let distY =
                if attackUp() then
                    state.ball.pos.Y +
                    state.pitch.length / 2.0f
                else
                    state.pitch.length / 2.0f
                    - state.ball.pos.Y
            abs(state.ball.pos.X) < Pitch.penaltyBoxWidth / 2.0f && distY < Pitch.penaltyBoxHeight

        let runToDefensivePos() =
            let keeper = 0
            let state = getMatchState()
            let ballPos2 = TypedVector2<m>(state.ball.pos.X, state.ball.pos.Y)
            let goalPos = getAbsPos { x = 0.0f ; y = -1.0f }
            let goalToBall = ballPos2 - goalPos
            match TypedVector.tryNormalize2 goalToBall with
            | Some dir ->
                let len = min 16.0f<m> (goalToBall.Length / 3.0f)
                RunningTo (goalPos + len * dir, dir) |> assign keeper
            | None ->
                RunningTo (goalPos, absUp()) |> assign keeper

        let rec waitStart =
            task {
                let! msg = keeperControl.Receive()
                match msg with
                | KeeperReleased -> return()
                | KeeperGrabbed -> return! waitStart
            }
        let shouldStop =
            task {
                if keeperControl.IsEmpty() then
                    return false
                else
                    let! msg = keeperControl.Receive()
                    match msg with
                    | KeeperReleased -> return false
                    | KeeperGrabbed -> return true
            }
        let rec controlKeeper killed =
            let keeper = 0
            task {
                do! env.WaitNextFrame()
                if not !killed then
                    if ballInBox() then
                        let keeperState = getTeam().onPitch.[0]
                        let target =
                            keeperState.pos + 50.0f<m> * getClearUpDirection keeperState.direction
                        CrossingTo target |> assign keeper
                    else
                        runToDefensivePos()
                let! stop = shouldStop
                if not(stop || !killed) then
                    return! controlKeeper killed
                else
                    return ()
            }

        task {
            while not !killed do
                do! waitStart
                do! controlKeeper killed
        }

    // Compute the objective of the player closest to the ball.
    let driveBall closestToBall =
        task {
            let team = getTeam()
            let ball = getBall()

            let player = team.onPitch.[closestToBall]
            let ballPos2 = TypedVector2<m>(ball.pos.X, ball.pos.Y)

            if closestToBall = 0 then
                // Goal keeper is closest to the ball, clear it.
                let target =
                    player.pos + 50.0f<m> * getClearUpDirection player.direction
                CrossingTo target |> assign closestToBall
                let! _ = waitKick 1.0f<s> closestToBall
                ()
            else
                // Run with the ball for a very short while, to avoid pinball-style play.
                let goalPos = getAbsPos { x = 0.0f ; y = 1.0f }
                RunningWithBallTo goalPos |> assign closestToBall
                do! env.Wait(0.25f)
                let team = getTeam()
                let player = team.onPitch.[closestToBall]
                let ball = getBall()
                let ballPos2 = TypedVector2<m>(ball.pos.X, ball.pos.Y)
                let distToBall = player.pos - ballPos2 |> TypedVector.len2

                let decidePass =
                    let valueOfAttackingPlayer (player : Player.State) =        
                        let timeToGoal =
                            TypedVector.len2(player.pos - goalPos) / Player.getRunSpeed player

                        let distanceToOwnGoal =
                            TypedVector.len2(player.pos - getAbsPos { x = 0.0f ; y = -1.0f })

                        let distToBallScore =
                            let bestDist = 15.0f<m>
                            let span = 5.0f<m>
                            max 0.0f (1.0f - abs (distToBall - bestDist) / span)
        
                        let timeToGoalScore =
                            let maxTime = 10.0f<s>
                            max 0.0f ((maxTime - timeToGoal) / maxTime)

                        let crossToOwnGoalPenalty =
                            if distanceToOwnGoal < 30.0f<m> && TypedVector.len2(player.pos - ballPos2) > 20.0f<m> then
                                -1.0f
                            else
                                0.0f

                        distToBallScore + timeToGoalScore + crossToOwnGoalPenalty

                    fun pos ->
                        let otherPlayers =
                            team.onPitch
                            |> Array.mapi (fun i player -> (i, player))
                            |> Array.filter (fun (_, { pos = pos }) -> TypedVector.dot2(pos - player.pos, player.direction) > 0.0f<m>)

                        if Array.isEmpty otherPlayers then
                            None
                        else
                            let passTo, _ =
                                otherPlayers
                                |> Array.map (fun (i, player) -> (i, valueOfAttackingPlayer player))
                                |> Array.maxBy snd
                            let targetPos = team.onPitch.[passTo].pos
                            if TypedVector.len2(pos - targetPos) > 30.0f<m> then
                                Some (CrossingTo targetPos)
                            else
                                Some (PassingTo passTo)

                let bestPass = decidePass player.pos
                let targetPos =
                    match bestPass with
                    | Some (PassingTo idx) -> Some (team.onPitch.[idx].pos)
                    | Some (CrossingTo x) -> Some x
                    | None -> None
                    | _ -> failwith "Unhandled pass type"
                    
                let (|ShouldShoot|ShouldRun|ShouldPass|) (playerPos : TypedVector2<m>, targetPos : TypedVector2<m> option) =
                    let distToGoal = 
                        goalPos - playerPos |> TypedVector.len2
                    // Shoot if close to the goal
                    if distToGoal < 6.0f<m> then
                        ShouldShoot
                    elif distToGoal < 12.0f<m> then
                        let dirToGoal =
                            goalPos - playerPos |> TypedVector.tryNormalize2
                        match dirToGoal with
                        | Some d ->
                            if abs d.X < 0.8f then
                                ShouldShoot
                            else
                                ShouldRun
                        | None ->
                            ShouldShoot
                    else
                        match targetPos with
                        | Some targetPos ->                                         
                            // Run with the ball if in opponent's half and the pass receiver is further back
                            if getRelY playerPos > 0.0f && getRelY targetPos < getRelY playerPos then
                                ShouldRun
                            // Run with the ball if pass receiver is too close
                            elif TypedVector.len2 (targetPos - playerPos) < 10.0f<m> then
                                ShouldRun
                            else ShouldPass
                        | None -> ShouldRun

                let maxTime = 0.5f<s>
                match (player.pos, targetPos) with
                | ShouldPass ->
                    match bestPass with
                    | Some bestPass ->
                        bestPass |> assign closestToBall                                    
                        let! passed = waitKick maxTime closestToBall
                        if passed then
                            match bestPass with
                            | PassingTo idx ->
                                RunningToBall |> assign idx
                            | _ ->
                                ()
                        else
                            ()

                    | None ->
                        do! env.WaitNextFrame()
                | ShouldShoot ->
                    ShootingAtGoal goalPos |> assign closestToBall
                    let! shotCompleted = waitKick maxTime closestToBall
                    return ()
                | ShouldRun ->
                    RunningWithBallTo goalPos
                    |> assign closestToBall
                    return! env.Wait (maxTime / 1.0f<s>)
        }

    let normalPlay =
        task {
            let team = getTeam()
            let ball = getBall()

            // All field players run to the ball
            team.onPitch
            |> Array.iteri (fun i player ->
                if not player.isKeeper then
                    RunningToBall |> assign i
            )

            // Wait until a player is close to the ball
            let! closestToBall =
                task {
                    let rec work =
                        task {
                            match getMatchState() with
                            | { ball = { inPlay = Ball.LiveBall }} ->
                                let team = getTeam()
                                let ball = getBall()
                                let closestToBall, distance =
                                    team.onPitch
                                    |> Array.mapi (fun i player -> i, (player.pos - Physics.vector2Of3 ball.pos).Length)
                                    |> Array.minBy snd
                                if distance < 10.0f<m> then
                                    return Some closestToBall
                                else
                                    do! env.WaitNextFrame()
                                    return! work
                            | _ -> return None
                        }
                    return! work
                }

            match closestToBall with
            | Some closestToBall ->
                let formation =
                    Tactics.getPlayFormation Tactics.formation442 side (getMatchState())
                    |> List.map getAbsPos
                    |> Array.ofList

                // Order the player closest to the ball to run to the ball,
                // Other players run to the position assigned by the formation.
                team.onPitch
                |> Array.iteri (fun i _ ->
                    if i = closestToBall then
                        RunningToBall |> assign closestToBall
                    elif i >= 1 then
                        RunningTo (formation.[i - 1], absUp()) |> assign i
                    else
                        RunningTo (getAbsPos { x = 0.0f ; y = -0.95f }, absUp()) |> assign i)

                // Activate or pause the keeper task
                match closestToBall with
                | 0 ->
                    // Pause
                    do! grabKeeper RunningToBall
                | _ ->
                    // Activate
                    do! releaseKeeper

                return! driveBall closestToBall

            | None ->
                ()
        }

    let orderFormation situationFun =
        let formation =
            situationFun Tactics.formation442 side (getMatchState())
            |> List.map getAbsPos
            |> Array.ofList

        formation
        |> Array.iteri (fun i v -> RunningTo (v, absUp()) |> assign (i+1))

    let waitTeamStill =
        task {
            do! env.WaitUntil <|
                fun () ->
                    getTeam().onPitch
                    |> Array.exists (function { activity = Player.Standing ; speed = 0.0f<m/s> } -> false | _ -> true)
                    |> not        
        }
                    
    let defendCorner =
        task {
            do! grabKeeper (RunningTo (getAbsPos { x = 0.0f ; y = -1.0f }, absUp()))

            orderFormation Tactics.getCornerDefenseFormation

            return! waitUntilBallInPlay
        }

    let kickCorner pitchSide =
        task {
            do! grabKeeper (RunningTo (getAbsPos { x = 0.0f ; y = -0.75f }, absUp()))
            let state = getMatchState()
            orderFormation Tactics.getCornerAttackFormation
            do! env.WaitNextFrame()
            do! waitTeamStill
            let x =
                match pitchSide with
                | Ball.Left -> -state.pitch.width / 2.0f
                | Ball.Right -> state.pitch.width / 2.0f
            let y =
                if attackUp() then
                    state.pitch.length / 2.0f
                else
                    -state.pitch.length / 2.0f
            let corner = TypedVector2<m>(x, y)
            let dir =
                TypedVector2<1>(
                    match pitchSide with
                    | Ball.Left -> 1.0f<1>
                    | Ball.Right -> -1.0f<1>
                    ,
                    if attackUp() then
                        -1.0f<1>
                    else
                        1.0f<1>
                )
                |> TypedVector.normalize2

            let kicker, receiver = pickTwoClosest corner (getTeam().onPitch |> Array.map (function { pos = pos } -> pos ))

            RunningTo (corner - 5.0f<m> * dir, dir) |> assign kicker
            RunningTo (corner + 6.0f<m> * dir, -1.0f * dir) |> assign receiver

            do! waitMove 1.0f<s> kicker
            do! waitStill 10.0f<s> kicker
            do! waitStill 10.0f<s> receiver

            do! waitUntilBallEngaged
            kickerReady.Trigger()

            let targetY =
                if attackUp() then
                    state.pitch.length / 2.0f - 6.0f<m>
                else
                    -state.pitch.length / 2.0f + 6.0f<m>

            CrossingTo(TypedVector2<m>(0.0f<m>, targetY)) |> assign kicker
            let! _ = waitKick 1.0f<s> kicker

            return! waitUntilBallInPlay
        }

    let defendThrowIn pitchSide y =
        task {
            do! grabKeeper (RunningTo (getAbsPos { x = 0.0f ; y = -1.0f }, absUp()))
            orderFormation (Tactics.getThrowInFormation pitchSide y)
            return! waitUntilBallInPlay
        }

    let throwIn pitchSide y =
        task {
            do! grabKeeper (RunningTo (getAbsPos { x = 0.0f ; y = -1.0f }, absUp()))
            orderFormation (Tactics.getThrowInFormation pitchSide y)
            do! env.WaitNextFrame()
            do! waitTeamStill

            // Find the players closest to the throw-in position
            let x =
                let pitch = getMatchState().pitch
                match pitchSide with
                | Ball.Left -> -pitch.width / 2.0f
                | Ball.Right -> pitch.width / 2.0f

            let pos = TypedVector2<m>(x, y)

            let team = getTeam()
            let thrower, receiver =
                team.onPitch
                |> Array.map (function { pos = v } -> v)
                |> pickTwoClosest pos

            // Order to move into position to throw and receive the ball
            if thrower >= 0 && receiver >= 0 then
                RunningTo(pos, absUp()) |> assign thrower
                let xReceiver =
                    let space = 10.0f<m>
                    match pitchSide with
                    | Ball.Left -> x + space
                    | Ball.Right -> x - space
                RunningTo(TypedVector2<m>(xReceiver, y), absUp()) |> assign receiver
            else
                failwith "No player available to throw the ball in"

            // Wait until they are in position
            do! env.WaitNextFrame()
            do! env.WaitUntil <|
                fun () ->
                    let team = getTeam()
                    let thrower, receiver = team.onPitch.[thrower], team.onPitch.[receiver]
                    thrower.speed = 0.0f<m/s> && receiver.speed = 0.0f<m/s>

            // Do the throw in
            kickerReady.Trigger()
            do! env.WaitNextFrame()
            PassingTo receiver |> assign thrower
            do! env.WaitNextFrame()

            // Wait until the throw is completed
            do! env.WaitUntil <|
                fun () ->
                    let team = getTeam()
                    let thrower = team.onPitch.[thrower]
                    match thrower.activity with
                    | Player.Passing -> true
                    | _ -> false

            // The thrower is done
            FollowingTactic |> assign thrower

            return! waitUntilBallInPlay
        }

    let kickIn pitchSide =
        task {
            let state = getMatchState()
            let y =
                if attackUp() then
                    -state.pitch.length / 2.0f
                else
                    state.pitch.length / 2.0f
            let x =
                match pitchSide with
                | Ball.Left -> -Pitch.goalBoxWidth / 2.0f
                | Ball.Right -> Pitch.goalBoxWidth / 2.0f

            do! grabKeeper (RunningTo (TypedVector2<m>(x, y), absUp()))

            orderFormation Tactics.getKickInFormation
            do! env.WaitNextFrame()
            do! waitTeamStill

            kickerReady.Trigger()
            let keeper = 0
            ShootingAtGoal (TypedVector2<m>.Zero) |> assign keeper

            let! _ = waitKick 1.0f<s> keeper
            return! waitUntilBallInPlay
        }

    let defendKickIn =
        task {
            do! grabKeeper (RunningTo (getAbsPos { x = 0.0f ; y = -1.0f }, absUp()))
            orderFormation Tactics.getKickInFormation
            return! waitUntilBallInPlay
        }

    let moveAwayFrom center pos =
        let minDist = 9.5f<m>
        let relPos = pos - center
        let dist = TypedVector.len2 relPos
        if dist < minDist then
            let d = (1.0f / dist) * relPos
            center + minDist * d
        else
            pos

    let defendFreeKick kickPos =
        task {
            do! grabKeeper (RunningTo (getAbsPos { x = 0.0f ; y = -1.0f }, absUp()))
            orderFormation Tactics.getFreeKickDefenseFormation

            let ballInPlay =
                env.Spawn <|
                    fun _ -> waitUntilBallInPlay

            let moveAwayFromBall =
                env.Spawn <|
                    fun _ -> task {
                        do! waitTeamStill
                        getTeam().onPitch
                        |> Array.iteri (fun i { pos = pos } -> RunningTo(moveAwayFrom kickPos pos, absUp()) |> assign i)
                    }

            do! env.WaitUntil <| fun () -> ballInPlay.IsDead
            moveAwayFromBall.Kill()
        }

    let waitOpponentsAway center =
        task {
            let minDist = 5.0f<m>
            return! env.WaitUntil <|
                fun () ->
                    getOpponents().onPitch
                    |> Array.exists (fun { pos = pos } -> (pos - center).Length < minDist)
                    |> not        
        }

    let freeKick kickPos =
        task {
            do! grabKeeper (RunningTo (getAbsPos { x = 0.0f ; y = -1.0f }, absUp()))
            orderFormation Tactics.getFreeKickFormation
            do! env.WaitNextFrame()
            do! waitTeamStill

            let closest, second = pickTwoClosest kickPos (getTeam().onPitch |> Array.map (fun { pos = pos } -> pos))
            let target = getAbsPos { x = 0.0f ; y = 1.0f }

            do! waitOpponentsAway kickPos
            kickerReady.Trigger()

            if (target - kickPos).Length < 30.0f<m> then
                ShootingAtGoal target |> assign closest
            else
                PassingTo second |> assign closest

            return! waitUntilBallInPlay
        }

    let defendPenalty =
        task {
            return! env.WaitNextFrame()
        }

    let kickPenalty =
        task {
            return! env.WaitNextFrame()
        }

    let celebrate =
        task {
            return! env.WaitNextFrame()
        }

    let matchIsOver() =
        match getMatchState().period with
        | Match.MatchOver -> true
        | _ -> false

    let loop =
        task {
            match getMatchState() with
            | { period = Match.MatchOver } ->
                return ()
            | { ball = { inPlay = Ball.KickOff _ } } ->
                printfn "%A: Prepare for kick off" side
                return! prepareForKickOff
            | { ball = { inPlay = Ball.ThrowIn(owner, pitchSide, y) } } ->
                if owner = side then
                    printfn "%A: Do throw in" side
                    return! throwIn pitchSide y
                else
                    printfn "%A: Defend throw in" side
                    return! defendThrowIn pitchSide y
            | { ball = { inPlay = Ball.KickIn(owner, pitchSide) } } ->
                if owner = side then
                    printfn "%A: Do kick in" side
                    return! kickIn pitchSide
                else
                    printfn "%A: Defend kick in" side
                    return! defendKickIn
            | { ball = { inPlay = Ball.CornerKick(_, owner, pitchSide) } } ->
                if owner = side then
                    printfn "%A: Kick corner" side
                    return! kickCorner pitchSide
                else
                    printfn "%A: Defend corner" side
                    return! defendCorner
            | { ball = { inPlay = Ball.FreeKick(owner, kickPos) } } ->
                if owner = side then
                    printfn "%A: Kick free kick" side
                    return! freeKick kickPos
                else
                    printfn "%A: Defend free kick" side
                    return! defendFreeKick kickPos
            | { ball = { inPlay = Ball.LiveBall } as ball } ->
                printfn "%A: normal play" side
                return! normalPlay
        }

    task {
        let keeper = env.Spawn keeper
        let loop = env.SpawnRepeat loop
        do! env.WaitUntil <|
            fun () ->
                match getMatchState() with
                | { period = Match.MatchOver } -> true
                | _ -> false
        loop.Kill()
        keeper.Kill()
        do! env.WaitUntil(fun () -> keeper.IsDead && loop.IsDead)
    }


let actPlayerOnObjective (team : Player.State[]) (pitch : Pitch.PitchTraits) (ball : Ball.State) objective (playerState : Player.State) =

    let ballPos2 = TypedVector2<m>(ball.pos.X, ball.pos.Y)
    let ballSpeed2 = TypedVector2<m/s>(ball.speed.X, ball.speed.Y)
    let distToBall pos =
        pos - ballPos2
        |> TypedVector.len2

    let runToPos destination =
        let dir = destination - playerState.pos
        let dist = dir.Length
        if dist > 0.1f<m> then
            let dir = 1.0f / dist * dir
            { playerState with direction = dir; activity = Player.Standing; speed = Player.getRunSpeed playerState }
        else
            { playerState with activity = Player.Standing; speed = 0.0f<m/s> }

    let runWithBallTo target =
        let relPos = ballPos2 - playerState.pos
        match TypedVector.tryNormalize2 relPos with
        | Some toBall ->
            match target - ballPos2 |> TypedVector.tryNormalize2 with
            | Some dir ->
                let ballNearLine =
                    abs(ballPos2.X) > pitch.width / 2.0f - 3.0f<m>
                    ||
                    abs(ballPos2.Y) > pitch.length / 2.0f - 3.0f<m>
                if ballNearLine && Physics.canTrap ball playerState then
                    // Avoid running out of the pitch with the ball
                    { playerState with direction = dir; speed = 0.0f<m/s> ; activity = Player.Trapping }
                else
                    let side = TypedVector2<1>(dir.Y, -dir.X)
                    match TypedVector.dot2(toBall, dir) with
                    | x when x >= 0.0f ->
                        // Player is on the right side of the ball
                        if x > 0.95f || Physics.canPush ball playerState then
                            // Push the ball
                            runToPos target
                        else
                            // Adjust course    
                            let playerSide = TypedVector2<1>(toBall.Y, -toBall.X)
                            let newDir =
                                dir +
                                if TypedVector.dot2(toBall, side) >= 0.0f then
                                    playerSide
                                else
                                    -1.0f * playerSide
                                |> TypedVector.normalize2
                            { playerState with direction = newDir ; speed = Player.getRunSpeed playerState }
                    | x ->
                        if Physics.canTrap ball playerState then
                            { playerState with direction = dir ; speed = 0.0f<m/s> ; activity = Player.Trapping }
                        else
                            runToPos ballPos2
            | None ->
                // Already at target. Stop
                { playerState with speed = 0.0f<m/s> ; activity = Player.Standing }
        | None ->
            runToPos target

    match objective, playerState.activity with
    | _, Player.Fallen _
    | _, Player.Stumbling _
    | _, Player.Jumping _
    | _, Player.KeeperDive _
    | _, Player.Kicking _
    | _, Player.Tackling _ ->
        // Player unavailable, activity cannot be changed
        playerState
    | FollowingTactic, _ ->
        { playerState with speed = 0.0f<m/s> }
    | RunningTo (dest, dir), _ ->
        let newState = runToPos dest
        match newState with
        | { speed = 0.0f<m/s> } -> { newState with direction = dir }
        | _ -> newState
    | RunningToBall, _ ->
        runToPos ballPos2
    | PassingTo other, Player.Standing ->
        if Physics.canTrap ball playerState then
            { playerState with activity = Player.Trapping }
        else
            runToPos ballPos2
    | PassingTo other, Player.Trapping ->
        let dir =
            if other >= 0 && other < team.Length then
                team.[other].pos - playerState.pos
                |> TypedVector.tryNormalize2
            else None

        match dir with
        | None ->
            { playerState with activity = Player.Passing }
        | Some d ->
            { playerState with direction = d ; activity = Player.Passing }
    | _, Player.Passing ->
        { playerState with activity = Player.Stumbling 0.0f<s> }

    | CrossingTo target, Player.Standing ->
        if Physics.canTrap ball playerState then
            match target - playerState.pos |> TypedVector.tryNormalize2 with
            | Some d ->
                { playerState with activity = Player.Trapping ; direction = d }
            | None ->
                { playerState with activity = Player.Trapping }
        else
        runToPos ballPos2
    | CrossingTo _, Player.Trapping ->
        { playerState with activity = Player.Crossing }
    | CrossingTo _, Player.Crossing ->
        { playerState with activity = Player.Standing ; speed = 0.0f<m/s> }

    | ShootingAtGoal target, Player.Standing ->
        if Physics.canTrap ball playerState then
            match target - playerState.pos |> TypedVector.tryNormalize2 with
            | Some d ->
                { playerState with activity = Player.Trapping ; direction = d }
            | None ->
                { playerState with activity = Player.Trapping }
        else
            runToPos ballPos2
    | ShootingAtGoal _, Player.Trapping ->
        { playerState with activity = Player.Kicking 0.0f<kf> }
    
    | RunningWithBallTo target, Player.Standing
    | RunningWithBallTo target, Player.Trapping ->
        runWithBallTo target
