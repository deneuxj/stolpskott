module CleverRake.StolpSkott.PlayerAi

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


let assignObjectives (env : Environment) formation assign side (getMatchState : unit -> Match.MatchState) (kickerReady : Event<_>) =
    let getTeam() =
        match side with
        | Team.TeamA -> getMatchState().teamA
        | Team.TeamB -> getMatchState().teamB
        
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

    let timeToBall (ball : Ball.State) player =
        let speed = Player.getRunSpeed player
        let relPos = TypedVector2<m>(ball.pos.X, ball.pos.Y) - player.pos

        match TypedVector.tryNormalize2 relPos with
        | Some relDir ->
            let s0 = TypedVector.dot2(TypedVector2<m/s>(ball.speed.X, ball.speed.Y), relDir)
            let relSpeed = speed - s0
            if relSpeed > 0.0f<m/s> then
                relPos.Length / relSpeed
            else
                1.0f<s> * System.Single.PositiveInfinity             
            
        | None -> 0.0f<s>
                
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
                do! env.WaitUntil <|
                    fun () ->
                        let team = getTeam()
                        let player0 = player0 + 1 // Index in destinations -> Index in players (goalie not included in destinations)
                        if player0 > 0 && player0 < team.onPitch.Length then
                            let player0 = team.onPitch.[player0]
                            match player0 with
                            | { activity = Player.Passing } -> true
                            | _ -> false
                        else
                            true
                
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
                                match getTeam().onPitch.[player] with
                                | { activity = Player.Crossing }
                                | { activity = Player.Passing }
                                | { activity = Player.Kicking _ } -> false
                                | _ -> true

                    do! env.WaitUntil <|
                            fun () ->
                                match getTeam().onPitch.[player] with
                                | { activity = Player.Crossing }
                                | { activity = Player.Passing }
                                | { activity = Player.Kicking _ } -> true
                                | _ -> false

                    completed := true
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

    let rec normalPlay =
        task {
            let matchState = getMatchState()
            match matchState with
            | { ball = { inPlay = Ball.LiveBall } as ball ; teamA = teamA ; teamB = teamB } ->
                let team =
                    match side with
                    | Team.TeamA -> teamA
                    | Team.TeamB -> teamB

                let formation =
                    Tactics.tactics Tactics.formation442 side matchState
                    |> List.map getAbsPos
                    |> Array.ofList

                let closestToBall, _ =
                    team.onPitch
                    |> Array.mapi (fun i player -> i, timeToBall ball player)
                    |> Array.minBy snd

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

                // If defending, order a backup player to run to where the ball will soon be
                let backup =
                    if (ball.pos.Y < 0.0f<m>) = attackUp() then
                        let delta = 1.0f<s>
                        let futureBall =
                            { ball with pos = ball.pos + delta * ball.speed }
                        let backup, _ =                        
                            team.onPitch
                            |> Array.mapi (fun i player ->
                                i,
                                if i <> closestToBall then
                                    timeToBall futureBall player
                                else 100.0f<s>)
                            |> Array.minBy snd
                        if backup <> closestToBall then
                            Some (backup, futureBall.pos)
                        else
                            None
                    else
                        None

                match backup with
                | Some (backup, pos) ->
                    RunningTo (TypedVector2<m>(pos.X, pos.Y), absUp()) |> assign backup
                | None ->
                    ()

                // Wait until the ball chaser has reached the ball, with a time limit.
                do! env.WaitUnless(
                        1.0f,
                        fun () ->
                            match getMatchState() with
                            | { ball = { inPlay = Ball.LiveBall } as ball ; teamA = teamA ; teamB = teamB } ->
                                let team =
                                    match side with
                                    | Team.TeamA -> teamA
                                    | Team.TeamB -> teamB
                                let player = team.onPitch.[closestToBall]
                                let ballPos2 = TypedVector2<m>(ball.pos.X, ball.pos.Y)
                                let dist = player.pos - ballPos2 |> TypedVector.len2
                                dist < 1.0f<m>
                            | { ball = { inPlay = Ball.DeadBall _ } } ->
                                true)

                // If the player is close to the ball, decide what to do
                match getMatchState() with
                | { ball = { inPlay = Ball.LiveBall } as ball ; teamA = teamA ; teamB = teamB } ->
                    let team =
                        match side with
                        | Team.TeamA -> teamA
                        | Team.TeamB -> teamB
                    let player = team.onPitch.[closestToBall]
                    let ballPos2 = TypedVector2<m>(ball.pos.X, ball.pos.Y)
                    let distToBall = player.pos - ballPos2 |> TypedVector.len2
                    if distToBall < 1.0f<m> then
                        let goalPos = getAbsPos { x = 0.0f ; y = 1.0f }

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
                            elif distToGoal < 30.0f<m> then
                                let dirToGoal =
                                    goalPos - playerPos |> TypedVector.tryNormalize2
                                match dirToGoal with
                                | Some d ->
                                    if d.X < 0.2f then
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

                        let maxTime = 1.0f<s>
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
                                    | _ -> ()

                            | None ->
                                do! env.WaitNextFrame()
                        | ShouldShoot ->
                            let target = getAbsPos { x = 0.0f ; y = 1.0f }
                            ShootingAtGoal target |> assign closestToBall
                            let! shotCompleted = waitKick maxTime closestToBall
                            if shotCompleted then
                                printfn "SHOOT!"
                            else
                                printfn "Failed to shoot"
                        | ShouldRun ->
                            RunningWithBallTo goalPos
                            |> assign closestToBall
                            do! env.Wait (maxTime / 1.0f<s>)
                | { ball = { inPlay = Ball.DeadBall _ } } -> ()

                return! normalPlay

            | { ball = { inPlay = Ball.DeadBall _ } } ->
                return()
        }

    let orderFormation() =
        let formation =
            Tactics.tactics Tactics.formation442 side (getMatchState())
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
            let keeper = 0
            RunningTo (getAbsPos { x = 0.0f ; y = -1.0f }, absUp()) |> assign keeper

            orderFormation()

            return! waitUntilBallInPlay
        }

    let kickCorner pitchSide =
        task {
            let state = getMatchState()
            orderFormation()
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
            orderFormation()
            return! waitUntilBallInPlay
        }

    let throwIn pitchSide y =
        task {
            orderFormation()
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

            let keeper = 0
            RunningTo (TypedVector2<m>(x, y), absUp()) |> assign keeper

            orderFormation()
            do! env.WaitNextFrame()
            do! waitTeamStill

            kickerReady.Trigger()
            ShootingAtGoal (TypedVector2<m>.Zero) |> assign keeper

            let! _ = waitKick 1.0f<s> keeper
            return! waitUntilBallInPlay
        }

    let defendKickIn =
        task {
            orderFormation()
            return! waitUntilBallInPlay
        }
        
    let defendFreeKick =
        task {
            return! env.WaitNextFrame()
        }

    let freeKick =
        task {
            return! env.WaitNextFrame()
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

    let rec main =
        task {
            match getMatchState() with
            | { period = Match.MatchOver } -> return ()
            | { ball = { inPlay = Ball.KickOff _ } } ->
                printfn "%A: Prepare for kick off" side
                do! prepareForKickOff
                return! main
            | { ball = { inPlay = Ball.ThrowIn(owner, pitchSide, y) } } ->
                if owner = side then
                    printfn "%A: Do throw in" side
                    do! throwIn pitchSide y
                else
                    printfn "%A: Defend throw in" side
                    do! defendThrowIn pitchSide y
                return! main
            | { ball = { inPlay = Ball.KickIn(owner, pitchSide) } } ->
                if owner = side then
                    printfn "%A: Do kick in" side
                    do! kickIn pitchSide
                else
                    printfn "%A: Defend kick in" side
                    do! defendKickIn
                return! main
            | { ball = { inPlay = Ball.CornerKick(_, owner, pitchSide) } } ->
                if owner = side then
                    printfn "%A: Kick corner" side
                    do! kickCorner pitchSide
                else
                    printfn "%A: Defend corner" side
                    do! defendCorner
                return! main
            | { ball = { inPlay = Ball.LiveBall } } ->
                printfn "%A: normal play" side
                do! normalPlay
                return! main
        }

    main


let kickDistance = 0.5f * (Physics.kickMaxDistance + Physics.pushedDistance)
let passDistance = 0.5f * (Physics.controlMaxDistance + Physics.pushedDistance)

let actPlayerOnObjective side (matchState : Match.MatchState) objective (playerState : Player.State) =
    let ball = matchState.ball
    let team =
        match side with
        | Team.TeamA -> matchState.teamA.onPitch
        | Team.TeamB -> matchState.teamB.onPitch

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
        match target - playerState.pos |> TypedVector.tryNormalize2 with
        | Some dir ->
            match distToBall playerState.pos with
            | x when x > 2.0f * Physics.controlMaxDistance ->
                runToPos ballPos2
            | x when x > (5.0f * Physics.pushedDistance + Physics.controlMaxDistance) / 6.0f ->
                let relPos = ballPos2 - playerState.pos
                let toBall = TypedVector.normalize2 relPos
                let side = TypedVector2<1>(toBall.Y, -toBall.X)
                let proj = TypedVector.dot2(dir, side)
                let newDir = toBall - 0.75f * proj * side |> TypedVector.normalize2
                { playerState with direction = newDir ; speed = Player.getRunSpeed playerState }
            | _ ->
                if TypedVector.dot2(ballPos2 - playerState.pos, dir) >= 0.0f<m> then
                    { playerState with direction = dir ; speed = Player.getRunSpeed playerState }
                else // Ball behind the player
                    { playerState with direction = -1.0f * playerState.direction ; speed = Player.getRunSpeed playerState }
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
        match distToBall playerState.pos with
        | x when x < passDistance ->
            { playerState with activity = Player.Trapping }
        | _ ->
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
        { playerState with activity = Player.Standing }
    | CrossingTo pos, Player.Standing ->
        match distToBall playerState.pos with
        | x when x < passDistance ->
            match pos - playerState.pos |> TypedVector.tryNormalize2 with
            | Some d ->
                { playerState with activity = Player.Crossing ; direction = d }
            | None ->
                { playerState with speed = 0.0f<m/s> }
        | _ ->
            runToPos ballPos2
    | CrossingTo _, Player.Crossing ->
        { playerState with activity = Player.Standing ; speed = 0.0f<m/s> }
    | ShootingAtGoal target, Player.Standing ->
        if TypedVector.dot2(target - playerState.pos, playerState.direction) > 0.0f<m> then 
            match distToBall playerState.pos with
            | x when x < Physics.pushedDistance ->
                runToPos (playerState.pos + ballPos2 - target)
            | x when x < kickDistance ->
                match target - playerState.pos |> TypedVector.tryNormalize2 with
                | Some d ->
                    { playerState with activity = Player.Kicking 0.0f<kf> ; direction = d }
                | None ->
                    { playerState with activity = Player.Kicking 0.0f<kf> }
            | _ ->
                runToPos ballPos2
        else
            runWithBallTo target
    | ShootingAtGoal _, Player.Trapping ->
        { playerState with activity = Player.Standing }
    | RunningWithBallTo target, Player.Standing ->
        runWithBallTo target
    | RunningWithBallTo target, Player.Trapping ->
        { playerState with activity = Player.Standing }


