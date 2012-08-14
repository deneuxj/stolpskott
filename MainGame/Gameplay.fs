module CleverRake.StolpSkott.Gameplay

open Microsoft.Xna.Framework
open CleverRake.StolpSkott
open CleverRake.StolpSkott.Units
open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils.CoopMultiTasking

type State =
    { player : Player.State
      ball : Ball.State
    }

type MatchGameplay(game, content : Content.ContentManager, playerIndex, playerSide) =
    inherit DrawableGameComponent(game)

    let rscs : Rendering.Resources option ref = ref None
    let spriteBatch : Graphics.SpriteBatch option ref = ref None
    let font : Graphics.SpriteFont option ref = ref None

    let config : Controls.Configuration =
        { direction = fun state -> state.ThumbSticks.Left.X, state.ThumbSticks.Left.Y
          trap = fun state -> state.Triggers.Left > 0.1f
          kick = fun state -> state.Triggers.Right > 0.1f
          cross = fun state -> state.Buttons.RightShoulder = Input.ButtonState.Pressed
        }
    let pitch : Pitch.PitchTraits =
        { width = 68.0f<m>
          length = 105.0f<m>
        }
    let goalCenters =
        [| TypedVector2<m>(0.0f<m>, pitch.length / 2.0f)
           TypedVector2<m>(0.0f<m>, -pitch.length / 2.0f)
        |]
    let state : Match.MatchState ref =
        let playerTraits : Player.Traits =
            { speed = Player.maxRunSpeed
              stamina = 1.0f<sta>
              strength = 1.0f<st>
              length = 1.8f<m>
              ballControl = 0.8f<bc>
            }
        let team : Player.State[] =
            [| for i in 1..11 ->        
                { pos = TypedVector2<m>(-50.0f<m>, -3.0f<m>)
                  direction = TypedVector2<1>(0.0f, 1.0f)
                  speed = 0.0f<m/s>
                  travelled = 0.0f<m>
                  runningFrame = 0
                  activity = Player.Standing
                  traits = playerTraits
                  isKeeper = (i = 1)
                  health = 1.0f<he>
                  condition = 1.0f<sta> }
            |]
        ref {
            teamA =
                { onPitch = team
                  substitutes = []
                }
            teamB =
                { onPitch = team
                  substitutes = [] }
            pitch = pitch
            period = Match.FirstHalf
            periodTime = 0.0f<s>
            ball =
                { pos = TypedVector3<m>(0.0f<m>, 0.0f<m>, 1.0f<m>)
                  speed = TypedVector3<m/s>(0.0f<m/s>, 0.0f<m/s>, 0.0f<m/s>)
                  inPlay = Ball.KickOff(Ball.WaitWhistle, Team.TeamA)
                  lastTouchedBy = None
                }
        }

    let teamAObjectives =
        ref Map.empty

    let assignObjectiveA idx objective =
        teamAObjectives :=
            Map.add idx objective teamAObjectives.Value

    let teamBObjectives =
        ref Map.empty

    let assignObjectiveB idx objective =
        teamBObjectives :=
            Map.add idx objective teamBObjectives.Value

    let goalsA = ref 0
    let goalsB = ref 0

    let ballPhysicsEnabled = ref false
    let kickerReady = new Event<_>()
    let ballKicked = new Event<Team.TeamSide>()
    let matchOver = new Event<int * int>()

    let mutable prePad = Input.GamePad.GetState(playerIndex)
    let scheduler = new Scheduler()
    let env = new Environment(scheduler)

    let playerControlled : (Team.TeamSide * int) option ref = ref None
    let controller =
        task {
            while true do
                do! env.WaitNextFrame()
                let team() =
                    match playerSide with
                    | Team.TeamA -> state.Value.teamA
                    | Team.TeamB -> state.Value.teamB

                let ballPos2() = TypedVector2<m>(state.Value.ball.pos.X, state.Value.ball.pos.Y)

                let getCandidate() =
                    let ballPos2 = ballPos2()
                    team().onPitch
                    |> SeqUtil.minBy (
                        function
                        | { pos = pos ; isKeeper = false } ->
                            pos - ballPos2 |> TypedVector.len2 |> float32
                        | { isKeeper = true } ->
                            System.Single.PositiveInfinity)

                let restPeriod = 0.05f<s>
                let newControledTask =
                    match playerControlled.Value, state.Value.ball.inPlay with
                    | None, Ball.InPlay ->
                        let candidate = getCandidate()
                        if candidate <= 0 then
                            task {
                                playerControlled := None
                                return()
                            }
                        else
                            task {
                                playerControlled := Some(playerSide, candidate)
                                return! env.WaitUnless(
                                            restPeriod / 1.0f<s>,
                                            fun() -> state.Value.ball.inPlay <> Ball.InPlay)
                            }

                    | Some (_, player), Ball.InPlay ->
                        task {
                            let candidate = getCandidate()
                            if candidate <= 0 then
                                return ()
                            else
                                playerControlled := Some(playerSide, candidate)
                                return! env.WaitUnless(
                                            restPeriod / 1.0f<s>,
                                            fun() -> state.Value.ball.inPlay <> Ball.InPlay)
                        }

                    | _, _ ->
                        task {
                            playerControlled := None
                            return! env.WaitUntil <|
                                fun () ->
                                    state.Value.ball.inPlay = Ball.InPlay
                        }

                do! newControledTask                        
        }

    let mutable x = 0.0f<m>
    let mutable y = 0.0f<m>

    member this.UpdateDebugView(dt : float32<s>) =
        let kbState = Input.Keyboard.GetState()
        let velocity = 20.0f<m/s>
        if kbState.IsKeyDown(Input.Keys.Up) then
            y <- y + velocity * dt
        if kbState.IsKeyDown(Input.Keys.Down) then
            y <- y - velocity * dt
        if kbState.IsKeyDown(Input.Keys.Left) then
            x <- x - velocity * dt
        if kbState.IsKeyDown(Input.Keys.Right) then
            x <- x + velocity * dt

    override this.Initialize() =
        base.Initialize()

        PlayerAi.assignObjectives
            env
            Tactics.formation442
            assignObjectiveA
            Team.TeamA
            (fun () -> state.Value)
            kickerReady
        |> scheduler.AddTask

        PlayerAi.assignObjectives
            env
            Tactics.formation442
            assignObjectiveB
            Team.TeamB
            (fun () -> state.Value)
            kickerReady
        |> scheduler.AddTask

        let increaseScore =
            function
            | Team.TeamA -> goalsA := !goalsA + 1
            | Team.TeamB -> goalsB := !goalsB + 1

        task {
            let speedUp = 15.0f // Games last 6 minutes
            let! _ = Referee.refereeTask env speedUp (fun () -> state.Value) (fun s -> state := s) increaseScore kickerReady.Publish ballKicked.Publish
            return ()
        }
        |> scheduler.AddTask

        Director.directorTask
            env
            (fun () -> state.Value)
            (fun ballState -> state := { state.Value with ball = ballState })
            (fun t -> ballPhysicsEnabled := t)
            kickerReady.Publish
        |> scheduler.AddTask

        controller
        |> scheduler.AddTask

    override this.LoadContent() =
        rscs :=
            Some {
                grassLight = content.Load("grass-light")
                grassDark = content.Load("grass-dark")
                goalUpper = content.Load("goal-top")
                goalLower = content.Load("goal-bottom")
                ball = content.Load("ball")
                ballShadow = content.Load("ball")
                playerSprites = content.Load("player")
                playerShadows = content.Load("player")
                whiteLine = content.Load("white")
                ballKick = content.Load("ball-kick")
            }
        spriteBatch :=
            Some (new Graphics.SpriteBatch(this.GraphicsDevice))
        font :=
            Some (content.Load("spriteFont1"))

    override this.Update(gt) =
        match state.Value.period with
        | Match.MatchOver ->
            matchOver.Trigger(goalsA.Value, goalsB.Value)
        | _ ->
            ()

        let dt = 1.0f<s> * float32 gt.ElapsedGameTime.TotalSeconds
        this.UpdateDebugView(dt)

        scheduler.RunFor (float32 dt)

        let pad = Input.GamePad.GetState(playerIndex)

        let updateTeam side =
            match side with
            | Team.TeamA -> state.Value.teamA.onPitch
            | Team.TeamB -> state.Value.teamB.onPitch
            |> Array.map (fun playerState ->
                playerState
                |> Player.updateKeyFrame dt
                |> Player.updatePlayer dt)
            |> Array.mapi (fun i playerState ->
                match playerControlled.Value with
                | Some (team, idx) when team = side && idx = i ->
                    // Human-controlled
                    let hasBallControl =
                        let ballPos2 = Physics.vector2Of3 state.Value.ball.pos
                        let minDistanceToBall =
                            match side with
                            | Team.TeamA -> state.Value.teamB.onPitch
                            | Team.TeamB -> state.Value.teamA.onPitch
                            |> Array.map (fun { pos = pos } -> (pos - ballPos2).Length)
                            |> Array.min
                        (playerState.pos - ballPos2).Length <= minDistanceToBall
                    Controls.updateControl config dt prePad pad hasBallControl (state.Value.ball.pos.Z > Physics.rakeHeight) playerState
                | Some _
                | None ->
                    // AI-controlled
                    let objectives =
                        match side with
                        | Team.TeamA -> teamAObjectives.Value
                        | Team.TeamB -> teamBObjectives.Value

                    match objectives.TryFind i with
                    | Some objective -> PlayerAi.actPlayerOnObjective side state.Value objective playerState
                    | None -> playerState)
        
        let teamA =
            updateTeam Team.TeamA

        let teamB =
            updateTeam Team.TeamB

        let allPlayers =
            let allPlayersA =
                teamA
                |> Array.mapi (fun i playerState -> (Team.TeamA, i), playerState)

            let allPlayersB =
                teamB
                |> Array.mapi (fun i playerState -> (Team.TeamB, i), playerState)
        
            Array.append allPlayersA allPlayersB

        let impulses =
            allPlayers
            |> Array.map (fun player -> Physics.collideBallWithPlayer dt player state.Value.ball)

        let ballState, impulse =
            if ballPhysicsEnabled.Value then
                Physics.updateBall state.Value.pitch dt impulses state.Value.ball
            else
                state.Value.ball, Physics.Free

        // Tackling players that touch the ball: update their state
        let allPlayerStates =
            Array.zip allPlayers impulses
            |> Array.map(fun ((playerId, state), impulse) ->
                match state.activity, impulse with
                | Player.Tackling(keyframe, _), Physics.BouncedOffPlayer(_, _) ->
                    { state with activity = Player.Tackling(keyframe, true) }
                | _ -> state)        
        let teamA = allPlayerStates.[0 .. teamA.Length - 1]
        let teamB = allPlayerStates.[teamA.Length..]

        let ballState =
            match impulse with
            | Physics.Trapped (side, idx) ->
                let player =
                    match side with
                    | Team.TeamA ->
                        teamA.[idx]
                    | Team.TeamB ->
                        teamB.[idx]
                { ballState with pos = TypedVector3<m>(player.pos.X, player.pos.Y, Ball.ballRadius) }
            | _ -> ballState
            |> Pitch.boundBall pitch

        let lastTouchedBy =
            match impulse with
            | Physics.BallImpulse.Trapped (id, _)
            | Physics.BallImpulse.BouncedOffPlayer((id, _), _)
            | Physics.BallImpulse.Kicked((id, _), _)
            | Physics.BallImpulse.Pushed((id, _), _) -> Some id
            | _ -> ballState.lastTouchedBy

        match impulse with
        | Physics.BallImpulse.BouncedOffPlayer ((id, _), _)
        | Physics.BallImpulse.Kicked ((id, _), _) ->
            ballKicked.Trigger(id)
        | _ -> ()

        // Ball sounds
        match impulse with
        | Physics.BallImpulse.Trapped _
        | Physics.BallImpulse.Free -> ()

        | Physics.BallImpulse.Bounced impulse
        | Physics.BallImpulse.BouncedOffPlayer (_, impulse)
        | Physics.BallImpulse.Pushed (_, impulse)
        | Physics.BallImpulse.Kicked (_, impulse) ->
            if impulse.Length > 0.5f<m/s> then
                rscs.Value.Value.ballKick.Play() |> ignore

        let ballState =
            if Input.Keyboard.GetState().IsKeyDown(Input.Keys.Space) then
                { ballState with
                    pos = ballState.pos + TypedVector3<m>(0.0f<_>, 0.0f<_>, 10.0f<_>)
                    lastTouchedBy = lastTouchedBy }
            else
                { ballState with
                    lastTouchedBy = lastTouchedBy }

        prePad <- pad
        state := { state.Value with teamA = { state.Value.teamA with onPitch = teamA } ; teamB = { state.Value.teamB with onPitch = teamB } ; ball = ballState }

    override this.Draw(_) =
        match spriteBatch.Value, rscs.Value with
        | Some spriteBatch, Some textures ->
            let teamA =
                state.Value.teamA.onPitch
                |> Array.map (fun playerState -> (Team.TeamA, playerState))
            let teamB =
                state.Value.teamB.onPitch
                |> Array.map (fun playerState -> (Team.TeamB, playerState))
            let allPlayers = Array.append teamA teamB
            let highlights =
                match playerControlled.Value with
                | Some (side, idx) ->
                    let player =
                        match side with
                        | Team.TeamA -> teamA.[idx] |> snd
                        | Team.TeamB -> teamB.[idx] |> snd
                    [ player.pos.X, player.pos.Y ]
                | None ->
                    []
            Rendering.testRender(base.GraphicsDevice, spriteBatch, textures.grassDark, textures.grassLight, textures.whiteLine, textures.ball, textures.playerSprites, textures.goalUpper, textures.goalLower, pitch, allPlayers, highlights, state.Value.ball, (x, y))
        | _ -> ()

        match spriteBatch.Value, font.Value with
        | Some spriteBatch, Some font ->
            let space = 30.0f
            let x = base.GraphicsDevice.Viewport.TitleSafeArea.Left |> float32
            let y = base.GraphicsDevice.Viewport.TitleSafeArea.Top |> float32

            let minutes, seconds =
                match state.Value with
                | { periodTime = time } ->
                    let minutes = int (time / 60.0f)
                    let seconds = int (time - (float32 minutes) * 60.0f<s>)
                    minutes, seconds

            try
                spriteBatch.Begin()
                spriteBatch.DrawString(font, goalsA.Value.ToString(), Vector2(x, y), Color.Blue)
                spriteBatch.DrawString(font, goalsB.Value.ToString(), Vector2(x + space, y), Color.Red)
                spriteBatch.DrawString(font, sprintf "%d:%d" minutes seconds, Vector2(x + 5.0f * space, y), Color.Black)
            finally
                spriteBatch.End()
        | _ -> ()

    [<CLIEvent>]
    member this.MatchOver = matchOver.Publish