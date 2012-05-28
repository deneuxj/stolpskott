module CleverRake.StolpSkott.Gameplay

open Microsoft.Xna.Framework
open CleverRake.StolpSkott
open CleverRake.StolpSkott.Units
open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

type State =
    { player : Player.State
      ball : Ball.State
    }

type TrainingGameplay(game, content : Content.ContentManager, playerIndex) =
    inherit DrawableGameComponent(game)

    let textures : Rendering.Resources option ref = ref None
    let spriteBatch : Graphics.SpriteBatch option ref = ref None

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
    let state = ref {
            player =
                { pos = TypedVector2<m>(0.0f<m>, -3.0f<m>)
                  direction = TypedVector2<1>(0.0f, 1.0f)
                  speed = 0.0f<m/s>
                  travelled = 0.0f<m>
                  runningFrame = 0
                  activity = Player.Standing
                  traits =
                    { speed = Player.maxRunSpeed
                      stamina = 1.0f<sta>
                      strength = 1.0f<st>
                      length = 1.8f<m>
                      ballControl = 0.5f<bc>
                    }
                  isKeeper = false
                  health = 1.0f<he>
                  condition = 1.0f<sta> }
            ball =
                { pos = TypedVector3<m>(0.0f<m>, 0.0f<m>, 1.0f<m>)
                  speed = TypedVector3<m/s>(0.0f<m/s>, 0.0f<m/s>, 0.0f<m/s>)
                  inPlay = Ball.InPlay
                }
        }
    let mutable prePad = Input.GamePad.GetState(playerIndex)

    override this.LoadContent() =
        textures :=
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
            }
        spriteBatch :=
            Some (new Graphics.SpriteBatch(this.GraphicsDevice))

    override this.Update(gt) =
        let dt = 1.0f<s> * float32 gt.ElapsedGameTime.TotalSeconds
        let hasBallControl =
            (state.Value.player.pos - TypedVector2<m>(state.Value.ball.pos.X, state.Value.ball.pos.Y)).Length < 1.5f<m>
        let pad = Input.GamePad.GetState(playerIndex)
        let playerState =
            Controls.updateControl config dt prePad pad hasBallControl (state.Value.ball.pos.Z > 1.5f<m>) state.Value.player
        let playerState = Player.updateKeyFrame dt playerState
        let playerState = Player.updatePlayer dt playerState
        
        let ballState, impulse = Physics.updateBall goalCenters dt [| ((Match.TeamA, 0), playerState) |] state.Value.ball
        let ballState =
            match impulse with
            | Physics.Trapped owner when owner = (Match.TeamA, 0) ->
                { ballState with pos = TypedVector3<m>(playerState.pos.X, playerState.pos.Y, Ball.ballRadius) }
            | _ -> ballState
            |> Pitch.boundBall pitch

        let ballState =
            if Input.Keyboard.GetState().IsKeyDown(Input.Keys.Space) then
                { ballState with pos = ballState.pos + TypedVector3<m>(0.0f<_>, 0.0f<_>, 10.0f<_>) }
            else
                ballState

        prePad <- pad
        state := { state.Value with player = playerState ; ball = ballState }

    override this.Draw(_) =
        match spriteBatch.Value, textures.Value with
        | Some spriteBatch, Some textures ->
            Rendering.testRender(base.GraphicsDevice, spriteBatch, textures.grassDark, textures.grassLight, textures.whiteLine, textures.ball, textures.playerSprites, textures.goalUpper, textures.goalLower, pitch, state.Value.player, state.Value.ball)
        | _ -> ()