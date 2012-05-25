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

type TrainingGameplay(game, content : Content.ContentManager) =
    inherit DrawableGameComponent(game)

    let textures : Rendering.Resources option ref = ref None
    let spriteBatch : Graphics.SpriteBatch option ref = ref None

    let config : Controls.Configuration =
        { direction = fun state -> state.ThumbSticks.Left.X, state.ThumbSticks.Left.Y
          trap = fun state -> state.Triggers.Left > 0.1f
          kick = fun state -> state.Triggers.Right > 0.1f
        }
    let pitch : Team.PitchTraits =
        { width = 68.0f<m>
          length = 105.0f<m>
        }
    let state = ref {
            player =
                { pos = TypedVector2<m>()
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
                      ballControl = 1.0f<bc>
                    }
                  isKeeper = false
                  health = 1.0f<he>
                  condition = 1.0f<sta> }
            ball =
                { pos = TypedVector3<m>()
                  speed = TypedVector3<m/s>()
                  inPlay = Ball.InPlay
                }
        }
    let hasBallControl = ref false
    let controller = ref Controls.Running

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
        let control, playerState =
            Controls.updateControl config (Input.GamePad.GetState(PlayerIndex.One)) hasBallControl.Value (state.Value.ball.pos.Z > 1.5f<m>) (controller.Value, state.Value.player)
        let playerState = Player.updateKeyFrame dt playerState
        let playerState = Player.updatePlayer dt playerState
        
        controller := control
        state := { state.Value with player = playerState }

    override this.Draw(_) =
        match spriteBatch.Value, textures.Value with
        | Some spriteBatch, Some textures ->
            Rendering.testRender(base.GraphicsDevice, spriteBatch, textures.grassDark, textures.grassLight, textures.whiteLine, textures.ball, textures.playerSprites, textures.goalUpper, textures.goalLower, pitch, state.Value.player, state.Value.player.pos.X, state.Value.player.pos.Y)
        | _ -> ()