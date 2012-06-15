using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Audio;
using Microsoft.Xna.Framework.Content;
using Microsoft.Xna.Framework.GamerServices;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;
using Microsoft.Xna.Framework.Media;

namespace StolpSkott {
  /// <summary>
  /// This is the main type for your game
  /// </summary>
  public class Game1 : Microsoft.Xna.Framework.Game, CleverRake.XnaUtils.Application.IUiContentProvider {
    GraphicsDeviceManager graphics;
    SpriteBatch spriteBatch;
    Texture2D darkGrass;
    Texture2D lightGrass;
    Texture2D white;
    Texture2D players;
    Texture2D ball;
    Texture2D goalUpper;
    Texture2D goalLower;
    SpriteFont font;
    float x = 0.0f;
    float y = 0.0f;
    CleverRake.XnaUtils.Application.ScreenManager screenManager;
    CleverRake.XnaUtils.CoopMultiTasking.Sys.Environment menuEnvironment;
    CleverRake.XnaUtils.CoopMultiTasking.Sys.Scheduler scheduler;
    CleverRake.StolpSkott.Gameplay.MatchGameplay gameplay;

    public Game1() {
      graphics = new GraphicsDeviceManager(this);
      Content.RootDirectory = "Content";
    }

    /// <summary>
    /// Allows the game to perform any initialization it needs to before starting to run.
    /// This is where it can query for any required services and load any non-graphic
    /// related content.  Calling base.Initialize will enumerate through any components
    /// and initialize them as well.
    /// </summary>
    protected override void Initialize() {
      // TODO: Add your initialization logic here
      scheduler = new CleverRake.XnaUtils.CoopMultiTasking.Sys.Scheduler();
      menuEnvironment = new CleverRake.XnaUtils.CoopMultiTasking.Sys.Environment(scheduler);
      screenManager = new CleverRake.XnaUtils.Application.ScreenManager(this, this);
#if XBOX360
      scheduler.AddTask(CleverRake.StolpSkott.Menus.mainTask(menuEnvironment, screenManager));
#else
      scheduler.AddTask(CleverRake.StolpSkott.Menus.quickTask(menuEnvironment));
#endif
      this.Components.Add(screenManager);

      var gamerServices = new GamerServicesComponent(this);
      this.Components.Add(gamerServices);

      CleverRake.StolpSkott.Menus.registerStartGame(playerIndex =>
      {
          screenManager.Enabled = false;
          screenManager.Visible = false;
          gameplay = new CleverRake.StolpSkott.Gameplay.MatchGameplay(this, this.Content, PlayerIndex.One, CleverRake.StolpSkott.Team.TeamSide.TeamA);
          gameplay.MatchOver += (src, score) =>
          {
              this.Components.Remove(gameplay);
              screenManager.Enabled = true;
              screenManager.Visible = true;
              scheduler.AddTask(CleverRake.StolpSkott.Menus.afterMatch(menuEnvironment, screenManager, playerIndex, score.Item1, score.Item2));
          };
          this.Components.Add(gameplay);
      });
      base.Initialize();
    }

    /// <summary>
    /// LoadContent will be called once per game and is the place to load
    /// all of your content.
    /// </summary>
    protected override void LoadContent() {
      // Create a new SpriteBatch, which can be used to draw textures.
      spriteBatch = new SpriteBatch(GraphicsDevice);

      // TODO: use this.Content to load your game content here
      darkGrass = Content.Load<Texture2D>("grass-dark");
      lightGrass = Content.Load<Texture2D>("grass-light");
      white = Content.Load<Texture2D>("white");
      players = Content.Load<Texture2D>("player");
      goalLower = Content.Load<Texture2D>("goal-bottom");
      goalUpper = Content.Load<Texture2D>("goal-top");
      ball = Content.Load<Texture2D>("ball");
      font = Content.Load<SpriteFont>("spriteFont1");
    }

    /// <summary>
    /// UnloadContent will be called once per game and is the place to unload
    /// all content.
    /// </summary>
    protected override void UnloadContent() {
      // TODO: Unload any non ContentManager content here
    }

    /// <summary>
    /// Allows the game to run logic such as updating the world,
    /// checking for collisions, gathering input, and playing audio.
    /// </summary>
    /// <param name="gameTime">Provides a snapshot of timing values.</param>
    protected override void Update(GameTime gameTime) {
      // Allows the game to exit
      if (screenManager.Visible && GamePad.GetState(PlayerIndex.One).Buttons.Back == ButtonState.Pressed)
        this.Exit();

      // TODO: Add your update logic here
      float k = 10.0f * (float)gameTime.ElapsedGameTime.TotalSeconds;
      scheduler.RunFor((float)gameTime.ElapsedGameTime.TotalSeconds);

      x += GamePad.GetState(PlayerIndex.One).ThumbSticks.Left.X * k;
      y += GamePad.GetState(PlayerIndex.One).ThumbSticks.Left.Y * k;
      base.Update(gameTime);
    }

    /// <summary>
    /// This is called when the game should draw itself.
    /// </summary>
    /// <param name="gameTime">Provides a snapshot of timing values.</param>
    protected override void Draw(GameTime gameTime) {
      GraphicsDevice.Clear(Color.DarkGreen);

      // TODO: Add your drawing code here
      base.Draw(gameTime);
    }

    Texture2D CleverRake.XnaUtils.Application.IUiContentProvider.Blank
    {
        get { return white; }
    }

    SpriteFont CleverRake.XnaUtils.Application.IUiContentProvider.Font1
    {
        get { return font; }
    }

    SpriteFont CleverRake.XnaUtils.Application.IUiContentProvider.Font2
    {
        get { return font; }
    }

    SpriteBatch CleverRake.XnaUtils.Application.IUiContentProvider.SpriteBatch
    {
        get { return spriteBatch; }
    }
  }
}
