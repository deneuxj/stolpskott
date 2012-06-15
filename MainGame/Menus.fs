module CleverRake.StolpSkott.Menus

open Microsoft.Xna.Framework
open CleverRake.XnaUtils
open CleverRake.XnaUtils.Application
open CleverRake.XnaUtils.CoopMultiTasking

let startGame = new Event<PlayerIndex>()

type StartGameDelegate = delegate of PlayerIndex -> unit

let registerStartGame(dlg : StartGameDelegate) =
    startGame.Publish.Add <| fun pi -> dlg.Invoke(pi)

type MainMenuActions =
    | Controls
    | Start

let quickTask env =
    task {
        startGame.Trigger(PlayerIndex.One)
        return()
    }

let mainTask env (screenManager : ScreenManager) =
    task {
        use pressStart = new PressStartScreen.PressStartScreen(env, 0.5f, 0.5f, 0.5f)
        let! playerIndex = screenManager.AddDoRemove(pressStart, pressStart.Task)
        
        use menu =
            let entries = 
                [| (Controls, "Controls") ; (Start, "Start Match") |]
            let anim : AnimationParameters = 
                { period = 0.5f
                  shift = 0.5f
                }
            let placement : PlacementParameters =
                { left = 100.0f
                  top = 200.0f
                  spacing = 100.0f
                }
            new MenuScreen<MainMenuActions>(playerIndex, env, entries, anim, placement)
        let rec work =
            task {
                let! entry = screenManager.AddDoRemove(menu, menu.Task)
                match entry with
                | Some Start ->
                    startGame.Trigger(playerIndex)
                    return()
                | Some Controls ->
                    use instructions =
                        let lines =
                            [|
                                "This is a soccer game which uses the top-down view"
                                "popular in early games."
                                "You control the player closest to the ball in the"
                                "blue team."
                                "You cannot control the goal keeper."
                                ""
                                "Use the left stick to direct your player."
                                ""
                                "The left trigger, when pressed and held, traps the"
                                "ball."
                                "When released, a short pass is made in the direction"
                                "faced by the player."
                                ""
                                "The right trigger is used for sliding tackles, hard"
                                "shots and headers."
                                ""
                                "The right bumper is used for crosses."
                                ""
                                "Press B to get back to the main menu."
                            |]
                        let placement : PlacementParameters =
                            { left = 100.0f
                              top = 100.0f
                              spacing = 20.0f
                            }
                        new TextScreen(playerIndex, env, lines, placement)
                    do! screenManager.AddDoRemove(instructions, instructions.Task)
                    return! work
                | None ->
                    return! work
            }

        return! work
    }

let afterMatch env (screenManager : ScreenManager) playerIndex goalsA goalsB =
    task {
        use result =
            let lines =
                [|
                    "Match over. Final result:"
                    sprintf "BLUE: %d" goalsA
                    sprintf "RED: %d" goalsB
                    (if goalsA > goalsB then
                        "Team BLUE wins"
                     elif goalsA < goalsB then
                        "Team RED wins"
                     else
                        "Draw")
                    ""
                    "Press B to get back to the main menu."
                |]
            let placement : PlacementParameters =
                { left = 100.0f
                  top = 100.0f
                  spacing = 20.0f
                }
            new TextScreen(playerIndex, env, lines, placement)
        do! screenManager.AddDoRemove(result, result.Task)
        return! mainTask env screenManager
    }