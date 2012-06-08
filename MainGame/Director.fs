module CleverRake.StolpSkott.Director

open CleverRake.XnaUtils.CoopMultiTasking
open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

open Match

// Directs the ball when it's not in play.
let directorTask (env : Environment) (getMatchState : unit -> MatchState) (setBallState : Ball.State -> unit) (toggleBallPhysics : bool -> unit) (kickerReady : unit -> bool) (resetKickerReady : unit -> unit) =
    task {
        while getMatchState().period <> MatchOver do
            // Wait until the ball becomes dead
            do! env.WaitUntil <|
                fun () ->
                    match getMatchState().ball.inPlay with
                    | Ball.DeadBall _ -> true
                    | Ball.LiveBall _ -> false

            // Let the ball bounce around for a second
            do! env.Wait 1.0f

            toggleBallPhysics false

            // Move the ball to where it's supposed to be
            let newPos =
                match getMatchState() with
                | { ball = { inPlay = Ball.CornerKick(_, owner, side) } ; pitch = pitch ; period = period } ->
                    let y = 
                        if isTeamAttackingUp owner period then
                            pitch.length / 2.0f - Ball.ballRadius
                        else
                            -pitch.length / 2.0f + Ball.ballRadius
                    let x =
                        match side with
                        | Ball.Left -> -pitch.width / 2.0f + Ball.ballRadius
                        | Ball.Right -> pitch.width / 2.0f - Ball.ballRadius
                    
                    TypedVector3<m>(x, y, Ball.ballRadius)

                | { ball = { inPlay = Ball.FreeKick(owner, kickPos) } } ->
                    TypedVector3<m>(kickPos.X, kickPos.Y, Ball.ballRadius)

                | { ball = { inPlay = Ball.KickIn(owner, side) } ; period = period ; pitch = pitch } ->
                    let x =
                        match side with
                        | Ball.Left -> -Pitch.goalBoxWidth / 2.0f
                        | Ball.Right -> Pitch.goalBoxWidth / 2.0f
                    let y =
                        if isTeamAttackingUp owner period then
                            pitch.length / 2.0f - Pitch.goalBoxHeight
                        else
                            -pitch.length / 2.0f + Pitch.goalBoxHeight
                    TypedVector3<m>(x, y, Ball.ballRadius)

                | { ball = { inPlay = Ball.KickOff _ } } ->
                    TypedVector3<m>(0.0f<m>, 0.0f<m>, Ball.ballRadius)

                | { ball = { inPlay = Ball.Penalty(_, owner) } ; pitch = pitch ; period = period } ->
                    let x = 0.0f<m>
                    let y =
                        if isTeamAttackingUp owner period then
                            pitch.length / 2.0f - 11.0f<m>
                        else
                            -pitch.length / 2.0f + 11.0f<m>
                    TypedVector3<m>(x, y, Ball.ballRadius)

                | { ball = { inPlay = Ball.ThrowIn(_, side, y) } ; pitch = pitch } ->
                    let x =
                        match side with
                        | Ball.Left -> -pitch.width / 2.0f
                        | Ball.Right -> pitch.width / 2.0f
                    TypedVector3<m>(x, y, Ball.ballRadius)

                | { ball = { inPlay = Ball.TrappedByKeeper owner } ; teamA = teamA ; teamB = teamB } ->
                    let team =
                        match owner with
                        | Team.TeamA -> teamA.onPitch
                        | Team.TeamB -> teamB.onPitch
                    if Array.isEmpty team then
                        failwith "No player in this team"
                    else
                        let keeper = team.[0]
                        TypedVector3<m>(keeper.pos.X, keeper.pos.Y, 1.0f<m>)

                | { ball = { inPlay = Ball.InPlay ; pos = pos } } ->
                    failwith "Ball shouldn't be in play"

            setBallState { getMatchState().ball with pos = newPos ; speed = TypedVector3<m/s>.Zero }

            // Wait until the kicker is ready
            do! env.WaitUntil kickerReady
            toggleBallPhysics true

            do! env.WaitNextFrame()
            resetKickerReady()

            // Wait for the ball to be live again
            do! env.WaitUntil <|
                fun () ->
                    match getMatchState().ball.inPlay with
                    | Ball.LiveBall -> true
                    | Ball.DeadBall _ -> false
    }