module CleverRake.StolpSkott.Match

open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils

type TeamSide = TeamA | TeamB

type MatchPeriod =
    | FirstHalf
    | FirstHalfExtra
    | SecondHalf
    | SecondHalfExtra
    | TieFirstHalf
    | TieSecondHalf
    | TieExtra

let isTeamAttackingUp side period =
    let inverted =
        match period with
        | FirstHalf
        | FirstHalfExtra
        | TieFirstHalf -> false
        | _ -> true

    match inverted, side with
    | true, TeamB
    | false, TeamA -> true
    | _ -> false

type MatchState =
    { teamA : Team.Team
      teamB : Team.Team
      ball : Ball.State
      pitch : Pitch.PitchTraits
      period : MatchPeriod
      periodTime : float32<s>
    }
