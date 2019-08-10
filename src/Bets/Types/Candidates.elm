module Bets.Types.Candidates exposing
    ( candidates
    , findCandidates
    , findTeams
    , mergeCandidates
    )

import Bets.Init.Euro2020.Tournament exposing (initTeamData)
import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Answers, BestThirds, Candidates, Draw, Group, Match(..), Points, Pos(..), Round, SecondRoundCandidate(..), SecondRoundCandidates, Selected(..), Team)
import Bets.Types.Team as Team



{-
   Find the candidates for the answer.
   Candidates are tuples of Group, Team and Maybe Position. The latter
   tells whether the team has already been selected for either the
   requested position, or for another one.

   The list is used for creating buttons for team selection used for
   answering the question.
-}


candidates : Answers -> Answer -> Candidates
candidates answers (( aid, answr ) as answer) =
    case answr of
        AnswerGroupPosition grp pos draw points ->
            teams answers grp
                |> List.map free
                |> mergeCandidates (findCandidates answers grp pos)

        AnswerGroupBestThirds bestThirds points ->
            let
                topThirds =
                    List.map (\( g, t, _ ) -> ( g, t, TopThird )) bestThirds

                thirds =
                    allThirds answers
            in
            mergeCandidates topThirds thirds

        _ ->
            []


secondRoundCandidates : Answers -> Answer -> SecondRoundCandidates
secondRoundCandidates answers (( aid, answr ) as answer) =
    case answr of
        AnswerSecondRound (FirstPlace grp) draw _ ->
            selectSecondRoundCandidates grp
                |> annotateSecondRoundCandidates answers draw

        AnswerSecondRound (SecondPlace grp) draw _ ->
            selectSecondRoundCandidates grp
                |> annotateSecondRoundCandidates answers draw

        AnswerSecondRound (BestThirdFrom grps) draw _ ->
            List.map selectSecondRoundCandidates grps
                |> List.concat
                |> annotateSecondRoundCandidates answers draw

        _ ->
            []


selectSecondRoundCandidates : Group -> SecondRoundCandidates
selectSecondRoundCandidates grp =
    List.filter (\t -> t.group == grp) initTeamData
        |> List.map (\t -> ( t.group, t.team, NotSelected ))


annotateSecondRoundCandidates : Answers -> Draw -> SecondRoundCandidates -> SecondRoundCandidates
annotateSecondRoundCandidates answers draw cands =
    let
        annotate t s =
            List.map (singleAnnotation t s) cands

        singleAnnotation t1 s1 ( g, t2, s2 ) =
            if Team.equal t1 t2 then
                ( g, t2, s1 )

            else
                ( g, t2, s2 )

        determineSelected drawID =
            if drawID == Tuple.first draw then
                SelectedForThisSpot

            else
                SelectedForOtherSpot
    in
    case answers of
        [] ->
            cands

        ( _, AnswerSecondRound _ ( _, Nothing ) _ ) :: rest ->
            annotateSecondRoundCandidates rest draw cands

        ( _, AnswerSecondRound _ ( drawID, Just t ) _ ) :: rest ->
            annotate t (determineSelected drawID)
                |> annotateSecondRoundCandidates rest draw

        ( _, _ ) :: rest ->
            annotateSecondRoundCandidates rest draw cands



{-
   Find all group teams that have been selected for a position already.
   These teams found in the AnswerGroupPositions. This list won't be complete
   for the group, as non-selected group teams will not be found. (Other teams
   in the group will be merged with this list in mergeCandidates later on.)
-}


findCandidates : Answers -> Group -> Pos -> List ( Group, Team, Pos )
findCandidates answers grp pos =
    let
        groupPos =
            \a -> List.member a [ First, Second, Third ]
    in
    case answers of
        [] ->
            []

        ( _, answer ) :: rest ->
            case answer of
                AnswerGroupPosition g pos2 ( drawID, Just t ) points ->
                    if g == grp then
                        ( g, t, pos2 ) :: findCandidates rest grp pos

                    else
                        findCandidates rest grp pos

                _ ->
                    findCandidates rest grp pos


findCandidatesForSecondRound : Answers -> Group -> Pos -> List ( Group, Team, Pos )
findCandidatesForSecondRound answers grp pos =
    let
        groupPos =
            \a -> List.member a [ First, Second, Third ]
    in
    case answers of
        [] ->
            []

        ( _, answer ) :: rest ->
            case answer of
                AnswerGroupPosition g pos2 ( drawID, Just t ) points ->
                    if g == grp then
                        ( g, t, pos2 ) :: findCandidates rest grp pos

                    else
                        findCandidates rest grp pos

                _ ->
                    findCandidates rest grp pos



{-
   Helper function to merge a list of candidates that were found because these
   were selected, with a list of teams that are candidates, but weren't selected
   for any position yet.
-}


mergeCandidates : Candidates -> Candidates -> Candidates
mergeCandidates cands teams_ =
    let
        positionedTeams =
            List.map (\( _, a, _ ) -> a) cands

        notSelected ( group, team, pos ) =
            not (List.member team positionedTeams)
    in
    List.filter notSelected teams_
        |> List.append cands
        |> List.sortBy (\( _, t, _ ) -> .teamName t)



{-
   Get the list of all teams that have been positioned third in their groups.
-}


allThirds : Answers -> Candidates
allThirds answers =
    let
        isThird ( _, answr ) =
            case answr of
                AnswerGroupPosition g Third ( drawId, Just t ) _ ->
                    Just ( g, t, Third )

                _ ->
                    Nothing
    in
    List.filterMap isThird answers



{-
   Get the list of teams in a group. Since we don't have a specific list of
   teams per group, these have to be extracted from the matches.
-}


findTeams : Answers -> Group -> List ( Group, Team )
findTeams answers grp =
    case answers of
        [] ->
            []

        ( _, answer ) :: rest ->
            case answer of
                AnswerGroupMatch g (Match ( _, mHome ) ( _, mAway ) _ _) _ _ ->
                    if g == grp then
                        case ( mHome, mAway ) of
                            ( Just h, Just a ) ->
                                ( g, h ) :: (( g, a ) :: findTeams rest grp)

                            ( Just h, Nothing ) ->
                                ( g, h ) :: findTeams rest grp

                            ( Nothing, Just a ) ->
                                ( g, a ) :: findTeams rest grp

                            ( Nothing, Nothing ) ->
                                findTeams rest grp

                    else
                        findTeams rest grp

                _ ->
                    findTeams rest grp



{-
   Get the list of teams in a group. Since we don't have a specific list of
   teams per group, these have to be extracted from the matches.
-}


teams : Answers -> Group -> List ( Group, Team )
teams answers group =
    findTeams answers group
        |> uniques []


uniques : List Team -> List ( Group, Team ) -> List ( Group, Team )
uniques found tms =
    case tms of
        ( group, team ) :: rest ->
            if List.member team found then
                uniques found rest

            else
                ( group, team ) :: uniques (team :: found) rest

        [] ->
            []


free : ( Group, Team ) -> ( Group, Team, Pos )
free ( group, team ) =
    ( group, team, Free )
