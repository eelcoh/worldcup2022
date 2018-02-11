module Form  exposing
  ( Step(..)
  , Info(..)
  , Action(..)
  )

import Bets.Bet exposing (Answer)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Animation as Anim


type alias Step =
  { step : T
  , style : Anim.Animation
  }


type T
  = InfoStep Info
  | AnswerStep Answer
  | SubmitStep


type Info
  = Intro
  | FirstRoundIntro
  | GroupIntro Group
  | GroupStandingsIntro Group
  | BestThirdIntro
  | BracketIntro
  | BracketView
  | BracketRoundIntro Round
  | TopscorerIntro
  | AboutYouIntro
  | Submit
  | ThankYou


type Action
  = Show Step


update : Action -> Step -> Step
update act step =
  case act of
    Show s =


view : Signal.Address Action -> Step -> Bet -> Html
view address step =
  let
    c = case step.t of

      InfoStep info ->
        viewInfoStep address step info bet

      AnswerStep answer ->
        viewAnswerStep address step answer bet

      SubmitStep ->
        viewSubmitStep address step bet

  in
    div [(class "step")] [c]


viewInfoStep : Signal.Address Action -> Step -> Info -> Bet -> Html
viewInfoStep address state info =
  case info of
    Intro ->
      div [] [text "Hello Intro"]

    FirstRoundIntro ->
      div [] [text "Hello FirstRoundIntro"]

    GroupIntro group ->
      div [] [text ("Hello GroupIntro " ++ (toString group))]

    GroupStandingsIntro group ->
      div [] [text ("Hello GroupStandingsIntro " ++ (toString group))]

    BestThirdIntro ->
      div [] [text "Hello BestThirdIntro"]

    BracketIntro ->
      div [] [h1 [] [text "Hello BracketIntro"]]

    BracketView ->
      div [] [text "Hello BracketView"]

    BracketRoundIntro round ->
      div [] [text ("Hello BracketRoundIntro " ++ (toString round))]

    TopscorerIntro ->
      div [] [text "Hello TopscorerIntro"]

    AboutYouIntro ->
      div [] [text "Hello AboutYouIntro"]

    Submit ->
      div [] [
        button [ onClick address Send] [ text "Send form" ]
      ]

    ThankYou ->
      div [] [text "Hello ThankYou"]

  = AnswerMatch Match (Maybe Score) Points
  | AnswerGroupWinner Group DrawID (Maybe Team) Points
  | AnswerGroupRunnerUp Group DrawID (Maybe Team) Points
  | AnswerGroupThird Group (Maybe Team) Points
  | AnswerGroupBestThirds (List (Group, Team, DrawID)) Points
  | AnswerMatchWinner Match DrawID (Maybe Team) Points
  | AnswerTopscorer String (Maybe Team) Points
  | AnswerParticipant (Maybe Participant)

viewAnswerStep : Signal.Address Action -> Step -> Answer -> Bet -> Html
viewAnswerStep address state answer bet =
  case answer of
    AnswerGroupMatch match mScore points ->
      QMatchScore.view (Signal.forwardTo address (AnswerGroupMatchAnswer step match)) match mScore

    AnswerGroupWinner group drawID mTeam points ->
      QGroupPosition.view (Signal.forwardTo address AnswerGroupPositionAnswer) group 1 candidates

    AnswerGroupWinner group drawID mTeam points ->
      QGroupPosition.view (Signal.forwardTo address AnswerGroupPositionAnswer) group 2 candidates

    AnswerGroupWinner group drawID mTeam points ->
      QGroupPosition.view (Signal.forwardTo address AnswerGroupPositionAnswer) group 3 candidates

    AnswerBestThird group ->
      QGroupPosition.view (Signal.forwardTo address AnswerBestThirdAnswer) state.schema group 3

{-
    MatchWinnerAnswer matchID ->
      case (getMatch state.schema matchID) of
        Nothing
          -> div [] [text ("Error: MatchWinnerAnswer ID not found: " ++ (toString matchID))]
        Just match
          -> QMatchWinner.view (Signal.forwardTo address AnswerMatchWinnerAnswer) state.schema match
-}

    AnswerMatchWinner draw ->
      QBracket.view (Signal.forwardTo address AnswerBracketAnswer) draw

    TopscorerTeamAnswer ->
      div [] [text "Hello TopscorerTeamAnswer"]

    TopscorerPlayerAnswer ->
      div [] [text "Hello TopscorerPlayerAnswer"]
