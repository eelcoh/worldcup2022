module Bets.Form  exposing(init, update, view)

import Bets.Bet exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Animation as UI

import UI.Card exposing (Card)

-- type definitions
type alias CardID = Int

type alias Cards = List (CardID, Card)


type alias State =
  { bet : Bet
  , betId : Maybe String
  , views : List FormView
  , prevViews : List FormView
  , currentView : Int
  , card : Card.Card
  }




init : Maybe String -> (State, Effects Action)
init mFormId =
  let
    bet = Bets.init schema

    state =
      { bet = bet
      , betId = Nothing
      , steps = List.map AnswerStep Bet
      , currentStep = 0
      , card = Card.init <| infoView Step Intro
      , prevViews = []
      }
  in
    (state, Effects.none)

type Action
  = Send
  | PostSend
  | ShowCard Card.Card Card.action
  | HideCard Card.Card Card.action
  | Step


update Action -> State -> (State, Effects Action)
update action state =

  case action of

    Send ->
      (state, placeBet state.bet state.formId PostSend)

    PostSend response ->
      case response of
        Just (schema, maybeId) ->
          case maybeId of
            Just id ->
              ( {state | schema = schema, formId = maybeId}, Effects.none )
            _ ->
              ( state, Effects.none )
        Nothing ->
          ( state, Effects.none )

    Step ->


    ShowCard card act ->
      let
        (crd, fx) = Card.update act card
      in
        ({state | currentCard = crd}, Effects.map (ShowCard card)fx

    HideCard card act ->
      let
        (crd, fx) = Card.update act card
      in
        ({state | currentCard = crd}, Effects.map (ShowCard card)fx


view : Signal.Address Action -> State -> Html
view address state =
  Card.view address (toView address state)


toView : Signal.Address Action -> State -> Html
toView address state =
  let
    v = currentStep state
  in
    case v of
      InfoStep i ->
        infoView address i
      AnswerStep a ->
        Bets.view address (state.bet, a)
      SubmitStep ->
        Card.view address (submitView address state)


submitView : Signal.Address Action -> State -> Html
submitView address state =
  div [] [
    button [ onClick address Send] [ text "Send form" ]
  ]


infoView : Signal.Address Action -> Info -> Html
infoView address state info =
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

    ThankYou ->
      div [] [text "Hello ThankYou"]
