module Bets.Types.Answer.Topscorer exposing
    ( decode
    , encode
    , isComplete
    , set
    , summary
    )

import Bets.Types exposing (Answer(..), AnswerTopscorer, Points, Topscorer)
import Bets.Types.Points
import Bets.Types.Topscorer
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode


set : AnswerTopscorer -> Topscorer -> AnswerTopscorer
set (Answer _ points) newTopscorer =
    Answer newTopscorer points


isComplete : AnswerTopscorer -> Bool
isComplete (Answer topscorer _) =
    Bets.Types.Topscorer.isComplete topscorer


summary : String
summary =
    "Topscorer"


encode : AnswerTopscorer -> Json.Encode.Value
encode (Answer topscorer points) =
    Json.Encode.object
        [ ( "topscorer", Bets.Types.Topscorer.encode topscorer )
        , ( "points", Bets.Types.Points.encode points )
        ]


constructor : Topscorer -> Points -> AnswerTopscorer
constructor topscorer points =
    Answer topscorer points


decode : Decoder AnswerTopscorer
decode =
    Decode.succeed constructor
        |> required "topscorer" Bets.Types.Topscorer.decode
        |> required "points" Bets.Types.Points.decode
