module Bets.Types.Answer.Bracket exposing
    ( decode
    , encode
    , isComplete
    , isCompleteQualifiers
    , setQualifier
    , setWinner
    , summary
    )

import Bets.Types exposing (Answer(..), AnswerBracket, Qualifier, Slot, Winner)
import Bets.Types.Bracket
import Bets.Types.Points
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode


setWinner : AnswerBracket -> Slot -> Winner -> AnswerBracket
setWinner (Answer bracket points) slot homeOrAway =
    let
        newBracket =
            Bets.Types.Bracket.proceed bracket slot homeOrAway
    in
    Answer newBracket points


setQualifier : AnswerBracket -> Slot -> Qualifier -> AnswerBracket
setQualifier (Answer bracket points) slot qualifier =
    let
        newBracket =
            Bets.Types.Bracket.unsetQualifier bracket qualifier
                |> setSlot

        setSlot br =
            Bets.Types.Bracket.set br slot qualifier
    in
    Answer newBracket points


isComplete : AnswerBracket -> Bool
isComplete (Answer bracket _) =
    Bets.Types.Bracket.isComplete bracket


isCompleteQualifiers : AnswerBracket -> Bool
isCompleteQualifiers (Answer bracket _) =
    Bets.Types.Bracket.isCompleteQualifiers bracket


summary : String
summary =
    "Schema"


encode : AnswerBracket -> Json.Encode.Value
encode (Answer bracket points) =
    Json.Encode.object
        [ ( "bracket", Bets.Types.Bracket.encode bracket )
        , ( "points", Bets.Types.Points.encode points )
        ]


decode : Decoder AnswerBracket
decode =
    Decode.succeed Answer
        |> required "bracket" Bets.Types.Bracket.decode
        |> required "points" Bets.Types.Points.decode
