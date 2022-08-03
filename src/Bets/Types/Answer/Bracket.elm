module Bets.Types.Answer.Bracket exposing
    ( cleanThirds
    , decode
    , encode
    , isComplete
    , isCompleteQualifiers
    , setQualifier
    , setWinner
    , summary
    )

import Bets.Init.WorldCup2022.Tournament exposing (groupMembers)
import Bets.Types exposing (Answer(..), AnswerBracket, Bracket(..), Candidate(..), Group, Qualifier, Slot, Team, Winner)
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


setQualifier : AnswerBracket -> Slot -> Group -> Qualifier -> AnswerBracket
setQualifier (Answer bracket points) slot grp qualifier =
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


cleanThirds : AnswerBracket -> Group -> AnswerBracket
cleanThirds (Answer bracket points) grp =
    let
        bestThirdCandidate : Bracket -> List Bets.Types.Team -> List Qualifier
        bestThirdCandidate br cands =
            case br of
                MatchNode _ _ home away _ _ ->
                    let
                        homecandidates =
                            bestThirdCandidate home cands

                        awaycandidates =
                            bestThirdCandidate away cands
                    in
                    homecandidates ++ awaycandidates

                TeamNode slot candidate qualifier hasQualified ->
                    case candidate of
                        FirstPlace _ ->
                            []

                        SecondPlace _ ->
                            []

                        BestThirdFrom grps ->
                            if List.member grp grps then
                                case qualifier of
                                    Just t ->
                                        if List.member t cands then
                                            [ qualifier ]

                                        else
                                            []

                                    Nothing ->
                                        []

                            else
                                []

        bestThirdFromGroup =
            groupMembers grp
                |> bestThirdCandidate bracket

        makeNewBracket br teams =
            case teams of
                t :: ts ->
                    let
                        nb =
                            Bets.Types.Bracket.unsetQualifier br t
                    in
                    makeNewBracket nb ts

                [] ->
                    br

        newBracket =
            bestThirdFromGroup
                |> makeNewBracket bracket
    in
    Answer newBracket points


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
