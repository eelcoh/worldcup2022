module Bets.Types.SecondRoundCandidate exposing (encode)

import Bets.Types exposing (Group, SecondRoundCandidate(..))
import Bets.Types.Group as Group
import Json.Encode


encode : SecondRoundCandidate -> Json.Encode.Value
encode spot =
    case spot of
        FirstPlace grp ->
            Json.Encode.object
                [ ( "candidate-type", Json.Encode.string "first-place" )
                , ( "group", Group.encode grp )
                ]

        SecondPlace grp ->
            Json.Encode.object
                [ ( "candidate-type", Json.Encode.string "second-place" )
                , ( "group", Group.encode grp )
                ]

        BestThirdFrom grps ->
            Json.Encode.object
                [ ( "candidate-type", Json.Encode.string "best-thirds-from" )
                , ( "groups", Json.Encode.list Group.encode grps )
                ]
