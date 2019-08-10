module Bets.Types exposing
    ( Answer
    , AnswerID
    , AnswerT(..)
    , Answers
    , Away
    , BestThird
    , BestThirds
    , Bet
    , Bracket(..)
    , Candidates
    , Date
    , DateString
    , DateTime(..)
    , Draw
    , DrawID
    , Group(..)
    , HasQualified(..)
    , Home
    , Match(..)
    , NextID
    , Participant
    , Player
    , Points
    , Pos(..)
    , Qualifier
    , Round(..)
    , Score
    , SecondRoundCandidate(..)
    , SecondRoundCandidates
    , Selected(..)
    , Slot
    , Stadium
    , Team
    , TeamData
    , TeamDatum
    , TeamID
    , TeamWithPlayers
    , TeamsWithPlayers
    , Time
    , Topscorer
    , Tournament6x4
    , Tournament8x4
    , Winner(..)
    )

import Time


type alias AnswerID =
    String


type alias Answer =
    ( AnswerID, AnswerT )


type alias Answers =
    List Answer


type alias Bet =
    { answers : Answers
    , betId : Maybe Int
    , uuid : Maybe String
    , active : Bool
    }


type alias Points =
    Maybe Int


type alias DateString =
    String


type alias TeamID =
    String


type alias Team =
    { teamID : TeamID
    , teamName : String
    }


type Group
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H


type Round
    = I
    | II
    | III
    | IV
    | V
    | VI


type alias Topscorer =
    ( Maybe String, Maybe Team )


type alias Player =
    String


type alias TeamWithPlayers a =
    { a | players : List Player }


type alias TeamsWithPlayers a =
    List (TeamWithPlayers a)


type alias TeamDatum =
    { team : Team
    , players : List Player
    , group : Group
    }


type alias TeamData =
    List TeamDatum


type alias Score =
    ( Maybe Int, Maybe Int )


type alias DrawID =
    String


type alias NextID =
    Maybe DrawID


type alias Draw =
    ( DrawID, Maybe Team )


type alias BestThird =
    ( Group, Team, DrawID )


type alias BestThirds =
    List BestThird


type alias Stadium =
    { stadium : String
    , town : String
    }


type Match
    = Match Draw Draw Time.Posix Stadium



{- Types for Bracket -}


type Bracket
    = MatchNode Slot Winner Home Away Round HasQualified
    | TeamNode Slot Qualifier HasQualified


type HasQualified
    = In
    | Out
    | TBD


type Winner
    = HomeTeam
    | AwayTeam
    | None


type alias Home =
    Bracket


type alias Away =
    Bracket


type alias Qualifier =
    Maybe Team


type alias Slot =
    DrawID



{- Types for GroupPosition -}


type Pos
    = First
    | Second
    | Third
    | TopThird
    | Free


type alias Candidates =
    List ( Group, Team, Pos )


type alias SecondRoundCandidates =
    List ( Group, Team, Selected )


type Selected
    = NotSelected
    | SelectedForThisSpot
    | SelectedForOtherSpot



{- Types for Answers and Bet -}


type AnswerT
    = AnswerGroupMatch Group Match (Maybe Score) Points
    | AnswerGroupPosition Group Pos Draw Points
    | AnswerGroupBestThirds BestThirds Points
    | AnswerSecondRound SecondRoundCandidate Draw Points
    | AnswerMatchWinner Round Match NextID (Maybe Team) Points
    | AnswerBracket Bracket Points
    | AnswerTopscorer Topscorer Points
    | AnswerParticipant Participant


type SecondRoundCandidate
    = FirstPlace Group
    | SecondPlace Group
    | BestThirdFrom (List Group)



{- Types for participants -}


type alias Participant =
    { name : Maybe String
    , address : Maybe String
    , residence : Maybe String
    , phone : Maybe String
    , email : Maybe String
    , howyouknowus : Maybe String
    }


type DateTime
    = DateTime Date Time


type alias Date =
    { year : Int
    , month : Time.Month
    , day : Int
    }


type alias Time =
    { hour : Int
    , minutes : Int
    }


type alias Tournament6x4 =
    { a1 : Draw
    , a2 : Draw
    , a3 : Draw
    , a4 : Draw
    , b1 : Draw
    , b2 : Draw
    , b3 : Draw
    , b4 : Draw
    , c1 : Draw
    , c2 : Draw
    , c3 : Draw
    , c4 : Draw
    , d1 : Draw
    , d2 : Draw
    , d3 : Draw
    , d4 : Draw
    , e1 : Draw
    , e2 : Draw
    , e3 : Draw
    , e4 : Draw
    , f1 : Draw
    , f2 : Draw
    , f3 : Draw
    , f4 : Draw
    }


type alias Tournament8x4 =
    { a1 : Draw
    , a2 : Draw
    , a3 : Draw
    , a4 : Draw
    , b1 : Draw
    , b2 : Draw
    , b3 : Draw
    , b4 : Draw
    , c1 : Draw
    , c2 : Draw
    , c3 : Draw
    , c4 : Draw
    , d1 : Draw
    , d2 : Draw
    , d3 : Draw
    , d4 : Draw
    , e1 : Draw
    , e2 : Draw
    , e3 : Draw
    , e4 : Draw
    , f1 : Draw
    , f2 : Draw
    , f3 : Draw
    , f4 : Draw
    , g1 : Draw
    , g2 : Draw
    , g3 : Draw
    , g4 : Draw
    , h1 : Draw
    , h2 : Draw
    , h3 : Draw
    , h4 : Draw
    }
