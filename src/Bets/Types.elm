module Bets.Types exposing
    ( Answer(..)
    , AnswerBracket
    , AnswerGroupMatch
    , AnswerGroupMatches
    , AnswerTopscorer
    , Answers
    , Away
    , Bet
    , Bracket(..)
    , Candidate(..)
    , Candidates
    , CurrentSlot(..)
    , Date
    , DateString
    , DateTime(..)
    , Draw
    , DrawID
    , Group(..)
    , GroupMatch(..)
    , HasQualified(..)
    , Home
    , Match(..)
    , MatchID
    , NextID
    , Participant
    , Player
    , Points
    , Qualifier
    , Round(..)
    , Score
    , Selected(..)
    , Selection
    , Slot
    , Stadium
    , StringField(..)
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


type alias Answers =
    { matches : AnswerGroupMatches
    , bracket : AnswerBracket
    , topscorer : AnswerTopscorer
    }


type alias Bet =
    { answers : Answers
    , betId : Maybe String
    , uuid : Maybe String
    , active : Bool
    , participant : Participant
    }


type alias AnswerGroupMatches =
    List ( MatchID, AnswerGroupMatch )


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


type alias MatchID =
    String


type GroupMatch
    = GroupMatch Group Match (Maybe Score)


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


type alias Stadium =
    { stadium : String
    , town : String
    }


type Match
    = Match MatchID Draw Draw Time.Posix Stadium



{- Types for Bracket -}


type Bracket
    = MatchNode Slot Winner Home Away Round HasQualified
    | TeamNode Slot Candidate Qualifier HasQualified


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


type CurrentSlot
    = ThisSlot
    | OtherSlot Slot
    | NoSlot


type alias Selection =
    { currentSlot : CurrentSlot
    , group : Group
    , team : Team
    }



{- Types for GroupPosition -}
-- type Pos
--     = First
--     | Second
--     | Third
--     | TopThird
--     | Free


type alias Candidates =
    List ( Group, Team, Selected )


type Selected
    = NotSelected
    | SelectedForThisSpot
    | SelectedForOtherSpot



{- Types for Answers and Bet -}
-- type AnswerT
--     = AnswerGroupMatch Group Match (Maybe Score) Points
--     | AnswerBracket Bracket Points
--     | AnswerTopscorer Topscorer Points
--     | AnswerParticipant Participant


type Answer a
    = Answer a Points


type alias AnswerGroupMatch =
    Answer GroupMatch


type alias AnswerBracket =
    Answer Bracket


type alias AnswerTopscorer =
    Answer Topscorer


type Candidate
    = FirstPlace Group
    | SecondPlace Group
    | BestThirdFrom (List Group)



{- Types for participants -}


type alias Participant =
    { name : StringField
    , address : StringField
    , residence : StringField
    , phone : StringField
    , email : StringField
    , howyouknowus : StringField
    }


type StringField
    = Initial String
    | Changed String
    | Error String


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
