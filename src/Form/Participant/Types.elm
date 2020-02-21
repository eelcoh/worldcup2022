module Form.Participant.Types exposing (Attr(..), Msg(..))


type Msg
    = Set Attr


type Attr
    = Name String
    | Postal String
    | Residence String
    | Email String
    | Phone String
    | Knows String
