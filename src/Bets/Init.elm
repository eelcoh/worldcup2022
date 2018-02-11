module Bets.Init exposing (bet)

import Bets.Types exposing (..)
import Bets.Types.Team exposing (team)
import Bets.Types.Participant as P
import Bets.Types.Date exposing (toDate)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


bet : Bet
bet =
    { answers = answers
    , betId = Nothing
    , uuid = Nothing
    , active = True
    }


answers : Answers
answers =
    let
        a1 =
            "A1" => Just (team "FRA" "France")

        a2 =
            "A2" => Just (team "ROU" "Romania")

        a3 =
            "A3" => Just (team "ALB" "Albania")

        a4 =
            "A4" => Just (team "SUI" "Switzerland")

        b1 =
            "B1" => Just (team "ENG" "England")

        b2 =
            "B2" => Just (team "RUS" "Russia")

        b3 =
            "B3" => Just (team "WAL" "Wales")

        b4 =
            "B4" => Just (team "SVK" "Slovakia")

        c1 =
            "C1" => Just (team "GER" "Germany")

        c2 =
            "C2" => Just (team "UKR" "Ukraine")

        c3 =
            "C3" => Just (team "POL" "Poland")

        c4 =
            "C4" => Just (team "NIR" "Northern Ireland")

        d1 =
            "D1" => Just (team "ESP" "Spain")

        d2 =
            "D2" => Just (team "CZE" "Czech Republic")

        d3 =
            "D3" => Just (team "TUR" "Turkey")

        d4 =
            "D4" => Just (team "CRO" "Croatia")

        e1 =
            "E1" => Just (team "BEL" "Belgium")

        e2 =
            "E2" => Just (team "ITA" "Italy")

        e3 =
            "E3" => Just (team "IRL" "Ireland")

        e4 =
            "E4" => Just (team "SWE" "Sweden")

        f1 =
            "F1" => Just (team "POR" "Portugal")

        f2 =
            "F2" => Just (team "ISL" "Iceland")

        f3 =
            "F3" => Just (team "AUT" "Austria")

        f4 =
            "F4" => Just (team "HUN" "Hungary")

        wa =
            "WA" => Nothing

        wb =
            "WB" => Nothing

        wc =
            "WC" => Nothing

        wd =
            "WD" => Nothing

        we =
            "WE" => Nothing

        wf =
            "WF" => Nothing

        ra =
            "RA" => Nothing

        rb =
            "RB" => Nothing

        rc =
            "RC" => Nothing

        rd =
            "RD" => Nothing

        re =
            "RE" => Nothing

        rf =
            "RF" => Nothing

        za =
            "ZA" => Nothing

        zb =
            "ZB" => Nothing

        zc =
            "ZC" => Nothing

        zd =
            "ZD" => Nothing

        ze =
            "ZE" => Nothing

        zf =
            "ZF" => Nothing

        bt =
            "BT" => Nothing

        -- AnswerBestThird
        t1 =
            "T1" => Nothing

        -- AnswerBestThird
        t2 =
            "T2" => Nothing

        -- AnswerBestThird
        t3 =
            "T3" => Nothing

        -- AnswerBestThird
        t4 =
            "T4" => Nothing

        -- AnswerBestThird
        w37 =
            "W37" => Nothing

        w38 =
            "W38" => Nothing

        w39 =
            "W39" => Nothing

        w40 =
            "W40" => Nothing

        w41 =
            "W41" => Nothing

        w42 =
            "W42" => Nothing

        w43 =
            "W43" => Nothing

        w44 =
            "W44" => Nothing

        w45 =
            "W45" => Nothing

        w46 =
            "W46" => Nothing

        w47 =
            "W47" => Nothing

        w48 =
            "W48" => Nothing

        w49 =
            "W49" => Nothing

        w50 =
            "W50" => Nothing

        w51 =
            "W51" => Nothing

        bordeaux =
            { town = "Bordeaux", stadium = "Stade de Bordeaux" }

        lens =
            { town = "Lens", stadium = "Stade Bollaert-Delelis" }

        lille =
            { town = "Lille", stadium = "Stade Pierre Mauroy" }

        lyon =
            { town = "Lyon", stadium = "Stade Lyon" }

        marseille =
            { town = "Marseille", stadium = "Stade Vélodrome" }

        nice =
            { town = "Nice", stadium = "Stade de Nice" }

        paris =
            { town = "Paris", stadium = "Parc des Princes" }

        saintdenis =
            { town = "Saint-Denis", stadium = "Stade de France" }

        saintetienne =
            { town = "Saint-Etienne", stadium = "Stade Geoffroy Guichard" }

        toulouse =
            { town = "Toulouse", stadium = "Stadium de Toulouse" }

        tnwa =
            TeamNode "WA" Nothing TBD

        tnwb =
            TeamNode "WB" Nothing TBD

        tnwc =
            TeamNode "WC" Nothing TBD

        tnwd =
            TeamNode "WD" Nothing TBD

        tnwe =
            TeamNode "WE" Nothing TBD

        tnwf =
            TeamNode "WF" Nothing TBD

        tnra =
            TeamNode "RA" Nothing TBD

        tnrb =
            TeamNode "RB" Nothing TBD

        tnrc =
            TeamNode "RC" Nothing TBD

        tnrd =
            TeamNode "RD" Nothing TBD

        tnre =
            TeamNode "RE" Nothing TBD

        tnrf =
            TeamNode "RF" Nothing TBD

        tnt1 =
            TeamNode "T1" Nothing TBD

        tnt2 =
            TeamNode "T2" Nothing TBD

        tnt3 =
            TeamNode "T3" Nothing TBD

        tnt4 =
            TeamNode "T4" Nothing TBD

        mn37 =
            MatchNode "m37" None tnra tnrc II TBD

        -- "2016/06/15 15:00" saintetienne (Just "W37")
        mn38 =
            MatchNode "m38" None tnwb tnt2 II TBD

        -- "2016/06/15 15:00" paris (Just "W38")
        mn39 =
            MatchNode "m39" None tnwd tnt4 II TBD

        -- "2016/06/15 15:00" lens (Just "W39")
        mn40 =
            MatchNode "m40" None tnwa tnt1 II TBD

        -- "2016/06/15 15:00" lyon (Just "W40")
        mn41 =
            MatchNode "m41" None tnwc tnt3 II TBD

        -- "2016/06/15 15:00" lille (Just "W41")
        mn42 =
            MatchNode "m42" None tnwf tnre II TBD

        -- "2016/06/15 15:00" toulouse (Just "W42")
        mn43 =
            MatchNode "m43" None tnwe tnrd II TBD

        -- "2016/06/15 15:00" saintdenis (Just "W43")
        mn44 =
            MatchNode "m44" None tnrb tnrf II TBD

        -- "2016/06/15 15:00" nice (Just "W44")
        mn45 =
            MatchNode "m45" None mn37 mn39 III TBD

        -- "2016/06/15 15:00" marseille (Just "W45")
        mn46 =
            MatchNode "m46" None mn38 mn42 III TBD

        -- "2016/06/15 15:00" lille (Just "W46")
        mn47 =
            MatchNode "m47" None mn41 mn43 III TBD

        -- "2016/06/15 15:00" bordeaux (Just "W47")
        mn48 =
            MatchNode "m48" None mn40 mn44 III TBD

        -- "2016/06/15 15:00" saintdenis (Just "W48")
        mn49 =
            MatchNode "m49" None mn45 mn46 IV TBD

        -- "2016/06/15 15:00" lyon (Just "W49")
        mn50 =
            MatchNode "m50" None mn47 mn48 IV TBD

        -- "2016/06/15 15:00" marseille (Just "W50")
        mn51 =
            MatchNode "m51" None mn49 mn50 V TBD

        -- "2016/06/15 15:00" saintdenis Nothing
        bracket =
            mn51
    in
        [ "m01" => answerGroupMatch A a1 a2 "2016/06/10 21:00" saintdenis
        , "m02" => answerGroupMatch A a3 a4 "2016/06/11 15:00" lens
        , "m03" => answerGroupMatch B b3 b4 "2016/06/11 18:00" bordeaux
        , "m04" => answerGroupMatch B b1 b2 "2016/06/11 21:00" marseille
        , "m05" => answerGroupMatch D d3 d4 "2016/06/12 15:00" paris
        , "m06" => answerGroupMatch C c3 c4 "2016/06/12 18:00" nice
        , "m07" => answerGroupMatch C c1 c2 "2016/06/12 21:00" lille
        , "m08" => answerGroupMatch D d1 d2 "2016/06/13 15:00" toulouse
        , "m09" => answerGroupMatch E e3 e4 "2016/06/13 18:00" saintdenis
        , "m10" => answerGroupMatch E e1 e2 "2016/06/13 21:00" lyon
        , "m11" => answerGroupMatch F f3 f4 "2016/06/14 18:00" bordeaux
        , "m12" => answerGroupMatch F f1 f2 "2016/06/14 21:00" saintetienne
        , "m13" => answerGroupMatch B b2 b4 "2016/06/15 15:00" lille
        , "m14" => answerGroupMatch A a2 a4 "2016/06/15 18:00" paris
        , "m15" => answerGroupMatch A a1 a3 "2016/06/15 21:00" marseille
        , "m16" => answerGroupMatch B b1 b3 "2016/06/16 15:00" lens
        , "m17" => answerGroupMatch C c2 c4 "2016/06/16 18:00" lyon
        , "m18" => answerGroupMatch C c1 c3 "2016/06/16 21:00" saintdenis
        , "m19" => answerGroupMatch E e2 e4 "2016/06/17 15:00" toulouse
        , "m20" => answerGroupMatch D d2 d4 "2016/06/17 18:00" saintetienne
        , "m21" => answerGroupMatch D d1 d3 "2016/06/17 21:00" nice
        , "m22" => answerGroupMatch E e1 e3 "2016/06/18 15:00" bordeaux
        , "m23" => answerGroupMatch F f2 f4 "2016/06/18 18:00" marseille
        , "m24" => answerGroupMatch F f1 f3 "2016/06/18 21:00" paris
        , "m25" => answerGroupMatch A a2 a3 "2016/06/19 21:00" lyon
        , "m26" => answerGroupMatch A a4 a1 "2016/06/19 21:00" lille
        , "m27" => answerGroupMatch B b2 b3 "2016/06/20 21:00" toulouse
        , "m28" => answerGroupMatch B b4 b1 "2016/06/20 21:00" saintetienne
        , "m29" => answerGroupMatch C c2 c3 "2016/06/21 18:00" marseille
        , "m30" => answerGroupMatch C c4 c1 "2016/06/21 18:00" paris
        , "m31" => answerGroupMatch D d2 d3 "2016/06/21 21:00" lens
        , "m32" => answerGroupMatch D d4 d1 "2016/06/21 21:00" bordeaux
        , "m33" => answerGroupMatch F f2 f3 "2016/06/22 18:00" saintdenis
        , "m34" => answerGroupMatch F f4 f1 "2016/06/22 18:00" lyon
        , "m35" => answerGroupMatch E e2 e3 "2016/06/22 21:00" lille
        , "m36" => answerGroupMatch E e4 e1 "2016/06/22 21:00" nice
        , "wa" => AnswerGroupPosition A First wa Nothing
        , "ra" => AnswerGroupPosition A Second ra Nothing
        , "za" => AnswerGroupPosition A Third za Nothing
        , "wb" => AnswerGroupPosition B First wb Nothing
        , "rb" => AnswerGroupPosition B Second rb Nothing
        , "zb" => AnswerGroupPosition B Third zb Nothing
        , "wc" => AnswerGroupPosition C First wc Nothing
        , "rc" => AnswerGroupPosition C Second rc Nothing
        , "zc" => AnswerGroupPosition C Third zc Nothing
        , "wd" => AnswerGroupPosition D First wd Nothing
        , "rd" => AnswerGroupPosition D Second rd Nothing
        , "zd" => AnswerGroupPosition D Third zd Nothing
        , "we" => AnswerGroupPosition E First we Nothing
        , "re" => AnswerGroupPosition E Second re Nothing
        , "ze" => AnswerGroupPosition E Third ze Nothing
        , "wf" => AnswerGroupPosition F First wf Nothing
        , "rf" => AnswerGroupPosition F Second rf Nothing
        , "zf" => AnswerGroupPosition F Third zf Nothing
        , "bt" => AnswerGroupBestThirds [] Nothing
          --    , "m37" => answerMatchWinnerInit II ra rc "2016/06/15 15:00" saintetienne (Just "W37")
          --    , "m38" => answerMatchWinnerInit II wb t2 "2016/06/15 15:00" paris (Just "W38")
          --    , "m39" => answerMatchWinnerInit II wd t4 "2016/06/15 15:00" lens (Just "W39")
          --    , "m40" => answerMatchWinnerInit II wa t1 "2016/06/15 15:00" lyon (Just "W40")
          --    , "m41" => answerMatchWinnerInit II wc t3 "2016/06/15 15:00" lille (Just "W41")
          --    , "m42" => answerMatchWinnerInit II wf re "2016/06/15 15:00" toulouse (Just "W42")
          --    , "m43" => answerMatchWinnerInit II we rd "2016/06/15 15:00" saintdenis (Just "W43")
          --    , "m44" => answerMatchWinnerInit II rb rf "2016/06/15 15:00" nice (Just "W44")
          --    , "m45" => answerMatchWinnerInit III w37 w39 "2016/06/15 15:00" marseille (Just "W45")
          --    , "m46" => answerMatchWinnerInit III w38 w42 "2016/06/15 15:00" lille (Just "W46")
          --    , "m47" => answerMatchWinnerInit III w41 w43 "2016/06/15 15:00" bordeaux (Just "W47")
          --    , "m48" => answerMatchWinnerInit III w40 w44 "2016/06/15 15:00" saintdenis (Just "W48")
          --    , "m49" => answerMatchWinnerInit IV w45 w46 "2016/06/15 15:00" lyon (Just "W49")
          --    , "m50" => answerMatchWinnerInit IV w47 w48 "2016/06/15 15:00" marseille (Just "W50")
          --    , "m51" => answerMatchWinnerInit V w49 w50 "2016/06/15 15:00" saintdenis Nothing
        , "ts" => AnswerTopscorer ( Nothing, Nothing ) Nothing
        , "me" => AnswerParticipant (P.init)
        , "br" => AnswerBracket bracket Nothing
        ]


answerGroupMatch : Group -> Draw -> Draw -> DateString -> Stadium -> AnswerT
answerGroupMatch group home away dateStr stadium =
    let
        date =
            toDate dateStr

        points =
            Nothing

        score =
            Nothing

        match =
            ( home, away, date, stadium )
    in
        AnswerGroupMatch group match score points


answerMatchWinnerInit : Round -> Draw -> Draw -> DateString -> Stadium -> Maybe DrawID -> AnswerT
answerMatchWinnerInit round home away dateStr stadium mNextId =
    let
        date =
            toDate dateStr

        points =
            Nothing

        team =
            Nothing

        match =
            ( home, away, date, stadium )
    in
        AnswerMatchWinner round match mNextId team points
