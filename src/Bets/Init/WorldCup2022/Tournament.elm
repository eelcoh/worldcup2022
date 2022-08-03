module Bets.Init.WorldCup2022.Tournament exposing
    ( amsterdam
    , baku
    , bilbao
    , boekarest
    , bracket
    , budapest
    , glasgow
    , groupMembers
    , initTeamData
    , kopenhagen
    , londen
    , matches
    , munchen
    , petersburg
    , rome
    , teams
    , tournament
    )

import Bets.Types exposing (Bracket(..), Candidate(..), Group(..), HasQualified(..), Round(..), Stadium, Team, TeamData, TeamDatum, Tournament8x4, Winner(..))
import Bets.Types.DateTime exposing (date, time)
import Bets.Types.Group as Group
import Bets.Types.Match exposing (match)
import Bets.Types.Team exposing (team)
import Time exposing (Month(..))
import Tuple exposing (pair)


tournament : Tournament8x4
tournament =
    { a1 = slot "A1" qatar
    , a2 = slot "A2" ecuador
    , a3 = slot "A3" senegal
    , a4 = slot "A4" netherlands

    --     -- Group B (Copenhagen/St Petersburg): Denmark (hosts), Finland, Belgium, Russia (hosts)
    , b1 = slot "B1" england
    , b2 = slot "B2" iran
    , b3 = slot "B3" usa
    , b4 = slot "B4" wales

    --     --- Group C (Amsterdam/Bucharest): Netherlands (hosts), Ukraine, Austria, Play-off winner D or A
    , c1 = slot "C1" argentina
    , c2 = slot "C2" saudi_arabia
    , c3 = slot "C3" mexico
    , c4 = slot "C4" poland

    --     -- Group D (London/Glasgow): England (hosts), Croatia, Play-off winner C, Czech Republic
    , d1 = slot "D1" france
    , d2 = slot "D2" australia
    , d3 = slot "D3" denmark
    , d4 = slot "D4" tunisia

    --     -- Group E (Bilbao/Dublin): Spain (hosts), Sweden, Poland, Play-off winner B
    , e1 = slot "E1" spain
    , e2 = slot "E2" costa_rica
    , e3 = slot "E3" germany
    , e4 = slot "E4" japan

    --     --- Group F (Munich/Budapest): Play-off winner A or D, Portugal (holders), France, Germany (hosts)
    , f1 = slot "F1" belgium
    , f2 = slot "F2" canada
    , f3 = slot "F3" morocco
    , f4 = slot "F4" croatia

    --
    , g1 = slot "G1" brazil
    , g2 = slot "G2" serbia
    , g3 = slot "G3" switzerland
    , g4 = slot "G4" cameroon

    --
    , h1 = slot "H1" portugal
    , h2 = slot "H2" ghana
    , h3 = slot "H3" uruguay
    , h4 = slot "H4" south_korea
    }


slot : String -> TeamDatum -> ( String, Maybe Team )
slot s t =
    pair s (Just t.team)


bracket : Bracket
bracket =
    let
        firstPlace grp =
            let
                code =
                    "W" ++ Group.toString grp
            in
            TeamNode code (FirstPlace grp) Nothing TBD

        secondPlace grp =
            let
                code =
                    "R" ++ Group.toString grp
            in
            TeamNode code (SecondPlace grp) Nothing TBD

        bestThird idx grps =
            let
                code =
                    "T" ++ String.fromInt idx
            in
            TeamNode code (BestThirdFrom grps) Nothing TBD

        tnwa =
            firstPlace A

        tnwb =
            firstPlace B

        tnwc =
            firstPlace C

        tnwd =
            firstPlace D

        tnwe =
            firstPlace E

        tnwf =
            firstPlace F

        tnwg =
            firstPlace G

        tnwh =
            firstPlace H

        tnra =
            secondPlace A

        tnrb =
            secondPlace B

        tnrc =
            secondPlace C

        tnrd =
            secondPlace D

        tnre =
            secondPlace E

        tnrf =
            secondPlace F

        tnrg =
            secondPlace G

        tnrh =
            secondPlace H

        -- tnt1 =
        --     bestThird 1 [ A, B, C ]
        -- tnt2 =
        --     bestThird 2 [ A, B, C, D ]
        -- tnt3 =
        --     bestThird 3 [ A, D, E, F ]
        -- tnt4 =
        --     bestThird 4 [ D, E, F ]
        -- Second round matches
        mn49 =
            MatchNode "m49" None tnwa tnrb II TBD

        mn50 =
            MatchNode "m50" None tnwc tnrd II TBD

        mn51 =
            MatchNode "m51" None tnwb tnra II TBD

        mn52 =
            MatchNode "m52" None tnwd tnrc II TBD

        mn53 =
            MatchNode "m53" None tnwe tnrf II TBD

        mn54 =
            MatchNode "m54" None tnwg tnrh II TBD

        mn55 =
            MatchNode "m55" None tnwf tnre II TBD

        mn56 =
            MatchNode "m56" None tnwh tnrg II TBD

        -- quarter finals
        mn57 =
            MatchNode "m57" None mn49 mn50 III TBD

        mn58 =
            MatchNode "m58" None mn53 mn54 III TBD

        mn59 =
            MatchNode "m59" None mn51 mn52 III TBD

        mn60 =
            MatchNode "m60" None mn55 mn56 III TBD

        -- semi finals
        mn61 =
            MatchNode "m61" None mn57 mn58 IV TBD

        mn62 =
            MatchNode "m62" None mn59 mn60 IV TBD

        -- finals
        mn64 =
            MatchNode "m64" None mn61 mn62 V TBD
    in
    mn64



-- Stadiums


rome : Stadium
rome =
    { town = "Rome", stadium = "Olimpico" }


baku : Stadium
baku =
    { town = "baku", stadium = "Baku Olympic Stadion" }


petersburg : Stadium
petersburg =
    { town = "Sint-Petersburg", stadium = "Sint-Petersburg Stadion" }


kopenhagen : Stadium
kopenhagen =
    { town = "Lopenhagen", stadium = "Parken Stadion" }


amsterdam : Stadium
amsterdam =
    { town = "Amsterdam", stadium = "Amsterdam Arena" }


boekarest : Stadium
boekarest =
    { town = "Boekarest", stadium = "National Arena" }


londen : Stadium
londen =
    { town = "Londen", stadium = "Wembley" }


glasgow : Stadium
glasgow =
    { town = "Glasgow", stadium = "Hampden Park" }


bilbao : Stadium
bilbao =
    { town = "Bilbao", stadium = "San Mamés" }


dublin : Stadium
dublin =
    { town = "Dublin", stadium = "Arena" }


munchen : Stadium
munchen =
    { town = "München", stadium = "Allianz Arena" }


budapest : Stadium
budapest =
    { town = "Boedapest", stadium = "Puskás Arena" }



-- Matches


matches =
    let
        t =
            tournament
    in
    { -- Group A
      -- - qatar
      -- - ecuador
      -- - senegal
      -- - netherlands
      m01 = match "m01" t.a1 t.a2 (date 2022 Nov 21) (time 17 0) rome
    , m02 = match "m02" t.a3 t.a4 (date 2022 Nov 21) (time 15 0) baku
    , m18 = match "m18" t.a1 t.a3 (date 2022 Nov 25) (time 20 0) rome
    , m19 = match "m19" t.a4 t.a2 (date 2022 Nov 25) (time 17 0) baku
    , m36 = match "m36" t.a4 t.a1 (date 2022 Nov 29) (time 16 0) rome
    , m35 = match "m35" t.a2 t.a3 (date 2022 Nov 29) (time 16 0) baku

    -- Group B
    -- - england
    -- - iran
    -- - usa
    -- - wales
    , m03 = match "m03" t.b1 t.b2 (date 2022 Nov 21) (time 20 0) petersburg
    , m04 = match "m04" t.b3 t.b4 (date 2022 Nov 21) (time 17 0) kopenhagen
    , m20 = match "m20" t.b1 t.b3 (date 2022 Nov 25) (time 14 0) petersburg
    , m17 = match "m17" t.b4 t.b2 (date 2022 Nov 25) (time 20 0) kopenhagen
    , m34 = match "m34" t.b4 t.b1 (date 2022 Nov 29) (time 20 0) petersburg
    , m33 = match "m33" t.b2 t.b3 (date 2022 Nov 29) (time 20 0) kopenhagen

    -- Group C
    -- - argentina
    -- - saoudi_arabia
    -- - mexico
    -- - poland
    , m08 = match "m08" t.c1 t.c2 (date 2022 Nov 22) (time 12 0) amsterdam
    , m07 = match "m07" t.c3 t.c4 (date 2022 Nov 22) (time 18 0) boekarest
    , m22 = match "m22" t.c4 t.c2 (date 2022 Nov 26) (time 14 0) amsterdam
    , m24 = match "m24" t.c1 t.c3 (date 2022 Nov 26) (time 17 0) boekarest
    , m39 = match "m39" t.c4 t.c1 (date 2022 Nov 30) (time 16 0) amsterdam
    , m40 = match "m40" t.c2 t.c3 (date 2022 Nov 30) (time 16 0) boekarest

    -- Group D
    -- - france
    -- - australia
    -- - denmark
    -- - tunisia
    , m05 = match "m05" t.d1 t.d2 (date 2022 Nov 22) (time 15 0) londen
    , m06 = match "m06" t.d3 t.d4 (date 2022 Nov 22) (time 21 0) glasgow
    , m21 = match "m21" t.d4 t.d2 (date 2022 Nov 26) (time 17 0) glasgow
    , m23 = match "m23" t.d1 t.d3 (date 2022 Nov 26) (time 20 0) londen
    , m37 = match "m37" t.d2 t.d3 (date 2022 Nov 30) (time 20 0) glasgow
    , m38 = match "m38" t.d4 t.d1 (date 2022 Nov 30) (time 20 0) londen

    -- Group E
    -- - spain
    -- - costa_rica
    -- - germany
    -- - japan
    , m10 = match "m10" t.e1 t.e2 (date 2022 Nov 22) (time 14 0) bilbao
    , m11 = match "m11" t.e3 t.e4 (date 2022 Nov 22) (time 20 0) dublin
    , m25 = match "m25" t.e4 t.e2 (date 2022 Nov 26) (time 14 0) dublin
    , m28 = match "m28" t.e1 t.e3 (date 2022 Nov 26) (time 20 0) bilbao
    , m43 = match "m43" t.e2 t.e3 (date 2022 Nov 30) (time 20 0) bilbao
    , m44 = match "m44" t.e4 t.e1 (date 2022 Nov 30) (time 20 0) dublin

    -- Group F
    -- belgium
    -- canada
    -- morocco
    -- croatia
    , m12 = match "m12" t.f3 t.f4 (date 2022 Nov 23) (time 14 0) boekarest
    , m09 = match "m09" t.f1 t.f2 (date 2022 Nov 23) (time 17 0) munchen
    , m27 = match "m27" t.f4 t.f2 (date 2022 Nov 27) (time 17 0) munchen
    , m26 = match "m26" t.f1 t.f3 (date 2022 Nov 27) (time 20 0) boekarest
    , m42 = match "m42" t.f2 t.f3 (date 2022 Dec 1) (time 16 0) boekarest
    , m41 = match "m41" t.f4 t.f1 (date 2022 Dec 1) (time 16 0) munchen

    -- Group G
    -- - brazil
    -- - serbia
    -- - switzerland
    -- - cameroon
    , m13 = match "m13" t.g3 t.g4 (date 2022 Nov 24) (time 14 0) boekarest
    , m16 = match "m16" t.g1 t.g2 (date 2022 Nov 24) (time 17 0) munchen
    , m29 = match "m29" t.g4 t.g2 (date 2022 Nov 28) (time 17 0) munchen
    , m31 = match "m31" t.g1 t.g3 (date 2022 Nov 28) (time 20 0) boekarest
    , m47 = match "m47" t.g2 t.g3 (date 2022 Dec 2) (time 16 0) boekarest
    , m48 = match "m48" t.g4 t.g1 (date 2022 Dec 2) (time 16 0) munchen

    -- Group H
    -- - portugal
    -- - ghana
    -- - uruguay
    -- - south_korea
    , m14 = match "m14" t.h3 t.h4 (date 2022 Nov 24) (time 14 0) boekarest
    , m15 = match "m15" t.h1 t.h2 (date 2022 Nov 24) (time 17 0) munchen
    , m30 = match "m30" t.h4 t.h2 (date 2022 Nov 28) (time 17 0) munchen
    , m32 = match "m32" t.h1 t.h3 (date 2022 Nov 28) (time 20 0) boekarest
    , m45 = match "m45" t.h2 t.h3 (date 2022 Dec 2) (time 16 0) boekarest
    , m46 = match "m46" t.h4 t.h1 (date 2022 Dec 2) (time 16 0) munchen
    }



-- Group A: Turkey, Italy, Wales, Switzerland.
-- Group B: Denmark, Finland, Belgium, Russia.
-- Group C: Netherlands, Ukraine, Austria, North Macedonia.
-- Group D: England, Croatia, Scotland, Czech Republic.
-- Group E: Spain, Sweden, Poland, Slovakia.
-- Group F: Hungary, Portugal, France, Germany.
-- Group A: qatar , ecuador , senegal , netherlands
-- Group B: england , iran , usa , wales
-- Group C: argentina , saoudi_arabia , mexico , poland
-- Group D: france , australia , denmark , tunisia
-- Group E: spain , costa_rica , germany , japan
-- Group F: belgium, canada, morocco, croatia
-- Group G: brazil , serbia , switzerland , cameroon
-- Group H: portugal , ghana , uruguay , south_korea


initTeamData : TeamData
initTeamData =
    [ --
      qatar
    , ecuador
    , senegal
    , netherlands

    --
    , england
    , iran
    , usa
    , wales

    --
    , argentina
    , saudi_arabia
    , mexico
    , poland

    --
    , france
    , australia
    , denmark
    , tunisia

    --
    , spain
    , costa_rica
    , germany
    , japan

    --
    , belgium
    , canada
    , morocco
    , croatia

    --
    , brazil
    , serbia
    , switzerland
    , cameroon

    --
    , portugal
    , ghana
    , uruguay
    , south_korea
    ]


teams : List Team
teams =
    List.map .team initTeamData


groupMembers : Group -> List Team
groupMembers grp =
    let
        inGroup td =
            td.group == grp
    in
    List.filter inGroup initTeamData
        |> List.map .team



-- teams
-- regex  \([^)]*\),
-- Group A (Rome/Baku): Turkey, Italy (hosts), Wales, Switzerland


qatar : TeamDatum
qatar =
    { team = { teamID = "QAT", teamName = "Qatar" }
    , players =
        []
    , group = A
    }


ecuador : TeamDatum
ecuador =
    { team = { teamID = "ECU", teamName = "Ecuador" }
    , players =
        []
    , group = A
    }


senegal : TeamDatum
senegal =
    { team = { teamID = "SEN", teamName = "Senegal" }
    , players =
        []
    , group = A
    }


netherlands : TeamDatum
netherlands =
    { team = { teamID = "NED", teamName = "Nederland" }
    , players =
        [ "Marco Bizot"
        , "Tim Krul"
        , "Maarten Stekelenburg"
        , "Patrick van Aanholt"
        , "Nathan Aké"
        , "Daley Blind"
        , "Denzel Dumfries"
        , "Matthijs de Ligt"
        , "Jurriën Timber"
        , "Joël Veltman"
        , "Stefan de Vrij"
        , "Owen Wijndal"
        , "Donny van de Beek"
        , "Ryan Gravenberch"
        , "Frenkie de Jong"
        , "Davy Klaassen"
        , "Teun Koopmeiners"
        , "Marten de Roon"
        , "Georginio Wijnaldum"
        , "Steven Berghuis"
        , "Memphis Depay"
        , "Cody Gakpo"
        , "Luuk de Jong"
        , "Donyell Malen"
        , "Quincy Promes"
        , "Wout Weghorst"
        ]
    , group = A
    }



--


england : TeamDatum
england =
    { team = { teamID = "ENG", teamName = "Engeland" }
    , players =
        [ "Dean Henderson"
        , "Sam Johnstone"
        , "Jordan Pickford"
        , "Aaron Ramsdale"
        , "Trent Alexander-Arnold"
        , "Ben Chilwell"
        , "Conor Coady"
        , "Ben Godfrey"
        , "Reece James"
        , "Harry Maguire"
        , "Tyrone Mings"
        , "Luke Shaw"
        , "John Stones"
        , "Kieran Trippier"
        , "Kyle Walker"
        , "Ben White"
        , "Jude Bellingham"
        , "Phil Foden"
        , "Jack Grealish"
        , "Jordan Henderson"
        , "Jesse Lingard"
        , "Mason Mount"
        , "Kalvin Phillips"
        , "Declan Rice"
        , "Bukayo Saka"
        , "Jadon Sancho"
        , "James Ward-Prowse"
        , "Dominic Calvert-Lewin"
        , "Mason Greenwood"
        , "Harry Kane"
        , "Marcus Rashford"
        , "Raheem Sterling"
        , "Ollie Watkins"
        ]
    , group = B
    }


wales : TeamDatum
wales =
    { team = { teamID = "WAL", teamName = "Wales" }
    , players =
        [ "Wayne Hennessey", "Danny Ward", "Adam Davies", "Chris Gunter", "Ben Davies", "Connor Roberts", "Ethan Ampadu", "Chris Mepham", "Joe Rodon", "James Lawrence", "Neco Williams", "Rhys Norrington-Davies", "Ben Cabango", "Aaron Ramsey", "Joe Allen", "Jonny Williams (Cardiff City, Harry Wilson", "Daniel James", "David Brooks", "Joe Morrell", "Matt Smith", "Dylan Levitt", "Rubin Colwill", "Gareth Bale", "Kieffer Moore", "Tyler Roberts" ]
    , group = B
    }


usa : TeamDatum
usa =
    { team = { teamID = "USA", teamName = "Amerika" }
    , players =
        []
    , group = B
    }


iran : TeamDatum
iran =
    { team = { teamID = "IRN", teamName = "Iran" }
    , players =
        []
    , group = B
    }


argentina : TeamDatum
argentina =
    { team = { teamID = "ARG", teamName = "Argentina" }
    , players =
        []
    , group = C
    }


saudi_arabia : TeamDatum
saudi_arabia =
    { team = { teamID = "KSA", teamName = "Saoedi Arabië" }
    , players =
        []
    , group = C
    }


mexico : TeamDatum
mexico =
    { team = { teamID = "MEX", teamName = "Mexico" }
    , players =
        []
    , group = C
    }


poland : TeamDatum
poland =
    { team = team "POL" "Polen"
    , players =
        [ "Lukasz Fabianski"
        , "Lukasz Skorupski"
        , "Wojciech Szczesny"
        , "Jan Bednarek"
        , "Bartosz Bereszynski"
        , "Pawel Dawidowicz"
        , "Kamil Glik"
        , "Michal Helik"
        , "Tomasz Kedziora"
        , "Kamil Piatkowski"
        , "Tymoteusz Puchacz"
        , "Maciej Rybus"
        , "Przemyslaw Frankowski"
        , "Kamil Józwiak"
        , "Mateusz Klich"
        , "Kacper Kozlowski"
        , "Grzegorz Krychowiak"
        , "Karol Linetty"
        , "Jakub Moder"
        , "Przemyslaw Placheta"
        , "Piotr Zielinski"
        , "Rafal Augustyniak"
        , "Kamil Grosicki"
        , "Robert Gumny"
        , "Radoslaw Majecki"
        , "Sebastian Szymanski"
        ]
    , group = C
    }



--
--
--


france : TeamDatum
france =
    { team = { teamID = "FRA", teamName = "Frankrijk" }
    , players =
        [ "Hugo Lloris"
        , "Steve Mandanda"
        , "Mike Meignan"
        , "Lucas Digne"
        , "Léo Dubois"
        , "Lucas Hernández"
        , "Presnel Kimpembe"
        , "Jules Koundé"
        , "Clément Lenglet"
        , "Benjamin Pavard"
        , "Raphaël Varane"
        , "Kurt Zouma"
        , "N'Golo Kanté"
        , "Thomas Lemar"
        , "Paul Pogba"
        , "Adrien Rabiot"
        , "Moussa Sissoko"
        , "Corentin Tolisso"
        , "Wissam Ben Yedder"
        , "Karim Benzema"
        , "Kingsley Coman"
        , "Ousmane Dembélé"
        , "Olivier Giroud"
        , "Antoine Griezmann"
        , "Kylian Mbappé"
        , "Marcus Thuram"
        ]
    , group = D
    }


australia : TeamDatum
australia =
    { team = { teamID = "AUS", teamName = "Australia" }
    , players =
        []
    , group = D
    }


tunisia : TeamDatum
tunisia =
    { team = { teamID = "TUN", teamName = "Tunisië" }
    , players =
        []
    , group = D
    }


denmark : TeamDatum
denmark =
    { team = { teamID = "DEN", teamName = "Denmark" }
    , players =
        [ "Jonas Lössl"
        , "Frederik Rønnow"
        , "Kasper Schmeichel"
        , "Joachim Andersen"
        , "Nicolai Boilesen"
        , "Andreas Christensen"
        , "Mathias Jørgensen"
        , "Simon Kjaer"
        , "Joakim Maehle"
        , "Jens Stryger Larsen"
        , "Jannik Vestergaard"
        , "Daniel Wass"
        , "Anders Christiansen"
        , "Mikkel Damsgaard"
        , "Thomas Delaney"
        , "Christian Eriksen"
        , "Pierre-Emile Højbjerg"
        , "Mathias Jensen"
        , "Christian Nørgaard"
        , "Robert Skov"
        , "Martin Braithwaite"
        , "Andreas Cornelius"
        , "Kasper Dolberg"
        , "Andreas Skov Olsen"
        , "Yussuf Poulsen"
        , "Jonas Wind"
        ]
    , group = D
    }



-----


spain : TeamDatum
spain =
    { team = team "ESP" "Spanje"
    , players =
        [ "David de Gea"
        , "Unai Simón"
        , "Robert Sánchez"
        , "José Gayà"
        , "Jordi Alba"
        , "Pau Torres"
        , "Aymeric Laporte"
        , "Eric García"
        , "Diego Llorente"
        , "César Azpilicueta"
        , "Marcos Llorente"
        , "Sergio Busquets"
        , "Rodri Hernández"
        , "Pedri"
        , "Thiago Alcántara"
        , "Koke"
        , "Fabián Ruiz"
        , "Dani Olmo"
        , "Mikel Oyarzabal"
        , "Gerard Moreno"
        , "Álvaro Morata"
        , "Ferran Torres"
        , "Adama Traoré"
        , "Pablo Sarabia"
        ]
    , group = E
    }


germany : TeamDatum
germany =
    { team = team "GER" "Duitsland"
    , players =
        [ "Bernd Leno"
        , "Manuel Neuer"
        , "Kevin Trapp"
        , "Matthias Ginter"
        , "Robin Gosens"
        , "Christian Günter"
        , "Marcel Halstenberg"
        , "Mats Hummels"
        , "Lukas Klostermann"
        , "Robin Koch"
        , "Antonio Rüdiger"
        , "Niklas Süle"
        , "Emre Can"
        , "Leon Goretzka"
        , "Ilkay Gündogan"
        , "Kai Havertz"
        , "Joshua Kimmich"
        , "Toni Kroos"
        , "Jamal Musiala"
        , "Florian Neuhaus"
        , "Serge Gnabry"
        , "Jonas Hofmann"
        , "Thomas Müller"
        , "Leroy Sané"
        , "Kevin Volland"
        , "Timo Werner"
        ]
    , group = E
    }


costa_rica : TeamDatum
costa_rica =
    { team = { teamID = "CRC", teamName = "Costa Rica" }
    , players =
        []
    , group = E
    }


japan : TeamDatum
japan =
    { team = { teamID = "JPN", teamName = "Japan" }
    , players =
        []
    , group = D
    }



-----


belgium : TeamDatum
belgium =
    { team = team "BEL" "België"
    , players =
        [ "Thibaut Courtois"
        , "Simon Mignolet"
        , "Mats Selz"
        , "Toby Alderweireld"
        , "Dedryck Boyata"
        , "Jason Denayer"
        , "Thomas Vermaelen"
        , "Jan Vertonghen"
        , "Timothy Castagne"
        , "Nacer Chadli"
        , "Yannick Carrasco"
        , "Kevin De Bruyne"
        , "Leander Dendoncker"
        , "Thorgan Hazard"
        , "Thomas Meunier"
        , "Dennis Praet"
        , "Youri Tielemans"
        , "Hans Vanaken"
        , "Axel Witsel"
        , "Michy Batshuayi"
        , "Christian Benteke"
        , "Jérémy Doku"
        , "Eden Hazard"
        , "Romelu Lukaku"
        , "Dries Mertens"
        , "Leandro Trossard"
        ]
    , group = F
    }


croatia : TeamDatum
croatia =
    { team = team "CRO" "Kroatië"
    , players =
        [ "Lovre Kalinic"
        , "Dominik Livakovic"
        , "Simon Sluga"
        , "Borna Barisic"
        , "Domagoj Bradaric"
        , "Duje Caleta-Car"
        , "Josko Gvardiol"
        , "Josip Juranovic"
        , "Dejan Lovren"
        , "Mile Skoric"
        , "Domagoj Vida"
        , "Sime Vrsaljko"
        , "Milan Badelj"
        , "Marcelo Brozovic"
        , "Luka Ivanusec"
        , "Mateo Kovacic"
        , "Luka Modric"
        , "Mario Pasalic"
        , "Ivan Perisic"
        , "Nikola Vlasic"
        , "Josip Brekalo"
        , "Ante Budimir"
        , "Andrej Kramaric"
        , "Mislav Orsic"
        , "Bruno Petkovic"
        , "Ante Rebic"
        ]
    , group = F
    }


canada : TeamDatum
canada =
    { team = { teamID = "CAN", teamName = "Canada" }
    , players =
        []
    , group = F
    }


morocco : TeamDatum
morocco =
    { team = { teamID = "MAR", teamName = "Marokko" }
    , players =
        []
    , group = F
    }



-----


switzerland : TeamDatum
switzerland =
    { team = { teamID = "SUI", teamName = "Switzerland" }
    , players =
        [ "Yann Sommer"
        , "Yvon Mvogo"
        , "Jonas Omlin"
        , "Gregor Kobel"
        , "Manuel Akanji"
        , "Loris Benito"
        , "Nico Elvedi"
        , "Kevin Mbabu"
        , "Becir Omeragic"
        , "Ricardo Rodríguez"
        , "Silvan Widmer"
        , "Fabian Schär"
        , "Jordan Lotomba"
        , "Eray Cömert"
        , "Granit Xhaka"
        , "Denis Zakaria"
        , "Remo Freuler"
        , "Djibril Sow"
        , "Admir Mehmedi"
        , "Xherdan Shaqiri"
        , "Ruben Vargas"
        , "Steven Zuber"
        , "Edimilson Fernandes"
        , "Christian Fassnacht"
        , "Dan Ndoye"
        , "Andi Zeqiri"
        , "Breel Embolo"
        , "Mario Gavranović"
        , "Haris Seferović"
        ]
    , group = G
    }


brazil : TeamDatum
brazil =
    { team = { teamID = "BRA", teamName = "Brazil" }
    , players =
        []
    , group = G
    }


serbia : TeamDatum
serbia =
    { team = { teamID = "SRB", teamName = "Servië" }
    , players =
        []
    , group = G
    }


cameroon : TeamDatum
cameroon =
    { team = { teamID = "CAM", teamName = "Kameroen" }
    , players =
        []
    , group = G
    }



-- Group B (Copenhagen/St Petersburg): Denmark (hosts), Finland, Belgium, Russia (hosts)


portugal : TeamDatum
portugal =
    { team = team "POR" "Portugal"
    , players =
        [ "Anthony Lopes"
        , "Rui Patrício"
        , "Rui Silva"
        , "João Cancelo"
        , "Rúben Dias"
        , "José Fonte"
        , "Raphael Guerreiro"
        , "Nuno Mendes"
        , "Pepe"
        , "Nélson Semedo"
        , "William Carvalho"
        , "Bruno Fernandes"
        , "João Moutinho"
        , "Rúben Neves"
        , "Sérgio Oliveira"
        , "João Palhinha"
        , "Danilo Pereira"
        , "Renato Sanches"
        , "João Félix"
        , "Pedro Gonçalves"
        , "Gonçalo Guedes"
        , "Diogo Jota"
        , "Cristiano Ronaldo"
        , "André Silva"
        , "Bernardo Silva"
        , "Rafa Silva"
        ]
    , group = H
    }


ghana : TeamDatum
ghana =
    { team = { teamID = "GHA", teamName = "Ghana" }
    , players =
        []
    , group = H
    }


uruguay : TeamDatum
uruguay =
    { team = { teamID = "URU", teamName = "Uruguay" }
    , players =
        []
    , group = H
    }


south_korea : TeamDatum
south_korea =
    { team = { teamID = "KOR", teamName = "Zuid Korea" }
    , players =
        []
    , group = H
    }
