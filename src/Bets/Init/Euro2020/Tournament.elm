module Bets.Init.Euro2020.Tournament exposing
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

import Bets.Types exposing (Bracket(..), Candidate(..), Group(..), HasQualified(..), Round(..), Stadium, Team, TeamData, TeamDatum, Tournament6x4, Winner(..))
import Bets.Types.DateTime exposing (date, time)
import Bets.Types.Group as Group
import Bets.Types.Match exposing (match)
import Bets.Types.Team exposing (team)
import Time exposing (Month(..))
import Tuple exposing (pair)


tournament : Tournament6x4
tournament =
    { a1 = slot "A1" turkey
    , a2 = slot "A2" italy
    , a3 = slot "A3" wales
    , a4 = slot "A4" switzerland

    --     -- Group B (Copenhagen/St Petersburg): Denmark (hosts), Finland, Belgium, Russia (hosts)
    , b1 = slot "B1" denmark
    , b2 = slot "B2" finland
    , b3 = slot "B3" belgium
    , b4 = slot "B4" russia

    --     --- Group C (Amsterdam/Bucharest): Netherlands (hosts), Ukraine, Austria, Play-off winner D or A
    , c1 = slot "C1" netherlands
    , c2 = slot "C2" ukraine
    , c3 = slot "C3" austria
    , c4 = slot "C4" north_macedonia

    --     -- Group D (London/Glasgow): England (hosts), Croatia, Play-off winner C, Czech Republic
    , d1 = slot "D1" england
    , d2 = slot "D2" croatia
    , d3 = slot "D3" scotland
    , d4 = slot "D4" czechia

    --     -- Group E (Bilbao/Dublin): Spain (hosts), Sweden, Poland, Play-off winner B
    , e1 = slot "E1" spain
    , e2 = slot "E2" sweden
    , e3 = slot "E3" poland
    , e4 = slot "E4" slovakia

    --     --- Group F (Munich/Budapest): Play-off winner A or D, Portugal (holders), France, Germany (hosts)
    , f1 = slot "F1" hungary
    , f2 = slot "F2" portugal
    , f3 = slot "F3" france
    , f4 = slot "F4" germany
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

        tnt1 =
            bestThird 1 [ A, B, C ]

        tnt2 =
            bestThird 2 [ A, B, C, D ]

        tnt3 =
            bestThird 3 [ A, D, E, F ]

        tnt4 =
            bestThird 4 [ D, E, F ]

        -- Second round matches
        mn37 =
            MatchNode "m37" None tnwa tnrc II TBD

        mn38 =
            MatchNode "m38" None tnra tnrb II TBD

        mn39 =
            MatchNode "m39" None tnwb tnt3 II TBD

        mn40 =
            MatchNode "m40" None tnwc tnt4 II TBD

        mn41 =
            MatchNode "m41" None tnwf tnt1 II TBD

        mn42 =
            MatchNode "m42" None tnrd tnre II TBD

        mn43 =
            MatchNode "m43" None tnwe tnt2 II TBD

        mn44 =
            MatchNode "m44" None tnwd tnrf II TBD

        -- quarter finals
        mn45 =
            MatchNode "m45" None mn41 mn42 III TBD

        mn46 =
            MatchNode "m46" None mn37 mn39 III TBD

        mn47 =
            MatchNode "m47" None mn38 mn40 III TBD

        mn48 =
            MatchNode "m48" None mn43 mn44 III TBD

        -- semi finals
        mn49 =
            MatchNode "m49" None mn45 mn46 IV TBD

        mn50 =
            MatchNode "m50" None mn47 mn48 IV TBD

        -- finals
        mn51 =
            MatchNode "m51" None mn49 mn50 V TBD
    in
    mn51



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
      m01 = match "m01" t.a1 t.a2 (date 2020 Jun 12) (time 17 0) rome
    , m02 = match "m02" t.a3 t.a4 (date 2020 Jun 13) (time 15 0) baku
    , m14 = match "m14" t.a1 t.a3 (date 2020 Jun 17) (time 20 0) rome
    , m13 = match "m13" t.a4 t.a2 (date 2020 Jun 17) (time 17 0) baku
    , m26 = match "m26" t.a4 t.a1 (date 2020 Jun 21) (time 16 0) rome
    , m25 = match "m25" t.a2 t.a3 (date 2020 Jun 21) (time 16 0) baku

    -- Group B
    , m03 = match "m03" t.b1 t.b2 (date 2020 Jun 13) (time 20 0) petersburg
    , m04 = match "m04" t.b3 t.b4 (date 2020 Jun 13) (time 17 0) kopenhagen
    , m15 = match "m15" t.b1 t.b3 (date 2020 Jun 17) (time 14 0) petersburg
    , m16 = match "m16" t.b4 t.b2 (date 2020 Jun 18) (time 20 0) kopenhagen
    , m28 = match "m28" t.b4 t.b1 (date 2020 Jun 22) (time 20 0) petersburg
    , m27 = match "m27" t.b2 t.b3 (date 2020 Jun 22) (time 20 0) kopenhagen

    -- Group C
    , m05 = match "m05" t.c1 t.c2 (date 2020 Jun 14) (time 12 0) amsterdam
    , m06 = match "m06" t.c3 t.c4 (date 2020 Jun 14) (time 18 0) boekarest
    , m17 = match "m17" t.c4 t.c2 (date 2020 Jun 18) (time 14 0) amsterdam
    , m18 = match "m18" t.c1 t.c3 (date 2020 Jun 18) (time 17 0) boekarest
    , m29 = match "m29" t.c4 t.c1 (date 2020 Jun 22) (time 16 0) amsterdam
    , m30 = match "m30" t.c2 t.c3 (date 2020 Jun 22) (time 16 0) boekarest

    -- Group D
    , m07 = match "m07" t.d1 t.d2 (date 2020 Jun 14) (time 15 0) londen
    , m08 = match "m08" t.d3 t.d4 (date 2020 Jun 15) (time 21 0) glasgow
    , m20 = match "m20" t.d1 t.d3 (date 2020 Jun 19) (time 20 0) londen
    , m19 = match "m19" t.d4 t.d2 (date 2020 Jun 19) (time 17 0) glasgow
    , m32 = match "m32" t.d4 t.d1 (date 2020 Jun 23) (time 20 0) londen
    , m31 = match "m31" t.d2 t.d3 (date 2020 Jun 23) (time 20 0) glasgow

    -- Group E
    , m09 = match "m09" t.e3 t.e4 (date 2020 Jun 15) (time 14 0) bilbao
    , m10 = match "m10" t.e1 t.e2 (date 2020 Jun 15) (time 20 0) dublin
    , m21 = match "m21" t.e1 t.e3 (date 2020 Jun 19) (time 14 0) dublin
    , m22 = match "m22" t.e4 t.e2 (date 2020 Jun 20) (time 20 0) bilbao
    , m33 = match "m33" t.e4 t.e1 (date 2020 Jun 24) (time 20 0) bilbao
    , m34 = match "m34" t.e2 t.e3 (date 2020 Jun 24) (time 20 0) dublin

    -- Group F
    , m11 = match "m11" t.f3 t.f4 (date 2020 Jun 16) (time 14 0) boekarest
    , m12 = match "m12" t.f1 t.f2 (date 2020 Jun 16) (time 17 0) munchen
    , m24 = match "m24" t.f4 t.f2 (date 2020 Jun 20) (time 17 0) munchen
    , m23 = match "m23" t.f1 t.f3 (date 2020 Jun 20) (time 20 0) boekarest
    , m35 = match "m35" t.f2 t.f3 (date 2020 Jun 24) (time 16 0) boekarest
    , m36 = match "m36" t.f4 t.f1 (date 2020 Jun 24) (time 16 0) munchen
    }



-- Group A: Turkey, Italy, Wales, Switzerland.
-- Group B: Denmark, Finland, Belgium, Russia.
-- Group C: Netherlands, Ukraine, Austria, North Macedonia.
-- Group D: England, Croatia, Scotland, Czech Republic.
-- Group E: Spain, Sweden, Poland, Slovakia.
-- Group F: Hungary, Portugal, France, Germany.


initTeamData : TeamData
initTeamData =
    [ -- Group A (Rome/Baku): Turkey, Italy (hosts), Wales, Switzerland
      turkey
    , italy
    , wales
    , switzerland

    -- Group B (Copenhagen/St Petersburg): Denmark (hosts), Finland, Belgium, Russia (hosts)
    , denmark
    , finland
    , belgium
    , russia

    --- Group C (Amsterdam/Bucharest): Netherlands (hosts), Ukraine, Austria, Play-off winner D or A
    , netherlands
    , ukraine
    , austria
    , north_macedonia

    -- Group D (London/Glasgow): England (hosts), Croatia, Play-off winner C, Czech Republic
    , england
    , croatia
    , scotland
    , czechia

    -- Group E (Bilbao/Dublin): Spain (hosts), Sweden, Poland, Play-off winner B
    , spain
    , sweden
    , poland
    , slovakia

    --- Group F (Munich/Budapest): Play-off winner A or D, Portugal (holders), France, Germany (hosts)
    , hungary
    , portugal
    , france
    , germany
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


turkey : TeamDatum
turkey =
    { team = { teamID = "TUR", teamName = "Turkey" }
    , players =
        [ "Gökhan Akkan"
        , "Altay Bayindir"
        , "Ugurcan Çakir"
        , "Mert Günok"
        , "Kaan Ayhan"
        , "Zeki Çelik"
        , "Merih Demiral"
        , "Ozan Kabak"
        , "Umut Meras"
        , "Mert Müldür"
        , "Çaglar Söyüncü"
        , "Ridvan Yilmaz"
        , "Taylan Antalyali"
        , "Hakan Çalhanoglu"
        , "Irfan Can Kahveci"
        , "Orkun Kökçü"
        , "Mahmut Tekdemir"
        , "Dorukhan Toköz"
        , "Ozan Tufan"
        , "Yusuf Yazici"
        , "Okay Yokuslu"
        , "Halil Akbunar"
        , "Kerem Aktürkoglu"
        , "Halil Dervisoglu"
        , "Efecan Karaca"
        , "Kenan Karaman"
        , "Abdülkadir Ömür"
        , "Enes Ünal"
        , "Cengiz Ünder"
        , "Burak Yilmaz"
        ]
    , group = A
    }


wales : TeamDatum
wales =
    { team = { teamID = "WAL", teamName = "Wales" }
    , players =
        [ "Wayne Hennessey", "Danny Ward", "Adam Davies", "Chris Gunter", "Ben Davies", "Connor Roberts", "Ethan Ampadu", "Chris Mepham", "Joe Rodon", "James Lawrence", "Neco Williams", "Rhys Norrington-Davies", "Ben Cabango", "Aaron Ramsey", "Joe Allen", "Jonny Williams (Cardiff City, Harry Wilson", "Daniel James", "David Brooks", "Joe Morrell", "Matt Smith", "Dylan Levitt", "Rubin Colwill", "Gareth Bale", "Kieffer Moore", "Tyler Roberts" ]
    , group = A
    }


italy : TeamDatum
italy =
    { team = { teamID = "ITA", teamName = "Italy" }
    , players =
        [ "Alessio Cragno"
        , "Gianluigi Donnarumma"
        , "Alex Meret"
        , "Salvatore Sirigu"
        , "Francesco Acerbi"
        , "Alessandro Bastoni"
        , "Cristiano Biraghi"
        , "Leonardo Bonucci"
        , "Giorgio Chiellini"
        , "Giovanni Di Lorenzo"
        , "Alessandro Florenzi"
        , "Manuel Lazzari"
        , "Gianluca Mancini"
        , "Leonardo Spinazzola"
        , "Rafael Tolói"
        , "Nicolò Barella"
        , "Gaetano Castrovilli"
        , "Bryan Cristante"
        , "Manuel Locatelli"
        , "Lorenzo Pellegrini"
        , "Matteo Pessina"
        , "Stefano Sensi"
        , "Marco Verratti"
        , "Andrea Belotti"
        , "Domenico Berardi"
        , "Federico Bernardeschi"
        , "Federico Chiesa"
        , "Vincenzo Grifo"
        , "Ciro Immobile"
        , "Lorenzo Insigne"
        , "Moise Kean"
        , "Matteo Politano"
        , "Giacomo Raspadori"
        ]
    , group = A
    }


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
    , group = A
    }



-- Group B (Copenhagen/St Petersburg): Denmark (hosts), Finland, Belgium, Russia (hosts)


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
    , group = B
    }


finland : TeamDatum
finland =
    { team = { teamID = "FIN", teamName = "Finland" }
    , players =
        [ "Lukáš Hrádecký"
        , "Jesse Joronen"
        , "Anssi Jaakkola"
        , "Nikolai Alho"
        , "Paulus Arajuuri"
        , "Albin Granlund"
        , "Nicholas Hamalainen"
        , "Robert Ivanov"
        , "Daniel O’Shaughnessy"
        , "Juhani Ojala"
        , "Jere Uronen"
        , "Leo Väisänen"
        , "Sauli Väisänen"
        , "Frederik Jensen"
        , "Glen Kamara"
        , "Joni Kauko"
        , "Thomas Lam"
        , "Rasmus Schüller"
        , "Pyry Soiri"
        , "Tim Sparv"
        , "Onni Valakari"
        , "Jasin-Amin Assehnoun"
        , "Marcus Forss"
        , "Lassi Lappalainen"
        , "Joel Pohjanpalo"
        , "Teemu Pukki"
        ]
    , group = B
    }


russia : TeamDatum
russia =
    { team = team "RUS" "Rusland"
    , players =
        [ "Yuri Dyupin"
        , "Andrey Lunev"
        , "Matvey Safonov"
        , "Anton Shunin"
        , "Igor Diveyev"
        , "Georgi Dzhikiya"
        , "Mário Fernandes"
        , "Vyacheslav Karavaev"
        , "Fedor Kudryashov"
        , "Ilya Samoshnikov"
        , "Andrey Semenov"
        , "Roman Yevgenyev"
        , "Yuri Zhirkov"
        , "Dmitri Barinov"
        , "Daniil Fomin"
        , "Aleksandr Golovin"
        , "Daler Kuzyaev"
        , "Denis Makarov"
        , "Aleksey Miranchuk"
        , "Maksim Mukhin"
        , "Magomed Ozdoev"
        , "Arsen Zakharyan"
        , "Roman Zobnin"
        , "Denis Cheryshev"
        , "Artem Dzyuba"
        , "Alexey Ionov"
        , "Andrey Mostovoy"
        , "Aleksandr Sobolev"
        , "Anton Zabolotny"
        , "Rifat Zhemaletdinov"
        ]
    , group = B
    }


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
    , group = B
    }



-- Group C (Amsterdam/Bucharest): Netherlands (hosts), Ukraine, Austria, Play-off winner D or A


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
    , group = C
    }


ukraine : TeamDatum
ukraine =
    { team = { teamID = "UKR", teamName = "Oekraïne" }
    , players =
        [ "Georgiy Bushchan"
        , "Andriy Lunin"
        , "Andriy Pyatov"
        , "Anatolii Trubin"
        , "Oleksandr Karavaev"
        , "Viktor Korniienko"
        , "Serhiy Kryvtsov"
        , "Mykola Matviyenko"
        , "Bogdan Mykhaylichenko"
        , "Vitaliy Mykolenko"
        , "Denys Popov"
        , "Eduard Sobol"
        , "Oleksandr Syrota"
        , "Oleksandr Tymchyk"
        , "Illia Zabarnyi"
        , "Oleksandr Zinchenko"
        , "Oleksandr Andriyevskiy"
        , "Artem Bondarenko"
        , "Vitaliy Buyalskiy"
        , "Viktor Kovalenko"
        , "Bogdan Lednev"
        , "Yevhen Makarenko"
        , "Ruslan Malinovskyi"
        , "Marlos"
        , "Mykola Shaparenko"
        , "Volodymyr Shepelev"
        , "Taras Stepanenko"
        , "Heorhiy Sudakov"
        , "Serhiy Sydorchuk"
        , "Artem Besedin"
        , "Artem Dovbyk"
        , "Yevhen Konoplyanka"
        , "Viktor Tsygankov"
        , "Roman Yaremchuk"
        , "Andriy Yarmolenko"
        , "Oleksandr Zubkov"
        ]
    , group = C
    }


austria : TeamDatum
austria =
    { team = { teamID = "AUT", teamName = "Oostenrijk" }
    , players =
        [ "Daniel Bachmann"
        , "Pavao Pervan"
        , "Alexander Schlager"
        , "David Alaba"
        , "Aleksandar Dragovic"
        , "Marco Friedl"
        , "Martin Hinteregger"
        , "Stefan Lainer"
        , "Philipp Lienhart"
        , "Stefan Posch"
        , "Christopher Trimmel"
        , "Andreas Ulmer"
        , "Julian Baumgartlinger"
        , "Christoph Baumgartner"
        , "Florian Grillitsch"
        , "Stefan Ilsanker"
        , "Konrad Laimer"
        , "Valentino Lazaro"
        , "Marcel Sabitzer"
        , "Louis Schaub"
        , "Xaver Schlager"
        , "Alessandro Schöpf"
        , "Marko Arnautovic"
        , "Michael Gregoritsch"
        , "Sasa Kalajdzic"
        , "Karim Onisiwo"
        ]
    , group = C
    }


north_macedonia : TeamDatum
north_macedonia =
    { team = { teamID = "MAC", teamName = "Macedonië" }
    , players =
        [ "Stole Dimitrievski"
        , "Riste Jankov"
        , "Damjan Shishkovski"
        , "Ezgjan Alioski"
        , "Egzon Bejtulai"
        , "Visar Musliu"
        , "Kire Ristevski"
        , "Stefan Ristovski"
        , "Darko Velkovski"
        , "Gjoko Zajkov"
        , "Arijan Ademi"
        , "Daniel Avramovski"
        , "Enis Bardhi"
        , "Darko Churlinov"
        , "Eljif Elmaz"
        , "Ferhan Hasani"
        , "Tihomir Kostadinov"
        , "Boban Nikolov"
        , "Marjan Radeski"
        , "Stefan Spirovski"
        , "Goran Pandev"
        , "Milan Ristovski"
        , "Vlatko Stojanovski"
        , "Aleksandar Trajkovski"
        , "Ivan Trichkovski"
        , "Krste Velkovski"
        ]
    , group = C
    }



-- Group D (London/Glasgow): England (hosts), Croatia, Play-off winner C, Czech Republic


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
    , group = D
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
    , group = D
    }


scotland : TeamDatum
scotland =
    { team = { teamID = "SCO", teamName = "Schotland" }
    , players =
        [ "Craig Gordon"
        , "David Marshall"
        , "Jon McLaughlin"
        , "Liam Cooper"
        , "Declan Gallagher"
        , "Grant Hanley"
        , "Jack Hendry"
        , "Scott McKenna"
        , "Stephen O'Donnell"
        , "Nathan Patterson"
        , "Andy Robertson"
        , "Greg Taylor"
        , "Kieran Tierney"
        , "Stuart Armstrong"
        , "Ryan Christie"
        , "John Fleck"
        , "Billy Gilmour"
        , "John McGinn"
        , "Callum McGregor"
        , "Scott McTominay"
        , "David Turnbull"
        , "Ché Adams"
        , "Lyndon Dykes"
        , "James Forrest"
        , "Ryan Fraser"
        , "Kevin Nisbet"
        ]
    , group = D
    }


czechia : TeamDatum
czechia =
    { team = { teamID = "CZE", teamName = "Tsjechië" }
    , players =
        [ "Ales Mandous"
        , "Jiri Pavlenka"
        , "Tomás Vaclík"
        , "Jan Boril"
        , "Jakub Brabec"
        , "Ondrej Celustka"
        , "Vladimír Coufal"
        , "Pavel Kaderábek"
        , "Tomás Kalas"
        , "Ales Mateju"
        , "David Zima"
        , "Antonín Barák"
        , "Vladimír Darida"
        , "Adam Hlozek"
        , "Tomás Holes"
        , "Jakub Jankto"
        , "Alex Král"
        , "Lukás Masopust"
        , "Jakub Pesek"
        , "Petr Sevcík"
        , "Tomás Soucek"
        , "Michael Krmenčík"
        , "Tomás Pekhart"
        , "Patrik Schick"
        , "Matej Vydra"
        ]
    , group = D
    }



-- Group E (Bilbao/Dublin): Spain (hosts), Sweden, Poland, Play-off winner B


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


sweden : TeamDatum
sweden =
    { team = team "SWE" "Zweden"
    , players =
        [ "Karl-Johan Johnsson"
        , "Kristoffer Nordfeldt"
        , "Robin Olsen"
        , "Ludwig Augustinsson"
        , "Marcus Danielson"
        , "Andreas Granqvist"
        , "Filip Helander"
        , "Pontus Jansson"
        , "Emil Krafth"
        , "Victor Lindelöf"
        , "Mikael Lustig"
        , "Martin Olsson"
        , "Jens-Lys Cajuste"
        , "Viktor Claesson"
        , "Albin Ekdal"
        , "Emil Forsberg"
        , "Sebastian Larsson"
        , "Kristoffer Olsson"
        , "Ken Sema"
        , "Mattias Svanberg"
        , "Gustav Svensson"
        , "Marcus Berg"
        , "Alexander Isak"
        , "Dejan Kulusevski"
        , "Jordan Larsson"
        , "Robin Quaison"
        ]
    , group = E
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
    , group = E
    }


slovakia : TeamDatum
slovakia =
    { team = { teamID = "SVK", teamName = "Slowakije" }
    , players =
        [ "Martin Dúbravka"
        , "Dusan Kuciak"
        , "Marek Rodák"
        , "Jakub Holúbek"
        , "Tomás Hubocan"
        , "Peter Pekarík"
        , "Lubomír Satka"
        , "Milan Skriniar"
        , "Denis Vavro"
        , "László Bénes"
        , "Matús Bero"
        , "Ondrej Duda"
        , "Marek Hamsík"
        , "Patrik Hrosovsky"
        , "Juraj Kucka"
        , "Stanislav Lobotka"
        , "Tomás Suslov"
        , "Vladimír Weiss"
        , "Róbert Bozeník"
        , "Michal Duris"
        , "Lukás Haraslín"
        , "Erik Jirka"
        , "Róbert Mak"
        , "David Strelec"
        ]
    , group = E
    }



-- Group F (Munich/Budapest): Play-off winner A or D, Portugal (holders), France, Germany (hosts)


hungary : TeamDatum
hungary =
    { team = { teamID = "HUN", teamName = "Hongarije" }
    , players =
        [ "Ádám Bogdán"
        , "Dénes Dibusz"
        , "Péter Gulácsi"
        , "Balázs Tóth"
        , "Bendegúz Bolla"
        , "Endre Botka"
        , "Attila Fiola"
        , "Szilveszter Hangya"
        , "Ákos Kecskés"
        , "Ádám Lang"
        , "Gergö Lovrencsics"
        , "Willi Orbán"
        , "Csaba Spandler"
        , "Attila Szalai"
        , "Tamás Cseri"
        , "Dániel Gazdag"
        , "Filip Holender"
        , "László Kleinheisler"
        , "Ádám Nagy"
        , "Loïc Négo"
        , "András Schäfer"
        , "Dávid Sigér"
        , "Dominik Szoboszlai"
        , "János Hahn"
        , "Nemanja Nikolic"
        , "Roland Sallai"
        , "Szabolcs Schön"
        , "Ádám Szalai"
        , "Kevin Varga"
        , "Roland Varga"
        ]
    , group = F
    }


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
    , group = F
    }


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
    , group = F
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
    , group = F
    }
