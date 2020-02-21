module Bets.Init.Euro2020.Tournament exposing
    ( amsterdam
    , baku
    , bilbao
    , boekarest
    , bracket
    , budapest
    , glasgow
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
    , c4 = slot "C4" playoffDA

    --     -- Group D (London/Glasgow): England (hosts), Croatia, Play-off winner C, Czech Republic
    , d1 = slot "D1" england
    , d2 = slot "D2" croatia
    , d3 = slot "D3" playoffC
    , d4 = slot "D4" czechia

    --     -- Group E (Bilbao/Dublin): Spain (hosts), Sweden, Poland, Play-off winner B
    , e1 = slot "E1" spain
    , e2 = slot "E2" sweden
    , e3 = slot "E3" poland
    , e4 = slot "E4" playoffB

    --     --- Group F (Munich/Budapest): Play-off winner A or D, Portugal (holders), France, Germany (hosts)
    , f1 = slot "F1" playoffAD
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
        tnwa =
            TeamNode "WA" (FirstPlace A) Nothing TBD

        tnwb =
            TeamNode "WB" (FirstPlace B) Nothing TBD

        tnwc =
            TeamNode "WC" (FirstPlace C) Nothing TBD

        tnwd =
            TeamNode "WD" (FirstPlace D) Nothing TBD

        tnwe =
            TeamNode "WE" (FirstPlace E) Nothing TBD

        tnwf =
            TeamNode "WF" (FirstPlace F) Nothing TBD

        tnra =
            TeamNode "RA" (SecondPlace A) Nothing TBD

        tnrb =
            TeamNode "RB" (SecondPlace B) Nothing TBD

        tnrc =
            TeamNode "RC" (SecondPlace C) Nothing TBD

        tnrd =
            TeamNode "RD" (SecondPlace D) Nothing TBD

        tnre =
            TeamNode "RE" (SecondPlace E) Nothing TBD

        tnrf =
            TeamNode "RF" (SecondPlace F) Nothing TBD

        tnt1 =
            TeamNode "T1" (BestThirdFrom [ A, B, C ]) Nothing TBD

        tnt2 =
            TeamNode "T2" (BestThirdFrom [ A, B, C, D ]) Nothing TBD

        tnt3 =
            TeamNode "T3" (BestThirdFrom [ A, D, E, F ]) Nothing TBD

        tnt4 =
            TeamNode "T4" (BestThirdFrom [ D, E, F ]) Nothing TBD

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
    , m22 = match "m22" t.e4 t.e2 (date 2020 Jun 20) (time 20 0) bilbao
    , m21 = match "m21" t.e1 t.e3 (date 2020 Jun 19) (time 14 0) dublin
    , m33 = match "m33" t.e4 t.e1 (date 2020 Jun 24) (time 20 0) bilbao
    , m34 = match "m34" t.e2 t.e3 (date 2020 Jun 24) (time 20 0) dublin

    -- Group F
    , m12 = match "m12" t.f1 t.f2 (date 2020 Jun 16) (time 17 0) munchen
    , m11 = match "m11" t.f3 t.f4 (date 2020 Jun 16) (time 14 0) boekarest
    , m24 = match "m24" t.f4 t.f2 (date 2020 Jun 20) (time 17 0) munchen
    , m23 = match "m23" t.f1 t.f3 (date 2020 Jun 20) (time 20 0) boekarest
    , m36 = match "m36" t.f4 t.f1 (date 2020 Jun 24) (time 16 0) munchen
    , m35 = match "m35" t.f2 t.f3 (date 2020 Jun 24) (time 16 0) boekarest
    }


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
    , playoffDA

    -- Group D (London/Glasgow): England (hosts), Croatia, Play-off winner C, Czech Republic
    , england
    , croatia
    , playoffC
    , czechia

    -- Group E (Bilbao/Dublin): Spain (hosts), Sweden, Poland, Play-off winner B
    , spain
    , sweden
    , poland
    , playoffB

    --- Group F (Munich/Budapest): Play-off winner A or D, Portugal (holders), France, Germany (hosts)
    , playoffAD
    , portugal
    , france
    , germany
    ]


teams : List Team
teams =
    List.map .team initTeamData



-- teams
-- Group A (Rome/Baku): Turkey, Italy (hosts), Wales, Switzerland


turkey : TeamDatum
turkey =
    { team = { teamID = "TUR", teamName = "Turkey" }
    , players =
        []
    , group = A
    }


wales : TeamDatum
wales =
    { team = { teamID = "WAL", teamName = "Wales" }
    , players =
        []
    , group = A
    }


italy : TeamDatum
italy =
    { team = { teamID = "ITA", teamName = "Italy" }
    , players =
        []
    , group = A
    }


switzerland : TeamDatum
switzerland =
    { team = { teamID = "SUI", teamName = "Switzerland" }
    , players =
        []
    , group = A
    }



-- Group B (Copenhagen/St Petersburg): Denmark (hosts), Finland, Belgium, Russia (hosts)


denmark : TeamDatum
denmark =
    { team = { teamID = "DEN", teamName = "Denmark" }
    , players =
        [ "Kasper Schmeichel"
        , "Jonas Lössl"
        , "Frederik Rönnow"
        , "Jesper Hansen"
        , "Simon Kjaer"
        , "Andreas Christensen"
        , "Mathias 'Zanka' Jörgensen"
        , "Jannik Vestergaard"
        , "Andreas Bjelland"
        , "Henrik Dalsgaard"
        , "Peter Ankersen"
        , "Nicolai Boilesen"
        , "Jens Stryger"
        , "Riza Durmisi"
        , "Jonas Knudsen"
        , "William Kvist"
        , "Robert Skov"
        , "Thomas Delaney"
        , "Lukas Lerager"
        , "Lasse Schöne"
        , "Mike Jensen"
        , "Christian Eriksen"
        , "Daniel Wass"
        , "Pierre-Emile Höjbjerg"
        , "Mathias Jensen"
        , "Michael Krohn-Dehli"
        , "Pione Sisto"
        , "Martin Braithwaite"
        , "Andreas Cornelius"
        , "Viktor Fischer"
        , "Yussuf Poulsen"
        , "Nicolai Jörgensen"
        , "Nicklas Bendtner"
        , "Kasper Dolberg"
        , "Kenneth Zohore"
        ]
    , group = B
    }


finland : TeamDatum
finland =
    { team = { teamID = "FIN", teamName = "Finland" }
    , players =
        []
    , group = A
    }


russia : TeamDatum
russia =
    { team = team "RUS" "Rusland"
    , players =
        [ "Soslan Dzhanaev"
        , "Igor Akinfeev"
        , "Vladimir Gabulov"
        , "Andrey Lyunev"
        , "Vladimir Granat"
        , "Ruslan Kambolov"
        , "Fyodor Kudryashov"
        , "Ilya Kutepov"
        , "Roman Neustädter"
        , "Konstantin Rausch"
        , "Igor Smolnikov,"
        , "Mario Fernandes"
        , "Andrei Semyonov"
        , "Yuri Gazinski"
        , "Aleksandr Golovin"
        , "Alan Dzagoev"
        , "Aleksandr Erokhin"
        , "Yuri Zhirkov"
        , "Daler Kuzyaev"
        , "Roman Zobnin"
        , "Aleksandr Samedov"
        , "Anton Miranchuk"
        , "Denis Cheryshev"
        , "Aleksandr Tashaev"
        , "Fyodor Chalov"
        , "Artem Dzyuba"
        , "Fyodor Smolov"
        , "Aleksei Miranchuk"
        ]
    , group = B
    }


belgium : TeamDatum
belgium =
    { team = team "BEL" "België"
    , players =
        [ "Thibaut Courtois"
        , "Simon Mignolet"
        , "Koen Casteels"
        , "Matz Sels"
        , "Toby Alderweireld"
        , "Dedryck Boyata"
        , "Jan Vertonghen"
        , "Thomas Vermaelen"
        , "Thomas Meunier"
        , "Christian Kabasele"
        , "Vincent Kompany"
        , "Laurent Ciman"
        , "Jordan Lukaku"
        , "Leander Dendoncker"
        , "Axel Witsel"
        , "Kevin De Bruyne"
        , "Marouane Fellaini"
        , "Mousa Dembele"
        , "Youri Tielemans"
        , "Yannick Carrasco"
        , "Nacer Chadli"
        , "Adnan Januzaj"
        , "Romelu Lukaku"
        , "Eden Hazard"
        , "Dries Mertens"
        , "Thorgan Hazard"
        , "Michy Batshuayi"
        , "Christian Benteke"
        ]
    , group = B
    }



-- Group C (Amsterdam/Bucharest): Netherlands (hosts), Ukraine, Austria, Play-off winner D or A


netherlands : TeamDatum
netherlands =
    { team = { teamID = "NED", teamName = "Nederland" }
    , players =
        []
    , group = C
    }


ukraine : TeamDatum
ukraine =
    { team = { teamID = "UKR", teamName = "Oekraïne" }
    , players =
        []
    , group = C
    }


austria : TeamDatum
austria =
    { team = { teamID = "AUT", teamName = "Oostenrijk" }
    , players =
        []
    , group = C
    }


playoffDA : TeamDatum
playoffDA =
    { team = { teamID = "PDA", teamName = "Playoff DA" }
    , players =
        []
    , group = C
    }



-- Group D (London/Glasgow): England (hosts), Croatia, Play-off winner C, Czech Republic


england : TeamDatum
england =
    { team = { teamID = "ENG", teamName = "Engeland" }
    , players =
        [ "Jack Butland"
        , "Jordan Pickford"
        , "Nick Pope"
        , "Trent Alexander-Arnold"
        , "Gary Cahill"
        , "Kyle Walker"
        , "John Stones"
        , "Harry Maguire"
        , "Kieran Trippier"
        , "Danny Rose"
        , "Phil Jones"
        , "Ashley Young"
        , "Eric Dier"
        , "Dele Alli"
        , "Jesse Lingard"
        , "Jordan Henderson"
        , "Fabian Delph"
        , "Ruben Loftus-Cheek"
        , "Jamie Vardy"
        , "Marcus Rashford"
        , "Raheem Sterling"
        , "Danny Welbeck"
        , "Harry Kane"
        ]
    , group = D
    }


croatia : TeamDatum
croatia =
    { team = team "CRO" "Kroatië"
    , players =
        [ "Danijel Subasic"
        , "Lovre Kalinic"
        , "Dominik Livakovic"
        , "Vedran Corluka"
        , "Domagoj Vida"
        , "Ivan Strinic"
        , "Dejan Lovren"
        , "Sime Vrsaljko"
        , "Josip Pivaric"
        , "Tin Jedvaj"
        , "Matej Mitrovic"
        , "Duje Caleta-Car"
        , "Luka Modric"
        , "Mateo Kovacic"
        , "Ivan Rakitic"
        , "Milan Badelj"
        , "Marcelo Brozovic"
        , "Filip Bradaric"
        , "Mario Mandzukic"
        , "Ivan Perisic"
        , "Nikola Kalinic"
        , "Andrej Kramaric"
        , "Marko Pjaca"
        , "Ante Rebic"
        ]
    , group = D
    }


playoffC : TeamDatum
playoffC =
    { team = { teamID = "PfC", teamName = "Playoff C" }
    , players =
        []
    , group = D
    }


czechia : TeamDatum
czechia =
    { team = { teamID = "CZE", teamName = "Tsjechië" }
    , players =
        []
    , group = D
    }



-- Group E (Bilbao/Dublin): Spain (hosts), Sweden, Poland, Play-off winner B


spain : TeamDatum
spain =
    { team = team "ESP" "Spanje"
    , players =
        [ "David de Gea"
        , "Pepe Reina"
        , "Kepa Arrizabalaga"
        , "Jordi Alba"
        , "Nacho Monreal"
        , "Álvaro Odriozola"
        , "Nacho Fernandez"
        , "Dani Carvajal"
        , "Gerard Piqué"
        , "Sergio Ramos"
        , "Cesar Azpilicueta"
        , "Sergio Busquets"
        , "Isco"
        , "Thiago Alcántara"
        , "David Silva"
        , "Andrés Iniesta"
        , "Saúl Ñíguez"
        , "Koke"
        , "Marco Asensio"
        , "Iago Aspas"
        , "Diego Costa"
        , "Rodrigo Moreno"
        , "Lucas Vázquez"
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
        , "Andreas Granqvist"
        , "Filip Helander"
        , "Pontus Jansson"
        , "Emil Krafth"
        , "Mikael Lustig"
        , "Victor Linderlöf"
        , "Martin Olsson"
        , "Viktor Claesson"
        , "Jimmy Durmaz"
        , "Albin Ekdal"
        , "Emil Forsberg"
        , "Oscar Hiljemark"
        , "Sebastian Larsson"
        , "Marcus Rohdén"
        , "Gustav Svensson"
        , "Marcus Berg"
        , "John Guidetti"
        , "Isaac Kiese Thelin"
        , "Ola Toivonen"
        ]
    , group = E
    }


poland : TeamDatum
poland =
    { team = team "POL" "Polen"
    , players =
        [ "Bartosz Bialkowski"
        , "Lukasz Fabianski"
        , "Lukasz Skorupski"
        , "Wojciech Szczesny"
        , "Jan Bednarek"
        , "Bartosz Bereszynski"
        , "Thiago Cionek"
        , "Kamil Glik"
        , "Michal Pazdan"
        , "Artur Jedrzejczyk"
        , "Marcin Kaminski"
        , "Tomasz Kedziora"
        , "Lukasz Piszczek"
        , "Jakub Blaszczykowski"
        , "Pawel Dawidowicz"
        , "Przemyslaw Frankowski"
        , "Jacek Goralski"
        , "Kamil Grosicki"
        , "Rafal Kurzawa"
        , "Szymon Zurkowski"
        , "Damian Kadzior"
        , "Grzegorz Krychowiak"
        , "Karol Linetty"
        , "Maciej Makuszewski"
        , "Sebastian Szymanski"
        , "Krzysztof Maczynski"
        , "Slawomir Peszko"
        , "Maciej Rybus"
        , "Piotr Zielinski"
        , "Dawid Kownacki"
        , "Robert Lewandowski"
        , "Arkadiusz Milik"
        , "Krzysztof Piatek"
        , "Lukasz Teodorczyk"
        , "Kamil Wilczek"
        ]
    , group = E
    }


playoffB : TeamDatum
playoffB =
    { team = { teamID = "PlB", teamName = "Playoff B" }
    , players =
        []
    , group = E
    }



-- Group F (Munich/Budapest): Play-off winner A or D, Portugal (holders), France, Germany (hosts)


playoffAD : TeamDatum
playoffAD =
    { team = { teamID = "PAD", teamName = "Playoff AD" }
    , players =
        []
    , group = F
    }


portugal : TeamDatum
portugal =
    { team = team "POR" "Portugal"
    , players =
        [ "Anthony Lopes"
        , "Beto"
        , "Rui Patricio"
        , "Bruno Alves"
        , "Cédric Soares"
        , "José Fonte"
        , "Mário Rui"
        , "Pepe"
        , "Raphaël Guerreiro"
        , "Ricardo Pereira"
        , "Rúben Dias"
        , "Adrien Silva"
        , "Bruno Fernandes"
        , "William Carvalho"
        , "João Mario"
        , "João Moutinho"
        , "Manuel Fernandes"
        , "André Silva"
        , "Bernardo Silva"
        , "Cristiano Ronaldo"
        , "Gelson Martins"
        , "Gonçalo Guedes"
        , "Ricardo Quaresma"
        ]
    , group = F
    }


france : TeamDatum
france =
    { team = { teamID = "FRA", teamName = "Frankrijk" }
    , players =
        [ "Hugo Lloris"
        , "Steve Mandanda"
        , "Alphonse Areola"
        , "Djibril Sidibé"
        , "Benjamin Pavard"
        , "Raphaël Varane"
        , "Presnel Kimpembe"
        , "Adil Rami"
        , "Samuel Umtiti"
        , "Lucas Hernández"
        , "Benjamin Mendy"
        , "Paul Pogba"
        , "N'Golo Kanté"
        , "Corentin Tolisso"
        , "Blaise Matuidi"
        , "Steven N’Zonzi"
        , "Thomas Lemar"
        , "Kylian Mbappé"
        , "Olivier Giroud"
        , "Antoine Griezmann"
        , "Ousmane Dembélé"
        , "Nabil Fékir"
        , "Florian Thauvin"
        ]
    , group = F
    }


germany : TeamDatum
germany =
    { team = team "GER" "Duitsland"
    , players =
        [ "Manuel Neuer"
        , "Bernd Leno"
        , "Marc-André ter Stegen"
        , "Kevin Trapp"
        , "Mats Hummels"
        , "Joshua Kimmich"
        , "Niklas Süle"
        , "Jerome Boateng"
        , "Matthias Ginter"
        , "Jonas Hector"
        , "Marvin Plattenhardt"
        , "Antonio Rüdiger"
        , "Jonathan Tah"
        , "Sebastian Rudy"
        , "Thomas Müller"
        , "Leon Goretzka"
        , "Ilkay Gündogan"
        , "Sami Khedira"
        , "Toni Kroos"
        , "Mesut Özil"
        , "Marco Reus"
        , "Mario Gomez"
        , "Leroy Sané"
        , "Julian Draxler"
        , "Timo Werner"
        , "Julian Brandt"
        , "Nils Petersen"
        ]
    , group = F
    }
