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
        [ "Saad Al Sheeb"
        , "Meshaal Barsham"
        , "Yousef Hassan"
        , "Salah Zakaria"
        , "Abdelkarim Hassan"
        , "Boualem Khoukhi"
        , "Ró-Ró"
        , "Tarek Salman"
        , "Bassam Al-Rawi"
        , "Musab Kheder"
        , "Homam Ahmed"
        , "Mohammed Emad"
        , "Jassem Gaber"
        , "Karim Boudiaf"
        , "Abdulaziz Hatem"
        , "Ali Assadalla"
        , "Mohammed Waad"
        , "Abdelrahman Moustafa"
        , "Osama Al-Tairi"
        , "Ahmed Fadhel"
        , "Mostafa Tarek"
        , "Hassan Al-Haydos"
        , "Akram Afif"
        , "Almoez Ali"
        , "Ismaeel Mohammad"
        , "Mohammed Muntari"
        , "Ahmed Alaaeldin"
        , "Yusuf Abdurisag"
        , "Khalid Muneer"
        , "Naif Al-Hadhrami"
        ]
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
        [ "Jasper Cillessen"
        , "Justin Bijlow"
        , "Mark Flekken"
        , "Remko Pasveer"
        , "Andries Noppert"
        , "Daley Blind"
        , "Stefan de Vrij"
        , "Virgil van Dijk\u{00A0}(captain)"
        , "Matthijs de Ligt"
        , "Denzel Dumfries"
        , "Nathan Aké"
        , "Jurriën Timber"
        , "Tyrell Malacia"
        , "Devyne Rensch"
        , "Mitchel Bakker"
        , "Sven Botman"
        , "Jeremie Frimpong"
        , "Pascal Struijk"
        , "Micky van de Ven"
        , "Frenkie de Jong"
        , "Steven Berghuis"
        , "Davy Klaassen"
        , "Marten de Roon"
        , "Jordy Clasie"
        , "Ryan Gravenberch"
        , "Teun Koopmeiners"
        , "Guus Til"
        , "Kenneth Taylor"
        , "Xavi Simons"
        , "Memphis Depay"
        , "Luuk de Jong"
        , "Steven Bergwijn"
        , "Vincent Janssen"
        , "Donyell Malen"
        , "Wout Weghorst"
        , "Cody Gakpo"
        , "Arnaut Danjuma"
        , "Noa Lang"
        , "Brian Brobbey"
        ]
    , group = A
    }



--


england : TeamDatum
england =
    { team = { teamID = "ENG", teamName = "Engeland" }
    , players =
        []
    , group = B
    }


wales : TeamDatum
wales =
    { team = { teamID = "WAL", teamName = "Wales" }
    , players =
        []
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
        [ "Emiliano Martínez"
        , "Franco Armani"
        , "Agustín Marchesín"
        , "Gerónimo Rulli"
        , "Juan Musso"
        , "Agustín Rossi"
        , "Nicolás Otamendi"
        , "Marcos Acuña"
        , "Nicolás Tagliafico"
        , "Germán Pezzella"
        , "Nahuel Molina"
        , "Gonzalo Montiel"
        , "Juan Foyth"
        , "Cristian Romero"
        , "Lucas Martínez Quarta"
        , "Lisandro Martínez"
        , "Facundo Medina"
        , "Marcos Senesi"
        , "Nehuén Pérez"
        , "Ángel Di María"
        , "Leandro Paredes"
        , "Rodrigo De Paul"
        , "Giovani Lo Celso"
        , "Guido Rodríguez"
        , "Nicolás González"
        , "Exequiel Palacios"
        , "Roberto Pereyra"
        , "Alejandro Gómez"
        , "Nicolás Domínguez"
        , "Maximiliano Meza"
        , "Lucas Ocampos"
        , "Alexis Mac Allister"
        , "Enzo Fernández"
        , "Emiliano Buendía"
        , "Thiago Almada"
        , "Matías Soulé"
        , "Nicolás Paz"
        , "Luka Romero"
        , "Valentín Carboni"
        , "Lionel Messi"
        , "Lautaro Martínez"
        , "Paulo Dybala"
        , "Ángel Correa"
        , "Joaquín Correa"
        , "Julián Álvarez"
        , "Lucas Alario"
        , "Giovanni Simeone"
        , "Alejandro Garnacho"
        ]
    , group = C
    }


saudi_arabia : TeamDatum
saudi_arabia =
    { team = { teamID = "KSA", teamName = "Saoedi Arabië" }
    , players =
        [ "Mohammed Al-Owais"
        , "Fawaz Al-Qarni"
        , "Mohammed Al Rubaie"
        , "Nawaf Al-Aqidi"
        , "Yasser Al-Shahrani"
        , "Mohammed Al-Breik"
        , "Ali Al-Bulaihi"
        , "Sultan Al-Ghanam"
        , "Saud Abdulhamid"
        , "Abdulelah Al-Amri"
        , "Hassan Tambakti"
        , "Abdullah Madu"
        , "Ahmed Bamsaud"
        , "Fahad Al-Muwallad"
        , "Salman Al-Faraj"
        , "Salem Al-Dawsari"
        , "Nawaf Al-Abed"
        , "Abdullah Otayf"
        , "Hattan Bahebri"
        , "Mohamed Kanno"
        , "Abdulellah Al-Malki"
        , "Sami Al-Najei"
        , "Ali Al-Hassan"
        , "Nasser Al-Dawsari"
        , "Ayman Yahya"
        , "Abdulrahman Al-Aboud"
        , "Riyadh Sharahili"
        , "Firas Al-Buraikan"
        , "Abdullah Al-Hamdan"
        , "Saleh Al-Shehri"
        , "Haitham Asiri"
        , "Abdullah Radif"
        ]
    , group = C
    }


mexico : TeamDatum
mexico =
    { team = { teamID = "MEX", teamName = "Mexico" }
    , players =
        [ "Guillermo Ochoa"
        , "Alfredo Talavera"
        , "Rodolfo Cota"
        , "Héctor Moreno"
        , "Jesús Gallardo"
        , "Néstor Araujo"
        , "César Montes"
        , "Jorge Sánchez"
        , "Gerardo Arteaga"
        , "Jesús Angulo"
        , "Kevin Álvarez"
        , "Johan Vásquez"
        , "Andrés Guardado"
        , "Héctor Herrera"
        , "Jesús Corona"
        , "Edson Álvarez"
        , "Orbelín Pineda"
        , "Uriel Antuna"
        , "Érick Gutiérrez"
        , "Carlos Rodríguez"
        , "Roberto Alvarado"
        , "Luis Romo"
        , "Diego Lainez"
        , "Alexis Vega"
        , "Érick Sánchez"
        , "Luis Chávez"
        , "Raúl Jiménez"
        , "Hirving Lozano"
        , "Henry Martín"
        , "Rogelio Funes Mori"
        , "Santiago Giménez"
        ]
    , group = C
    }


poland : TeamDatum
poland =
    { team = team "POL" "Polen"
    , players =
        [ "Wojciech Szczęsny"
        , "Łukasz Skorupski"
        , "Bartłomiej Drągowski"
        , "Kamil Grabara"
        , "Radosław Majecki"
        , "Kamil Glik"
        , "Bartosz Bereszyński"
        , "Jan Bednarek"
        , "Artur Jędrzejczyk"
        , "Tomasz Kędziora"
        , "Arkadiusz Reca"
        , "Tymoteusz Puchacz"
        , "Paweł Dawidowicz"
        , "Matty Cash"
        , "Michał Helik"
        , "Nicola Zalewski"
        , "Robert Gumny"
        , "Jakub Kiwior"
        , "Michał Karbownik"
        , "Paweł Bochniewicz"
        , "Mateusz Wieteska"
        , "Patryk Kun"
        , "Maik Nawrocki"
        , "Grzegorz Krychowiak"
        , "Kamil Grosicki"
        , "Piotr Zieliński"
        , "Karol Linetty"
        , "Mateusz Klich"
        , "Przemysław Frankowski"
        , "Kamil Jóźwiak"
        , "Jacek Góralski"
        , "Sebastian Szymański"
        , "Damian Szymański"
        , "Kacper Kozłowski"
        , "Szymon Żurkowski"
        , "Krystian Bielik"
        , "Jakub Kamiński"
        , "Mateusz Łęgowski"
        , "Michał Skóraś"
        , "Patryk Dziczek"
        , "Jakub Piotrowski"
        , "Robert Lewandowski"
        , "Arkadiusz Milik"
        , "Krzysztof Piątek"
        , "Karol Świderski"
        , "Adam Buksa"
        , "Dawid Kownacki"
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
        []
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
        []
    , group = D
    }



-----


spain : TeamDatum
spain =
    { team = team "ESP" "Spanje"
    , players =
        [ "Unai Simón"
        , "Kepa Arrizabalaga"
        , "Robert Sánchez"
        , "David Raya"
        , "David Soria"
        , "Sergio Ramos"
        , "Jordi Alba"
        , "César Azpilicueta"
        , "Dani Carvajal"
        , "Pau Torres"
        , "Iñigo Martínez"
        , "José Gayà"
        , "Eric García"
        , "Aymeric Laporte"
        , "Diego Llorente"
        , "Marcos Alonso"
        , "Hugo Guillamón"
        , "Alejandro Balde"
        , "Arnau Martínez"
        , "Sergio Busquets"
        , "Koke"
        , "Thiago"
        , "Rodri"
        , "Marcos Llorente"
        , "Pedri"
        , "Gavi"
        , "Carlos Soler"
        , "Mikel Merino"
        , "Sergi Roberto"
        , "Sergio Canales"
        , "Brais Méndez"
        , "Oihan Sancet"
        , "Alvaro Morata"
        , "Ferran Torres"
        , "Marco Asensio"
        , "Rodrigo"
        , "Pablo Sarabia"
        , "Dani Olmo"
        , "Mikel Oyarzabal"
        , "Iago Aspas"
        , "Gerard Moreno"
        , "Yeremy Pino"
        , "Ansu Fati"
        , "Nico Williams"
        , "Borja Iglesias"
        ]
    , group = E
    }


germany : TeamDatum
germany =
    { team = team "GER" "Duitsland"
    , players =
        []
    , group = E
    }


costa_rica : TeamDatum
costa_rica =
    { team = { teamID = "CRC", teamName = "Costa Rica" }
    , players =
        [ "Keylor Navas"
        , "Esteban Alvarado"
        , "Patrick Sequeira"
        , "Francisco Calvo"
        , "Bryan Oviedo"
        , "Óscar Duarte"
        , "Kendall Waston"
        , "Rónald Matarrita"
        , "Keysher Fuller"
        , "Juan Pablo Vargas"
        , "Carlos Martínez"
        , "Celso Borges"
        , "Bryan Ruiz"
        , "Yeltsin Tejeda"
        , "Gerson Torres"
        , "Jewison Bennette"
        , "Daniel Chacón"
        , "Youstin Salas"
        , "Roan Wilson"
        , "Brandon Aguilera"
        , "Douglas López"
        , "Anthony Hernández"
        , "Álvaro Zamora"
        , "Joel Campbell"
        , "Johan Venegas"
        , "Anthony Contreras"
        ]
    , group = E
    }


japan : TeamDatum
japan =
    { team = { teamID = "JPN", teamName = "Japan" }
    , players =
        [ "Eiji Kawashima"
        , "Shūichi Gonda"
        , "Daniel Schmidt"
        , "Miki Yamane"
        , "Shogo Taniguchi"
        , "Ko Itakura"
        , "Yuto Nagatomo"
        , "Takehiro Tomiyasu"
        , "Hiroki Sakai"
        , "Maya Yoshida"
        , "Hiroki Ito"
        , "Wataru Endo"
        , "Gaku Shibasaki"
        , "Ritsu Dōan"
        , "Kaoru Mitoma"
        , "Takumi Minamino"
        , "Takefusa Kubo"
        , "Hidemasa Morita"
        , "Junya Ito"
        , "Daichi Kamada"
        , "Ao Tanaka"
        , "Yuki Soma"
        , "Takuma Asano"
        , "Ayase Ueda"
        , "Daizen Maeda"
        ]
    , group = E
    }



-----


belgium : TeamDatum
belgium =
    { team = team "BEL" "België"
    , players =
        []
    , group = F
    }


croatia : TeamDatum
croatia =
    { team = team "CRO" "Kroatië"
    , players =
        [ "Dominik Livaković"
        , "Ivica Ivušić"
        , "Ivo Grbić"
        , "Dominik Kotarski"
        , "Nediljko Labrović"
        , "Domagoj Vida"
        , "Dejan Lovren"
        , "Borna Barišić"
        , "Duje Ćaleta-Car"
        , "Josip Juranović"
        , "Joško Gvardiol"
        , "Borna Sosa"
        , "Josip Stanišić"
        , "Marin Pongračić"
        , "Martin Erlić"
        , "Josip Šutalo"
        , "Luka Modrić"
        , "Mateo Kovačić"
        , "Marcelo Brozović"
        , "Mario Pašalić"
        , "Nikola Vlašić"
        , "Luka Ivanušec"
        , "Lovro Majer"
        , "Kristijan Jakić"
        , "Luka Sučić"
        , "Josip Mišić"
        , "Ivan Perišić"
        , "Andrej Kramarić"
        , "Josip Brekalo"
        , "Bruno Petković"
        , "Mislav Oršić"
        , "Ante Budimir"
        , "Marko Livaja"
        , "Antonio-Mirko Čolak"
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
        []
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
        [ "Rui Patrício"
        , "Anthony Lopes"
        , "Diogo Costa"
        , "Rui Silva"
        , "José Sá"
        , "Pepe"
        , "Danilo Pereira"
        , "Raphaël Guerreiro"
        , "José Fonte"
        , "Rúben Dias"
        , "João Cancelo"
        , "Cédric Soares"
        , "Nélson Semedo"
        , "Nuno Mendes"
        , "Mário Rui"
        , "Diogo Dalot"
        , "Domingos Duarte"
        , "Fábio Cardoso"
        , "David Carmo"
        , "Thierry Correia"
        , "Tiago Djaló"
        , "Gonçalo Inácio"
        , "Diogo Leite"
        , "Nuno Santos"
        , "António Silva"
        , "Nuno Tavares"
        , "João Moutinho"
        , "William Carvalho"
        , "Bernardo Silva"
        , "João Mário"
        , "Bruno Fernandes"
        , "Renato Sanches"
        , "Rúben Neves"
        , "João Palhinha"
        , "Sérgio Oliveira"
        , "Matheus Nunes"
        , "Otávio"
        , "Vitinha"
        , "Fábio Carvalho"
        , "Florentino Luís"
        , "Fábio Vieira"
        , "Cristiano Ronaldo"
        , "André Silva"
        , "Gonçalo Guedes"
        , "João Félix"
        , "Rafael Leão"
        , "Francisco Trincão"
        , "Ricardo Horta"
        , "Paulinho"
        , "Pedro Gonçalves"
        , "Daniel Podence"
        , "Beto"
        , "Jota"
        , "Gonçalo Ramos"
        , "Vitinha"
        ]
    , group = H
    }


ghana : TeamDatum
ghana =
    { team = { teamID = "GHA", teamName = "Ghana" }
    , players =
        [ "Richard Ofori"
        , "Joe Wollacott"
        , "Lawrence Ati-Zigi"
        , "Abdul Manaf Nurudeen"
        , "Ibrahim Danlad"
        , "Jonathan Mensah"
        , "Abdul Rahman Baba"
        , "Daniel Amartey"
        , "Andy Yiadom"
        , "Alexander Djiku"
        , "Kasim Nuhu"
        , "Joseph Aidoo"
        , "Gideon Mensah"
        , "Denis Odoi"
        , "Alidu Seidu"
        , "Mohammed Salisu"
        , "Tariq Lamptey"
        , "Dennis Nkrumah-Korsah"
        , "Patrick Kpozo"
        , "Abdul Mumin"
        , "Stephan Ambrosius"
        , "Ibrahim Imoro"
        , "André Ayew"
        , "Mubarak Wakaso"
        , "Thomas Partey"
        , "Jeffrey Schlupp"
        , "Iddrisu Baba"
        , "Mohammed Kudus"
        , "Daniel-Kofi Kyereh"
        , "Edmund Addo"
        , "Joseph Paintsil"
        , "Majeed Ashimeru"
        , "Elisha Owusu"
        , "Daniel Afriyie"
        , "Salifu Mudasiru"
        , "Salis Abdul Samed"
        , "Jordan Ayew"
        , "Richmond Boakye"
        , "Samuel Owusu"
        , "Caleb Ekuban"
        , "Kamaldeen Sulemana"
        , "Abdul Fatawu Issahaku"
        , "Joel Fameyeh"
        , "Felix Afena-Gyan"
        , "Kwasi Okyere Wriedt"
        , "Osman Bukari"
        , "Yaw Yeboah"
        , "Emmanuel Gyasi"
        , "Christopher Antwi-Adjei"
        , "Iñaki Williams"
        , "Antoine Semenyo"
        , "Mohammed Dauda"
        , "Kamal Sowah"
        , "Ransford-Yeboah Königsdörffer"
        , "Ernest Nuamah"
        ]
    , group = H
    }


uruguay : TeamDatum
uruguay =
    { team = { teamID = "URU", teamName = "Uruguay" }
    , players =
        [ "Fernando Muslera"
        , "Sergio Rochet"
        , "Sebastián Sosa"
        , "Guillermo de Amores"
        , "Santiago Mele"
        , "Gastón Olveira"
        , "Diego Godín"
        , "Martín Cáceres"
        , "José Giménez"
        , "Sebastián Coates"
        , "Matías Viña"
        , "Giovanni González"
        , "Ronald Araújo"
        , "Guillermo Varela"
        , "Mathías Olivera"
        , "Joaquín Piquerez"
        , "Damián Suárez"
        , "Sebastián Cáceres"
        , "Bruno Méndez"
        , "Agustín Rogel"
        , "Gastón Álvarez"
        , "Santiago Bueno"
        , "Leandro Cabrera"
        , "Alfonso Espino"
        , "Lucas Olaza"
        , "Federico Pereira"
        , "José Luis Rodríguez"
        , "Matías Vecino"
        , "Rodrigo Bentancur"
        , "Federico Valverde"
        , "Giorgian de Arrascaeta"
        , "Lucas Torreira"
        , "Nicolás de la Cruz"
        , "Mauro Arambarri"
        , "Fernando Gorriarán"
        , "Manuel Ugarte"
        , "César Araújo"
        , "Maximiliano Araújo"
        , "Felipe Carballo"
        , "Fabricio Díaz"
        , "Luis Suárez"
        , "Edinson Cavani"
        , "Jonathan Rodríguez"
        , "Maxi Gómez"
        , "Darwin Núñez"
        , "Facundo Torres"
        , "Facundo Pellistri"
        , "Agustín Álvarez Martínez"
        , "Diego Rossi"
        , "Agustín Canobbio"
        , "David Terans"
        , "Brian Ocampo"
        , "Martín Satriano"
        , "Thiago Borbas"
        , "Nicolás López"
        ]
    , group = H
    }


south_korea : TeamDatum
south_korea =
    { team = { teamID = "KOR", teamName = "Zuid Korea" }
    , players =
        []
    , group = H
    }
