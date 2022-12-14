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
    { town = "Bilbao", stadium = "San Mam??s" }


dublin : Stadium
dublin =
    { town = "Dublin", stadium = "Arena" }


munchen : Stadium
munchen =
    { town = "M??nchen", stadium = "Allianz Arena" }


budapest : Stadium
budapest =
    { town = "Boedapest", stadium = "Pusk??s Arena" }



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
        , "R??-R??"
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
        [ "Alexander Dom??nguez"
        , "Hern??n Gal??ndez"
        , "Mois??s Ram??rez"
        , "Gonzalo Valle"
        , "Pervis Estupi????n"
        , "??ngelo Preciado"
        , "Piero Hincapi??"
        , "Xavier Arreaga"
        , "Byron Castillo"
        , "Diego Palacios"
        , "Fernando Le??n"
        , "Jackson Porozo"
        , "William Pacho"
        , "??ngel Mena"
        , "Carlos Gruezo"
        , "Jhegson M??ndez"
        , "Gonzalo Plata"
        , "Mois??s Caicedo"
        , "Romario Ibarra"
        , "Alan Franco"
        , "Jos?? Cifuentes"
        , "Jeremy Sarmiento"
        , "Nilson Angulo"
        , "Patrickson Delgado"
        , "Anthony Valencia"
        , "Enner Valencia"
        , "Michael Estrada"
        , "Djorkaeff Reasco"
        ]
    , group = A
    }


senegal : TeamDatum
senegal =
    { team = { teamID = "SEN", teamName = "Senegal" }
    , players =
        [ "??douard Mendy"
        , "Alioune Badara Faty"
        , "Bingourou Kamara"
        , "Saliou Ciss"
        , "Youssouf Sabaly"
        , "Kalidou Koulibaly "
        , "Pape Abou Ciss??"
        , "Abdou Diallo"
        , "Fod?? Ballo-Tour??"
        , "Abdoulaye Seck"
        , "Bouna Sarr"
        , "Idrissa Gueye"
        , "Nampalys Mendy"
        , "Cheikhou Kouyat??"
        , "Pape Matar Sarr"
        , "Moustapha Name"
        , "Pape Gueye"
        , "Boulaye Dia"
        , "Sadio Man??"
        , "Isma??la Sarr"
        , "Bamba Dieng"
        , "Keita Bald??"
        , "Famara Di??dhiou"
        , "Mame Thiam"
        , "Habib Diallo"
        ]
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
        , "Nathan Ak??"
        , "Jurri??n Timber"
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
        [ "Nick Pope"
        , "Aaron Ramsdale"
        , "Dean Henderson"
        , "Reece James"
        , "Luke Shaw"
        , "John Stones"
        , "Eric Dier"
        , "Harry Maguire"
        , "Kieran Trippier"
        , "Kyle Walker"
        , "Conor Coady"
        , "Marc Gu??hi"
        , "Ben Chilwell"
        , "Trent Alexander-Arnold"
        , "Fikayo Tomori"
        , "Declan Rice"
        , "Jude Bellingham"
        , "Jordan Henderson"
        , "Mason Mount"
        , "James Ward-Prowse"
        , "Harry Kane"
        , "Raheem Sterling"
        , "Phil Foden"
        , "Bukayo Saka"
        , "Ivan Toney"
        , "Tammy Abraham"
        , "Jarrod Bowen"
        ]
    , group = B
    }


wales : TeamDatum
wales =
    { team = { teamID = "WAL", teamName = "Wales" }
    , players =
        [ "Wayne Hennessey"
        , "Danny Ward"
        , "Tom King"
        , "Chris Gunter"
        , "Connor Roberts"
        , "Joe Rodon"
        , "Neco Williams"
        , "Rhys Norrington-Davies"
        , "Ben Cabango"
        , "Jonny Williams"
        , "Joe Morrell"
        , "Matthew Smith"
        , "Dylan Levitt"
        , "Rubin Colwill"
        , "Sorba Thomas"
        , "Wes Burns"
        , "Jordan James"
        , "Gareth Bale"
        , "Daniel James"
        , "Kieffer Moore"
        , "Tyler Roberts"
        , "Brennan Johnson"
        , "Mark Harris"
        ]
    , group = B
    }


usa : TeamDatum
usa =
    { team = { teamID = "USA", teamName = "Amerika" }
    , players =
        [ "Matt Turner"
        , "Sean Johnson"
        , "Ethan Horvath"
        , "Zack Steffen"
        , "John Pulskamp"
        , "Sergi??o Dest"
        , "Mark McKenzie"
        , "Sam Vines"
        , "Erik Palmer-Brown"
        , "Joe Scally"
        , "Reggie Cannon"
        , "Cameron Carter-"
        , "Vickers"
        , "Chris Richards"
        , "Antonee Robinson"
        , "George Bello"
        , "Miles Robinson"
        , "Brooks Lennon"
        , "DeJuan Jones"
        , "Kobi Henry"
        , "Henry Kessler"
        , "Justin Che"
        , "Jonathan G??mez"
        , "Bryan Reynolds"
        , "Kevin Paredes"
        , "Kellyn Acosta"
        , "Weston McKennie"
        , "Tyler Adams"
        , "Luca de la Torre"
        , "Johnny Cardoso"
        , "Malik Tillman"
        , "Yunus Musah"
        , "Djordje Mihailovic"
        , "Gianluca Busio"
        , "James Sands"
        , "Sebastian Lletget"
        , "Jackson Yueill"
        , "Cole Bassett"
        , "Caden Clark"
        , "Christian Pulisic"
        , "Brenden Aaronson"
        , "Josh Sargent"
        , "Giovanni Reyna"
        , "Ricardo Pepi"
        , "Timothy Weah"
        , "Haji Wright"
        , "Jordan Pefok"
        , "Gyasi Zardes"
        , "Taylor Booth"
        , "Cade Cowell"
        ]
    , group = B
    }


iran : TeamDatum
iran =
    { team = { teamID = "IRN", teamName = "Iran" }
    , players =
        [ "Ehsan Hajsafi"
        , "Milad Mohammadi"
        , "Sadegh Moharrami"
        , "Majid Hosseini"
        , "Aref Aghasi"
        , "Mehdi Shiri"
        , "Aref Gholami"
        , "Danial Esmaeilifar"
        , "Farshad Faraji"
        , "Siavash Yazdani"
        , "Alireza Jahanbakhsh"
        , "Saeid Ezatolahi"
        , "Saman Ghoddos"
        , "Ahmad Nourollahi"
        , "Ali Gholizadeh"
        , "Ali Karimi"
        , "Mehdi Hosseini"
        , "Saeid Sadeghi"
        , "Reza Asadi"
        , "Mohammad Karimi"
        , "Yasin Salmani"
        , "Amirhossein Hosseinzadeh"
        , "Soroush Rafiei"
        , "Kamal Kamyabinia"
        , "Zobeir Niknafs"
        , "Karim Ansarifard"
        , "Sardar Azmoun"
        , "Mehdi Taremi"
        , "Mehdi Ghayedi"
        , "Shahriyar Moghanlou"
        , "Allahyar Sayyadmanesh"
        , "Ali Alipour"
        , "Kaveh Rezaei"
        , "Shahab Zahedi"
        ]
    , group = B
    }


argentina : TeamDatum
argentina =
    { team = { teamID = "ARG", teamName = "Argentina" }
    , players =
        [ "Emiliano Mart??nez"
        , "Franco Armani"
        , "Agust??n Marches??n"
        , "Ger??nimo Rulli"
        , "Juan Musso"
        , "Agust??n Rossi"
        , "Nicol??s Otamendi"
        , "Marcos Acu??a"
        , "Nicol??s Tagliafico"
        , "Germ??n Pezzella"
        , "Nahuel Molina"
        , "Gonzalo Montiel"
        , "Juan Foyth"
        , "Cristian Romero"
        , "Lucas Mart??nez Quarta"
        , "Lisandro Mart??nez"
        , "Facundo Medina"
        , "Marcos Senesi"
        , "Nehu??n P??rez"
        , "??ngel Di Mar??a"
        , "Leandro Paredes"
        , "Rodrigo De Paul"
        , "Giovani Lo Celso"
        , "Guido Rodr??guez"
        , "Nicol??s Gonz??lez"
        , "Exequiel Palacios"
        , "Roberto Pereyra"
        , "Alejandro G??mez"
        , "Nicol??s Dom??nguez"
        , "Maximiliano Meza"
        , "Lucas Ocampos"
        , "Alexis Mac Allister"
        , "Enzo Fern??ndez"
        , "Emiliano Buend??a"
        , "Thiago Almada"
        , "Mat??as Soul??"
        , "Nicol??s Paz"
        , "Luka Romero"
        , "Valent??n Carboni"
        , "Lionel Messi"
        , "Lautaro Mart??nez"
        , "Paulo Dybala"
        , "??ngel Correa"
        , "Joaqu??n Correa"
        , "Juli??n ??lvarez"
        , "Lucas Alario"
        , "Giovanni Simeone"
        , "Alejandro Garnacho"
        ]
    , group = C
    }


saudi_arabia : TeamDatum
saudi_arabia =
    { team = { teamID = "KSA", teamName = "Saoedi Arabi??" }
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
        , "H??ctor Moreno"
        , "Jes??s Gallardo"
        , "N??stor Araujo"
        , "C??sar Montes"
        , "Jorge S??nchez"
        , "Gerardo Arteaga"
        , "Jes??s Angulo"
        , "Kevin ??lvarez"
        , "Johan V??squez"
        , "Andr??s Guardado"
        , "H??ctor Herrera"
        , "Jes??s Corona"
        , "Edson ??lvarez"
        , "Orbel??n Pineda"
        , "Uriel Antuna"
        , "??rick Guti??rrez"
        , "Carlos Rodr??guez"
        , "Roberto Alvarado"
        , "Luis Romo"
        , "Diego Lainez"
        , "Alexis Vega"
        , "??rick S??nchez"
        , "Luis Ch??vez"
        , "Ra??l Jim??nez"
        , "Hirving Lozano"
        , "Henry Mart??n"
        , "Rogelio Funes Mori"
        , "Santiago Gim??nez"
        ]
    , group = C
    }


poland : TeamDatum
poland =
    { team = team "POL" "Polen"
    , players =
        [ "Wojciech Szcz??sny"
        , "??ukasz Skorupski"
        , "Bart??omiej Dr??gowski"
        , "Kamil Grabara"
        , "Rados??aw Majecki"
        , "Kamil Glik"
        , "Bartosz Bereszy??ski"
        , "Jan Bednarek"
        , "Artur J??drzejczyk"
        , "Tomasz K??dziora"
        , "Arkadiusz Reca"
        , "Tymoteusz Puchacz"
        , "Pawe?? Dawidowicz"
        , "Matty Cash"
        , "Micha?? Helik"
        , "Nicola Zalewski"
        , "Robert Gumny"
        , "Jakub Kiwior"
        , "Micha?? Karbownik"
        , "Pawe?? Bochniewicz"
        , "Mateusz Wieteska"
        , "Patryk Kun"
        , "Maik Nawrocki"
        , "Grzegorz Krychowiak"
        , "Kamil Grosicki"
        , "Piotr Zieli??ski"
        , "Karol Linetty"
        , "Mateusz Klich"
        , "Przemys??aw Frankowski"
        , "Kamil J????wiak"
        , "Jacek G??ralski"
        , "Sebastian Szyma??ski"
        , "Damian Szyma??ski"
        , "Kacper Koz??owski"
        , "Szymon ??urkowski"
        , "Krystian Bielik"
        , "Jakub Kami??ski"
        , "Mateusz ????gowski"
        , "Micha?? Sk??ra??"
        , "Patryk Dziczek"
        , "Jakub Piotrowski"
        , "Robert Lewandowski"
        , "Arkadiusz Milik"
        , "Krzysztof Pi??tek"
        , "Karol ??widerski"
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
        [ "Alban Lafont"
        , "Steve Mandanda"
        , "Alphonse Areola"
        , "Benjamin Pavard"
        , "William Saliba"
        , "Rapha??l Varane"
        , "Jules Kound??"
        , "Dayot Upamecano"
        , "Jonathan Clauss"
        , "Beno??t Badiashile"
        , "Ferland Mendy"
        , "Adrien Truffert"
        , "Eduardo"
        , "Camavinga"
        , "Aur??lien"
        , "Tchouam??ni"
        , "Matteo Guendouzi"
        , "Jordan Veretout"
        , "Youssouf Fofana"
        , "Antoine"
        , "Griezmann"
        , "Olivier Giroud"
        , "Kylian Mbapp??"
        , "Christopher"
        , "Nkunku"
        , "Randal Kolo Muani"
        , "Ousmane Demb??l??"
        ]
    , group = D
    }


australia : TeamDatum
australia =
    { team = { teamID = "AUS", teamName = "Australia" }
    , players =
        [ "Mathew"
        , "Ryan"
        , "Danny Vukovic"
        , "Andrew"
        , "Redmayne"
        , "Aziz Behich"
        , "Milo?? Degenek"
        , "Bailey Wright"
        , "Fran Kara??i??"
        , "Harry Souttar"
        , "Nathaniel"
        , "Atkinson"
        , "Joel King"
        , "Kye Rowles"
        , "Thomas Deng"
        , "Aaron Mooy"
        , "Jackson Irvine"
        , "Ajdin Hrustic"
        , "Riley McGree"
        , "Keanu Baccus"
        , "Cameron Devlin"
        , "Mathew Leckie"
        , "Awer Mabil"
        , "Jamie Maclaren"
        , "Mitchell Duke"
        , "Martin Boyle"
        , "Craig Goodwin"
        , "Jason Cummings"
        , "Garang Kuol"
        ]
    , group = D
    }


tunisia : TeamDatum
tunisia =
    { team = { teamID = "TUN", teamName = "Tunisi??" }
    , players =
        [ "Mohamed Sedki Debchi"
        , "Aymen Dahmen"
        , "Bechir Ben Sa??d"
        , "Bilel Ifa"
        , "Montassar Talbi"
        , "Ali Abdi"
        , "Nader Ghandri"
        , "Dylan Bronn"
        , "Mortadha Ben Ouanes"
        , "Ali Ma??loul"
        , "Omar Rekik"
        , "Mohamed Dr??ger"
        , "Hamza Mathlouthi"
        , "Rami Kaib"
        , "Yan Valery"
        , "Sa??f-Eddine Khaoui"
        , "Ferjani Sassi"
        , "Hannibal Mejbri"
        , "Ellyes Skhiri"
        , "Ghailene Chaalali"
        , "Anis Ben Slimane"
        , "Cha??m El Djebali"
        , "A??ssa La??douni"
        , "Youssef"
        , "Msakni"
        , "Wahbi Khazri"
        , "Taha Yassine Khenissi"
        , "Seifeddine Jaziri"
        , "Na??m Sliti"
        , "Issam Jebali"
        , "Sayfallah Ltaief"
        ]
    , group = D
    }


denmark : TeamDatum
denmark =
    { team = { teamID = "DEN", teamName = "Denmark" }
    , players =
        [ "Kasper Schmeichel"
        , "Oliver Christensen"
        , "Simon Kj??r"
        , "Andreas Christensen"
        , "Jens Stryger Larsen"
        , "Daniel Wass"
        , "Joakim M??hle"
        , "Joachim Andersen"
        , "Rasmus Kristensen"
        , "Victor Nelsson"
        , "Christian Eriksen"
        , "Thomas Delaney"
        , "Pierre-Emile H??jbjerg"
        , "Mathias Jensen"
        , "Martin Braithwaite"
        , "Andreas Cornelius"
        , "Kasper Dolberg"
        , "Andreas Skov Olsen"
        , "Mikkel Damsgaard"
        , "Jonas Wind"
        , "Jesper Lindstr??m"
        ]
    , group = D
    }



-----


spain : TeamDatum
spain =
    { team = team "ESP" "Spanje"
    , players =
        [ "Unai Sim??n"
        , "Kepa Arrizabalaga"
        , "Robert S??nchez"
        , "David Raya"
        , "David Soria"
        , "Sergio Ramos"
        , "Jordi Alba"
        , "C??sar Azpilicueta"
        , "Dani Carvajal"
        , "Pau Torres"
        , "I??igo Mart??nez"
        , "Jos?? Gay??"
        , "Eric Garc??a"
        , "Aymeric Laporte"
        , "Diego Llorente"
        , "Marcos Alonso"
        , "Hugo Guillam??n"
        , "Alejandro Balde"
        , "Arnau Mart??nez"
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
        , "Brais M??ndez"
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
        [ "Oliver Baumann"
        , "Kevin Trapp"
        , "Marc-Andr?? ter Stegen"
        , "David Raum"
        , "Matthias Ginter"
        , "Thilo Kehrer"
        , "Niklas S??le"
        , "Benjamin Henrichs"
        , "Armel Bella-Kotchap"
        , "Robin Gosens"
        , "Nico Schlotterbeck"
        , "Joshua Kimmich"
        , "Kai Havertz"
        , "Maximilian Arnold"
        , "Jamal Musiala"
        , "Jonas Hofmann"
        , "??lkay G??ndo??an"
        , "Timo Werner"
        , "Serge Gnabry"
        , "Thomas"
        , "M??ller"
        , "Leroy San??"
        ]
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
        , "??scar Duarte"
        , "Kendall Waston"
        , "R??nald Matarrita"
        , "Keysher Fuller"
        , "Juan Pablo Vargas"
        , "Carlos Mart??nez"
        , "Celso Borges"
        , "Bryan Ruiz"
        , "Yeltsin Tejeda"
        , "Gerson Torres"
        , "Jewison Bennette"
        , "Daniel Chac??n"
        , "Youstin Salas"
        , "Roan Wilson"
        , "Brandon Aguilera"
        , "Douglas L??pez"
        , "Anthony Hern??ndez"
        , "??lvaro Zamora"
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
        , "Sh??ichi Gonda"
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
        , "Ritsu D??an"
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
    { team = team "BEL" "Belgi??"
    , players =
        [ "Thibaut Courtois"
        , "Simon Mignolet"
        , "Koen Casteels"
        , "Matz Sels"
        , "Toby Alderweireld"
        , "Arthur Theate"
        , "Zeno Debast"
        , "Jan Vertonghen"
        , "Thomas Meunier"
        , "Brandon Mechele"
        , "Dedryck Boyata"
        , "Jason Denayer"
        , "Wout Faes"
        , "Axel Witsel"
        , "Kevin De Bruyne\u{00A0}(third"
        , "captain)"
        , "Youri Tielemans"
        , "Yannick Carrasco"
        , "Charles De Ketelaere"
        , "Leandro Trossard"
        , "Amadou Onana"
        , "Leander Dendoncker"
        , "Hans Vanaken"
        , "Timothy Castagne"
        , "Alexis Saelemaekers"
        , "Lo??s Openda"
        , "Eden Hazard"
        , "Dries Mertens"
        , "Michy Batshuayi"
        , "Thorgan Hazard"
        , "Dodi Lukebakio"
        ]
    , group = F
    }


croatia : TeamDatum
croatia =
    { team = team "CRO" "Kroati??"
    , players =
        [ "Dominik Livakovi??"
        , "Ivica Ivu??i??"
        , "Ivo Grbi??"
        , "Dominik Kotarski"
        , "Nediljko Labrovi??"
        , "Domagoj Vida"
        , "Dejan Lovren"
        , "Borna Bari??i??"
        , "Duje ??aleta-Car"
        , "Josip Juranovi??"
        , "Jo??ko Gvardiol"
        , "Borna Sosa"
        , "Josip Stani??i??"
        , "Marin Pongra??i??"
        , "Martin Erli??"
        , "Josip ??utalo"
        , "Luka Modri??"
        , "Mateo Kova??i??"
        , "Marcelo Brozovi??"
        , "Mario Pa??ali??"
        , "Nikola Vla??i??"
        , "Luka Ivanu??ec"
        , "Lovro Majer"
        , "Kristijan Jaki??"
        , "Luka Su??i??"
        , "Josip Mi??i??"
        , "Ivan Peri??i??"
        , "Andrej Kramari??"
        , "Josip Brekalo"
        , "Bruno Petkovi??"
        , "Mislav Or??i??"
        , "Ante Budimir"
        , "Marko Livaja"
        , "Antonio-Mirko ??olak"
        ]
    , group = F
    }


canada : TeamDatum
canada =
    { team = { teamID = "CAN", teamName = "Canada" }
    , players =
        [ "Dayne St. Clair"
        , "James Pantemis"
        , "Thomas Hasal"
        , "Doneil Henry"
        , "Richie Laryea"
        , "Alistair Johnston"
        , "Kamal Miller"
        , "Zachary Brault-Guillard"
        , "Raheem Edwards"
        , "Lukas MacNaughton"
        , "Joel Waterman"
        , "Samuel Piette"
        , "Jonathan Osorio"
        , "Mark-Anthony Kaye"
        , "Liam Fraser"
        , "Isma??l Kon??"
        , "Mathieu Choini??re"
        , "Lucas Cavallini"
        , "Jayden Nelson"
        , "Jacob Shaffelburg"
        , "Ayo Akinola"
        ]
    , group = F
    }


morocco : TeamDatum
morocco =
    { team = { teamID = "MAR", teamName = "Marokko" }
    , players =
        [ "Yassine"
        , "Bounou"
        , "Munir Mohamedi"
        , "Ahmed Reda"
        , "Tagnaouti"
        , "Anas Zniti"
        , "Achraf Hakimi"
        , "Noussair Mazraoui"
        , "Jawad El Yamiq"
        , "Romain"
        , "Sa??ss"
        , "Achraf Dari"
        , "Samy Mmaee"
        , "Yahia Attiyat Allah"
        , "Badr Benoun"
        , "Fahd Moufi[67]"
        , "Sofyan Amrabat"
        , "Azzedine Ounahi"
        , "Youn??s Belhanda"
        , "Abdelhamid Sabiri"
        , "Ilias Chair"
        , "Selim Amallah"
        , "Amine Harit"
        , "Yahya Jabrane"
        , "Hakim Ziyech"
        , "Munir El Haddadi"
        , "Zakaria Aboukhlal"
        , "Abde Ezzalzouli"
        , "Sofiane Boufal"
        , "Youssef En-Nesyri"
        , "Walid Cheddira"
        , "Ryan Mmaee"
        , "Ayoub El Kaabi"
        , "Soufiane Rahimi"
        ]
    , group = F
    }



-----


switzerland : TeamDatum
switzerland =
    { team = { teamID = "SUI", teamName = "Switzerland" }
    , players =
        [ "Yann Sommer"
        , "Yvon Mvogo"
        , "David von Ballmoos"
        , "Ricardo Rodriguez"
        , "Fabian Sch??r"
        , "Manuel Akanji"
        , "Nico Elvedi"
        , "Silvan Widmer"
        , "Kevin Mbabu"
        , "Eray C??mert"
        , "Xherdan Shaqiri"
        , "Granit Xhaka"
        , "Remo Freuler"
        , "Denis Zakaria"
        , "Djibril Sow"
        , "Renato Steffen"
        , "Fabian Frei"
        , "Michel Aebischer"
        , "Ardon Jashari"
        , "Haris Seferovic"
        , "Breel Embolo"
        , "Ruben Vargas"
        , "Cedric Itten"
        , "Dan Ndoye"
        , "Zeki Amdouni"
        ]
    , group = G
    }


brazil : TeamDatum
brazil =
    { team = { teamID = "BRA", teamName = "Brazil" }
    , players =
        [ "Alisson"
        , "Ederson"
        , "Weverton"
        , "Dani Alves"
        , "Marquinhos"
        , "Thiago Silva"
        , "Eder Milit??o"
        , "Danilo"
        , "Alex Sandro"
        , "Alex Telles"
        , "Bremer, Casemiro"
        , "Fred"
        , "Fabinho"
        , "Bruno Guimar??es"
        , "Lucas Paquet??"
        , "Everton Ribeiro"
        , "Neymar"
        , "Vinicius Jr."
        , "Richarlison"
        , "Raphinha"
        , "Antony"
        , "Gabriel Jesus"
        , "Gabriel Martinelli"
        , "Pedro"
        , "Rodrygo"
        ]
    , group = G
    }


serbia : TeamDatum
serbia =
    { team = { teamID = "SRB", teamName = "Servi??" }
    , players =
        [ "Marko Dmitrovi??"
        , "Vanja Milinkovi??-Savi??"
        , "Marko Ili??"
        , "Stefan Mitrovi??"
        , "Strahinja Pavlovi??"
        , "Milo?? Veljkovi??"
        , "Filip Mladenovi??"
        , "Aleksa Terzi??"
        , "Sr??an Babi??"
        , "Erhan Ma??ovi??"
        , "Strahinja Erakovi??"
        , "Du??an Tadi??"
        , "Filip Kosti??"
        , "Nemanja Maksimovi??"
        , "Nemanja Radonji??"
        , "Filip ??uri??i??"
        , "Sa??a Luki??"
        , "Andrija ??ivkovi??"
        , "Darko Lazovi??"
        , "Ivan Ili??"
        , "Stefan Mitrovi??"
        , "Aleksandar Mitrovi??"
        , "Luka Jovi??"
        , "Du??an Vlahovi??"
        ]
    , group = G
    }


cameroon : TeamDatum
cameroon =
    { team = { teamID = "CAM", teamName = "Kameroen" }
    , players =
        [ "Simon Ngapandouetnbu"
        , "Devis Epassy"
        , "Andr?? Onana"
        , "James Bievenue Djaoyang"
        , "Simon Omossola"
        , "Jean Efala"
        , "Narcisse Nlend"
        , "Darlin Yongwa"
        , "Nicolas Nkoulou"
        , "Christopher Wooh"
        , "Oumar Gonzalez"
        , "Nouhou Tolo"
        , "Olivier Mbaizo"
        , "Collins Fai"
        , "Jean-Charles Castelletto"
        , "Enzo Ebosse"
        , "Michael Ngadeu-Ngadjui"
        , "Enzo Tchato"
        , "Ambroise Oyongo"
        , "Duplexe Tchamba"
        , "Harold Moukoudi"
        , "J??r??me Ongu??n??"
        , "Joyskim Dawa"
        , "Jean-Claude Billong"
        , "Sacha Boey"
        , "Samuel Kotto"
        , "Olivier Ntcham"
        , "Georges Mandjeck"
        , "Pierre Kunde"
        , "Martin Hongla"
        , "Samuel Gouet"
        , "Ga??l Ondoua"
        , "Jean Onana"
        , "Brice Ambina"
        , "Andr??-Frank Zambo"
        , "Anguissa"
        , "Jeando Fuchs"
        , "Arnaud Djoum"
        , "James L??a Siliki"
        , "Yvan Neyou"
        , "Moumi Ngamaleu"
        , "L??andre Tawamba"
        , "Vincent"
        , "Aboubakar"
        , "Jean-Pierre Nsame"
        , "Bryan Mbeumo"
        , "Georges-K??vin Nkoudou"
        , "Karl Toko Ekambi"
        , "Eric Maxim Choupo-Moting"
        , "Christian Bassogog"
        , "St??phane Bahoken"
        , "Ignatius Ganago"
        , "Danny Loader"
        , "Didier Lamkel Z??"
        , "K??vin Soni"
        , "Clinton N'Jie"
        , "Paul-Georges Ntep"
        , "John Mary"
        , "Jeremy Ebobisse"
        ]
    , group = G
    }



-- Group B (Copenhagen/St Petersburg): Denmark (hosts), Finland, Belgium, Russia (hosts)


portugal : TeamDatum
portugal =
    { team = team "POR" "Portugal"
    , players =
        [ "Rui Patr??cio"
        , "Anthony Lopes"
        , "Diogo Costa"
        , "Rui Silva"
        , "Jos?? S??"
        , "Pepe"
        , "Danilo Pereira"
        , "Rapha??l Guerreiro"
        , "Jos?? Fonte"
        , "R??ben Dias"
        , "Jo??o Cancelo"
        , "C??dric Soares"
        , "N??lson Semedo"
        , "Nuno Mendes"
        , "M??rio Rui"
        , "Diogo Dalot"
        , "Domingos Duarte"
        , "F??bio Cardoso"
        , "David Carmo"
        , "Thierry Correia"
        , "Tiago Djal??"
        , "Gon??alo In??cio"
        , "Diogo Leite"
        , "Nuno Santos"
        , "Ant??nio Silva"
        , "Nuno Tavares"
        , "Jo??o Moutinho"
        , "William Carvalho"
        , "Bernardo Silva"
        , "Jo??o M??rio"
        , "Bruno Fernandes"
        , "Renato Sanches"
        , "R??ben Neves"
        , "Jo??o Palhinha"
        , "S??rgio Oliveira"
        , "Matheus Nunes"
        , "Ot??vio"
        , "Vitinha"
        , "F??bio Carvalho"
        , "Florentino Lu??s"
        , "F??bio Vieira"
        , "Cristiano Ronaldo"
        , "Andr?? Silva"
        , "Gon??alo Guedes"
        , "Jo??o F??lix"
        , "Rafael Le??o"
        , "Francisco Trinc??o"
        , "Ricardo Horta"
        , "Paulinho"
        , "Pedro Gon??alves"
        , "Daniel Podence"
        , "Beto"
        , "Jota"
        , "Gon??alo Ramos"
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
        , "Andr?? Ayew"
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
        , "I??aki Williams"
        , "Antoine Semenyo"
        , "Mohammed Dauda"
        , "Kamal Sowah"
        , "Ransford-Yeboah K??nigsd??rffer"
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
        , "Sebasti??n Sosa"
        , "Guillermo de Amores"
        , "Santiago Mele"
        , "Gast??n Olveira"
        , "Diego God??n"
        , "Mart??n C??ceres"
        , "Jos?? Gim??nez"
        , "Sebasti??n Coates"
        , "Mat??as Vi??a"
        , "Giovanni Gonz??lez"
        , "Ronald Ara??jo"
        , "Guillermo Varela"
        , "Math??as Olivera"
        , "Joaqu??n Piquerez"
        , "Dami??n Su??rez"
        , "Sebasti??n C??ceres"
        , "Bruno M??ndez"
        , "Agust??n Rogel"
        , "Gast??n ??lvarez"
        , "Santiago Bueno"
        , "Leandro Cabrera"
        , "Alfonso Espino"
        , "Lucas Olaza"
        , "Federico Pereira"
        , "Jos?? Luis Rodr??guez"
        , "Mat??as Vecino"
        , "Rodrigo Bentancur"
        , "Federico Valverde"
        , "Giorgian de Arrascaeta"
        , "Lucas Torreira"
        , "Nicol??s de la Cruz"
        , "Mauro Arambarri"
        , "Fernando Gorriar??n"
        , "Manuel Ugarte"
        , "C??sar Ara??jo"
        , "Maximiliano Ara??jo"
        , "Felipe Carballo"
        , "Fabricio D??az"
        , "Luis Su??rez"
        , "Edinson Cavani"
        , "Jonathan Rodr??guez"
        , "Maxi G??mez"
        , "Darwin N????ez"
        , "Facundo Torres"
        , "Facundo Pellistri"
        , "Agust??n ??lvarez Mart??nez"
        , "Diego Rossi"
        , "Agust??n Canobbio"
        , "David Terans"
        , "Brian Ocampo"
        , "Mart??n Satriano"
        , "Thiago Borbas"
        , "Nicol??s L??pez"
        ]
    , group = H
    }


south_korea : TeamDatum
south_korea =
    { team = { teamID = "KOR", teamName = "Zuid Korea" }
    , players =
        [ "Kim Dong-jun"
        , "Kim Min-jae"
        , "Kim Ju-sung"
        , "Lee Jae-ik"
        , "Lee Yong"
        , "Jung Seung-hyun"
        , "Kang Sang-woo"
        , "Choi Ji-mook"
        , "Son Heung-min\u{00A0}"
        , "Lee Jae-sung"
        , "Hwang Hee-chan"
        , "Hwang In-beom"
        , "Jeong Woo-yeong"
        , "Lee Kang-in"
        , "Lee Yeong-jae"
        , "Kim Dong-hyun"
        , "Kang Seong-jin"
        , "Goh Young-joon"
        , "Lee Ki-hyuk"
        , "Nam Tae-hee"
        , "Lee Dong-jun"
        , "Won Du-jae"
        , "Lee Dong-gyeong"
        , "Eom Ji-sung"
        , "Kim Dae-won"
        , "Hwang Ui-jo"
        , "Cho Young-wook"
        , "Kim Gun-hee"
        ]
    , group = H
    }
