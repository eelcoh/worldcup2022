module Bets.Types.Team
    exposing
        ( team
        , display
        , mdisplay
        , mdisplayFull
        , log
        , flag
        , initTeamData
        , isComplete
        , decode
        , encode
        , encodeMaybe
        )

import Html exposing (..)
import Html.Attributes exposing (style, src, class)
import Json.Encode
import Json.Decode exposing (Decoder, maybe)
import Maybe.Extra as M
import Bets.Types exposing (Team, TeamID, TeamData, TeamDatum, Group(..))


initTeamData : TeamData
initTeamData =
    [ france
    , romania
    , albania
    , switzerland
    , england
    , russia
    , wales
    , slovakia
    , germany
    , ukraine
    , poland
    , northernIreland
    , spain
    , czechRepublic
    , turkey
    , croatia
    , belgium
    , italy
    , ireland
    , sweden
    , portugal
    , iceland
    , austria
    , hungary
    ]


team : TeamID -> String -> Team
team teamID teamName =
    Team teamID teamName


display : Team -> String
display team =
    team.teamID


mdisplayFull : Maybe Team -> String
mdisplayFull mteam =
    case mteam of
        Nothing ->
            ""

        Just team ->
            team.teamName


mdisplay : Maybe Team -> String
mdisplay mteam =
    case mteam of
        Nothing ->
            ""

        Just team ->
            display team


log : Team -> String
log team =
    team.teamName


flag : Maybe Team -> Html msg
flag mteam =
    let
        uri team =
            (flagUri 2) ++ team.teamID ++ ".png"

        default =
            team "xyz" ""
    in
        case mteam of
            Nothing ->
                img [ (src (uri default)) ] []

            Just t ->
                img [ (src (uri t)) ] []


flagUri : Int -> String
flagUri size =
    if (size > 5) || (size < 0) then
        "http://img.fifa.com/images/flags/3/"
    else
        "http://img.fifa.com/images/flags/" ++ (toString size) ++ "/"


isComplete : Maybe Team -> Bool
isComplete mTeam =
    M.isJust mTeam


decode : Decoder Team
decode =
    Json.Decode.map2 Team
        (Json.Decode.field "teamID" Json.Decode.string)
        (Json.Decode.field "teamName" Json.Decode.string)


encodeMaybe : Maybe Team -> Json.Encode.Value
encodeMaybe mTeam =
    case mTeam of
        Just t ->
            encode t

        Nothing ->
            Json.Encode.null


encode : Team -> Json.Encode.Value
encode team =
    Json.Encode.object
        [ ( "teamID", Json.Encode.string team.teamID )
        , ( "teamName", Json.Encode.string team.teamName )
        ]



-- teams


france : TeamDatum
france =
    { team = { teamID = "FRA", teamName = "Frankrijk" }
    , players =
        [ "Hugo Lloris"
        , "Steve Mandanda"
        , "Benoit Costil"
        , "Bacary Sagna"
        , "Christophe Jallet"
        , "Patrice Evra"
        , "Raphaël Varane"
        , "Laurent Koscielny"
        , "Eliaquim Mangala"
        , "Jérémy Mathieu"
        , "Lucas Digne"
        , "Lassana Diarra"
        , "N’Golo Kanté"
        , "Blaise Matuidi"
        , "Paul Pogba"
        , "Yohan Cabaye"
        , "Moussa Sissoko "
        , "Antoine Griezmann"
        , "André-Pierre Gignac"
        , "Anthony Martial"
        , "Dimitri Payet"
        , "Kingsley Coman"
        , "Olivier Giroud"
        ]
    , group = A
    }


romania : TeamDatum
romania =
    { team = (team "ROU" "Roemenië")
    , players =
        [ "Ciprian Tatarusanu"
        , "Costel Pantilimon"
        , "Silviu Lung"
        , "Cristian Sapunaru"
        , "Alexandru Matel"
        , "Vlad Chiriches"
        , "Valerica Gaman"
        , "Cosmin Moti"
        , "Dragos Grigore"
        , "Razvan Raţ"
        , "Steliano Filip"
        , "Alin Tosca"
        , "Mihai Pintilii"
        , "Ovidiu Hoban"
        , "Adrian Ropotan"
        , "Andrei Prepelita"
        , "Adrian Popa"
        , "Gabriel Torje"
        , "Alexandru Chipciu"
        , "Alexandru Maxim"
        , "Nicolae Stanciu"
        , "Lucian Sanmartean"
        , "Claudiu Keserü"
        , "Bogdan Stancu"
        , "Florin Andone"
        , "Denis Alibec"
        , "Ioan Hora"
        , "Andrei Ivan"
        ]
    , group = A
    }


albania : TeamDatum
albania =
    { team = (team "ALB" "Albanië")
    , players =
        [ "Etrit Berisha"
        , "Alban Hoxha"
        , "Orges Shehi"
        , "Lorik Cana"
        , "Arlind Ajeti"
        , "Berat Gjimshiti"
        , "Mërgim Mavraj"
        , "Amir Rrahmani"
        , "Elseid Hysaj"
        , "Ansi Agolli"
        , "Frederic Veseli"
        , "Naser Aliji"
        , "Ledjan Memushaj"
        , "Ergys Kace"
        , "Andi Lila"
        , "Migjen Basha"
        , "Odise Roshi"
        , "Burim Kukeli"
        , "Ermir Lenjani"
        , "Herolind Shala"
        , "Taulant Xhaka"
        , "Armir Abrashi"
        , "Bekim Balaj"
        , "Sokol Cikalleshi"
        , "Armando Sadiku"
        , "Milot Rashica"
        , "Shkëlzen Gashi"
        ]
    , group = A
    }


switzerland : TeamDatum
switzerland =
    { team = (team "SUI" "Zwitserland")
    , players =
        [ "Roman Bürki"
        , "Marwin Hitz"
        , "Yvon Mvongo"
        , "Yann Sommer"
        , "Johan Djourou"
        , "Nicol Elvedi"
        , "Michael Lang"
        , "Stephan Lichtsteiner"
        , "François Moubandje"
        , "Ricardo Rodriguez"
        , "Fabian Schär"
        , "Philippe Senderos"
        , "Steve Von Bergen"
        , "Silvan Widmer"
        , "Valon Behrami"
        , "Eeren Derdiyok"
        , "Blerim Dzemaili"
        , "Breel Embolo"
        , "Gelson Fernandes"
        , "Fabian Frei"
        , "Admir Mehmedi"
        , "Haris Seferovic"
        , "Xherdan Shaqiri"
        , "Renato Steffen"
        , "Shani Tarashaj"
        , "Granit Xhaka"
        , "Denis Zakaria"
        , "Luca Zuffi"
        ]
    , group = A
    }


england : TeamDatum
england =
    { team = { teamID = "ENG", teamName = "Engeland" }
    , players =
        [ "Joe Hart"
        , "Fraser Forster"
        , "Tom Heaton"
        , "Nathaniel Clyne"
        , "Kyle Walker"
        , "Danny Rose"
        , "Ryan Bertrand"
        , "Chris Smalling"
        , "John Stones"
        , "Gary Cahill"
        , "Dele Alli"
        , "Ross Barkley"
        , "Fabian Delph"
        , "Danny Drinkwater"
        , "Eric Dier"
        , "Jordan Henderson"
        , "James Milner"
        , "Adam Lallana"
        , "Raheem Sterling"
        , "Jack Wilshere"
        , "Andros Townsend"
        , "Wayne Rooney"
        , "Harry Kane"
        , "Jamie Vardy"
        , "Marcus Rashford"
        , "Daniel Sturridge"
        ]
    , group = B
    }


russia : TeamDatum
russia =
    { team = (team "RUS" "Rusland")
    , players =
        [ "Igor Akinfeev"
        , "Guilherme"
        , "Yuri Lodygin"
        , "Alexei Berezutsky"
        , "Vasily Berezutsky"
        , "Sergei Ignashevich"
        , "Dmitry Kombarov"
        , "Roman Neustädter"
        , "Georgy Shchennikov"
        , "Roman Shishkin"
        , "Igor Smolnikov"
        , "Igor Denisov"
        , "Dmitri Torbinski"
        , "Denis Glushakov"
        , "Alexander Golovin"
        , "Oleg Ivanov"
        , "Pavel Mamaev"
        , "Alexander Samedov"
        , "Oleg Shatov"
        , "Roman Shirokov"
        , "Artyom Dzyuba"
        , "Alexander Kokorin"
        , "Fyodor Smolov"
        ]
    , group = B
    }


wales : TeamDatum
wales =
    { team = (team "WAL" "Wales")
    , players =
        [ "Wayne Hennessey"
        , "Danny Ward"
        , "Owain Fon Williams"
        , "Ben Davies"
        , "Neil Taylor"
        , "Chris Gunter"
        , "Ashley Williams"
        , "James Chester"
        , "Ashley Richards"
        , "Paul Dummett"
        , "Adam Henley"
        , "Adam Matthews"
        , "James Collins"
        , "Aaron Ramsey"
        , "Joe Ledley"
        , "David Vaughan"
        , "Joe Allen"
        , "David Cotterill"
        , "Jonathan Williams"
        , "George Williams"
        , "Andy King"
        , "Emyr Huws"
        , "Dave Edwards (Wolverhampton Wanderers)."
        , "Hal Robson-Kanu"
        , "Sam Vokes"
        , "Tom Bradshaw"
        , "Tom Lawrence"
        , "Simon Church"
        , "Wes Burns"
        , "Gareth Bale"
        ]
    , group = B
    }


slovakia : TeamDatum
slovakia =
    { team = (team "SVK" "Slowakije")
    , players =
        [ "Matus Kozacik"
        , "Jan Mucha"
        , "Jan Novota"
        , "Peter Pekarik"
        , "Milan Skriniar"
        , "Martin Skrtel"
        , "Norbert Gyoember"
        , "Jan Durica"
        , "Kornel Salata"
        , "Tomas Hubocan"
        , "Dusan Svento"
        , "Lukas Tesak"
        , "Viktor Pecovsky"
        , "Matus Bero"
        , "Robert Mak"
        , "Erik Sabo"
        , "Juraj Kucka"
        , "Patrik Hrosovsky"
        , "Jan Gregus"
        , "Stanislav Sestak"
        , "Marek Hamsik"
        , "Ondrej Duda"
        , "Miroslav Stoch"
        , "Vladimir Weiss"
        , "Michal Duris"
        , "Adam Nemec"
        , "Adam Zrelak"
        ]
    , group = B
    }


germany : TeamDatum
germany =
    { team = (team "GER" "Duitsland")
    , players =
        [ "Manuel Neuer"
        , "Bernd Leno"
        , "Marc-André Ter Stegen"
        , "Jérôme Boateng"
        , "Emre Can"
        , "Jonas Hector"
        , "Benedikt Höwedes"
        , "Mats Hummels"
        , "Shkodran Mustafi"
        , "Sebastian Rudy"
        , "Antonio Rüdiger"
        , "Julian Brandt"
        , "Julian Draxler"
        , "Mario Götze"
        , "Sami Khedira"
        , "Joshua Kimmich"
        , "Toni Kroos"
        , "Thomas Muller"
        , "Mesut Özil"
        , "Bastian Schweinsteiger"
        , "Julian Weigl"
        , "Karim Bellarabi"
        , "Mario Gomez"
        , " Lukas Podolski"
        , "Marco Reus"
        , "Leroy Sané"
        , "André Schürrle"
        ]
    , group = C
    }


ukraine : TeamDatum
ukraine =
    { team = (team "UKR" "Oekraïne")
    , players =
        [ "Andriy Pyatov"
        , "Denys Boyko"
        , "Mykyta Shevchenko"
        , "Vyacheslav Shevchuk"
        , "Yaroslav Rakitskyi"
        , "Oleksandr Kucher"
        , "Yevgen Khacheridi"
        , "Artem Fedetskyi"
        , "Mykyta Kamenyuka"
        , "Bogdan Butko"
        , "Anatoliy Tymoshchuk"
        , "Taras Stepanenko"
        , "Viktor Kovalenko"
        , "Maksym Malyshev"
        , "Ruslan Rotan Dnipro Dnipropetrovsk)"
        , "Yevhen Shakhov  Dnipro Dnipropetrovsk)"
        , "Yevhen Konoplyanka"
        , "Oleg Gusev"
        , "Sergiy Rybalka"
        , "Denys Garmash"
        , "Sergiy Sydorchuk"
        , "Andriy Yarmolenko"
        , "Oleksandr Karavayev"
        , "Ivan Petryak"
        , "Oleksandr Zinchenko"
        , "Roman Zozulya"
        , "Artem Kravets"
        , "Pylyp Budkivskyi"
        ]
    , group = C
    }


poland : TeamDatum
poland =
    { team = (team "POL" "Polen")
    , players =
        [ "Artur Boruc"
        , "Lukasz Fabianski"
        , "Wojciech Szczesny"
        , "Przemysław Tyton (VfB Stuttgart)."
        , "Thiago Cionek"
        , "Pavel Dawidowicz"
        , "Kamil Glik"
        , "Artur Jedrzejczyk"
        , "Michał Pazdan"
        , "Lukasz Piszczek"
        , "Maciej Rybus"
        , "Bartosz Salamon"
        , "Jakub Wawrzyniak (Lechia Gdansk)."
        , "Jakub Blaszczykowski"
        , "Kamil Grosicki"
        , "Tomasz Jodlowiec"
        , "Bartosz Kapustka"
        , "Grzegorz Krychowiak"
        , "Karol Linetty"
        , "Krzysztof Maczynski"
        , "Slawomir Peszko"
        , "Filip Starzynski"
        , "Pavel Wszolek"
        , "Piotr Zielinski"
        , "Robert Lewandowski"
        , "Arek Milik"
        , "Artur Sobiech"
        , "Mariusz Stepinski"
        ]
    , group = C
    }


northernIreland : TeamDatum
northernIreland =
    { team = (team "NIR" "Noord-Ierland")
    , players =
        [ "Roy Carroll"
        , "Michael McGovern"
        , "Alan McManus"
        , "Craig Cathcart"
        , "Jonny Evans"
        , "Gareth McAuley"
        , "Luke McCullough"
        , "Conor McLaughlin"
        , "Aaron Hughes"
        , "Daniel Lafferty"
        , "Michael Smith"
        , "Lee Hodson"
        , "Chris Baird"
        , "Paddy McNair"
        , "Steven Davis"
        , "Oliver Norwood"
        , "Corry Evans"
        , "Jamie Ward"
        , "Stuart Dallas"
        , "Niall McGinn"
        , "Shane Ferguson"
        , "Ben Reeves"
        , "Will Grigg"
        , "Kyle Lafferty"
        , "Conor Washington"
        , "Billy McKay"
        , "Liam Boyce"
        , "Josh Magennis"
        ]
    , group = C
    }


spain : TeamDatum
spain =
    { team = (team "ESP" "Spanje")
    , players =
        [ "Iker Casillas"
        , "David De Gea"
        , "Sergio Rico"
        , "Sergio Ramos"
        , "Gerard Piqué"
        , "Dani Carvajal"
        , "Jordi Alba"
        , "Marc Bartra"
        , "Cesar Azpilicueta"
        , "Mikel San José"
        , "Juanfran"
        , "Bruno Soriano"
        , "Sergio Busquets"
        , "Koke"
        , "Thiago"
        , "Andrés Iniesta"
        , "Isco"
        , "David Silva"
        , "Pedro"
        , "Cesc Fabregas"
        , "Saúl"
        , "Aritz Aduriz"
        , "Nolito"
        , "Alvaro Morata"
        , "Lucas Vázquez"
        ]
    , group = D
    }


czechRepublic : TeamDatum
czechRepublic =
    { team = (team "CZE" "Tsjechië")
    , players =
        [ "Petr Cech"
        , "Tomas Vaclik"
        , "Tomas Koubek"
        , "Pavel Kaderabek"
        , "Theodor Gebre Selassie"
        , "Tomas Sivok"
        , "Michal Kadlec"
        , "Ondrej Zahustel"
        , "Roman Hubnik"
        , "David Limbersky"
        , "Daniel Pudil"
        , "Marek Suchy (FC Basel)."
        , "Vladimir Darida"
        , "Jaroslav Plasil"
        , "David Pavelka"
        , "Jiri Skalak"
        , "Ladislav Krejci"
        , "Borek Dockal"
        , "Tomas Rosicky"
        , "Daniel Kolar"
        , "Jan Kovarik"
        , "Lukas Marecek"
        , "Tomas Necid"
        , "Matej Vydra"
        , "David Lafata"
        , "Patrik Schick"
        , "Milan Skoda"
        ]
    , group = D
    }


turkey : TeamDatum
turkey =
    { team = (team "TUR" "Turkije")
    , players =
        [ "Ali Sasal Vural"
        , "Harun Tekin"
        , "Onur Kivrak"
        , "Volkan Babacan"
        , "Gökhan Gönül"
        , "Şener Özbayrakli"
        , "Ahmet Çalik"
        , "Çaglar Söyüncü"
        , "Hakan Balta"
        , "Mehmet Topal"
        , "Semih Kaya"
        , "Serdar Aziz"
        , "Caner Erkin"
        , "İsmail Köybasi"
        , "Emre Mor"
        , "Gökhan Töre"
        , "Volkan Sen"
        , "Yasin Öztekin"
        , "Hakan Calhanogu"
        , "Mahmut Tekdemir"
        , "Nuri Sahin"
        , "Oguzhan Özyakup"
        , "Ozan Tufan"
        , "Selçuk İnan"
        , "Alper Potuk"
        , "Arda Turan"
        , "Olcay Şahan"
        , "Burak Yilmaz"
        , "Cenk Tosun"
        , "Mevlüt Erdinç"
        , "Yunus Malli"
        ]
    , group = D
    }


croatia : TeamDatum
croatia =
    { team = (team "CRO" "Kroatië")
    , players =
        [ "Danijel Subasic"
        , "Lovre Kalinic"
        , "Ivan Vargic"
        , "Dominik Livakovic"
        , "Darijo Srna"
        , "Vedran Corluka"
        , "Domagoj Vida"
        , "Ivan Strinic"
        , "Gordon Schildenfeld"
        , "Sime Vrsaljko"
        , "Tin Jedvaj"
        , "Duje Caleta-Car"
        , "Luka Modric"
        , "Ivan Rakitic"
        , "Ivan Perisic"
        , "Mateo Kovacic"
        , "Milan Badelj"
        , "Marcelo Brozovic"
        , "Alen Halilovic"
        , "Domagoj Antolic"
        , "Marko Rog"
        , "Ante Coric"
        , "Mario Mandzukic"
        , "Nikola Kalinic"
        , "Andrej Kramaric"
        , "Marko Pjaca"
        , "Duje Cop"
        ]
    , group = D
    }


belgium : TeamDatum
belgium =
    { team = (team "BEL" "België")
    , players =
        [ "Thibaut Courtois"
        , "Simon Mignolet"
        , "Jean-François Gillet"
        , "Jan Vertonghen"
        , "Toby Alderweireld"
        , "Nicolas Lombaerts"
        , "Thomas Vermaelen"
        , "Jason Denayer"
        , "Jordan Lukaku"
        , "Björn Engels"
        , "Dedryck Boyata"
        , "Thomas Meunier"
        , "Kevin De Bruyne"
        , "Radja Nainggolan"
        , "Moussa Dembélé"
        , "Axel Witsel"
        , "Marouane Fellaini"
        , "Dries Mertens"
        , "Eden Hazard"
        , "Romelu Lukaku"
        , "Yannick Ferreira-Carrasco"
        , "Divock Origi"
        , "Michy Batshuayi"
        , "Christian Benteke"
        ]
    , group = E
    }


italy : TeamDatum
italy =
    { team = (team "ITA" "Italië")
    , players =
        [ "Gianluigi Buffon"
        , "Federico Marchetti"
        , "Salvatore Sirigu (Paris Saint Germain) "
        , "Davide Astori"
        , "Andrea Barzagli"
        , "Leonardo Bonucci"
        , "Giorgio Chiellini"
        , "Matteo Darmian"
        , "Mattia De Sciglio"
        , "Angelo Ogbonna"
        , "Daniele Rugani"
        , "Davide Zappacosta"
        , "Marco Benassi"
        , "Federico Bernardeschi"
        , "Giacomo Bonaventura"
        , "Antonio Candreva"
        , "Daniele De Rossi"
        , "Alessandro Florenzi"
        , "Emanuele Giaccherini"
        , "Jorge Luiz Jorginho"
        , "Riccardo Montolivo"
        , "Thiago Motta"
        , "Marco Parolo"
        , "Stefano Sturaro"
        , "Eder"
        , "Ciro Immobile"
        , "Stephan El Shaarawy"
        , "Lorenzo Insigne"
        , "Graziano Pellè"
        , "Simone Zaza"
        ]
    , group = E
    }


ireland : TeamDatum
ireland =
    { team = (team "IRL" "Ierland")
    , players =
        [ "Shay Given"
        , "Darren Randolph"
        , "David Forde"
        , "Keiren Westwood"
        , "Seamus Coleman"
        , "Cyrus Christie"
        , "Paul McShane"
        , "Ciaran Clark"
        , "Richard Keogh"
        , "John O'Shea"
        , "Alex Pearce"
        , "Shane Duffy"
        , "Marc Wilson"
        , "Stephen Ward"
        , "Aiden McGeady"
        , "James McClean"
        , "Glenn Whelan"
        , "James McCarthy"
        , "Jeff Hendrick"
        , "David Meyler"
        , "Stephen Quinn"
        , "Darron Gibson"
        , "Harry Arter"
        , "Wes Hoolahan"
        , "Eunan O'Kane"
        , "Anthony Pilkington"
        , "Robbie Brady"
        , "Jonathan Walters"
        , "Jonathan Hayes"
        , "Callum O'Dowda"
        , "Robbie Keane"
        , "Shane Long"
        , "David McGoldrick"
        , "Kevin Doyle"
        , "Daryl Murphy"
        ]
    , group = E
    }


sweden : TeamDatum
sweden =
    { team = (team "SWE" "Zweden")
    , players =
        [ "Patrick Carlgren"
        , "Andreas Isaksson"
        , "Robin Olsen"
        , "Ludwig Augustinsson"
        , "Andreas Granqvist"
        , "Pontus Jansson"
        , "Erik Johansson"
        , "Mikael Lustig"
        , "Victor Lindelöf"
        , "Martin Olsson"
        , "Jimmy Durmaz"
        , "Albin Ekdal"
        , "Emil Forsberg"
        , "Oscar Hiljemark"
        , "Kim Källström"
        , "Sebastian Larsson"
        , "Oscar Lewicki"
        , "Pontus Wernbloom"
        , "Erkan Zengin"
        , "Marcus Berg"
        , "John Guidetti"
        , "Zlatan Ibrahimovic"
        , "Emir Kujovic"
        ]
    , group = E
    }


portugal : TeamDatum
portugal =
    { team = (team "POR" "Portugal")
    , players =
        [ "Rui Patricio"
        , "Anthony Lopes"
        , "Eduardo"
        , "Vieirinha"
        , "Raphael Guerreiro"
        , "Cédric Soares"
        , "Eliseu"
        , "Bruno Alves"
        , "José Fonte"
        , "Ricardo Carvalho"
        , "Pepe"
        , "William Carvalho"
        , "Danilo Pereira"
        , "João Moutinho"
        , "Adrien Silva"
        , "João Mario"
        , "André Gomes"
        , "Renato Sanches"
        , "Cristiano Ronaldo"
        , "Eder"
        , "Nani"
        , "Ricardo Quaresma"
        , "Rafa Silva"
        ]
    , group = F
    }


iceland : TeamDatum
iceland =
    { team = (team "ISL" "IJsland")
    , players =
        [ "Hannes Halldorsson"
        , "Ögmundur Kristinsson"
        , "Ingvar Jonsson"
        , "Birkir Mar Saevarsson"
        , "Ragnar Sigurdsson"
        , "Kari Arnason"
        , "Ari Freyr Skulason"
        , "Haukur Heidar Hauksson"
        , "Sverrir Ingi Ingason"
        , "Hördur Björgvin Magnusson"
        , "Hjörtur Hermannsson"
        , "Aron Einar Gunnarsson"
        , "Emil Hallfredsson"
        , "Birkir Bjarnason"
        , "Johann Berg Gudmundsson"
        , "Gylfi Sigurdsson"
        , "Theodor Elmar Bjarnason"
        , "Runar Mar Sigurjonsson"
        , "Arnor Ingvi Traustason"
        , "Eidur Gudjohnsen"
        , "Kolbeinn Sigthorsson"
        , "Alfred Finnbogason"
        , "Jon Dadi Bödvarsson"
        ]
    , group = F
    }


austria : TeamDatum
austria =
    { team = (team "AUT" "Oostenrijk")
    , players =
        [ "Robert Almer"
        , "Heinz Lindner"
        , "Ramazan Ozcan"
        , "Aleksandar Dragovic"
        , "Christian Fuchs"
        , "Gyorgy Garics"
        , "Martin Hinteregger"
        , "Florian Klein"
        , "Sebastian Prodl"
        , "Markus Suttner"
        , "Kevin Wimmer"
        , "David Alaba"
        , "Marko Arnautovic"
        , "Julian Baumgartlinger"
        , "Martin Harnik"
        , "Stefan Ilsanker"
        , "Jakob Jantscher"
        , "Zlatko Junuzovic"
        , "Marcel Sabitzer"
        , "Alessandro Schopf"
        , "Valentino Lazaro"
        , "Lukas Hinterseer"
        , "Rubin Okotie"
        , "Marc Janko"
        ]
    , group = F
    }


hungary : TeamDatum
hungary =
    { team = (team "HUN" "Hongarije")
    , players =
        [ "Gábor Király"
        , "Balázs Megyeri"
        , "Dénes Dibusz"
        , "Péter Gulácsi"
        , "Roland Juhász"
        , "Tamás Kádár"
        , "Mihály Korhut"
        , "Richárd Guzmics"
        , "Attila Fiola"
        , "Ádám Lang"
        , "Gergö Kocsis"
        , "Barnabás Bese"
        , "Zsolt Korcsmár"
        , "Gergö Lovrencsis"
        , "Máté Vida"
        , "Zoltán Gera"
        , "Balázs Dzsudzsák"
        , "Ákos Elek"
        , "Ádám Pintér"
        , "Zoltán Stieber"
        , "Ádám Gyurcsó"
        , "Ádám Nagy"
        , "László Keinheisler"
        , "Roland Sallai"
        , "Tamás Priskin"
        , "Ádám Szalai"
        , "Krisztián Németh"
        , "Nemanja Nikolic"
        , "Dániel Böde"
        , "Lászlo Lencse"
        ]
    , group = F
    }
