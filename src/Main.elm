module Main exposing (..)

import List exposing (..)
import Dict exposing (Dict)

import Browser
import Browser.Navigation as Nav

import Html exposing (Html, text, br, hr, input, button, p, img, li, ol, ul, h1, h2)
import Html.Attributes exposing (style, value, placeholder, disabled, src)
import Html.Events exposing (onClick, onInput)

import Url

import Json.Decode as D


-- MAIN

main : Program D.Value Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type Role
  = GM
  | PL Player
  | Guest
  | InvalidToken

tokenToRole : Scenario -> String -> Role
tokenToRole scenario token =
  if scenario.gmToken == token then GM
  else case filter (\p -> p.token == token) scenario.players |> head of
    Just p  -> PL p
    Nothing -> InvalidToken

type alias Model =
  { role : Role
  , token : String
  , passOfEvidences : Dict String String
  , scenario : Result D.Error Scenario
  }

init : D.Value -> ( Model, Cmd Msg )
init value =
  (Model Guest "" Dict.empty (D.decodeValue decoderOfScenario value), Cmd.none)


-- JSON data

type alias Scenario =
  { title     : String
  , rules     : List String
  , gmToken   : String
  , players   : List Player
  , evidences : List Evidence
  }

decoderOfScenario : D.Decoder Scenario
decoderOfScenario =
  D.map5 Scenario
    (D.field "title" D.string)
    (D.field "rules" (D.list D.string))
    (D.field "gmToken" D.string)
    (D.field "players" (D.list decoderOfPlayer))
    (D.field "evidences" (D.list decoderOfEvidence))

type alias Player =
  { name      : String
  , token     : String
  , role      : String
  , timelines : List String
  , goal      : String
  , points    : List String
  , evidences : List String
  }

decoderOfPlayer : D.Decoder Player
decoderOfPlayer =
  D.map7 Player
    (D.field "name" D.string)
    (D.field "token" D.string)
    (D.field "role" D.string)
    (D.field "timelines" (D.list D.string))
    (D.field "goal" D.string)
    (D.field "points" (D.list D.string))
    (D.field "evidences" (D.list D.string))

type alias Evidence =
  { name     : String
  , password : String
  , icon     : Maybe Url.Url
  , contents : String
  }

decoderOfEvidence : D.Decoder Evidence
decoderOfEvidence =
  D.map4 Evidence
    (D.field "name" D.string)
    (D.field "password" D.string)
    (D.field "icon" D.string |> D.map Url.fromString)
    (D.field "contents" D.string)


-- UPDATE

type Msg
  = Enter
  | InputUserToken String
  | InputEvidenceToken String String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Enter ->
      case model.scenario of
        Ok scenario -> ({ model | role = tokenToRole scenario model.token }, Cmd.none)
        Err _ -> (model, Cmd.none)

    InputUserToken token -> ({ model | token = token }, Cmd.none)

    InputEvidenceToken evidenceName token ->
      ({ model | passOfEvidences = Dict.insert evidenceName token model.passOfEvidences }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Closed Circle"
  , body = case model.scenario of
      Err error -> errorBody (D.errorToString error)
      Ok scenario ->
        case model.role of
          InvalidToken -> invalidTokenBody model.token
          Guest -> guestBody model.token
          GM    -> gmBody scenario
          PL pl -> plBody pl scenario model.passOfEvidences
  }

errorBody : String -> List (Html Msg)
errorBody msg =
  [ text "Error!"
  , br [] []
  , text msg
  ]

invalidTokenBody : String -> List (Html Msg)
invalidTokenBody token = guestBody token ++ [ br [] [], p [ style "color" "#F00" ] [ text "InvalidToken." ] ]

guestBody : String -> List (Html Msg)
guestBody token =
  [ text "Input Token: "
  , input [ placeholder "Please input here", value token, onInput InputUserToken ] []
  , button [ onClick Enter ] [ text "Enter" ]
  ]

gmBody : Scenario -> List (Html Msg)
gmBody scenario =
  [ text "Game Master Mode"
  , br [] []
  ] ++ htmlOfEvidences (map (\evi -> (evi.name, evi.password)) scenario.evidences |> Dict.fromList) scenario.evidences
    ++ (concat <| intersperse [br [] [], hr [] [], br [] []] <| map htmlOfPlayer scenario.players)

plBody : Player -> Scenario -> Dict String String -> List (Html Msg)
plBody pl scenario passOfEvidences =
  [ h1 [] [ text ("Player Mode: " ++ pl.name) ]
  , br [] []
  ] ++
  [ hr [] []
  , h1 [] [ text "Player Info: " ]
  , br [] []
  ] ++ htmlOfPlayer pl ++
  [ hr [] []
  , h1 [] [ text ("Evidences:") ]
  , br [] []
  ] ++ htmlOfEvidences passOfEvidences scenario.evidences

htmlOfEvidences : Dict String String -> List Evidence -> List (Html Msg)
htmlOfEvidences dict =
  concat << intersperse [br [] [], hr [] [], br [] []]
  << map (\evi -> htmlOfEvidence (Dict.get evi.name dict |> Maybe.map (\p -> p == evi.password) |> Maybe.withDefault False) evi)

htmlOfEvidence : Bool -> Evidence -> List (Html Msg)
htmlOfEvidence unlocked evidence =
  [ img [ style "width" "40px", style "height" "40px", src <| Maybe.withDefault "" (Maybe.map Url.toString evidence.icon) ] []
  , p [] [ text evidence.name ]
  , input [ placeholder (if unlocked then "UNLOCKED!" else "LOCKED"), disabled unlocked, onInput (InputEvidenceToken evidence.name) ] []
  ] ++ if unlocked then [ br [] [], p [] [ text evidence.contents ] ] else []

htmlOfPlayer : Player -> List (Html Msg)
htmlOfPlayer pl =
  [ h2 [] [ text ("Name: " ++ pl.name) ]
  , br [] []
  , h2 [] [ text "Role" ]
  , p  [] [ text pl.role ]
  , br [] []
  , h2 [] [ text "Timelines"]
  , ul [] <| map (\str -> li [] [ text str ]) pl.timelines
  , br [] []
  , h2 [] [ text "Goals" ]
  , p  [] [ text pl.goal ]
  , br [] []
  , h2 [] [ text "Points" ]
  , ol [] <| map (\str -> li [] [ text str ]) pl.points
  , br [] []
  , h2 [] [ text "Known Evidences" ]
  , ol [] <| map (\str -> li [] [ text str ]) pl.evidences
  ]

