import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Html exposing (Html, button, div, text)
import Json.Decode as Decode

type alias Model =
    { count : Int, isLeftKeyDown : Bool, isRightKeyDown : Bool }


initialModel : Model
initialModel =
    { count = 0, isLeftKeyDown = False, isRightKeyDown = False }

main =
  Browser.document {
    init = \() -> (initialModel, Cmd.none),
    update = \msg model -> (update msg model, Cmd.none),
    view = view,
    subscriptions = subscriptions
  }

eventTypeMap : KeyEventType -> Bool
eventTypeMap eventType =
  case eventType of
    KeyUp -> False
    KeyDown -> True

update : KeyEvent -> Model -> Model
update msg model =
  case msg of
    ( eventType, Left ) ->
      { model | isLeftKeyDown = (eventTypeMap eventType) }

    ( eventType, Right ) ->
      { model | isRightKeyDown = (eventTypeMap eventType) }

    ( _, Other )  ->
      model

view model = {
  title = "Hello World",
  body =
    [
      div []
        [ text (String.fromInt model.count) ]
    ]
  }

subscriptions : Model -> Sub KeyEvent
subscriptions _ = Sub.batch [
    onKeyDown Sub.map (keyDecoder |> \dir -> (KeyDown, dir)),
    onKeyUp Sub.map (keyDecoder |> \dir -> (KeyUp, dir))
  ]

type KeyEventType
  = KeyUp
  | KeyDown

type Direction
  = Left
  | Right
  | Other

type alias KeyEvent = ( KeyEventType,  Direction )

keyDecoder : Decode.Decoder Direction
keyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Direction
toDirection string =
  case string of
    "ArrowLeft" ->
      Left

    "ArrowRight" ->
      Right

    _ ->
      Other
