import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html exposing (Html, button, div, text)
import Json.Decode as Decode
import Set exposing (Set)

expectedFramerate = 16.66666
friction = 0.85
minSpeed = 0.001
maxSpeed = 10

type alias Model =
    { xVelocity : Float, xPosition : Float, keysDown : Set String }


initialModel : Model
initialModel =
    { xVelocity = 0, xPosition = 0, keysDown = Set.empty }

type Msg
  = KeyChange Bool String
  | Tick Float

main =
  Browser.document {
    init = \() -> (initialModel, Cmd.none),
    update = update,
    view = view,
    subscriptions = subscriptions
  }

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    KeyChange isPressed key ->
      (handleKeyChange isPressed key model, Cmd.none)
    Tick delta ->
      (handleTick delta model, Cmd.none)

view model = {
  title = "Hello World",
  body =
    [
      div []
        [ text (String.fromFloat model.xVelocity), text (String.fromFloat model.xPosition) ]
    ]
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ onKeyDown (Decode.map (KeyChange True) keyDecoder),
      onKeyUp (Decode.map (KeyChange False) keyDecoder),
      onAnimationFrameDelta (Tick)
    ]

handleKeyChange : Bool -> String -> Model -> Model
handleKeyChange isPressed key model =
  { model | keysDown = (if isPressed then Set.insert else Set.remove) key model.keysDown }

keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

handleTick : Float -> Model -> Model
handleTick delta model =
  let xAcc = if Set.member "ArrowLeft" model.keysDown && Set.member "ArrowRight" model.keysDown then 0
             else if Set.member "ArrowLeft" model.keysDown then -1
             else if Set.member "ArrowRight" model.keysDown then 1
             else 0
  in
    let xAdjustedAcc = xAcc  * (delta / expectedFramerate)
    in
    { model | xVelocity = (capSpeed (friction * model.xVelocity)) + xAdjustedAcc
            , xPosition = model.xPosition + model.xVelocity + xAdjustedAcc }

capSpeed : Float -> Float
capSpeed speed =
  if abs speed < minSpeed then 0
  else if speed > 0 then clamp minSpeed 10 speed
  else clamp -maxSpeed -minSpeed speed
