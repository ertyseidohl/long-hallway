import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Set exposing (Set)

expectedFramerate = 16.66666
friction = 0.85
minSpeed = 0.001
maxSpeed = 10
width = 200
height = 800

doorWidth : Int
doorWidth =
  (floor (width * 0.12))
doorHeight : Int
doorHeight =
  (floor (height * 0.3))


type alias Model =
    { xVelocity : Float, xPosition : Float, doorPosition : Float, keysDown : Set String }


initialModel : Model
initialModel =
    { xVelocity = 0.0, xPosition = 0.0, doorPosition = 0.0, keysDown = Set.empty }

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
      div [
        style "position" "absolute",
        style "top" "620px"
      ] [
        --text (String.fromFloat model.xVelocity),
        text (String.fromFloat model.xPosition)
      ],
      div [ -- main container
          style "position" "relative",
          style "width" ((String.fromInt width) ++ "px"),
          style "height" ((String.fromInt height) ++ "px"),
          style "overflow" "hidden",
          style "background-color" "cornflowerblue"
        ] [
        div [ -- background
          style "background-image" "url(../assets/background.placeholder.png)",
          style "background-repeat" "repeat-x",
          style "background-size" "auto 100%",
          style "width" "100%",
          style "height" "100%",
          style "background-position" ((String.fromInt (floor -model.xPosition)) ++ "px"),
          style "position" "absolute",
          style "top" "0"
        ]
          [ ],
        div [ -- door
          style "background-image" "url(../assets/door.placeholder.png)",
          style "background-repeat" "no-repeat",
          style "background-size" "100% 100%",
          style "width" ((String.fromInt doorWidth) ++ "px"),
          style "height" ((String.fromInt doorHeight) ++ "px"),
          style "position" "absolute",
          style "top" (String.fromInt (height - doorHeight) ++ "px"),
          style "left" ((String.fromInt (floor -model.xPosition)) ++ "px")
        ] []
      ]
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
  else if speed > 0 then clamp minSpeed maxSpeed speed
  else clamp -maxSpeed -minSpeed speed
