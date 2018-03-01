module Nitro.Main where
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM
import FRP
import FRP.Behavior.Keyboard
import FRP.Behavior.Time
import Prelude

import Control.Monad.Eff.Console (log)
import Data.Array (snoc, (!!))
import Data.Maybe (Maybe(..))
import FRP.Event.Time (animationFrame)
import PrestoDOM.Core (PrestoDOM)
import PrestoDOM.Elements (imageView, linearLayout, relativeLayout, textView)
import PrestoDOM.Properties (background, backgroundColor, color, gravity, height, id_, imageUrl, margin, orientation, padding, rotation, stroke, text, width)
import PrestoDOM.Types (Length(..))
import PrestoDOM.Util (render, updateState)

type State =
  { distance :: Int
  , score :: Int
  , player :: Car
  , cars :: Array Car
  , currentScreen :: GameScreen
  }

renderCar :: forall i p. Car -> PrestoDOM i p
renderCar car =
  imageView
    [ id_ car.image
    , height (V 50)
    , width (V 100)
    , margin $ (show car.x) <> "," <> (show car.y) <> ",0,0"
    , rotation car.rotation
    , imageUrl car.image
    ]

renderTrack :: forall i p. State -> PrestoDOM i p
renderTrack state = 
  relativeLayout
    [ id_  "track"
    , height Match_Parent
    , width (V 300)
    , background "#282B2A"
    , gravity "center"
    ]
    [  relativeLayout
        [ id_ "div3"
        , height Match_Parent
        , width ( V 2)
        , background "#efe007"
        , margin "100,0,0,0"
        ]
        []
    , relativeLayout
        [ id_ "div4"
        , height Match_Parent
        , width ( V 2)
        , background "#efe007"
        , margin "190,0,0,0"
        ]
        []
    , relativeLayout
        [ id_ "div1"
        , height Match_Parent
        , width (V 10)
        , background "#ffff"
        , margin "0,0,0,0"
        ]
        []
    , relativeLayout
        [ id_ "div2"
        , height Match_Parent
        , width ( V 10)
        , background "#ffff"
        , margin "300,0,0,0"
        ]
        []
    
    ]

renderScore :: forall i p .State -> PrestoDOM i p
renderScore state = 
    linearLayout
        [ id_ "rightPane"
        , height (V 200)
        , width (V 200)
        , margin "320,10,0,0"
        , padding "10,10,10,10"
        , orientation "vertical"
        ]
        [ linearLayout
            [ id_  "scorecard"
            , height (V 50)
            , width Match_Parent
            , background "#282B2A"
            , orientation "horizontal"
            , gravity "center"
            , padding "10,0,0,0"
            ]
            [ textView
                [ id_ "score"
                , text $ "Distance: " <> (show state.score)
                , height (V 20)
                , width Match_Parent
                , color "white"
                ]
            ]
        , renderinstruction state
        ]

renderinstruction :: forall i p .State -> PrestoDOM i p
renderinstruction state = 
  linearLayout
    [ id_  "instructions"
    , height (V 50)
    , width Match_Parent
    , orientation "vertical"
    , padding "10,10,10,10"
    ]
    [ textView
        [ id_ "startgame"
        , text "Press Space to start"
        , height (V 24)
        , width Match_Parent
        , color "white"
        ]
      , textView
        [ id_ "restartgame"
        , text "Refresh page to restart game"
        , height (V 24)
        , width Match_Parent
        , color "white"
        ]
    ]

renderGame :: forall i p . State -> PrestoDOM i p
renderGame state =
  linearLayout
    [ id_ "playscreen"
    , height Match_Parent
    , width Match_Parent
    , gravity "center"
    , background "#478260"
    ]
    [ relativeLayout
        [ id_ "gameContainer"
        , height Match_Parent
        , width (V 300)
        ]
        [ renderTrack state
        , renderCar state.player
        , if state.currentScreen == PlayScreen then
            relativeLayout
                [ id_ "otherCars"
                , width Match_Parent
                , height Match_Parent
                ]
                (map renderCar state.cars)
            else relativeLayout
                    [ id_ "otherCars"
                    ]
                    []
        , renderScore state
        ]
    ]

main :: forall e. Eff ( dom :: DOM, frp :: FRP, console :: CONSOLE | e) Unit
main = do
  { stateBeh , updateState} <- render renderGame initialState
  updateState
        (eval <$> key 37 <*> key 39 <*> key 32 <*> stateBeh)
        animationFrame *>
    pure unit

eval :: Boolean -> Boolean -> Boolean -> State -> State
eval keyLeft keyRight keySpace state = 
    case state.currentScreen of
        PlayScreen -> ((movePlayer keyLeft keyRight) >>> otherCarsUpdate >>> updateScore) state
        GameOverScreen -> if keySpace then initialState { currentScreen = PlayScreen } else state

movePlayer :: Boolean -> Boolean -> State -> State
movePlayer true false state = state { player = state.player { x = (max 0 (min (state.player.x - 7) 200)) } }
movePlayer false true state = state { player = state.player { x = (max 0 (min (state.player.x + 7) 200)) } }
movePlayer _ _ state = state

otherCarsUpdate :: State -> State
otherCarsUpdate state = moveCars >>> checkCollisions $ state

updateScore :: State -> State
updateScore state = state { score = state.score + 1 }

moveCars :: State -> State
moveCars state = do
    let { state, cars } = moveCarHelper { state: state, cars: [] } 0
    state { cars = cars }

moveCarHelper :: { state :: State, cars :: Array Car } -> Int -> { state :: State, cars :: Array Car }
moveCarHelper { state, cars } currentIndex =
    case state.cars !! currentIndex of
        Nothing -> { state, cars }
        Just car ->
            moveCarHelper
                { state: state
                , cars: cars `snoc` (checkResetCarPosition car { y = car.y + car.dy + (min 10 (state.score/100) ) })
                }
                (currentIndex + 1)

checkCollisions :: State -> State
checkCollisions state = checkCollisionHelper state 0

intersects :: Car -> Car -> Boolean
intersects a b = not (a.x > b.x + 50 || b.x > a.x + 50) &&
                 not (a.y > b.y + 100 || b.y > a.y + 100)


checkCollisionHelper :: State -> Int -> State
checkCollisionHelper state currentIndex =
    case state.cars !! currentIndex of
        Nothing -> state
        Just car ->
            if intersects car state.player then
                state { currentScreen = GameOverScreen }
            else 
                 checkCollisionHelper state (currentIndex + 1)

  
checkResetCarPosition :: Car -> Car
checkResetCarPosition car =
  if car.y >= 760 then
    car { y = -600 }
  else car
initialState :: State
initialState =
  { distance: 0
  , score: 0
  , player:
        { x: 100
        , y: 700
        , dy: 0
        , image: "player"
        , rotation : "-90"
        }
  , cars:
        [ { x: 100
          , y: -300
          , dy: 5
          , image: "blue"
          , rotation : "90"
          }
        , { x: 10
          , y: -600
          , dy: 5
          , image: "blue2"
          , rotation : "90"
          }
        , { x: 200
          , y: -600
          , dy: 5
          , image: "yellow"
          , rotation : "90"
          }
        , { x: 100
          , y: -900
          , dy: 5
          , image: "purple"
          , rotation : "90"
          }
        , { x: 10
          , y: -1200
          , dy: 5
          , image: "green"
          , rotation : "90"
          }
        , { x: 200
          , y: -1200
          , dy: 5
          , image: "yellow1"
          , rotation : "90"
          }
        , { x: 100
          , y: -1500
          , dy: 5
          , image: "red1"
          , rotation : "90"
          }
        , { x: 200
          , y: -1500
          , dy: 5
          , image: "blue1"
          , rotation : "90"
          }
        ]
  , currentScreen : GameOverScreen
  }

type Car = 
  { x :: Int
  , y :: Int
  , dy :: Int
  , image :: String
  , rotation :: String
  }
  
data GameScreen = PlayScreen | GameOverScreen

instance eqGameScreen :: Eq GameScreen where
  eq PlayScreen PlayScreen = true
  eq GameOverScreen GameOverScreen = true
  eq _ _ = false