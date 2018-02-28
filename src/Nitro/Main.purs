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
  , gameover :: Int
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
  
-- renderCar :: forall i p .State -> PrestoDOM i p
-- renderCar state =
--   relativeLayout
--     [ id_ "carstrack"
--     ,height Match_Parent
--     , width ( V 300)
--     ]
--     [ imageView
--         [ 
--          imageUrl "car"
--         , height (V 50)
--         , width (V 100)
--         , rotation "90"
--         , margin $ (show state.car1X) <> "," <> (show state.car1Y) <> ",0,0"
--         ]
--     , imageView
--         [ 
--          imageUrl "car4"
--         , height (V 50)
--         , width (V 100)
--         , rotation "90"
--         , margin $ (show state.car2X) <> "," <> (show state.car2Y ) <> ",0,0"
--         ]
--     , imageView
--         [ 
--          imageUrl "driver"
--         , height (V 50)
--         , width (V 100)
--         , rotation "90"
--         , margin $ (show state.car3X) <> "," <> (show state.car3Y ) <> ",0,0"
--         ]
--     ]

-- renderCar' :: forall i p .State -> PrestoDOM i p
-- renderCar' state =
--   relativeLayout
--     [ id_ "carstrack2"
--     ,height Match_Parent
--     , width ( V 300)
--     , margin "0,0,0,0"
--     ]
--     [ imageView
--         [ 
--          imageUrl "car"
--         , height (V 50)
--         , width (V 100)
--         , rotation "90"
--         , margin $ (show state.car1X) <> "," <> (show state.car1Y) <> ",0,0"
--         ]
--     , imageView
--         [ 
--          imageUrl "car4"
--         , height (V 50)
--         , width (V 100)
--         , rotation "90"
--         , margin $ (show state.car2X) <> "," <> (show state.car2Y ) <> ",0,0"
--         ]
--     , imageView
--         [ 
--          imageUrl "driver"
--         , height (V 50)
--         , width (V 100)
--         , rotation "90"
--         , margin $ (show state.car3X) <> "," <> (show state.car3Y ) <> ",0,0"
--         ]
--     ]

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
    [ id_  "scorecard"
    , height (V 50)
    , width (V 200)
    , background "#282B2A"
    , orientation "horizontal"
    , margin "320,10,0,0"
    , padding "10,10,10,10"
    ]
    [ textView
        [ id_ "distance"
        , text "distance"
        , height (V 24)
        , width (V 100)
        , color "white"
        ]
    , textView
        [ id_ "score"
        , text "score"
        , height (V 24)
        , width ( V 25)
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
        , relativeLayout
            [ id_ "otherCars"
            , width Match_Parent
            , height Match_Parent
            ]
            (map renderCar state.cars)
        , renderScore state
        ]
    ]

main :: forall e. Eff ( dom :: DOM, frp :: FRP, console :: CONSOLE | e) Unit
main = do
  { stateBeh , updateState} <- render renderGame initialState
  updateState
        (eval <$> key 37 <*> key 39 <*> stateBeh)
        animationFrame *>
    pure unit

eval :: Boolean -> Boolean -> State -> State
eval keyLeft keyRight state = ((movePlayer keyLeft keyRight) >>> otherCarsUpdate) state

movePlayer :: Boolean -> Boolean -> State -> State
movePlayer true false state = state { player = state.player { x = (max 0 (min (state.player.x - 4) 200)) } }
movePlayer false true state = state { player = state.player { x = (max 0 (min (state.player.x + 4) 200)) } }
movePlayer _ _ state = state

otherCarsUpdate :: State -> State
otherCarsUpdate state = moveCars >>> checkCollisions $ state

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
                , cars: cars `snoc` (checkResetCarPosition car { y = car.y + car.dy })
                }
                (currentIndex + 1)

checkCollisions :: State -> State
checkCollisions state = state
  
checkResetCarPosition :: Car -> Car
checkResetCarPosition car =
  if car.y >= 750 then
    car { y = -100 }
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
        [ { x: 10
          , y: -100
          , dy: 5
          , image: "car4"
          , rotation : "90"
          }
        , { x: 100
          , y: -400
          , dy: 5
          , image: "car5"
          , rotation : "90"
          }
        , { x: 190
          , y: -400
          , dy: 5
          , image: "car7"
          , rotation : "90"
          }
        , { x: 10
          , y: -800
          , dy: 5
          , image: "car1"
          , rotation : "90"
          }
        , { x: 100
          , y: -900
          , dy: 5
          , image: "car3"
          , rotation : "90"
          }
        , { x: 190
          , y: -1280
          , dy: 5
          , image: "car2"
          , rotation : "90"
          }
        ]
  , gameover : 0
  }

type Car = 
  { x :: Int
  , y :: Int
  , dy :: Int
  , image :: String
  , rotation :: String
  }
  