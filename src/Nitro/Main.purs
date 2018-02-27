module Nitro.Main where
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM
import FRP
import FRP.Behavior.Keyboard
import FRP.Behavior.Time
import Prelude

import Control.Monad.Eff.Console (log)
import FRP.Event.Time (animationFrame)
import PrestoDOM.Core (PrestoDOM)
import PrestoDOM.Elements (imageView, linearLayout, relativeLayout, textView)
import PrestoDOM.Properties (background, backgroundColor, color, gravity, height, id_, imageUrl, margin, orientation, padding, rotation, stroke, text, width)
import PrestoDOM.Types (Length(..))
import PrestoDOM.Util (render, updateState)

type State =
  { distance :: Int
  , score :: Int
  , carX :: Int
  , carY :: Int
  , car1X :: Int
  , car1Y :: Int
  , car2X :: Int
  , car2Y :: Int
  , car3X :: Int
  , car3Y :: Int
  , car4X :: Int
  , car4Y :: Int
  , car5X :: Int
  , car5Y :: Int
  , car6X :: Int
  , car6Y :: Int
  }
renderCar :: forall i p .State -> PrestoDOM i p
renderCar state =
  relativeLayout
    [ id_ "carstrack"
    ,height Match_Parent
    , width ( V 300)
    ]
    [ imageView
        [ 
         imageUrl "car"
        , height (V 50)
        , width (V 100)
        , rotation "90"
        , margin $ (show state.car1X) <> "," <> (show state.car1Y) <> ",0,0"
        ]
    , imageView
        [ 
         imageUrl "car4"
        , height (V 50)
        , width (V 100)
        , rotation "90"
        , margin $ (show state.car2X) <> "," <> (show state.car2Y ) <> ",0,0"
        ]
    , imageView
        [ 
         imageUrl "driver"
        , height (V 50)
        , width (V 100)
        , rotation "90"
        , margin $ (show state.car3X) <> "," <> (show state.car3Y ) <> ",0,0"
        ]
    ]

renderCar' :: forall i p .State -> PrestoDOM i p
renderCar' state =
  relativeLayout
    [ id_ "carstrack2"
    ,height Match_Parent
    , width ( V 300)
    , margin "0,0,0,0"
    ]
    [ imageView
        [ 
         imageUrl "car"
        , height (V 50)
        , width (V 100)
        , rotation "90"
        , margin $ (show state.car1X) <> "," <> (show state.car1Y) <> ",0,0"
        ]
    , imageView
        [ 
         imageUrl "car4"
        , height (V 50)
        , width (V 100)
        , rotation "90"
        , margin $ (show state.car2X) <> "," <> (show state.car2Y ) <> ",0,0"
        ]
    , imageView
        [ 
         imageUrl "driver"
        , height (V 50)
        , width (V 100)
        , rotation "90"
        , margin $ (show state.car3X) <> "," <> (show state.car3Y ) <> ",0,0"
        ]
    ]

renderTrack :: forall i p. State -> PrestoDOM i p
renderTrack state = 
  relativeLayout
    [ id_  "track"
    , height Match_Parent
    , width (V 300)
    , background "#282B2A"
    ]
    [ textView
        [ id_ "text"
        , text "ok"
        ]
    , relativeLayout
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
    , imageView
        [ id_ "car"
        , imageUrl "driver"
        , rotation "-90"
        , height (V 50)
        , width (V 100)
        , margin $ (show state.carX) <> "," <> (show state.carY) <> ",0,0"
        ]
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
  relativeLayout
    [ id_ "playscreen"
    , height Match_Parent
    , width Match_Parent
    , gravity "center"
    , background "#478260"
    ]
    [ renderTrack state
    , renderCar state
    -- , renderCar' state
    , renderScore state
    ]

main :: forall e. Eff ( dom :: DOM, frp :: FRP, console :: CONSOLE | e) Unit
main = do
  { stateBeh , updateState} <- render renderGame initialState

  _ <- updateState
    (eval <$> key 37 <*> key 39 <*> stateBeh)
    animationFrame
  _ <- updateState
    (eval1 <$> millisSinceEpoch <*> stateBeh)
    animationFrame
  _ <- updateState
    (eval2 <$> millisSinceEpoch <*> stateBeh)
    animationFrame
  _ <- updateState
    (eval3 <$> millisSinceEpoch <*> stateBeh)
    animationFrame

  log "game loaded"
  
eval :: Boolean -> Boolean -> State -> State
eval true true state = state
eval true false state = moveLeft state
eval false true state = moveRight state
eval false false state = state

-- renderCars :: forall i p . State -> PrestoDOM i p
-- renderCars sate = 
eval1 :: Number -> State -> State
eval1 _ state = moveCar1 state

eval2 :: Number -> State -> State
eval2 _ state = moveCar2 state

eval3 :: Number -> State -> State
eval3 _ state = moveCar3 state


moveCar1 :: State -> State
moveCar1 state = state { car1Y = if state.car1Y == 750 then state.car1Y-1000 else state.car1Y + 5}

moveCar3 :: State -> State
moveCar3 state = state { car3Y = if state.car3Y == 750 then state.car3Y-1000 else state.car3Y + 5}

moveCar2 :: State -> State
moveCar2 state = state { car2Y = if state.car2Y == 750 then state.car2Y-1000 else state.car2Y + 5}

moveLeft :: State -> State
moveLeft state = state { carX = if state.carX == -12 then state.carX else state.carX -7  }

moveRight :: State -> State
moveRight state = state { carX = if state.carX == 219 then state.carX else state.carX +7  }

initialState :: State
initialState =
  { distance: 0
  , score: 0
  , carX: 100
  , carY: 700
  , car1X : 0
  , car1Y : 0
  , car2X : 100
  , car2Y : 200
  , car3X : 200
  , car3Y : 0
  , car4X : 0
  , car4Y : 0
  , car5X : 100
  , car5Y : 200
  , car6X : 200
  , car6Y : 0
  }
  