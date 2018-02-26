module Nitro.Main where
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM
import FRP
import FRP.Behavior.Keyboard
import Prelude

import Control.Monad.Eff.Console (log)
import FRP.Event.Time (animationFrame)
import PrestoDOM.Core (PrestoDOM)
import PrestoDOM.Elements (imageView, linearLayout, relativeLayout, textView)
import PrestoDOM.Properties (background, backgroundColor, gravity, height, id_, imageUrl, margin, rotation, stroke, text, width)
import PrestoDOM.Types (Length(..))
import PrestoDOM.Util (render, updateState)

type State =
  { distance :: Int
  , score :: Int
  , carX :: Int
  , carY :: Int
  }

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

renderGame :: forall i p . State -> PrestoDOM i p
renderGame state =
  linearLayout
    [ id_ "playscreen"
    , height Match_Parent
    , width Match_Parent
    , gravity "center"
    , background "#478260"
    ]
    [ renderTrack state
    ]

main :: forall e. Eff ( dom :: DOM, frp :: FRP, console :: CONSOLE | e) Unit
main = do
  { stateBeh , updateState} <- render renderGame initialState

  _ <- updateState
    (eval <$> key 37 <*> key 39 <*> stateBeh)
    animationFrame

  log "game loaded"
  
eval :: Boolean -> Boolean -> State -> State
eval true true state = state
eval true false state = moveLeft state
eval false true state = moveRight state
eval false false state = state

-- renderCars :: forall i p . State -> PrestoDOM i p
-- renderCars sate = 



moveLeft :: State -> State
moveLeft state = state { carX = if state.carX == 0 then state.carX else state.carX -4  }

moveRight :: State -> State
moveRight state = state { carX = if state.carX == 204 then state.carX else state.carX +4  }

initialState :: State
initialState =
  { distance: 0
  , score: 0
  , carX: 100
  , carY: 700
  }
  