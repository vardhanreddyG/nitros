module Nitro.Main where
import Prelude
import FRP.Behavior.Keyboard
import Control.Monad.Eff.Console (log)
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
    , imageView
        [ id_ "car"
        , imageUrl "car"
        , rotation "-90"
        , height (V 50)
        , width (V 100)
        , margin $ (show state.carX) <> "," <> (show state.carY) <> ",0,0"
        ]
    ]

renderGame :: forall i p . State -> PrestoDOM i p
renderGame state =
  linearLayout
    [ id_ "playscreen"
    , height Match_Parent
    , width Match_Parent
    , gravity "center"
    ]
    [ renderTrack state
    ]

main :: forall e. Eff ( dom :: DOM, frp :: FRP, console :: CONSOLE | e) Unit
main = do
  { stateBeh , updateState} <- render renderGame initialState
  

initialState :: State
initialState =
  { distance: 0
  , score: 0
  , carX: 100
  , carY: 700
  }
  