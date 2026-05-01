{-# LANGUAGE OverloadedStrings #-}

module UI.Screens.Start (
  buildStartScreen
) where

import Monomer
import UI.Event
import UI.Model

buildStartScreen :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildStartScreen _ _ =
  zstack
    [ image_ "assets/start-bg.jpg" [fitFill]
    , vstack
        [ image_ "assets/title.png" [fitWidth]
        , button "Начать" StartQuestionnaire
            `styleBasic`
              [ width 220
              , height 56
              , bgColor (rgbHex "#f7b6c8")
              , textColor (rgbHex "#a2274c")
              , radius 18
              , border 2 (rgbHex "#c45b7a")
              ]
        , image_ "assets/dog.png" [fitHeight]
            `styleBasic`
              [ height 650
              , paddingL 166
              , paddingT 20
              ]
        ]
        `styleBasic`
          [ paddingL 24
          , paddingR 24
          , paddingT 20
          ]
    ]