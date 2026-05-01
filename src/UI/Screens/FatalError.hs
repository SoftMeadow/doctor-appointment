{-# LANGUAGE OverloadedStrings #-}
module UI.Screens.FatalError (
  buildFatalErrorScreen
) where

import Config.Errors
import qualified Data.Text as T
import Monomer
import UI.Event
import UI.Model
import UI.Widgets

buildFatalErrorScreen :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildFatalErrorScreen _ model =
  zstack
    [ image_ "assets/other-bg.jpg" [fitFill]

    , box_ [alignCenter, alignMiddle] $
        vstack
          [ label "Ошибка загрузки приложения"
              `styleBasic`
                [ textSize 24
                , textColor black
                , paddingB 18
                ]

          , label_ (T.pack msg) [multiline]
              `styleBasic`
                [ textSize 15
                , textColor black
                , paddingB 24
                ]

          , mainActionButton "На старт" BackToStart
          ]
          `styleBasic`
            [ width 520
            , padding 30
            , bgColor (rgba 255 245 250 190)
            , radius 24
            , border 2 (rgbHex "#d7b7c2")
            ]
    ]
  where
    msg = fatalErrorText (fatalErrorMessage model)

fatalErrorText :: Maybe ErrorMessage -> String
fatalErrorText (Just (ErrorMessage text)) = text
fatalErrorText Nothing = "Неизвестная ошибка"