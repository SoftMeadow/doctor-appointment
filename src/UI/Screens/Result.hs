{-# LANGUAGE OverloadedStrings #-}
module UI.Screens.Result (
  buildResultScreen
) where

import Monomer
import UI.Event
import UI.Model
import UI.Widgets
import qualified Config.Types as Cfg

buildResultScreen :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildResultScreen _ model =
  resultContent model (appConfig model)

resultContent :: AppModel -> Maybe Cfg.AppConfig -> WidgetNode AppModel AppEvent
resultContent model (Just config) =
  zstack
    [ image_ "assets/other-bg.jpg" [fitFill]

    , box_ [alignCenter, alignMiddle] $
        vstack
          [ label "Результат"
              `styleBasic`
                [ textSize 24
                , textColor black
                , paddingB 20
                ]

          , buildSelectedSymptomsPanel config (selectedSymptoms model)
              `styleBasic`
                [ paddingB 20
                ]

          , box $
              buildRecommendationPanel config (currentRecommendation model)
            `styleBasic`
              [ width 720
              , paddingB 24
              ]

          , hstack
              [ secondaryButton "К опросу" ResetQuestionnaire
              , spacer
              , mainActionButton "На старт" BackToStart
              ]
          ]
          `styleBasic`
            [ width 820
            , padding 28
            , bgColor (rgba 255 245 250 185)
            , radius 24
            , border 2 (rgbHex "#d7b7c2")
            ]
    ]

resultContent _ Nothing =
  zstack
    [ image_ "assets/other-bg.jpg" [fitFill]

    , box_ [alignCenter, alignMiddle] $
        label "Конфигурация еще не загружена"
          `styleBasic`
            [ textSize 18
            , textColor black
            ]
    ]