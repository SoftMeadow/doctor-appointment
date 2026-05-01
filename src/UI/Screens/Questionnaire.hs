{-# LANGUAGE OverloadedStrings #-}
module UI.Screens.Questionnaire (
  buildQuestionnaireScreen
) where

import Monomer
import UI.Event
import UI.Model
import qualified Config.Types as Cfg
import UI.Widgets
import qualified Data.Text as T
import Domain.Types

buildQuestionnaireScreen :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildQuestionnaireScreen _ model =
  zstack
    [ image_ "assets/other-bg.jpg" [fitFill]
    , questionnaireContent model (appConfig model)
    ]

questionnaireContent :: AppModel -> Maybe Cfg.AppConfig -> WidgetNode AppModel AppEvent
questionnaireContent model (Just config) =
  box_ [alignCenter, alignMiddle] $
    hstack
      [ leftCard config model
      , spacer
      , rightCard config model
      ]
      `styleBasic`
        [ width 760
        , padding 20
        ]
questionnaireContent _ Nothing =
  label "Конфигурация не загружена"

leftCard :: Cfg.AppConfig -> AppModel -> WidgetNode AppModel AppEvent
leftCard config model =
  vstack
    [ label "Выберите жалобу"
        `styleBasic`
          [ textSize 22
          , textColor black
          , paddingB 16
          ]
    , sectionTitleWidget (currentPathTitles model)
    , box $
        vscroll $
          vstack (buildNodeButtons config (currentNodes model))
      `styleBasic`
        [ height 330
        , padding 10
        , bgColor (rgba 255 255 255 100)
        , radius 14
        , border 1 (rgbHex "#d7b7c2")
        ]

    , hstack
        [ secondaryButton "Назад" GoBack
        , spacer
        , secondaryButton "Сбросить" ResetQuestionnaire
        , spacer
        , secondaryButton "На главную" BackToStart
        ]
        `styleBasic` [ paddingT 18
                     , paddingB 14
                     ]

    , mainActionButton "Показать результат" ShowResults
        `styleBasic` [paddingT 6]
    ]
    `styleBasic`
      [ width 320
      , padding 22
      , bgColor (rgba 255 245 250 100)
      , radius 22
      , border 2 (rgbHex "#d7b7c2")
      ]

sectionTitleWidget :: [NodeLabel] -> WidgetNode AppModel AppEvent
sectionTitleWidget [] = spacer
sectionTitleWidget titles =
  let pathText =
        T.intercalate " / " $
          map (\(NodeLabel t) -> T.pack t) titles
  in
    label_ pathText [multiline, ellipsis]
      `styleBasic`
        [ textSize 14
        , textColor black
        , textLeft
        , paddingB 12
        ]

rightCard :: Cfg.AppConfig -> AppModel -> WidgetNode AppModel AppEvent
rightCard config model =
  vstack
    [ buildSelectedSymptomsPanel config (selectedSymptoms model)
    , spacer
    , buildRecommendationPanel config (currentRecommendation model)
    ]
    `styleBasic`
      [ width 320
      , padding 22
      , bgColor (rgba 255 245 250 100)
      , radius 22
      , border 2 (rgbHex "#d7b7c2")
      ]