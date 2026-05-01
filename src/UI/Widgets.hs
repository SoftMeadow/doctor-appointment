{-# LANGUAGE OverloadedStrings #-}
module UI.Widgets (
  mainActionButton,
  secondaryButton,
  buildNodeButtons,
  buildSelectedSymptomsPanel,
  buildRecommendationPanel
) where

import qualified Config.Types as Cfg
import qualified Data.Text as T
import Domain.Tree
import Domain.Types
import Monomer
import Presentation.Text
import UI.Event
import UI.Model

mainActionButton :: String -> AppEvent -> WidgetNode AppModel AppEvent
mainActionButton text evt =
  button (T.pack text) evt
    `styleBasic`
      [ width 240
      , textSize 14
      , height 52
      , bgColor (rgbHex "#f7b6c8")
      , textColor black
      , radius 16
      , border 2 (rgbHex "#c45b7a")
      ]

secondaryButton :: String -> AppEvent -> WidgetNode AppModel AppEvent
secondaryButton text evt =
  button (T.pack text) evt
    `styleBasic`
      [ width 84
      , textSize 11
      , height 46
      , bgColor (rgbHex "#ffffff")
      , textColor black
      , radius 14
      , border 1 (rgbHex "#d7b7c2")
      ]

      
buildNodeButtons :: Cfg.AppConfig -> [NodeId] -> [WidgetNode AppModel AppEvent]
buildNodeButtons config nodeIds = map buildOne nodeIds
  where
    buildOne nid = buildNodeWidget nid (lookupNode config nid)
    buildNodeWidget :: NodeId -> Maybe ComplaintNode -> WidgetNode AppModel AppEvent
    buildNodeWidget _ Nothing =
      label "Неизвестный узел"
        `styleBasic` [textSize 14, paddingB 8]
    buildNodeWidget nid (Just node) =
      let NodeLabel title = nodeLabel node
      in button (T.pack title) (OpenNode nid)
          `styleBasic`
            [ width 300
            , textSize 13
            , height 44
            , bgColor (rgba 255 255 255 220)
            , textColor black
            , radius 12
            , border 1 (rgbHex "#d9b8c3")
            , paddingB 8
            ]

buildSelectedSymptomsPanel :: Cfg.AppConfig -> [SymptomId] -> WidgetNode AppModel AppEvent
buildSelectedSymptomsPanel config symptoms =
  vstack
    ([ label "Выбранные жалобы"
         `styleBasic`
           [ textSize 14
           , textColor black
           , paddingB 12
           ]
     ] ++ symptomRows)
    `styleBasic`
      [ padding 16
      , bgColor (rgba 255 255 255 170)
      , radius 16
      , border 1 (rgbHex "#d7b7c2")
      ]
  where
    symptomRows
      | null symptoms =
          [ label "Пока ничего не выбрано"
              `styleBasic`
                [ textSize 14
                , textColor black
                ]
          ]
      | otherwise = map symptomRow symptoms

    symptomRow sid =
      hstack
        [ box_ [expandContent] $
            label_ (T.pack (symptomIdToLabel config sid)) [ellipsis]
              `styleBasic`
                [ textSize 14
                , textColor black
                ]

        , spacer

        , button "✕" (RemoveSymptom sid)
            `styleBasic`
              [ width 34
              , textSize 13
              , height 34
              , bgColor (rgba 255 250 252 220)
              , textColor black
              , radius 10
              , border 1 (rgbHex "#d7b7c2")
              ]
            ]
        `styleBasic` [paddingB 8]

buildRecommendationPanel :: Cfg.AppConfig -> Recommendation -> WidgetNode AppModel AppEvent
buildRecommendationPanel config recommendation =
  vstack
    [ label "Рекомендация"
        `styleBasic`
          [ textSize 16
          , textColor black
          , paddingB 12
          ]

    , box $
        label_ (T.pack (recommendationExplanation config recommendation)) [multiline]
          `styleBasic`
            [ textSize 13
            , textColor black
            ]
    ]
    `styleBasic`
      [ padding 16
      , bgColor (rgba 255 255 255 170)
      , radius 16
      , border 1 (rgbHex "#d7b7c2")
      ]