module UI.Event (
  AppEvent(..)
) where

import Domain.Types
import Config.Types
import Config.Errors

data AppEvent
  = AppInit
  | LoadConfigs
  | ConfigsLoaded AppConfig
  | StartQuestionnaire
  | ConfigsFailed ErrorMessage
  | OpenNode NodeId
  | SelectSymptom SymptomId
  | GoBack
  | RemoveSymptom SymptomId
  | ResetQuestionnaire
  | RecalculateRecommendation
  | ShowResults
  | BackToStart
  deriving (Show, Eq)