module UI.Model (
  Screen(..),
  AppModel(..),
  initialModel
) where

import Domain.Types
import Config.Types
import Config.Errors

data Screen
  = StartScreen
  | QuestionnaireScreen
  | ResultScreen
  | FatalErrorScreen
  deriving (Show, Eq)

data AppModel = AppModel
  { appConfig :: Maybe AppConfig
  , currentScreen :: Screen
  , navigationStack :: [NodeId]
  , currentNodes :: [NodeId]
  , selectedSymptoms :: [SymptomId]
  , historyLog :: [SymptomId]
  , currentRecommendation :: Recommendation
  , fatalErrorMessage :: Maybe ErrorMessage
  , specialistsPath :: FilePath
  , complaintsPath :: FilePath
  , currentPathTitles :: [NodeLabel]
  } deriving (Show, Eq)

initialModel :: AppModel
initialModel =
  AppModel
    { appConfig = Nothing
    , currentScreen = StartScreen
    , navigationStack = []
    , currentNodes = []
    , selectedSymptoms = []
    , historyLog = []
    , currentRecommendation = DiagnosisUnclear
    , fatalErrorMessage = Nothing
    , specialistsPath = "data/doctors.txt"
    , complaintsPath = "data/complaints.txt"
    , currentPathTitles = []
    }