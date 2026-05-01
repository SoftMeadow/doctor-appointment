module UI.Handler (
  handleEvent
) where

import Config.Build
import Config.Errors
import qualified Config.Types as CT
import Domain.Recommendation
import Domain.Tree
import Domain.Types
import Monomer
import Presentation.Text
import UI.Event
import UI.Model

addSymptom :: SymptomId -> [SymptomId] -> [SymptomId]
addSymptom sid symptoms
  | sid `elem` symptoms = symptoms
  | otherwise = symptoms ++ [sid]

removeSymptom :: SymptomId -> [SymptomId] -> [SymptomId]
removeSymptom sid = filter (/= sid)

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]

handleEvent _ _ _ AppInit =
  [Event LoadConfigs]

handleEvent _ _ model LoadConfigs =
  [ Task $ do
      result <- loadAppConfig (specialistsPath model) (complaintsPath model)
      return (loadConfigsResult result)
  ]

handleEvent _ _ model (ConfigsLoaded config) =
  [ Model
      model
        { appConfig = Just config
        , currentScreen = StartScreen
        , currentNodes = map nodeId (getRootNodes config)
        , currentRecommendation = DiagnosisUnclear
        , fatalErrorMessage = Nothing
        , currentPathTitles = []
        }
  ]

handleEvent _ _ model (ConfigsFailed errMsg) =
  [ Model model
      { currentScreen = FatalErrorScreen
      , fatalErrorMessage = Just errMsg
      }
  ]

handleEvent _ _ model (OpenNode nid) =
  openNodeHandler model nid (appConfig model)

handleEvent _ _ model (SelectSymptom sid) =
  selectSymptomHandler model sid (appConfig model)

handleEvent _ _ model GoBack =
  goBackHandler model (appConfig model)

handleEvent _ _ model (RemoveSymptom sid) =
  removeSymptomHandler model sid (appConfig model)

handleEvent _ _ model ResetQuestionnaire =
  resetQuestionnaireHandler model (appConfig model)

handleEvent _ _ model RecalculateRecommendation =
  recalculateRecommendationHandler model (appConfig model)

handleEvent _ _ model ShowResults =
  [Model model { currentScreen = ResultScreen }]

handleEvent _ _ model StartQuestionnaire =
  startQuestionnaireHandler model (appConfig model)

handleEvent _ _ model BackToStart =
  [Model (resetModel model)]

loadConfigsResult :: Either ValidationError CT.AppConfig -> AppEvent
loadConfigsResult (Left validationError) =
  ConfigsFailed (ErrorMessage (prettyValidationError validationError))
loadConfigsResult (Right config) =
  ConfigsLoaded config

openNodeHandler
  :: AppModel
  -> NodeId
  -> Maybe CT.AppConfig
  -> [AppEventResponse AppModel AppEvent]
openNodeHandler _ _ Nothing = []
openNodeHandler model nid (Just config) =
  openNodeWithConfig model nid config (lookupNode config nid)

openNodeWithConfig
  :: AppModel
  -> NodeId
  -> CT.AppConfig
  -> Maybe ComplaintNode
  -> [AppEventResponse AppModel AppEvent]
openNodeWithConfig _ _ _ Nothing = []
openNodeWithConfig model nid config (Just node) =
  openNodeBySymptom model nid config (nodeSymptom node)

openNodeBySymptom -- открывает узел, если это просто раздел, или выбирает симптом и открывает узел, если это симптом
  :: AppModel
  -> NodeId
  -> CT.AppConfig
  -> Maybe SymptomId
  -> [AppEventResponse AppModel AppEvent]
openNodeBySymptom model nid config Nothing =
  [Model (openNodeInModel config nid model)]
openNodeBySymptom _ _ _ (Just sid) =
  [Event (SelectSymptom sid)]

selectSymptomHandler
  :: AppModel
  -> SymptomId
  -> Maybe CT.AppConfig
  -> [AppEventResponse AppModel AppEvent]
selectSymptomHandler _ _ Nothing = []
selectSymptomHandler model sid (Just config) =
  [Model (selectSymptomInModel config sid model)]

goBackHandler
  :: AppModel
  -> Maybe CT.AppConfig
  -> [AppEventResponse AppModel AppEvent]
goBackHandler _ Nothing = []
goBackHandler model (Just config) =
  goBackByStack config model (reverse (navigationStack model))

removeSymptomHandler
  :: AppModel
  -> SymptomId
  -> Maybe CT.AppConfig
  -> [AppEventResponse AppModel AppEvent]
removeSymptomHandler _ _ Nothing = []

removeSymptomHandler model sid (Just config) =
  [Model (removeSymptomInModel config sid model)]

resetQuestionnaireHandler
  :: AppModel
  -> Maybe CT.AppConfig
  -> [AppEventResponse AppModel AppEvent]
resetQuestionnaireHandler _ Nothing = []

resetQuestionnaireHandler model (Just config) =
  [ Model model
      { currentScreen = QuestionnaireScreen
      , navigationStack = []
      , currentNodes = map nodeId (getRootNodes config)
      , selectedSymptoms = []
      , historyLog = []
      , currentRecommendation = DiagnosisUnclear
      , currentPathTitles = []
      }
  ]

recalculateRecommendationHandler
  :: AppModel
  -> Maybe CT.AppConfig
  -> [AppEventResponse AppModel AppEvent]
recalculateRecommendationHandler _ Nothing = []

recalculateRecommendationHandler model (Just config) =
  [Model (recalculateModelRecommendation config model)]

startQuestionnaireHandler
  :: AppModel
  -> Maybe CT.AppConfig
  -> [AppEventResponse AppModel AppEvent]
startQuestionnaireHandler _ Nothing = []
startQuestionnaireHandler model (Just config) =
  [ Model model
      { currentScreen = QuestionnaireScreen
      , navigationStack = []
      , currentNodes = map nodeId (getRootNodes config)
      , currentPathTitles = []
      }
  ]

goBackByStack :: CT.AppConfig -> AppModel -> [NodeId] -> [AppEventResponse AppModel AppEvent]
goBackByStack _ _ [] = []

goBackByStack config model [_current] =
  [ Model model
      { navigationStack = []
      , currentNodes = map nodeId (getRootNodes config)
      , currentPathTitles = []
      }
  ]

goBackByStack config model (_current:parent:restReversed) =
  let newStack = reverse (parent : restReversed)
      newNodes = map nodeId (getChildNodes config parent)
      newPath =
        case currentPathTitles model of
          [] -> []
          xs -> init xs
  in
    [ Model model
        { navigationStack = newStack
        , currentNodes = newNodes
        , currentPathTitles = newPath
        }
    ]

openNodeInModel :: CT.AppConfig -> NodeId -> AppModel -> AppModel
openNodeInModel config nid model =
  let nextNodes = map nodeId (getChildNodes config nid)
      newPathTitles =
        case lookupNode config nid of
          Just node -> currentPathTitles model ++ [nodeLabel node]
          Nothing -> currentPathTitles model
  in model
      { navigationStack = navigationStack model ++ [nid]
      , currentNodes = nextNodes
      , currentPathTitles = newPathTitles
      }

selectSymptomInModel :: CT.AppConfig -> SymptomId -> AppModel -> AppModel
selectSymptomInModel config sid model =
  let newSymptoms = addSymptom sid (selectedSymptoms model)
      newHistory = addSymptom sid (historyLog model)
      nextModel =
        model
          { selectedSymptoms = newSymptoms
          , historyLog = newHistory
          }
  in recalculateModelRecommendation config nextModel

removeSymptomInModel :: CT.AppConfig -> SymptomId -> AppModel -> AppModel
removeSymptomInModel config sid model =
  let nextModel =
        model
          { selectedSymptoms = removeSymptom sid (selectedSymptoms model)
          , historyLog = removeSymptom sid (historyLog model)
          }
  in recalculateModelRecommendation config nextModel

resetModel :: AppModel -> AppModel
resetModel model =
  model
    { currentScreen = StartScreen
    , navigationStack = []
    , currentNodes = []
    , selectedSymptoms = []
    , historyLog = []
    , currentRecommendation = DiagnosisUnclear
    , fatalErrorMessage = Nothing
    , currentPathTitles = []
    }

recalculateModelRecommendation :: CT.AppConfig -> AppModel -> AppModel
recalculateModelRecommendation config model =
  model { currentRecommendation = recommendDoctor config (selectedSymptoms model) }