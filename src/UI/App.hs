module UI.App (
  buildUI
) where

import Monomer
import UI.Model
import UI.Event
import UI.Screens.Start
import UI.Screens.Questionnaire
import UI.Screens.Result
import UI.Screens.FatalError

buildUI :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI env model =
  buildScreen env model (currentScreen model)

buildScreen :: WidgetEnv AppModel AppEvent -> AppModel -> Screen -> WidgetNode AppModel AppEvent
buildScreen env model StartScreen = buildStartScreen env model
buildScreen env model QuestionnaireScreen = buildQuestionnaireScreen env model
buildScreen env model ResultScreen = buildResultScreen env model
buildScreen env model FatalErrorScreen = buildFatalErrorScreen env model