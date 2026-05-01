module Config.Build (
  loadAppConfig,
  buildAppConfig,
  buildSymptomsAssoc,
  buildNodesAssoc
) where

import Config.Errors
import Config.Parser
import Config.Types
import Config.Validation
import Domain.Types

loadAppConfig :: FilePath -> FilePath -> IO (Either ValidationError AppConfig)
loadAppConfig specialistsPath complaintsPath = do
  specialistsRes <- readSpecialistsFile specialistsPath -- грязная часть
  complaintsRes <- readComplaintsFile complaintsPath
  return $ do
    specialists <- specialistsRes
    complaints <- complaintsRes
    buildAppConfig specialists complaints

buildAppConfig :: SpecialistsFile -> ComplaintsFile -> Either ValidationError AppConfig
buildAppConfig specialists complaints = do
  _ <- validateAll specialists complaints
  Right AppConfig
    { cfgSymptoms = buildSymptomsAssoc (sfSymptoms specialists)
    , cfgDoctors = sfDoctors specialists
    , cfgNodes = buildNodesAssoc (cfNodes complaints)
    , cfgRootIds = cfRootIds complaints
    }

buildSymptomsAssoc :: [Symptom] -> [(SymptomId, Symptom)]
buildSymptomsAssoc symptoms = map (\symptom -> (symptomId symptom, symptom)) symptoms

buildNodesAssoc :: [ComplaintNode] -> [(NodeId, ComplaintNode)]
buildNodesAssoc nodes = map (\node -> (nodeId node, node)) nodes
