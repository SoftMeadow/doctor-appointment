module Config.Types (
  SpecialistsFile(..),
  ComplaintsFile(..),
  AppConfig(..)
) where

import Domain.Types

data SpecialistsFile = SpecialistsFile
  { sfSymptoms :: [Symptom]
  , sfDoctors :: [Doctor]
  } deriving (Show, Eq)

data ComplaintsFile = ComplaintsFile
  { cfRootIds :: [NodeId]
  , cfNodes :: [ComplaintNode]
  } deriving (Show, Eq)

data AppConfig = AppConfig
  { cfgSymptoms :: [(SymptomId, Symptom)]
  , cfgDoctors :: [Doctor]
  , cfgNodes :: [(NodeId, ComplaintNode)]
  , cfgRootIds :: [NodeId]
  } deriving (Show, Eq)