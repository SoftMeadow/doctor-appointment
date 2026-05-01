module Domain.Types (
  SymptomId(..),
  NodeId(..),
  DoctorId(..),
  DoctorName(..),
  DoctorProfile(..),
  SymptomLabel(..),
  NodeLabel(..),
  ComplaintCategory(..),
  Symptom(..),
  Doctor(..),
  ComplaintNode(..),
  Recommendation(..)
) where

newtype SymptomId = SymptomId String
  deriving (Show, Eq, Ord)

newtype NodeId = NodeId String
  deriving (Show, Eq, Ord)

newtype DoctorId = DoctorId String
  deriving (Show, Eq, Ord)

newtype DoctorName = DoctorName String
  deriving (Show, Eq, Ord)

newtype DoctorProfile = DoctorProfile String
  deriving (Show, Eq, Ord)

newtype SymptomLabel = SymptomLabel String
  deriving (Show, Eq, Ord)

newtype NodeLabel = NodeLabel String
  deriving (Show, Eq, Ord)

data ComplaintCategory
  = Pain
  | StateChange
  | ExternalSigns
  | OtherCategory String
  deriving (Show, Eq)

data Symptom = Symptom
  { symptomId :: SymptomId
  , symptomLabel :: SymptomLabel
  } deriving (Show, Eq)

data Doctor = Doctor
  { doctorId :: DoctorId
  , doctorName :: DoctorName
  , doctorProfile :: DoctorProfile
  , doctorRedFlags :: [SymptomId]
  , doctorKeySymptoms :: [SymptomId]
  , doctorCommonSymptoms :: [SymptomId]
  , doctorSecondarySymptoms :: [SymptomId]
  } deriving (Show, Eq)

data ComplaintNode = ComplaintNode
  { nodeId :: NodeId
  , nodeLabel :: NodeLabel
  , nodeCategory :: Maybe ComplaintCategory
  , nodeSymptom :: Maybe SymptomId
  , childIds :: [NodeId]
  } deriving (Show, Eq)

data Recommendation
  = Recommended Doctor Double [SymptomId] String
  | RankedDoctors [(Doctor, Double, [SymptomId])] String
  | DiagnosisUnclear
  deriving (Show, Eq)
