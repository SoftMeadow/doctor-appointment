module Config.Errors (
  ErrorMessage(..),
  ValidationError(..)
) where

import Domain.Types

newtype ErrorMessage = ErrorMessage String
  deriving (Show, Eq, Ord)

data ValidationError
  = MissingConfigFile FilePath
  | InvalidSpecialistsFormat ErrorMessage
  | InvalidComplaintsFormat ErrorMessage
  | DuplicateSymptomId SymptomId
  | DuplicateDoctorId DoctorId
  | DuplicateNodeId NodeId
  | MissingNodeReference NodeId NodeId
  | MissingSymptomReference SymptomId
  | CycleInComplaintTree NodeId
  deriving (Show, Eq)