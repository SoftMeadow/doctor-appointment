module Config.Validation (
  validateSpecialistsFile,
  validateComplaintsFile,
  validateCrossReferences,
  validateAll,
  checkDuplicateSymptomIds,
  checkDuplicateDoctorIds,
  checkDuplicateNodeIds,
  checkNodeReferences,
  checkNodeCycles,
  checkSymptomReferences,
  checkDoctorSymptomReferences
) where

import Config.Errors
import Config.Types
import Domain.Types
import Data.List (group, sort)

validateSpecialistsFile :: SpecialistsFile -> Either ValidationError Bool
validateSpecialistsFile specialists = do
  _ <- checkDuplicateSymptomIds (sfSymptoms specialists)
  _ <- checkDuplicateDoctorIds (sfDoctors specialists)
  _ <- checkDoctorSymptomReferences (sfSymptoms specialists) (sfDoctors specialists)
  return True

validateComplaintsFile :: ComplaintsFile -> Either ValidationError Bool
validateComplaintsFile complaints = do
  _ <- checkDuplicateNodeIds (cfNodes complaints)
  _ <- checkNodeReferences (cfNodes complaints)
  _ <- checkNodeCycles (cfNodes complaints)
  return True

validateCrossReferences :: SpecialistsFile -> ComplaintsFile -> Either ValidationError Bool
validateCrossReferences specialists complaints = do
  _ <- checkSymptomReferences (sfSymptoms specialists) (cfNodes complaints)
  return True

validateAll :: SpecialistsFile -> ComplaintsFile -> Either ValidationError Bool
validateAll specialists complaints = do
  _ <- validateSpecialistsFile specialists
  _ <- validateComplaintsFile complaints
  _ <- validateCrossReferences specialists complaints
  return True

findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate values = findDuplicateGroups (filter ((> 1) . length) (group (sort values)))

findDuplicateGroups :: [[a]] -> Maybe a -- есть ли хотя бы один дубликат
findDuplicateGroups ((x:_):_) = Just x
findDuplicateGroups _ = Nothing

checkDuplicateSymptomIds :: [Symptom] -> Either ValidationError Bool
checkDuplicateSymptomIds symptoms =
  checkDuplicateSymptomIdsResult (findDuplicate (map symptomId symptoms))

checkDuplicateSymptomIdsResult :: Maybe SymptomId -> Either ValidationError Bool
checkDuplicateSymptomIdsResult (Just sid) = Left (DuplicateSymptomId sid)
checkDuplicateSymptomIdsResult Nothing = Right True

checkDuplicateDoctorIds :: [Doctor] -> Either ValidationError Bool
checkDuplicateDoctorIds doctors =
  checkDuplicateDoctorIdsResult (findDuplicate (map doctorId doctors))

checkDuplicateDoctorIdsResult :: Maybe DoctorId -> Either ValidationError Bool
checkDuplicateDoctorIdsResult (Just did) = Left (DuplicateDoctorId did)
checkDuplicateDoctorIdsResult Nothing = Right True

checkDuplicateNodeIds :: [ComplaintNode] -> Either ValidationError Bool
checkDuplicateNodeIds nodes =
  checkDuplicateNodeIdsResult (findDuplicate (map nodeId nodes))

checkDuplicateNodeIdsResult :: Maybe NodeId -> Either ValidationError Bool
checkDuplicateNodeIdsResult (Just nid) = Left (DuplicateNodeId nid)
checkDuplicateNodeIdsResult Nothing = Right True

checkNodeReferences :: [ComplaintNode] -> Either ValidationError Bool -- проверяет, что все nodeId, на которые ссылаются узлы, присутствуют в списке узлов
checkNodeReferences nodes = checkNodeReferencesResult missing
  where
    nodeIds = map nodeId nodes
    missing =
      [ MissingNodeReference (nodeId parent) child
      | parent <- nodes
      , child <- childIds parent
      , child `notElem` nodeIds
      ] -- список всех ссылок на несуществующие узлы

checkNodeReferencesResult :: [ValidationError] -> Either ValidationError Bool
checkNodeReferencesResult (err:_) = Left err
checkNodeReferencesResult [] = Right True

checkNodeCycles :: [ComplaintNode] -> Either ValidationError Bool
checkNodeCycles nodes = checkNodeCyclesResult firstCycle
  where
    firstCycle = findCycleInNodes nodes nodes

checkNodeCyclesResult :: Maybe NodeId -> Either ValidationError Bool
checkNodeCyclesResult (Just nid) = Left (CycleInComplaintTree nid)
checkNodeCyclesResult Nothing = Right True

findCycleInNodes :: [ComplaintNode] -> [ComplaintNode] -> Maybe NodeId
findCycleInNodes allNodes (node:rest) =
  case findCycleFromNode allNodes [] (nodeId node) of
    Just nid -> Just nid
    Nothing -> findCycleInNodes allNodes rest
findCycleInNodes _ [] = Nothing

findCycleFromNode :: [ComplaintNode] -> [NodeId] -> NodeId -> Maybe NodeId
findCycleFromNode nodes path currentId
  | currentId `elem` path = Just currentId
  | otherwise =
      case lookupNodeInList currentId nodes of
        Nothing -> Nothing
        Just node -> findCycleInChildren nodes (currentId : path) (childIds node)

findCycleInChildren :: [ComplaintNode] -> [NodeId] -> [NodeId] -> Maybe NodeId
findCycleInChildren nodes path (child:rest) =
  case findCycleFromNode nodes path child of
    Just nid -> Just nid
    Nothing -> findCycleInChildren nodes path rest
findCycleInChildren _ _ [] = Nothing

lookupNodeInList :: NodeId -> [ComplaintNode] -> Maybe ComplaintNode
lookupNodeInList _ [] = Nothing
lookupNodeInList nid (node:rest)
  | nodeId node == nid = Just node
  | otherwise = lookupNodeInList nid rest

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

checkSymptomReferences :: [Symptom] -> [ComplaintNode] -> Either ValidationError Bool -- проверяет, что все symptomId из файла жалоб, на которые ссылаются узлы, присутствуют в списке симптомов из файла специалистов
checkSymptomReferences symptoms nodes = checkSymptomReferencesResult missing
  where
    symptomIds = map symptomId symptoms
    referenced = [ sid | node <- nodes, sid <- maybeToList (nodeSymptom node) ] -- список всех symptomId, на которые ссылаются узлы
    missing = filter (`notElem` symptomIds) referenced -- список всех symptomId, на которые ссылаются узлы, но которых нет в списке симптомов

checkSymptomReferencesResult :: [SymptomId] -> Either ValidationError Bool
checkSymptomReferencesResult (sid:_) = Left (MissingSymptomReference sid)
checkSymptomReferencesResult [] = Right True

checkDoctorSymptomReferences :: [Symptom] -> [Doctor] -> Either ValidationError Bool
checkDoctorSymptomReferences symptoms doctors =
  checkSymptomReferencesResult missing
  where
    symptomIds = map symptomId symptoms
    referenced = concatMap doctorAllSymptoms doctors
    missing = filter (`notElem` symptomIds) referenced

doctorAllSymptoms :: Doctor -> [SymptomId]
doctorAllSymptoms doctor =
     doctorRedFlags doctor
  ++ doctorKeySymptoms doctor
  ++ doctorCommonSymptoms doctor
  ++ doctorSecondarySymptoms doctor