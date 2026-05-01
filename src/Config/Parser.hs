module Config.Parser (
  parseSymptomLine,
  parseDoctorLine,
  parseNodeLine,
  parseRootsLine,
  readSpecialistsFile,
  readComplaintsFile
) where

import Config.Errors
import Config.Types
import Domain.Types
import System.Directory (doesFileExist)
import Data.List.Split (splitOn)

clearCR :: String -> String
clearCR [] = []
clearCR ('\r':xs) = clearCR xs
clearCR (x:xs) = x : clearCR xs

parseIdList :: String -> [String] -- парсит часть с симптомами, красными флагами или точными совпадениями, превращая строку вида "S1,S2,S3" в список ["S1", "S2", "S3"]
parseIdList raw = filter (/= "") (map clearCR (splitOn "," raw))

parseCategory :: String -> Maybe ComplaintCategory -- парсит строку с категорией узла, превращая её в значение типа ComplaintCategory
parseCategory "" = Nothing
parseCategory "Pain" = Just Pain
parseCategory "StateChange" = Just StateChange
parseCategory "ExternalSigns" = Just ExternalSigns
parseCategory other = Just (OtherCategory other)

mkError :: (ErrorMessage -> ValidationError) -> String -> Either ValidationError a-- вспомогательная функция для создания ошибки
mkError constructor message = Left (constructor (ErrorMessage message))

parseSymptomLine :: String -> Either ValidationError Symptom -- парсит строку с симптомом, превращая её в значение типа Symptom, если строка некорректная - возвращает ошибку
parseSymptomLine line = parseSymptomParts line (map clearCR (splitOn "|" line))

parseSymptomParts :: String -> [String] -> Either ValidationError Symptom
parseSymptomParts _ ["SYMPTOM", sid, label] =
  Right Symptom
    { symptomId = SymptomId sid
    , symptomLabel = SymptomLabel label
    }
parseSymptomParts line _ =
  mkError InvalidSpecialistsFormat ("Некорректная строка симптома: " ++ line)

parseDoctorLine :: String -> Either ValidationError Doctor -- парсит строку с врачом, превращая её в значение типа Doctor, если строка некорректная - возвращает ошибку
parseDoctorLine line = parseDoctorParts line (map clearCR (splitOn "|" line))

parseDoctorParts :: String -> [String] -> Either ValidationError Doctor
parseDoctorParts _ ["DOCTOR", did, name, profile, redFlagsRaw, keyRaw, commonRaw, secondaryRaw] =
  Right Doctor
    { doctorId = DoctorId did
    , doctorName = DoctorName name
    , doctorProfile = DoctorProfile profile
    , doctorRedFlags = map SymptomId (parseIdList redFlagsRaw)
    , doctorKeySymptoms = map SymptomId (parseIdList keyRaw)
    , doctorCommonSymptoms = map SymptomId (parseIdList commonRaw)
    , doctorSecondarySymptoms = map SymptomId (parseIdList secondaryRaw)
    }
parseDoctorParts line _ =
  mkError InvalidSpecialistsFormat ("Некорректная строка врача: " ++ line)

parseNodeLine :: String -> Either ValidationError ComplaintNode -- парсит строку с узлом, превращая её в значение типа ComplaintNode, если строка некорректная - возвращает ошибку
parseNodeLine line = parseNodeParts line (map clearCR (splitOn "|" line))

parseNodeParts :: String -> [String] -> Either ValidationError ComplaintNode
parseNodeParts _ ["NODE", nid, label, categoryRaw, "", childrenRaw] = -- случай, когда узел - это просто раздел, без симптома
  Right ComplaintNode
    { nodeId = NodeId nid
    , nodeLabel = NodeLabel label
    , nodeCategory = parseCategory categoryRaw
    , nodeSymptom = Nothing
    , childIds = map NodeId (parseIdList childrenRaw)
    }
parseNodeParts _ ["NODE", nid, label, categoryRaw, symptomRaw, childrenRaw] = -- случай, когда узел - это симптом
  Right ComplaintNode
    { nodeId = NodeId nid
    , nodeLabel = NodeLabel label
    , nodeCategory = parseCategory categoryRaw
    , nodeSymptom = Just (SymptomId symptomRaw)
    , childIds = map NodeId (parseIdList childrenRaw)
    }
parseNodeParts line _ =
  mkError InvalidComplaintsFormat ("Некорректная строка узла: " ++ line)

parseRootsLine :: String -> Either ValidationError [NodeId] -- парсит строку с корнями дерева, превращая её в список id корневых узлов, если строка некорректная - возвращает ошибку
parseRootsLine line = parseRootsParts line (map clearCR (splitOn "|" line))

parseRootsParts :: String -> [String] -> Either ValidationError [NodeId]
parseRootsParts _ ["ROOTS", rootsRaw] =
  Right (map NodeId (parseIdList rootsRaw))
parseRootsParts line _ =
  mkError InvalidComplaintsFormat ("Некорректная строка корней: " ++ line)

readSpecialistsFile :: FilePath -> IO (Either ValidationError SpecialistsFile) -- читает файл специалистов, если файла нет - возвращает ошибку, если файл есть - парсит его и возвращает результат парсинга (либо SpecialistsFile, либо ошибку)
readSpecialistsFile path = do
  exists <- doesFileExist path
  if not exists
    then return (Left (MissingConfigFile path))
    else do
      content <- readFile path
      return (parseSpecialistsContent content)

parseSpecialistsContent :: String -> Either ValidationError SpecialistsFile
parseSpecialistsContent content = do
  let rows = map clearCR (lines content)
      symptomRows = filter isSymptomRow rows
      doctorRows = filter isDoctorRow rows
  symptoms <- parseAllSymptomLines symptomRows -- <- для удобства, чтобы прерывать выполнение при первой же ошибке
  doctors <- parseAllDoctorLines doctorRows
  return SpecialistsFile
    { sfSymptoms = symptoms
    , sfDoctors = doctors
    }

isSymptomRow :: String -> Bool
isSymptomRow row = isSymptomParts (splitOn "|" row)

isSymptomParts :: [String] -> Bool
isSymptomParts ("SYMPTOM":_) = True
isSymptomParts _ = False

isDoctorRow :: String -> Bool
isDoctorRow row = isDoctorParts (splitOn "|" row)

isDoctorParts :: [String] -> Bool
isDoctorParts ("DOCTOR":_) = True
isDoctorParts _ = False

parseAllSymptomLines :: [String] -> Either ValidationError [Symptom]
parseAllSymptomLines [] = Right []
parseAllSymptomLines (x:xs) = do
  firstSymptom <- parseSymptomLine x
  otherSymptoms <- parseAllSymptomLines xs
  return (firstSymptom : otherSymptoms) -- возвращает значение в Either-контексте (Right)

parseAllDoctorLines :: [String] -> Either ValidationError [Doctor]
parseAllDoctorLines [] = Right []
parseAllDoctorLines (x:xs) = do
  firstDoctor <- parseDoctorLine x
  otherDoctors <- parseAllDoctorLines xs
  return (firstDoctor : otherDoctors)

readComplaintsFile :: FilePath -> IO (Either ValidationError ComplaintsFile)
readComplaintsFile path = do
  exists <- doesFileExist path
  if not exists
    then return (Left (MissingConfigFile path))
    else do
      content <- readFile path
      return (parseComplaintsContent content)

parseComplaintsContent :: String -> Either ValidationError ComplaintsFile
parseComplaintsContent content = do
  let rows = map clearCR (lines content)
      rootsRows = filter isRootsRow rows
      nodeRows = filter isNodeRow rows
  rootIds <- getRootIds rootsRows
  nodes <- parseAllNodeLines nodeRows
  return ComplaintsFile
    { cfRootIds = rootIds
    , cfNodes = nodes
    }

isRootsRow :: String -> Bool
isRootsRow row = isRootsParts (splitOn "|" row)

isRootsParts :: [String] -> Bool
isRootsParts ("ROOTS":_) = True
isRootsParts _ = False

isNodeRow :: String -> Bool
isNodeRow row = isNodeParts (splitOn "|" row)

isNodeParts :: [String] -> Bool
isNodeParts ("NODE":_) = True
isNodeParts _ = False

getRootIds :: [String] -> Either ValidationError [NodeId]
getRootIds [] = mkError InvalidComplaintsFormat "Не найдена строка ROOTS"
getRootIds (x:_) = parseRootsLine x

parseAllNodeLines :: [String] -> Either ValidationError [ComplaintNode]
parseAllNodeLines [] = Right []
parseAllNodeLines (x:xs) = do
  firstNode <- parseNodeLine x
  otherNodes <- parseAllNodeLines xs
  return (firstNode : otherNodes)