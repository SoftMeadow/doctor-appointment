module Presentation.Text (
  prettyValidationError,
  recommendationExplanation,
  doctorReasonText,
  rankedDoctorsText
) where

import Config.Errors
import Config.Types
import Domain.Tree
import Domain.Types

prettyValidationError :: ValidationError -> String -- для виджетов ошибок при загрузке конфигурации
prettyValidationError (MissingConfigFile path) =
  "Не найден конфигурационный файл: " ++ path
prettyValidationError (InvalidSpecialistsFormat (ErrorMessage msg)) =
  "Ошибка в файле специалистов: " ++ msg
prettyValidationError (InvalidComplaintsFormat (ErrorMessage msg)) =
  "Ошибка в файле жалоб: " ++ msg
prettyValidationError (DuplicateSymptomId (SymptomId sid)) =
  "Повторяющийся id симптома: " ++ sid
prettyValidationError (DuplicateDoctorId (DoctorId did)) =
  "Повторяющийся id врача: " ++ did
prettyValidationError (DuplicateNodeId (NodeId nid)) =
  "Повторяющийся id узла: " ++ nid
prettyValidationError (MissingNodeReference (NodeId parent) (NodeId child)) =
  "Узел " ++ parent ++ " ссылается на отсутствующий узел " ++ child
prettyValidationError (MissingSymptomReference (SymptomId sid)) =
  "Узел ссылается на отсутствующий симптом: " ++ sid
prettyValidationError (CycleInComplaintTree (NodeId nid)) =
  "Обнаружен цикл в дереве жалоб около узла: " ++ nid

recommendationExplanation :: AppConfig -> Recommendation -> String -- для виджетов рекомендации
recommendationExplanation config (Recommended doctor score matched reason) =
  reason ++ " " ++ doctorReasonText config doctor score matched
recommendationExplanation config (RankedDoctors ranked reason) =
  reason ++ "\n" ++ rankedDoctorsText config ranked
recommendationExplanation _ DiagnosisUnclear =
  "Диагноз неясен. Недостаточно данных, чтобы уверенно направить к специалисту."

doctorReasonText :: AppConfig -> Doctor -> Double -> [SymptomId] -> String
doctorReasonText config doctor score symptomIds =
  let DoctorName doctorNameText = doctorName doctor
      labels = map (symptomIdToLabel config) symptomIds
  in
    "Рекомендуется врач: "
      ++ doctorNameText
      ++ ". Балл: "
      ++ show score
      ++ ". Совпавшие симптомы: "
      ++ joinWithComma labels

rankedDoctorsText :: AppConfig -> [(Doctor, Double, [SymptomId])] -> String
rankedDoctorsText config ranked = unlines (map oneLine ranked)
  where
    oneLine (doctor, score, symptomIds) =
      let DoctorName doctorNameText = doctorName doctor
          labels = map (symptomIdToLabel config) symptomIds
      in
        doctorNameText
          ++ " — балл: "
          ++ show score
          ++ " ("
          ++ joinWithComma labels
          ++ ")"

joinWithComma :: [String] -> String
joinWithComma [] = "нет"
joinWithComma [x] = x
joinWithComma (x:xs) = x ++ ", " ++ joinWithComma xs
