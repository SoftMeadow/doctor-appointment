module Domain.Tree (
  lookupSymptom,
  lookupNode,
  getRootNodes,
  getChildNodes,
  symptomIdToLabel
) where

import Config.Types
import Domain.Types
import Data.Maybe (mapMaybe)

lookupSymptom :: AppConfig -> SymptomId -> Maybe Symptom -- по id симптома возвращает его данные, если симптома нет - возвращает Nothing
lookupSymptom config sid = lookup sid (cfgSymptoms config)

lookupNode :: AppConfig -> NodeId -> Maybe ComplaintNode -- по id узла возвращает его данные, если узла нет - возвращает Nothing
lookupNode config nid = lookup nid (cfgNodes config)

getRootNodes :: AppConfig -> [ComplaintNode] -- по списку id корневых узлов возвращает список их данных, если какого-то узла нет - пропускает его
getRootNodes config = mapMaybe (lookupNode config) (cfgRootIds config)

getChildNodes :: AppConfig -> NodeId -> [ComplaintNode]
getChildNodes config nid = getChildNodesFromMaybe (lookupNode config nid) config

getChildNodesFromMaybe :: Maybe ComplaintNode -> AppConfig -> [ComplaintNode]
getChildNodesFromMaybe Nothing _ = []
getChildNodesFromMaybe (Just node) config = mapMaybe (lookupNode config) (childIds node)

symptomIdToLabel :: AppConfig -> SymptomId -> String -- по id симптома возвращает его название, если симптома нет - возвращает его id в виде строки
symptomIdToLabel config sid = symptomIdToLabelFromMaybe (lookupSymptom config sid) sid -- для видже

symptomIdToLabelFromMaybe :: Maybe Symptom -> SymptomId -> String 
symptomIdToLabelFromMaybe (Just symptom) _ = symptomLabelToString (symptomLabel symptom)
symptomIdToLabelFromMaybe Nothing sid = symptomIdToString sid -- если симптом не найден, возвращаем его id в виде строки, чтобы не показывать пустое место в интерфейсе
                                                              -- просто запасной вариант

symptomLabelToString :: SymptomLabel -> String -- распаковывает текст из типа SymptomLabel
symptomLabelToString (SymptomLabel label) = label

symptomIdToString :: SymptomId -> String -- распаковывает id из типа SymptomId
symptomIdToString (SymptomId raw) = raw