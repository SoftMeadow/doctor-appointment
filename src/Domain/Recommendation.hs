module Domain.Recommendation (
  recommendDoctor,
  applyRedFlagRule,
  applyKeySymptomRule,
  applyRankingRule,
  scoreDoctor,
  matchingSymptoms,
  sortRankedDoctors
) where

import Config.Types
import Domain.Types
import Data.List (sortBy)

redFlagWeight :: Double
redFlagWeight = 100.0

keySymptomWeight :: Double
keySymptomWeight = 10.0

commonSymptomWeight :: Double
commonSymptomWeight = 5.0

secondarySymptomWeight :: Double
secondarySymptomWeight = 2.0

minimumRecommendationScore :: Double
minimumRecommendationScore = 5.0

recommendDoctor :: AppConfig -> [SymptomId] -> Recommendation
recommendDoctor _ [] = DiagnosisUnclear
recommendDoctor config symptoms =
  chooseRedFlag (applyRedFlagRule (cfgDoctors config) symptoms) config symptoms

chooseRedFlag :: Maybe Recommendation -> AppConfig -> [SymptomId] -> Recommendation
-- разбирает результат правила красных флагов:
-- если есть рекомендация с тревожными симптомами, возвращает её,
-- если нет - переходит к правилу ключевых симптомов
chooseRedFlag (Just recommendation) _ _ = recommendation
chooseRedFlag Nothing config symptoms =
  chooseKeySymptom (applyKeySymptomRule (cfgDoctors config) symptoms) config symptoms

chooseKeySymptom :: Maybe Recommendation -> AppConfig -> [SymptomId] -> Recommendation
-- разбирает результат правила ключевых симптомов:
-- если есть уверенная рекомендация, возвращает её,
-- если нет - переходит к правилу ранжирования
chooseKeySymptom (Just recommendation) _ _ = recommendation
chooseKeySymptom Nothing config symptoms =
  applyRankingRule (cfgDoctors config) symptoms

applyRedFlagRule :: [Doctor] -> [SymptomId] -> Maybe Recommendation
-- если у врача есть совпавшие красные флаги, считает баллы для таких врачей,
-- сортирует их и, если лучший врач набрал достаточно высокий балл,
-- возвращает рекомендацию с этим врачом и списком совпавших симптомов;
-- если таких врачей нет - возвращает Nothing
applyRedFlagRule doctors symptoms =
  makeRedFlagRecommendation sorted
  where
    ranked =
      [ (doctor, scoreDoctor doctor symptoms, redFlagMatches doctor symptoms)
      | doctor <- doctors
      , not (null (redFlagMatches doctor symptoms))
      ]
    sorted = sortRankedDoctors ranked
    
redFlagMatches :: Doctor -> [SymptomId] -> [SymptomId]
redFlagMatches doctor symptoms =
  intersectIds (doctorRedFlags doctor) symptoms

makeRedFlagRecommendation :: [(Doctor, Double, [SymptomId])] -> Maybe Recommendation
makeRedFlagRecommendation ((doctor, score, matches):_) =
  Just (Recommended doctor score matches "Обнаружены тревожные симптомы.")
makeRedFlagRecommendation [] = Nothing

applyKeySymptomRule :: [Doctor] -> [SymptomId] -> Maybe Recommendation
-- если у врача есть совпавшие ключевые симптомы, считает баллы для таких врачей,
-- сортирует их и, если лучший врач заметно опережает остальных,
-- возвращает рекомендацию с этим врачом и списком совпавших симптомов;
-- если таких врачей нет - возвращает Nothing
applyKeySymptomRule doctors symptoms =
  makeKeySymptomRecommendation sorted
  where
    ranked =
      [ (doctor, scoreDoctor doctor symptoms, matchingSymptoms doctor symptoms)
      | doctor <- doctors
      , not (null (intersectIds (doctorKeySymptoms doctor) symptoms))
      ]
    sorted = sortRankedDoctors ranked

makeKeySymptomRecommendation :: [(Doctor, Double, [SymptomId])] -> Maybe Recommendation
makeKeySymptomRecommendation ((doctor, score, matches):rest)
  | score >= 15 && significantlyBetter score rest =
      Just (Recommended doctor score matches "Есть выраженное совпадение по ключевым симптомам.")
  | otherwise = Nothing
makeKeySymptomRecommendation [] = Nothing

applyRankingRule :: [Doctor] -> [SymptomId] -> Recommendation
-- создает список врачей с их оценками и совпавшими симптомами,
-- сортирует его по убыванию оценки,
-- фильтрует только тех врачей, у которых балл выше минимального,
-- и возвращает рекомендацию с этим списком врачей;
-- если таких врачей нет - возвращает DiagnosisUnclear
applyRankingRule doctors symptoms =
  makeRankingRecommendation positive
  where
    ranked =
      [ (doctor, scoreDoctor doctor symptoms, matchingSymptoms doctor symptoms)
      | doctor <- doctors
      ]
    sorted = sortRankedDoctors ranked
    positive = filter (\(_, score, _) -> score >= minimumRecommendationScore) sorted

makeRankingRecommendation :: [(Doctor, Double, [SymptomId])] -> Recommendation
makeRankingRecommendation [] = DiagnosisUnclear
makeRankingRecommendation positive =
  RankedDoctors positive "Подобран список врачей по суммарному весу совпавших симптомов."

scoreDoctor :: Doctor -> [SymptomId] -> Double
-- считает итоговый балл врача по весам симптомов:
-- красные флаги имеют максимальный вес,
-- ключевые симптомы - высокий,
-- обычные - средний,
-- второстепенные - низкий;
-- также добавляет бонусы за сильные сочетания симптомов
scoreDoctor doctor symptoms =
    fromIntegral (length (intersectIds (doctorRedFlags doctor) symptoms)) * redFlagWeight
  + fromIntegral (length (intersectIds (doctorKeySymptoms doctor) symptoms)) * keySymptomWeight
  + fromIntegral (length (intersectIds (doctorCommonSymptoms doctor) symptoms)) * commonSymptomWeight
  + fromIntegral (length (intersectIds (doctorSecondarySymptoms doctor) symptoms)) * secondarySymptomWeight
  + combinationBonus doctor symptoms

combinationBonus :: Doctor -> [SymptomId] -> Double
-- добавляет бонусы за клинически значимые сочетания:
-- красный флаг + ключевой симптом,
-- несколько ключевых симптомов,
-- несколько совпадений в целом
combinationBonus doctor symptoms =
  let redCount = length (intersectIds (doctorRedFlags doctor) symptoms)
      keyCount = length (intersectIds (doctorKeySymptoms doctor) symptoms)
      commonCount = length (intersectIds (doctorCommonSymptoms doctor) symptoms)
      totalCount = redCount + keyCount + commonCount
  in
      (if redCount > 0 && keyCount > 0 then 20 else 0)
    + (if keyCount >= 2 then 10 else 0)
    + (if totalCount >= 3 then 5 else 0)

matchingSymptoms :: Doctor -> [SymptomId] -> [SymptomId]
-- возвращает все совпавшие симптомы врача:
-- сначала красные флаги, потом ключевые, потом обычные, потом второстепенные
matchingSymptoms doctor symptoms =
     intersectIds (doctorRedFlags doctor) symptoms
  ++ intersectIds (doctorKeySymptoms doctor) symptoms
  ++ intersectIds (doctorCommonSymptoms doctor) symptoms
  ++ intersectIds (doctorSecondarySymptoms doctor) symptoms

sortRankedDoctors :: [(Doctor, Double, [SymptomId])] -> [(Doctor, Double, [SymptomId])]
sortRankedDoctors = sortBy compareRank
-- sortBy принимает функцию сравнения и список, и сортирует список в соответствии с этой функцией
  where
    compareRank (_, scoreA, _) (_, scoreB, _) = compare scoreB scoreA

significantlyBetter :: Double -> [(Doctor, Double, [SymptomId])] -> Bool
-- проверяет, насколько лучший врач опережает следующего:
-- если разница достаточно большая, можно вернуть одного врача,
-- иначе лучше показать список
significantlyBetter _ [] = True
significantlyBetter bestScore ((_, secondScore, _):_) =
  bestScore - secondScore >= 10

intersectIds :: Eq a => [a] -> [a] -> [a]
intersectIds xs ys = [ x | x <- xs, x `elem` ys ]