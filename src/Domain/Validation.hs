module Domain.Validation where

import ClassyPrelude
import Text.Regex.PCRE.Heavy

type Validation e a = a -> Maybe e

validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate constructor validations val =
  case concatMap (\f -> maybeToList $ f val) validations of
    [] -> Right $ constructor val
    errs -> Left errs

rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween minRange maxRange errMsg val =
  if val >= minRange && val <= maxRange
     then Nothing
     else Just errMsg

lengthBetween :: (MonoFoldable a) => Int -> Int -> e -> Validation e a
lengthBetween minLength maxLength errMsg val =
  rangeBetween minLength maxLength errMsg (length val)

regexMatches :: Regex -> e -> Validation e Text
regexMatches regex errMsg text =
  if text =~ regex
     then Nothing
     else Just errMsg
