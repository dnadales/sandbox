-- |

module ExercisesTraversable where

import           Data.Either.Validation

parseInt :: String -> Either String Int
parseInt str =
  case reads str of
    [(i, "")] -> Right i
    _ -> Left $ "could not parse " ++ str

parseIntV :: String -> Validation [String] Int
parseIntV str =
  case parseInt str of
    Right i -> Success i
    Left err -> Failure [err]

