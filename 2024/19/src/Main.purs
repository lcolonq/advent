module Main where

import Prelude

import Data.Array as Arr
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

possible :: Array String -> String -> Boolean
possible _ "" = true
possible towels design = Arr.any after towels
  where
    after :: String -> Boolean
    after towel = case Str.stripPrefix (Pattern towel) design of
      Nothing -> false
      Just rest -> possible towels rest

possibleCount :: Ref.Ref (Map.Map (Tuple (Array String) String) Number) -> Array String -> String -> Effect Number
possibleCount _ _ "" = pure 1.0
possibleCount rm towels design = Ref.read rm >>= Map.lookup (Tuple towels design) >>> case _ of
  Just res -> pure res
  Nothing -> do
    let
      viable :: Array String
      viable = Arr.filter (\t -> Str.contains (Pattern t) design) towels
      after :: String -> Effect Number
      after towel = case Str.stripPrefix (Pattern towel) design of
        Nothing -> pure 0.0
        Just rest -> possibleCount rm viable rest
    res <- Arr.foldM (\acc x -> (acc + _) <$> after x) 0.0 viable
    Ref.modify_ (Map.insert (Tuple towels design) res) rm
    pure res

main :: Effect Unit
main = do
  inp <- readTextFile UTF8 "input.txt"
  let sp = Str.split (Pattern "\n\n") inp
  Tuple towels designs <- case sp of
    [stowels, sdesigns] -> do
      pure $ Tuple
        (Str.split (Pattern ", ") stowels)
        (Arr.filter (not <<< Str.null) $ Str.split (Pattern "\n") sdesigns)
    _ -> throw "failed to parse input"
  rm <- Ref.new $ Map.empty
  let pos = Arr.filter (possible towels) designs
  log $ show $ Arr.length pos
  log <<< show =<< Arr.foldl (+) 0.0 <$> traverse (possibleCount rm towels) pos
