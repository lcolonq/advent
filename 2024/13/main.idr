module Main

import System.File.ReadWrite
import Data.String

data Machine : Type where
  M : (Int, Int) -> (Int, Int) -> (Int, Int) -> Machine
implementation Show Machine where
  show (M a b p) = "(M " <+> show a <+> " " <+> show b <+> " " <+> show p <+> ")"

aFromB : Machine -> Int -> Maybe Int
aFromB (M (ax, ay) (bx, by) (px, py)) b =
  if denom == 0
  then Just 19 -- call that a gaming maneuver
  else if mod num denom == 0
    then Just (div num denom)
    else Nothing
  where
    num = (bx - by)*b + (py - px)
    denom = ay - ax

score : Machine -> Int -> Maybe Int
score (M (ax, ay) (bx, by) (px, py)) b =
  Just (3 * div ((bx - by)*b + (py - px)) (ay - ax) + b)

trial : Machine -> Int -> Bool
trial m@(M (ax, ay) (bx, by) (px, py)) b = case aFromB m b of
  Just a => px == ax * a + bx * b && py == ay * a + by * b
  Nothing => False

leastTrial : Machine -> Maybe Int
leastTrial m =
  foldl
  (\acc => \b =>
    case (acc, score m b) of
      (Nothing, r) => r
      (r, Nothing) => r
      (Just accs, Just s) => if s < accs then Just s else acc
  )
  Nothing
  $ filter (\b => trial m b) [ b | b <- [0..100] ]

leastTrialFastBig : Machine -> Maybe Int
leastTrialFastBig (M (ax, ay) (bx, by) (ppx, ppy)) =
  if mod anum adenom == 0 && mod bnum bdenom == 0
  then Just $ 3 * (div anum adenom) + (div bnum bdenom)
  else Nothing
  where -- wolfram alpha save me
    px = ppx + 10000000000000
    py = ppy + 10000000000000
    anum = by * px - bx * py
    adenom = by * ax - bx * ay
    bnum = ax * py - ay * px
    bdenom = ax * by - ay * bx

splitMachines : String -> List String
splitMachines = go [] . unpack
  where
    go : List Char -> List Char -> List String
    go acc ('\n'::'\n'::rest) = pack (reverse acc) :: go [] rest
    go acc (x::rest) = go (x::acc) rest
    go acc [] = [pack $ reverse acc]

splitCommaSpace : String -> List String
splitCommaSpace = go [] . unpack
  where
    go : List Char -> List Char -> List String
    go acc (','::' '::rest) = pack (reverse acc) :: go [] rest
    go acc (x::rest) = go (x::acc) rest
    go acc [] = [pack $ reverse acc]
  
parseLine : Nat -> String -> Maybe (Int, Int)
parseLine d inp = case splitCommaSpace $ pack $ drop d $ unpack inp of
  [xs, ys] => (,) <$> parseInteger xs <*> parseInteger (pack $ drop 2 $ unpack ys)
  _ => Nothing

parseMachine : String -> Maybe Machine
parseMachine s = case lines s of
  [al, bl, pl] => M <$> parseLine 12 al <*> parseLine 12 bl <*> parseLine 9 pl
  _ => Nothing

parse : String -> List Machine
parse = mapMaybe parseMachine . splitMachines

main : IO ()
main = do
  readFile "input.txt" >>= \case
    Left err => putStrLn $ "error: " <+> show err
    Right inp => do
      let ms = parse inp
      let costs = mapMaybe leastTrialFastBig ms
      putStrLn $ show $ sum costs
