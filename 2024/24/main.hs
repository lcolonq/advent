{-# Language OverloadedStrings, PatternGuards, MultiWayIf, LambdaCase #-}

import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import Data.Bits ((.&.), (.|.), (.^.), shiftL, shiftR)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Graph as Graph

newtype Name = Name Text
  deriving (Show, Eq, Ord)

data Op = OpAnd | OpOr | OpXor
  deriving (Show, Eq, Ord)

op :: Op -> Word64 -> Word64 -> Word64
op OpAnd = (.&.)
op OpOr = (.|.)
op OpXor = (.^.)

data Gate = Gate Op Text Text Text
  deriving (Show, Eq, Ord)

parse :: Text -> (Map Text Word64, [Gate]) 
parse inp = (initial, gates)
  where
    (is, gs) = case Text.splitOn "\n\n" inp of [is, gs] -> (is, gs); _ -> error ":3"
    parseVar t = case Text.splitOn ": " t of [v, i] -> Just (v, read $ Text.unpack i); _ -> Nothing
    initial = Map.fromList $ mapMaybe parseVar $ Text.splitOn "\n" is
    opFrom o = case o of "AND" -> OpAnd; "OR" -> OpOr; "XOR" -> OpXor; _ -> error ":5"
    parseGate g = case Text.splitOn " " g of [i0, o, i1, _, r] -> Just $ Gate (opFrom o) r i0 i1; _ -> Nothing
    gates = mapMaybe parseGate $ Text.splitOn "\n" gs

toposort :: [Gate] -> [Gate]
toposort gs = (\v -> let (g, _, _) = nodeFromVertex v in g) <$> vs
  where
    (graph, nodeFromVertex, _) = Graph.graphFromEdges $
      (\g@(Gate _ r i0 i1) -> (g, r, [i0, i1])) <$> gs
    vs = Graph.reverseTopSort graph

parseBin :: [Word64] -> Word64
parseBin = go 0
  where
    go :: Word64 -> [Word64] -> Word64
    go acc [] = acc
    go acc (0:xs) = go (shiftL acc 1) xs
    go acc (1:xs) = go (shiftL acc 1 .|. 1) xs
    go acc (_:xs) = go acc xs

unparseBin :: Word64 -> [Word64]
unparseBin = go []
  where
    go :: [Word64] -> Word64 -> [Word64]
    go acc 0 = reverse acc
    go acc n = go ((n .&. 1):acc) (shiftR n 1)

getBitvector :: Map Text Word64 -> Text -> [Word64]
getBitvector st nm = reverse $ mapMaybe
  (\(r, v) -> if Text.isPrefixOf nm r
    then Just v
    else Nothing
  ) $ Map.toList st

computeBits :: Map Text Word64 -> [Gate] -> [Word64]
computeBits initial gates = reverse zs
  where
  final = List.foldl'
    (\st (Gate o r i0 i1) ->
        case (Map.lookup i0 st, Map.lookup i1 st) of
          (Just v0, Just v1) -> Map.insert r (op o v0 v1) st
          _ -> error "bad ordering"
    ) initial gates
  zs = getBitvector final "z"

sumInputs :: Map Text Word64 -> Word64
sumInputs st = parseBin (getBitvector st "x") + parseBin (getBitvector st "y")

compute :: Map Text Word64 -> [Gate] -> Word64
compute initial = parseBin . computeBits initial

allNames :: [Text]
allNames = do
  c0 <- ['a'..'w']
  c1 <- ['a'..'w']
  c2 <- ['a'..'w']
  pure $ Text.pack [c0, c1, c2]

toName :: Text -> Int -> Text
toName pfx i = pfx <> Text.pack (replicate (2 - length base) '0' <> base)
  where
    base = show i 

checkZ :: (Text -> Gate) -> Int -> [Text]
checkZ g 0 = case g $ toName "z" 0 of
  Gate OpXor _ y x
    | x /= "x00" -> [x]
    | y /= "y00" -> [y]
  _ -> []
checkZ g n = case g $ toName "z" n of
  Gate o r s c
    | o /= OpXor -> [r]
    | Gate os rs x y <- g s, os /= OpXor || x /= toName "x" n || y /= toName "y" n -> [rs]
    | Gate oc rc cc psa <- g c -> if
        | oc /= OpOr -> [rc]
        | Gate occ rcc x' y' <- g cc, occ /= OpAnd -> [rcc]
        | Gate occ rcc x' y' <- g cc, x' /= toName "x" (n - 1) -> [x']
        | Gate occ rcc x' y' <- g cc, y' /= toName "y" (n - 1) -> [y']
        | Gate opsa rpsa i0 i1 <- g psa, Gate _ _ e0 e1 <- g . toName "z" $ n - 1
        , opsa /= OpAnd -> [rpsa]
        | Gate opsa rpsa i0 i1 <- g psa, Gate _ _ e0 e1 <- g . toName "z" $ n - 1
        , i0 /= e0 -> [i0]
        | Gate opsa rpsa i0 i1 <- g psa, Gate _ _ e0 e1 <- g . toName "z" $ n - 1
        , i1 /= e1 -> [i1]

findCarries :: (Text -> Maybe Gate) -> [Gate] -> Map Int Text
findCarries g gates = go (Map.fromList [(0, "fgw"), (1, "wwp")]) 2
  where
    go :: Map Int Text -> Int -> Map Int Text
    go acc 44 = acc
    go acc n = go (Map.insert n c acc) $ n + 1
      where
        isBaseGate nm o m = case g nm of
          Just (Gate o' _ i0 i1) -> o == o'
            && ((i0 == toName "x" m && i1 == toName "y" m) || (i0 == toName "y" m && i1 == toName "x" m))
        isSubAnd nm = case g nm of
          Just (Gate OpAnd _ k0 k1) ->
            (isBaseGate k0 OpXor n && Just k1 == Map.lookup (n - 1) acc)
            || (isBaseGate k1 OpXor n && Just k0 == Map.lookup (n - 1) acc)
          _ -> False
        isCarry (Gate OpOr _ k0 k1) = 
            (isBaseGate k0 OpAnd n && isSubAnd k1)
            || (isBaseGate k1 OpAnd n && isSubAnd k0)
        isCarry _ = False
        c = case filter isCarry gates of
          [Gate _ c _ _] -> c
          [] -> error $ "no options for " <> show n <> " (" <> show acc <> ")"
          _ -> error $ "multiple options for " <> show n <> ": " <> show (filter isCarry gates)

main :: IO ()
main = do
  inp <- Text.IO.readFile "test/test4.txt"
  let (initial, gs) = parse inp
  let gates = toposort gs
  let gm = Map.fromList $ (\g@(Gate _ r _ _) -> (r, g)) <$> gates
  let g r = Map.lookup r gm
  Text.IO.putStrLn $ Text.pack $ show $ findCarries g gates
  Text.IO.putStrLn $ Text.pack $ show $ computeBits initial gates
  Text.IO.putStrLn $ Text.pack $ show $ unparseBin $ sumInputs initial
