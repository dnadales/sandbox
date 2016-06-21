module Main where

import           Data.BinTree
import           Data.Either
import qualified Data.Map.Strict     as Map
import           Options.Applicative
import           Text.Read

data Opts = Opts
  { version  :: String
  , treeSize :: Integer }

parseNumber :: Parser Integer
parseNumber =
  option auto
  (  long  "size"
  <> short 's'
  <> metavar "SIZE"
  <> help "Number of leaves of the tree")

optsParser :: Parser Opts
optsParser = Opts
  <$> strOption
  (  long "version"
  <> short 'v'
  <> help "version of the algorithm to run" )
  <*> parseNumber


runSafe :: Opts -> IO ()
runSafe opts = either reportErrors run (validateOpts opts)

reportErrors :: [String] -> IO ()
reportErrors = mapM_ putStrLn

validateOpts :: Opts -> Either [String] Opts
validateOpts opts@(Opts v s) =
  if null ls then Right opts
  else Left ls
  where ls = lefts [validateVersion v, validateSize s]

validateVersion :: String -> Either String ()
validateVersion v = if (v `Map.member` versions)
                    then Right ()
                    else Left $ v ++ " is not a version"

lower :: Integer
lower = 0

upper :: Integer
upper = 10^8

validateSize :: Integer -> Either String ()
validateSize s = if  lower <= s && s < upper
                 then Right ()
                 else Left $ (show s)
                      ++ " is not a valid size. It should be in the range ["
                      ++ (show lower)
                      ++ ".."
                      ++ (show upper)
                      ++ "]"

run :: Opts -> IO ()
run (Opts v s) = putStrLn $ "sum = " ++ (show total)
  where total = sum $ (prog) (mkTree [0 .. s])
        prog = Map.findWithDefault (const []) v versions


-- Versions what we will test
versions :: Map.Map String (BinTree a -> [a])
versions = Map.fromList
  [ ("leaves", leaves)
  , ("leavesT", leavesT)
  , ("leavesComp", leaves')
  , ("leavesS", leavesS)]

main :: IO ()
main = execParser opts >>= runSafe
  where opts = info (helper <*> optsParser)
               (fullDesc <> progDesc "benchmark leaves of a tree")
