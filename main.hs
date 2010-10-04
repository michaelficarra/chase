module Main where
import Lexer
import Parser
import Helpers
import Chase
import Data.List
import Data.Time
import Data.Time.Clock
import System.Locale
import System.Directory
import System
import System.Console.GetOpt
import Data.Maybe( fromMaybe )
import Control.Monad

data Options = Options  {
    optInput  :: IO String,
    optOutput :: [Model] -> IO ()
  }

defaultOptions :: Options
defaultOptions = Options {
    optInput  = getContents,
    optOutput = putStrLn.(intercalate "\n").(map showModel)
  }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['V'] ["version"] (NoArg showVersion)         "show version number",
    Option ['i'] ["input"]   (ReqArg readInput "FILE")   "input file to read",
    Option ['o'] ["output"]  (OptArg writeOutput "FILE") "output file to write"
  ]

showVersion _ = do
  putStrLn "0.1.12"
  exitWith ExitSuccess

readInput arg opt = return opt { optInput = readFile arg }
writeOutput arg opt = do
	time <- getCurrentTime
	let timeStr = formatTime (defaultTimeLocale) "%s" time
	let modelDir = fromMaybe "models" arg
	let modelOutputDir = modelDir ++ "/" ++ timeStr
	modelDirExists <- doesDirectoryExist modelDir
	unless modelDirExists (createDirectory modelDir)
	createDirectory modelOutputDir
	return opt { optOutput = writeModelsToFiles modelOutputDir }


main = do
	let parse = generate.scanTokens
	args <- getArgs
	let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
	opts <- foldl (>>=) (return defaultOptions) actions
	let Options { optInput = wrappedInput, optOutput = output } = opts
	input <- wrappedInput
	putStrLn input
	output (chase $ parse $ input)

	where
		prettyPrintArray arr = if length arr == 1 then "[ " ++ (head arr) ++ " ]" else "[ " ++ (intercalate "\n, " arr) ++ "\n]"
