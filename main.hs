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

data Options = Options
	{ optDebug  :: Bool
	, optInput  :: IO String
	, optOutput :: [Model] -> IO ()
	}

defaultOptions :: Options
defaultOptions = Options
	{ optDebug  = False
	, optInput  = getContents
	, optOutput = putStrLn.(intercalate "\n").(map showModel)
	}

options :: [OptDescr (Options -> IO Options)]
options =
	[ Option ['i'] ["input"]           (ReqArg readInput "FILE")   "input file containing geometric theory; stdin if omitted"
	, Option ['o'] ["output"]          (OptArg writeOutput "FILE") "output directory; defaults to \"models\"; stdout if omitted"
	, Option ['d'] ["debug","trace"]   (NoArg enableDebug)         "enable tracing output (not yet implemented)"
	, Option ['V'] ["version"]         (NoArg showVersion)         "show version number and exit"
	, Option ['?'] ["help"]            (NoArg showHelp)            "show this usage text and exit"
	]

showVersion _ = do
	putStrLn "0.1.13"
	exitWith ExitSuccess

enableDebug opt = do
	return opt { optDebug = True }

showHelp _ = do
	putStrLn $ usageInfo "\n  Usage: chase [option...] \n" options
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
