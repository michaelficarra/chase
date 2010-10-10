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

data OutputFormat = MachineReadable | HumanReadable

data Options = Options
	{ optDebug  :: Bool
	, optInput  :: IO String
	, optOutputDir :: Maybe String
	, optOutputFormat :: OutputFormat
	}

defaultOptions :: Options
defaultOptions = Options
	{ optDebug  = False
	, optInput  = getContents
	, optOutputDir = Nothing
	, optOutputFormat = HumanReadable
	}

options :: [OptDescr (Options -> IO Options)]
options =
	[ Option ['i'] ["input"]           (ReqArg readInput "FILE")   "input file containing geometric theory\nstdin if omitted"
	, Option ['o'] ["output"]          (OptArg writeOutput "FILE") "output directory; defaults to \"models\"\nstdout if omitted\nimplies -m"
	, Option ['d'] ["debug","trace"]   (NoArg enableDebug)         "enable tracing output (not yet implemented)"
	, Option ['h'] ["human"]           (NoArg humanReadable)       "switch output format to a human-readable form"
	, Option ['m'] ["machine"]         (NoArg machineReadable)     "switch output format to a machine-readable form"
	, Option ['V'] ["version"]         (NoArg showVersion)         "show version number and exit"
	, Option ['?'] ["help"]            (NoArg showHelp)            "show this usage text and exit"
	]

showVersion _ = do
	putStrLn "0.1.13"
	exitWith ExitSuccess

enableDebug opt = return opt { optDebug = True }

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
	return opt { optOutputFormat = MachineReadable, optOutputDir = Just modelOutputDir }

machineReadable opt = return opt { optOutputFormat = MachineReadable }
humanReadable opt = return opt { optOutputFormat = HumanReadable }


main = do
	let parse = generate.scanTokens
	args <- getArgs
	let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
	opts <- foldl (>>=) (return defaultOptions) actions
	input <- optInput opts
	let outputDir = optOutputDir opts
	putStrLn.prettyPrintArray.(map show).order $ parse input
	let models = chase $ parse input
	let formatter = case optOutputFormat opts of
		HumanReadable -> showModel
		MachineReadable -> exportModel
	case outputDir of
		Nothing -> putStrLn.(intercalate "\n") $ map formatter models
		Just directory -> writeModelsToFiles formatter directory models

	where
		prettyPrintArray arr = if length arr == 1 then "[ " ++ (head arr) ++ " ]" else "[ " ++ (intercalate "\n, " arr) ++ "\n]"
