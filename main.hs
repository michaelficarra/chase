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
import Control.Monad

main = do
	formulae <- getContents
	let parseTrees = generate (scanTokens formulae)
	time <- getCurrentTime
	-- putStrLn $ prettyPrintArray (map showFormula parseTrees)

	-- modelAStr <- loadModel "A"
	-- let modelA = parseModel modelAStr
	-- putStrLn $ "model A: " ++ showModel modelA

	-- chase function tests
	putStrLn "--- chase 0 ---"
	putStrLn.prettyPrintArray $ map showFormula theory0
	putStrLn.prettyPrintArray $ map showModel generatedModels0

	let timeStr = formatTime (defaultTimeLocale) "%s" time
	let modelDir = "models"
	modelDirExists <- doesDirectoryExist modelDir
	let modelOutputDir = modelDir ++ "/" ++ timeStr
	unless modelDirExists (createDirectory modelDir)
	createDirectory modelOutputDir
	writeModelsToFiles modelOutputDir generatedModels0

	-- putStrLn "--- chase 1 ---"
	-- putStrLn.prettyPrintArray $ map showFormula theory1
	-- putStrLn.prettyPrintArray $ map showModel generatedModels1

	where
		prettyPrintArray arr = if length arr == 1 then "[ " ++ (head arr) ++ " ]" else "[ " ++ (intercalate "\n, " arr) ++ "\n]"

		generatedModels0 = chase theory0
		theory0 = generate.scanTokens $
			"-> Exists y,z: R[y,z]" ++ "\n" ++
			"R[x,w] -> (Exists y: Q[x,y]) | (Exists z: P[x,z])" ++ "\n" ++
			"Q[u,v] -> (Exists z: R[u,z]) | (Exists z: R[z,w])" ++ "\n" ++
			"P[u,v] -> Contradiction"

		generatedModels1 = chase theory1
		theory1 = generate.scanTokens $
			"-> Exists a,b: P[a,b,b]" ++ "\n" ++
			"R[a,b] -> (Exists y: Q[a,y]) | (Exists z,b: P[z,b,a])" ++ "\n" ++
			"Q[a,a] -> Exists z: R[a,z]" ++ "\n" ++
			"P[a,b,c] -> R[a,a] & Q[b,b]"

