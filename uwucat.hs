import Data.Text (pack, unpack, replace)
import System.Exit (exitSuccess)
import System.Environment (getArgs)

-- Replace all 'l' and 'r' in string with 'w'
-- Example: "Hello, World!" -> "Hewwo, Wowwd!"
uwuChar :: String -> String
uwuChar = map replace where
    replace 'l' = 'w'
    replace 'r' = 'w'
    replace 'L' = 'W'
    replace 'R' = 'W'
    replace char = char

-- Replace "ove" with "uv"
-- Example: "love" -> "luv"
uwuOve :: String -> String
uwuOve = unpack . replace (pack "ove") (pack "uv") . pack

-- Apply all uwu transformations
uwu :: String -> String
uwu = uwuChar . uwuOve

-- Apply uwu to a file's contents
uwuFile :: String -> IO String
uwuFile path = do
    contents <- readFile path
    return $ uwu contents

-- Arguments handling
parseArgs :: [String] -> IO ()
parseArgs ["-h"] = putStr "Usage: uwucat [-vh] [file ...]" >> exit
parseArgs ["-v"] = putStr "uwucat 0.1" >> exit
-- If no arguments then get contents from stdin
parseArgs []     = do
    contents <- getContents
    putStr $ uwu contents
    exit
-- Otherwise get contents from each file in arguments
parseArgs args   = do
    contents <- mapM uwuFile args
    mapM_ putStr contents
    exit

exit :: IO ()
exit = exitSuccess

main :: IO ()
main = getArgs >>= parseArgs
