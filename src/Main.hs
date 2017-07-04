module Main where

import System.Environment
import System.Directory
import System.FilePath

main :: IO ()
main =
    getArgs >>= parse

parse [fileName] = whereIs fileName
parse [] = putStrLn "Will find a file entered as cmd line arg."

filesInDirectory :: FilePath -> IO([FilePath])
filesInDirectory fp = do
    isDir <- doesDirectoryExist fp
    if isDir then do
        files <- getDirectoryContents fp
        res <- sequence.map (filesInDirectory.((fp++"/")++)).filter (/=".").filter (/="..") $ files
        return $ concat res
    else
        return [fp]

whereIs fn = do
    content <- filesInDirectory "."
    putStrLn.head $ filter ((fn==).takeFileName) content


