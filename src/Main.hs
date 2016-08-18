module Main where

import System.Environment
import System.Directory
import Anca.Pipe
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
        res <- files |> filter (/="..") |> filter (/=".")|> map ((fp++"/")++) |> map filesInDirectory |> sequence
        return $ res |> concat 
    else
        return [fp]

whereIs fn = do
    content <- filesInDirectory "."
    content |> filter ((fn==).takeFileName) |> head |> putStrLn


