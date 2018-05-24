module Conversions where

    import PidiParsec
    import Printers
    import Parsers
    import Data.Maybe
    import Data.List.Split
                                                                
    jsonToJson :: FilePath -> Int -> IO () -- might be useful when reformatting (indentation, unreadable json to readable one)
    jsonToJson inputFile indent = let
                                    extension = head $ tail $ splitOn "." inputFile
                                    fileName = head $ splitOn "." inputFile
                                    newFileName = "new_" ++ fileName ++ ".json"
                                in
                                    do
                                    contents <- readFile inputFile
                                    let parsed = doParseEof parseJson contents
                                    writeFile newFileName (printJson (fromJust parsed) indent)


    jsonToXml :: FilePath -> Int -> IO ()
    jsonToXml inputFile indent = let
                                    extension = head $ tail $ splitOn "." inputFile
                                    fileName = head $ splitOn "." inputFile
                                    newFileName = fileName ++ ".xml"
                                in
                                    do
                                    contents <- readFile inputFile
                                    let parsed = doParseEof parseJson contents
                                    writeFile newFileName (printXml (fromJust parsed) indent)


    xmlToXml :: FilePath -> Int -> IO () -- might be useful when reformatting (indentation, unreadable xml to readable one)
    xmlToXml inputFile indent = let
                                    extension = head $ tail $ splitOn "." inputFile
                                    fileName = head $ splitOn "." inputFile
                                    newFileName = "new_" ++ fileName ++ ".xml"
                                in
                                    do
                                    contents <- readFile inputFile
                                    let parsed = doParseEof parseXml contents
                                    writeFile newFileName (printXml (fromJust parsed) indent)


    xmlToJson :: FilePath -> Int -> IO ()
    xmlToJson inputFile indent = let
                                    extension = head $ tail $ splitOn "." inputFile
                                    fileName = head $ splitOn "." inputFile
                                    newFileName = fileName ++ ".json"
                                in
                                    do
                                    contents <- readFile inputFile
                                    let parsed = doParseEof parseXml contents
                                    writeFile newFileName (printJson (fromJust parsed) indent)