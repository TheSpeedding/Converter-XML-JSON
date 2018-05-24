# XML/JSON Converter
Application written in Haskell offering conversions between XML and JSON formatted files.

It can parse both XML and JSON files. For this purpose, it uses PidiParsec library (lightweight version of Parsec) and internal data representation in abstract (not showable) format. Using this format, conversion functions can produce JSON and XML formatted files.

# Functions
There are 2 conversion functions for XML and 2 conversion functions for JSON.
It loads XML/JSON formatted file and can produce these outputs. Note that if the parsing fails, an exception is thrown.

1. `jsonToJson :: FilePath -> Int -> IO ()`</li> converts JSON file to JSON file. This may be useful to do some reformatting, indentation. The first argument is path to the file, the second argument is number of spaces in indentation. The output is written to the file specified in the first argument, prefixes with "new_".
2. `jsonToXml :: FilePath -> Int -> IO ()`</li> converts JSON file to XML file. Arguments are the same, the only difference that the output file has the same name as the input one, but the extension is changed to "*.xml"

Functions `xmlToJson` and `xmlToXml` have the same semantics, as you may expect.

# Examples
                     