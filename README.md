# XMON
Application written in Haskell offering conversions between XML and JSON formatted files.

It can parse both XML and JSON files. For this purpose, it uses PidiParsec library (lightweight version of Parsec) and internal data representation in abstract (not showable) format. Using this format, conversion functions can produce JSON and XML formatted files.

# API functions
There are 2 conversion functions for XML and 2 conversion functions for JSON.
It loads XML/JSON formatted file and can produce these outputs. Note that if the parsing fails, an exception is thrown.

1. `jsonToJson :: FilePath -> Int -> IO ()` converts JSON file to JSON file. This may be useful to do some reformatting, indentation. The first argument is path to the file, the second argument is number of spaces in indentation. The output is written to the file specified in the first argument, prefixed with `new_`.
2. `jsonToXml :: FilePath -> Int -> IO ()` converts JSON file to XML file. Arguments are the same, the only difference is that the output file has the same name as the input one, but the extension is changed to `*.xml`.

Functions `xmlToJson` and `xmlToXml` have the same semantics, as you may expect.

# Code division
The code is logically divided into three modules.
1. *Parsers* module. This module contains everything necessary for parsing XML and JSON files.
2. *Printers* module. This module contains everytthing necessary for printing XML and JSON.
3. *Conversions* module. This module combines previously mentioned modules, contains conversion functions.

# Notes
Although number can be treated as a numeric value in XML, it is always parsed as a string. The same holds for boolean values and null values. That's because XML does not support numeric data type (it can be interpreted as a numeric data type using XML Schema), everything is treated as a string. You can also see that attributes are converted to JSON objects, since JSON does not support attributes. You may notice that applying `xmlToJson` and `jsonToXml` functions in-order leads to different XML, since there's no binding between origin XML attribute - JSON objects are converted to XML elements. You can get symmetric function after two iterations, when you convert JSON with translated attributes into the XML document... Anyway, applying `xmlToXml` function leads to the same XML document.

The only thing both parsers cannot handle is parsing comments. You also cannot use mixed elements (text along with sublements), because it would violate JSON concept. The XML document to be parsed must not start with XML header, otherwise it would not parse. These minor bugs are intended to be a future work. It's also planned to implement parsing of JSON "attributes" to the real XML attributes. 

Parameter for indentation is not supported in current version, PrettyPrinter will be implemented ASAP.
