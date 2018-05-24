# XML/JSON Converter
Application written in Haskell offering conversions between XML and JSON formatted files.

It can parse both XML and JSON files. For this purpose, it uses PidiParsec library (lightweight version of Parsec) and internal data representation in abstract (not showable) format. Using this format, conversion functions can produce JSON and XML formatted files.

# API functions
There are 2 conversion functions for XML and 2 conversion functions for JSON.
It loads XML/JSON formatted file and can produce these outputs. Note that if the parsing fails, an exception is thrown.

1. `jsonToJson :: FilePath -> Int -> IO ()` converts JSON file to JSON file. This may be useful to do some reformatting, indentation. The first argument is path to the file, the second argument is number of spaces in indentation. The output is written to the file specified in the first argument, prefixed with "new_".
2. `jsonToXml :: FilePath -> Int -> IO ()` converts JSON file to XML file. Arguments are the same, the only difference is that the output file has the same name as the input one, but the extension is changed to "*.xml".

Functions `xmlToJson` and `xmlToXml` have the same semantics, as you may expect.

# CSV parser
The program can parse CSV files as well. You can use `csvtoXml` and `csvtoJson` functions for this purpose. As you may noticed, there is no equivalent for outputting the CSV files. They only can be parsed and outputted as XML or JSON. You can also see that the functions are transitive. If you use `jsonToXml` applied on the result of `csvToJson`, the result should be the same as the output of `csvToXml` function.

# Code distribution
The code is logically distributed into three modules.
1. *Parsers* module. This module contains everything necessary for parsing XML, JSON and CSV files.
2. *Printers* module. This module contains everytthing necessary for pritnting XML and JSON.
3. *Conversions* module. This module combines previously mentioned modules, contains conversion functions.

# Examples 
 Suppose following, very poorly formatted and without any indentation, JSON document, saved as "data.json" file. It was downloaded from [Transit Feed API](https://api.transitfeeds.com/v1), shortened a little bit and it represents "all" the locations that are transit feeds available for. 

 > {"status":"OK","ts":1527170641,"results":{"locations":[{"id":606,"pid":168,"t":"Aachen, Germany","n":"Aachen","lat":50.775346,"lng":6.083887},{"id":416,"pid":415,"t":"Addison County, VT, USA","n":"Addison County","lat":44.119729,"lng":-73.164338},{"id":4,"pid":3,"t":"Adelaide SA, Australia","n":"Adelaide","lat":-34.928621,"lng":138.599959},{"id":99,"pid":0,"t":"Africa","n":"Africa","lat":-8.783195,"lng":34.508523},{"id":11,"pid":9,"t":"Airlie Beach QLD 4802, Australia","n":"Airlie Beach","lat":-20.26872,"lng":148.718456},{"id":237,"pid":31,"t":"Alabama, USA","n":"Alabama","lat":32.318231,"lng":-86.902298},{"id":276,"pid":31,"t":"Alaska, USA","n":"Alaska","lat":64.200841,"lng":-149.493673},{"id":85,"pid":84,"t":"Albany, NY, USA","n":"Albany","lat":42.652579,"lng":-73.756232},{"id":328,"pid":63,"t":"Albany, OR, USA","n":"Albany","lat":44.636511,"lng":-123.105928},{"id":42,"pid":32,"t":"Alberta, Canada","n":"Alberta","lat":53.933271,"lng":-116.576504},{"id":81,"pid":80,"t":"Albuquerque, NM, USA","n":"Albuquerque","lat":35.110703,"lng":-106.609991},{"id":324,"pid":138,"t":"Alexandria, VA, USA","n":"Alexandria","lat":38.804836,"lng":-77.046921},{"id":375,"pid":373,"t":"Alice Springs NT 0870, Australia","n":"Alice Springs","lat":-23.70021,"lng":133.880611},{"id":134,"pid":133,"t":"Allegany, MD, USA","n":"Allegany","lat":39.625525,"lng":-78.6115},{"id":459,"pid":105,"t":"Allentown, PA, USA","n":"Allentown","lat":40.60843,"lng":-75.490183},]}}    

 Using `jsonToJson "data.json" 3`, it can be converted to "new_data.json" file where one tab holds for three spaces. The output is following.                    
