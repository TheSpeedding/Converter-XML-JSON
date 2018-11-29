# XML-JSON
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

# Examples 
### JSON to JSON with indentation && JSON to XML example
 Suppose following, very poorly formatted and without any indentation, JSON document, saved as `data.json` file. It was downloaded from [Transit Feed API](https://api.transitfeeds.com/v1), shortened a little bit and it represents "all" the locations that are transit feeds available for. 

```
 {"status":"OK","ts":1527170641,"results":{"locations":[{"id":606,"pid":168,"t":"Aachen, Germany","n":"Aachen","lat":"50.775346","lng":"6.083887"},{"id":416,"pid":415,"t":"Addison County, VT, USA","n":"Addison County","lat":"44.119729","lng":"-73.164338"},{"id":4,"pid":3,"t":"Adelaide SA, Australia","n":"Adelaide","lat":"-34.928621","lng":"138.599959"},{"id":99,"pid":0,"t":"Africa","n":"Africa","lat":"-8.783195","lng":"34.508523"},{"id":11,"pid":9,"t":"Airlie Beach QLD 4802, Australia","n":"Airlie Beach","lat":"-20.26872","lng":"148.718456"},{"id":237,"pid":31,"t":"Alabama, USA","n":"Alabama","lat":"32.318231","lng":"-86.902298"},{"id":276,"pid":31,"t":"Alaska, USA","n":"Alaska","lat":"64.200841","lng":"-149.493673"},{"id":85,"pid":84,"t":"Albany, NY, USA","n":"Albany","lat":"42.652579","lng":"-73.756232"},{"id":328,"pid":63,"t":"Albany, OR, USA","n":"Albany","lat":"44.636511","lng":"-123.105928"},{"id":42,"pid":32,"t":"Alberta, Canada","n":"Alberta","lat":"53.933271","lng":"-116.576504"},{"id":81,"pid":80,"t":"Albuquerque, NM, USA","n":"Albuquerque","lat":"35.110703","lng":"-106.609991"},{"id":324,"pid":138,"t":"Alexandria, VA, USA","n":"Alexandria","lat":"38.804836","lng":"-77.046921"},{"id":375,"pid":373,"t":"Alice Springs NT 0870, Australia","n":"Alice Springs","lat":"-23.70021","lng":"133.880611"},{"id":134,"pid":133,"t":"Allegany, MD, USA","n":"Allegany","lat":"39.625525","lng":"-78.6115"},{"id":459,"pid":105,"t":"Allentown, PA, USA","n":"Allentown","lat":"40.60843","lng":"-75.490183"}]}}
 ```

 Using `jsonToJson "data.json" 3`, it can be converted to `new_data.json` file where one tab holds for three spaces. The output is following.                    

```
{
   "status": "OK",
   "ts": 1527170641,
   "results": {
      "locations": [ {
         "id": 606,
         "pid": 168,
         "t": "Aachen, Germany",
         "n": "Aachen",
         "lat": "50.775346",
         "lng": "6.083887"
      }, {
         "id": 416,
         "pid": 415,
         "t": "Addison County, VT, USA",
         "n": "Addison County",
         "lat": "44.119729",
         "lng": "-73.164338"
      }, {
         "id": 4,
         "pid": 3,
         "t": "Adelaide SA, Australia",
         "n": "Adelaide",
         "lat": "-34.928621",
         "lng": "138.599959"
      }, {
         "id": 99,
         "pid": 0,
         "t": "Africa",
         "n": "Africa",
         "lat": "-8.783195",
         "lng": "34.508523"
      }, {
         "id": 11,
         "pid": 9,
         "t": "Airlie Beach QLD 4802, Australia",
         "n": "Airlie Beach",
         "lat": "-20.26872",
         "lng": "148.718456"
      }, {
         "id": 237,
         "pid": 31,
         "t": "Alabama, USA",
         "n": "Alabama",
         "lat": "32.318231",
         "lng": "-86.902298"
      }, {
         "id": 276,
         "pid": 31,
         "t": "Alaska, USA",
         "n": "Alaska",
         "lat": "64.200841",
         "lng": "-149.493673"
      }, {
         "id": 85,
         "pid": 84,
         "t": "Albany, NY, USA",
         "n": "Albany",
         "lat": "42.652579",
         "lng": "-73.756232"
      }, {
         "id": 328,
         "pid": 63,
         "t": "Albany, OR, USA",
         "n": "Albany",
         "lat": "44.636511",
         "lng": "-123.105928"
      }, {
         "id": 42,
         "pid": 32,
         "t": "Alberta, Canada",
         "n": "Alberta",
         "lat": "53.933271",
         "lng": "-116.576504"
      }, {
         "id": 81,
         "pid": 80,
         "t": "Albuquerque, NM, USA",
         "n": "Albuquerque",
         "lat": "35.110703",
         "lng": "-106.609991"
      }, {
         "id": 324,
         "pid": 138,
         "t": "Alexandria, VA, USA",
         "n": "Alexandria",
         "lat": "38.804836",
         "lng": "-77.046921"
      }, {
         "id": 375,
         "pid": 373,
         "t": "Alice Springs NT 0870, Australia",
         "n": "Alice Springs",
         "lat": "-23.70021",
         "lng": "133.880611"
      }, {
         "id": 134,
         "pid": 133,
         "t": "Allegany, MD, USA",
         "n": "Allegany",
         "lat": "39.625525",
         "lng": "-78.6115"
      }, {
         "id": 459,
         "pid": 105,
         "t": "Allentown, PA, USA",
         "n": "Allentown",
         "lat": "40.60843",
         "lng": "-75.490183"
      } ]
   }
}
```

As you can see, it's nicely indented. You can get XML result as well, using `jsonToXml "data.json" 3`.

```
<status>
   OK
</status>
<ts>
   1527170641
</ts>
<results>
   <locations>
      <item>
         <id>
            606
         </id>
         <pid>
            168
         </pid>
         <t>
            Aachen, Germany
         </t>
         <n>
            Aachen
         </n>
         <lat>
            50.775346
         </lat>
         <lng>
            6.083887
         </lng>
      </item>
      <item>
         <id>
            416
         </id>
         <pid>
            415
         </pid>
         <t>
            Addison County, VT, USA
         </t>
         <n>
            Addison County
         </n>
         <lat>
            44.119729
         </lat>
         <lng>
            -73.164338
         </lng>
      </item>
      <item>
         <id>
            4
         </id>
         <pid>
            3
         </pid>
         <t>
            Adelaide SA, Australia
         </t>
         <n>
            Adelaide
         </n>
         <lat>
            -34.928621
         </lat>
         <lng>
            138.599959
         </lng>
      </item>
      <item>
         <id>
            99
         </id>
         <pid>
            0
         </pid>
         <t>
            Africa
         </t>
         <n>
            Africa
         </n>
         <lat>
            -8.783195
         </lat>
         <lng>
            34.508523
         </lng>
      </item>
      <item>
         <id>
            11
         </id>
         <pid>
            9
         </pid>
         <t>
            Airlie Beach QLD 4802, Australia
         </t>
         <n>
            Airlie Beach
         </n>
         <lat>
            -20.26872
         </lat>
         <lng>
            148.718456
         </lng>
      </item>
      <item>
         <id>
            237
         </id>
         <pid>
            31
         </pid>
         <t>
            Alabama, USA
         </t>
         <n>
            Alabama
         </n>
         <lat>
            32.318231
         </lat>
         <lng>
            -86.902298
         </lng>
      </item>
      <item>
         <id>
            276
         </id>
         <pid>
            31
         </pid>
         <t>
            Alaska, USA
         </t>
         <n>
            Alaska
         </n>
         <lat>
            64.200841
         </lat>
         <lng>
            -149.493673
         </lng>
      </item>
      <item>
         <id>
            85
         </id>
         <pid>
            84
         </pid>
         <t>
            Albany, NY, USA
         </t>
         <n>
            Albany
         </n>
         <lat>
            42.652579
         </lat>
         <lng>
            -73.756232
         </lng>
      </item>
      <item>
         <id>
            328
         </id>
         <pid>
            63
         </pid>
         <t>
            Albany, OR, USA
         </t>
         <n>
            Albany
         </n>
         <lat>
            44.636511
         </lat>
         <lng>
            -123.105928
         </lng>
      </item>
      <item>
         <id>
            42
         </id>
         <pid>
            32
         </pid>
         <t>
            Alberta, Canada
         </t>
         <n>
            Alberta
         </n>
         <lat>
            53.933271
         </lat>
         <lng>
            -116.576504
         </lng>
      </item>
      <item>
         <id>
            81
         </id>
         <pid>
            80
         </pid>
         <t>
            Albuquerque, NM, USA
         </t>
         <n>
            Albuquerque
         </n>
         <lat>
            35.110703
         </lat>
         <lng>
            -106.609991
         </lng>
      </item>
      <item>
         <id>
            324
         </id>
         <pid>
            138
         </pid>
         <t>
            Alexandria, VA, USA
         </t>
         <n>
            Alexandria
         </n>
         <lat>
            38.804836
         </lat>
         <lng>
            -77.046921
         </lng>
      </item>
      <item>
         <id>
            375
         </id>
         <pid>
            373
         </pid>
         <t>
            Alice Springs NT 0870, Australia
         </t>
         <n>
            Alice Springs
         </n>
         <lat>
            -23.70021
         </lat>
         <lng>
            133.880611
         </lng>
      </item>
      <item>
         <id>
            134
         </id>
         <pid>
            133
         </pid>
         <t>
            Allegany, MD, USA
         </t>
         <n>
            Allegany
         </n>
         <lat>
            39.625525
         </lat>
         <lng>
            -78.6115
         </lng>
      </item>
      <item>
         <id>
            459
         </id>
         <pid>
            105
         </pid>
         <t>
            Allentown, PA, USA
         </t>
         <n>
            Allentown
         </n>
         <lat>
            40.60843
         </lat>
         <lng>
            -75.490183
         </lng>
      </item>
   </locations>
</results>
```

In this case, the XML contains no root element. Meaning that the XML is not well-formed. There is nothing to do with this, since well-formed JSON document can contain no root element as well. The result was saved in `data.xml` file. As you can see, JSON array items were printed in XML like subelements of the array element. That means, reverse function does not necessarily have to lead to the same JSON file.

### XML to JSON example
As an example, we can use (shortened) weather forecast data for Prague, downloaded freely from [yr.no](https://yr.no/) portal.

```
<weatherdata>
<location>
<name>Prague</name>
<type>Capital</type>
<country>Czech Republic</country>
<timezone id="Europe/Prague" utcoffsetMinutes="120"/>
<location altitude="202" latitude="50.08804" longitude="14.42076" geobase="geonames" geobaseid="3067696"/>
</location>
<credits/>
<links>
<link id="xmlSource" url="http://www.yr.no/place/Czech_Republic/Prague/Prague/forecast.xml"/>
<link id="xmlSourceHourByHour" url="http://www.yr.no/place/Czech_Republic/Prague/Prague/forecast_hour_by_hour.xml"/>
<link id="overview" url="http://www.yr.no/place/Czech_Republic/Prague/Prague/"/>
<link id="hourByHour" url="http://www.yr.no/place/Czech_Republic/Prague/Prague/hour_by_hour"/>
<link id="longTermForecast" url="http://www.yr.no/place/Czech_Republic/Prague/Prague/long"/>
</links>
<meta>
<lastupdate>2018-05-24T09:36:00</lastupdate>
<nextupdate>2018-05-24T22:00:00</nextupdate>
</meta>
<sun rise="2018-05-24T05:05:16" set="2018-05-24T20:53:47"/>
<forecast>
<tabular>
<time from="2018-05-24T18:00:00" to="2018-05-25T00:00:00" period="3">
<symbol number="4" numberEx="4" name="Cloudy" var="04"/>
<precipitation value="0"/>
<windDirection deg="86.1" code="E" name="East"/>
<windSpeed mps="5.6" name="Moderate breeze"/>
<temperature unit="celsius" value="23"/>
<pressure unit="hPa" value="1018.2"/>
</time>
<time from="2018-05-25T00:00:00" to="2018-05-25T06:00:00" period="0">
<symbol number="4" numberEx="4" name="Cloudy" var="04"/>
<precipitation value="0"/>
<windDirection deg="82.6" code="E" name="East"/>
<windSpeed mps="4.0" name="Gentle breeze"/>
<temperature unit="celsius" value="18"/>
<pressure unit="hPa" value="1018.9"/>
</time>
<time from="2018-05-25T06:00:00" to="2018-05-25T12:00:00" period="1">
<symbol number="3" numberEx="3" name="Partly cloudy" var="03d"/>
<precipitation value="0"/>
<windDirection deg="67.2" code="ENE" name="East-northeast"/>
<windSpeed mps="1.9" name="Light breeze"/>
<temperature unit="celsius" value="15"/>
<pressure unit="hPa" value="1019.4"/>
</time>
<time from="2018-05-25T12:00:00" to="2018-05-25T18:00:00" period="2">
<symbol number="4" numberEx="4" name="Cloudy" var="04"/>
<precipitation value="0"/>
<windDirection deg="95.7" code="E" name="East"/>
<windSpeed mps="4.5" name="Gentle breeze"/>
<temperature unit="celsius" value="23"/>
<pressure unit="hPa" value="1018.7"/>
</time>
<time from="2018-05-25T18:00:00" to="2018-05-26T00:00:00" period="3">
<symbol number="3" numberEx="3" name="Partly cloudy" var="03n"/>
<precipitation value="0"/>
<windDirection deg="81.0" code="E" name="East"/>
<windSpeed mps="4.6" name="Gentle breeze"/>
<temperature unit="celsius" value="23"/>
<pressure unit="hPa" value="1017.7"/>
</time>
</tabular>
</forecast>
</weatherdata>
```

Let's execute command `xmlToJson "data.xml" 3`, new file is awaited to be `data.json`. We got following output.

```
{
   "weatherdata": {
      "location": {
         "name": "Prague",
         "type": "Capital",
         "country": "Czech Republic",
         "timezone": {
            "attributes": {
               "id": "Europe/Prague",
               "utcoffsetMinutes": "120"
            }
         },
         "location": {
            "attributes": {
               "altitude": "202",
               "latitude": "50.08804",
               "longitude": "14.42076",
               "geobase": "geonames",
               "geobaseid": "3067696"
            }
         }
      },
      "credits": "",
      "links": {
         "link": {
            "attributes": {
               "id": "xmlSource",
               "url": "http://www.yr.no/place/Czech_Republic/Prague/Prague/forecast.xml"
            }
         },
         "link": {
            "attributes": {
               "id": "xmlSourceHourByHour",
               "url": "http://www.yr.no/place/Czech_Republic/Prague/Prague/forecast_hour_by_hour.xml"
            }
         },
         "link": {
            "attributes": {
               "id": "overview",
               "url": "http://www.yr.no/place/Czech_Republic/Prague/Prague/"
            }
         },
         "link": {
            "attributes": {
               "id": "hourByHour",
               "url": "http://www.yr.no/place/Czech_Republic/Prague/Prague/hour_by_hour"
            }
         },
         "link": {
            "attributes": {
               "id": "longTermForecast",
               "url": "http://www.yr.no/place/Czech_Republic/Prague/Prague/long"
            }
         }
      },
      "meta": {
         "lastupdate": "2018-05-24T09:36:00",
         "nextupdate": "2018-05-24T22:00:00"
      },
      "sun": {
         "attributes": {
            "rise": "2018-05-24T05:05:16",
            "set": "2018-05-24T20:53:47"
         }
      },
      "forecast": {
         "tabular": {
            "time": {
               "attributes": {
                  "from": "2018-05-24T18:00:00",
                  "to": "2018-05-25T00:00:00",
                  "period": "3"
               },
               "symbol": {
                  "attributes": {
                     "number": "4",
                     "numberEx": "4",
                     "name": "Cloudy",
                     "var": "04"
                  }
               },
               "precipitation": {
                  "attributes": {
                     "value": "0"
                  }
               },
               "windDirection": {
                  "attributes": {
                     "deg": "86.1",
                     "code": "E",
                     "name": "East"
                  }
               },
               "windSpeed": {
                  "attributes": {
                     "mps": "5.6",
                     "name": "Moderate breeze"
                  }
               },
               "temperature": {
                  "attributes": {
                     "unit": "celsius",
                     "value": "23"
                  }
               },
               "pressure": {
                  "attributes": {
                     "unit": "hPa",
                     "value": "1018.2"
                  }
               }
            },
            "time": {
               "attributes": {
                  "from": "2018-05-25T00:00:00",
                  "to": "2018-05-25T06:00:00",
                  "period": "0"
               },
               "symbol": {
                  "attributes": {
                     "number": "4",
                     "numberEx": "4",
                     "name": "Cloudy",
                     "var": "04"
                  }
               },
               "precipitation": {
                  "attributes": {
                     "value": "0"
                  }
               },
               "windDirection": {
                  "attributes": {
                     "deg": "82.6",
                     "code": "E",
                     "name": "East"
                  }
               },
               "windSpeed": {
                  "attributes": {
                     "mps": "4.0",
                     "name": "Gentle breeze"
                  }
               },
               "temperature": {
                  "attributes": {
                     "unit": "celsius",
                     "value": "18"
                  }
               },
               "pressure": {
                  "attributes": {
                     "unit": "hPa",
                     "value": "1018.9"
                  }
               }
            },
            "time": {
               "attributes": {
                  "from": "2018-05-25T06:00:00",
                  "to": "2018-05-25T12:00:00",
                  "period": "1"
               },
               "symbol": {
                  "attributes": {
                     "number": "3",
                     "numberEx": "3",
                     "name": "Partly cloudy",
                     "var": "03d"
                  }
               },
               "precipitation": {
                  "attributes": {
                     "value": "0"
                  }
               },
               "windDirection": {
                  "attributes": {
                     "deg": "67.2",
                     "code": "ENE",
                     "name": "East-northeast"
                  }
               },
               "windSpeed": {
                  "attributes": {
                     "mps": "1.9",
                     "name": "Light breeze"
                  }
               },
               "temperature": {
                  "attributes": {
                     "unit": "celsius",
                     "value": "15"
                  }
               },
               "pressure": {
                  "attributes": {
                     "unit": "hPa",
                     "value": "1019.4"
                  }
               }
            },
            "time": {
               "attributes": {
                  "from": "2018-05-25T12:00:00",
                  "to": "2018-05-25T18:00:00",
                  "period": "2"
               },
               "symbol": {
                  "attributes": {
                     "number": "4",
                     "numberEx": "4",
                     "name": "Cloudy",
                     "var": "04"
                  }
               },
               "precipitation": {
                  "attributes": {
                     "value": "0"
                  }
               },
               "windDirection": {
                  "attributes": {
                     "deg": "95.7",
                     "code": "E",
                     "name": "East"
                  }
               },
               "windSpeed": {
                  "attributes": {
                     "mps": "4.5",
                     "name": "Gentle breeze"
                  }
               },
               "temperature": {
                  "attributes": {
                     "unit": "celsius",
                     "value": "23"
                  }
               },
               "pressure": {
                  "attributes": {
                     "unit": "hPa",
                     "value": "1018.7"
                  }
               }
            },
            "time": {
               "attributes": {
                  "from": "2018-05-25T18:00:00",
                  "to": "2018-05-26T00:00:00",
                  "period": "3"
               },
               "symbol": {
                  "attributes": {
                     "number": "3",
                     "numberEx": "3",
                     "name": "Partly cloudy",
                     "var": "03n"
                  }
               },
               "precipitation": {
                  "attributes": {
                     "value": "0"
                  }
               },
               "windDirection": {
                  "attributes": {
                     "deg": "81.0",
                     "code": "E",
                     "name": "East"
                  }
               },
               "windSpeed": {
                  "attributes": {
                     "mps": "4.6",
                     "name": "Gentle breeze"
                  }
               },
               "temperature": {
                  "attributes": {
                     "unit": "celsius",
                     "value": "23"
                  }
               },
               "pressure": {
                  "attributes": {
                     "unit": "hPa",
                     "value": "1017.7"
                  }
               }
            }
         }
      }
   }
}
```

This example shows us several things. Although number can be treated as a numeric value in XML, it is always parsed as a string. The same holds for boolean values and null values. That's because XML does not support numeric data type (it can be interpreted as a numeric data type using XML Schema), everything is treated as a string. You can also see that attributes are converted to JSON objects, since JSON does not support attributes. You may notice that applying `xmlToJson` and `jsonToXml` functions in-order leads to different XML, since there's no binding between origin XML attribute - JSON objects are converted to XML elements. You can get symmetric function after two iterations, when you convert JSON with translated attributes into the XML document... Anyway, applying `xmlToXml` function leads to the same XML document. Empty tag was manually added just to demonstrate that the parser can handle empty tags, too.

The only thing both parsers cannot handle is parsing comments. You also cannot use mixed elements (text along with sublements), because it would violate JSON concept. The XML document to be parsed must not start with XML header, otherwise it would not parse. These minor bugs are intended to be a future work. It's also planned to implement parsing of JSON "attributes" to the real XML attributes.
