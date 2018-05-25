module Printers where

    import Parsers

    printJson :: Node -> String
    printJson x = printJsonImpl x where

      printJsonImpl (NumericContent x) = show x
    
      printJsonImpl (TextContent x) =  show x
    
      printJsonImpl (BoolContent x)
        | True  = "true"
        | False = "false"
    
      printJsonImpl (NullValue ()) = "null"
    
      printJsonImpl (NodesArray x) = let    
                                      printImpl (x:[]) = printJsonImpl x
                                      printImpl (x:xs) = (printJsonImpl x) ++ ", " ++ (printImpl xs)
                                    in
                                      "[ " ++ (printImpl x) ++ " ]" 
    
      printJsonImpl (NodeSubelements []) = "{}"
    
      printJsonImpl (NodeSubelements x) = let
                                                  -- treat attribute as an object, convert key as a key for object and value as a text content of the object
                                                  getAttrs [] objectContent = ("attributes", [], NodeSubelements objectContent)
                                                  getAttrs ((key, val):xs) objectContent = getAttrs xs (objectContent ++ [(key, [], (TextContent val))])
    
                                                  printAttrs attributes = printJsonImpl (NodeSubelements [(getAttrs attributes [])])
    
                                                  -- no data for empty instances, no attributes                                     
                                                  printImpl ((key, [], Empty):[]) = (show key) ++ ": \"\"" ++ "\n"
                                                  printImpl ((key, [], Empty):xs) = (show key) ++ ": \"\"" ++ ",\n" ++ (printImpl xs)                                            
    
                                                  -- indentation for objects only, no attributes                                        
                                                  printImpl ((key, [], (NodeSubelements value)):[]) = (show key) ++ ": " ++ (printJsonImpl (NodeSubelements (value))) ++ "\n"
                                                  printImpl ((key, [], (NodeSubelements value)):xs) = (show key) ++ ": " ++ (printJsonImpl (NodeSubelements (value))) ++ ",\n" ++ (printImpl xs)
    
                                                  -- no data for empty instances                                     
                                                  printImpl ((key, attributes, Empty):[]) = (show key) ++ ": " ++ (printAttrs attributes) ++ "\n"
                                                  printImpl ((key, attributes, Empty):xs) = (show key) ++ ": " ++ (printAttrs attributes) ++ ",\n" ++ (printImpl xs)                                            
    
                                                  -- indentation for objects only                                        
                                                  printImpl ((key, attributes, (NodeSubelements value)):[]) = (show key) ++ ": " ++ (printJsonImpl (NodeSubelements ([(getAttrs attributes [])] ++ value))) ++ "\n"
                                                  printImpl ((key, attributes, (NodeSubelements value)):xs) = (show key) ++ ": " ++ (printJsonImpl (NodeSubelements ([(getAttrs attributes [])] ++ value))) ++ ",\n" ++ (printImpl xs)
                                                                                          
                                                  -- simple value types cannot have attributes
                                                  printImpl ((key, [], value):[]) = (show key) ++ ": " ++ (printJsonImpl value) ++ "\n"
                                                  printImpl ((key, [], value):xs) = (show key) ++ ": " ++ (printJsonImpl value) ++ ",\n" ++ (printImpl xs)                                
                                                in 
                                                  "{\n" ++ (printImpl x) ++ "}"
    
    printXml :: Node -> String
    printXml x = printXmlImpl x where    
      printXmlImpl (NumericContent x) = show x
    
      printXmlImpl (TextContent x) = x
    
      printXmlImpl (BoolContent x)
        | True  = "true"
        | False = "false"
    
      printXmlImpl (NullValue ()) = "null"
    
      printXmlImpl (NodesArray x) = let
                                            tag = "item"
                                            printImpl (x:[]) = "<" ++ tag ++ ">\n" ++ (printXmlImpl x) ++ "\n" ++ "</" ++ tag ++ ">"
                                            printImpl (x:xs) = "<" ++ tag ++ ">\n" ++ (printXmlImpl x) ++ "\n" ++ "</" ++ tag ++ ">\n" ++ (printImpl xs)
                                          in
                                            printImpl x
    
      printXmlImpl (NodeSubelements []) = ""
                                        
      printXmlImpl (NodeSubelements x) = let
                                                printAttrs [] = ""
                                                printAttrs ((key, val):[]) = " " ++ key ++ "=\"" ++ val ++ "\""
                                                printAttrs ((key, val):xs) = " " ++ key ++ "=\"" ++ val ++ "\"" ++ printAttrs xs
    
                                                printImpl [] = ""
    
                                                -- print empty elements                                       
                                                printImpl ((key, attributes, Empty):[]) = "<" ++ key ++ (printAttrs attributes) ++ "/>"
                                                printImpl ((key, attributes, Empty):xs) = "<" ++ key ++ (printAttrs attributes) ++ "/>\n" ++ (printImpl xs)                           
                                              
                                                -- print non-empty elements                                       
                                                printImpl ((key, attributes, value):[]) = "<" ++ key ++ (printAttrs attributes) ++ ">\n" ++ (printXmlImpl value) ++ "\n" ++ "</" ++ key ++ ">"
                                                printImpl ((key, attributes, value):xs) = "<" ++ key ++ (printAttrs attributes) ++ ">\n" ++ (printXmlImpl value) ++ "\n" ++ "</" ++ key ++ ">\n" ++ (printImpl xs)                           
                                              in 
                                                printImpl x    