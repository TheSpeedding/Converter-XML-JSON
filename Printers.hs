module Printers where

    import Parsers

    printJson :: Node -> Int -> String
    printJson x numberOfSpaces = printJsonImpl x 0 where
      printIndent length = replicate (numberOfSpaces * length) ' '
    
      printJsonImpl (NumericContent x) indent = (printIndent indent) ++ (show x)
    
      printJsonImpl (TextContent x) indent = (printIndent indent) ++ (show x)
    
      printJsonImpl (BoolContent x) indent
        | True  = (printIndent indent) ++ "true"
        | False = (printIndent indent) ++ "false"
    
      printJsonImpl (NullValue ()) indent = (printIndent indent) ++ "null"
    
      printJsonImpl (NodesArray x) indent = let    
                                              -- indentation for objects only   
                                              printImpl ((NodeSubelements x):[]) = printJsonImpl (NodeSubelements x) (indent + 2)
                                              printImpl ((NodeSubelements x):xs) = (printJsonImpl (NodeSubelements x) (indent + 2)) ++ ", " ++ (printImpl xs)
                                              
                                              printImpl (x:[]) = printJsonImpl x 0
                                              printImpl (x:xs) = (printJsonImpl x 0) ++ ", " ++ (printImpl xs)
                                            in
                                              (printIndent indent) ++ "[ " ++ (printImpl x) ++ " ]" 
    
      printJsonImpl (NodeSubelements []) indent = "{}"
    
      printJsonImpl (NodeSubelements x) indent = let
                                                  -- treat attribute as an object, convert key as a key for object and value as a text content of the object
                                                  getAttrs [] [] = undefined -- should never be reached (debug cases)
                                                  getAttrs [] objectContent = ("attributes", [], NodeSubelements objectContent)
                                                  getAttrs ((key, val):xs) objectContent = getAttrs xs (objectContent ++ [(key, [], (TextContent val))])
    
                                                  printAttrs attributes = (printJsonImpl (NodeSubelements [(getAttrs attributes [])]) (indent + 1))
    
                                                  printImpl [] indent = printIndent indent
    
                                                  -- no data for empty instances, no attributes                                     
                                                  printImpl ((key, [], Empty):[]) indent = (printIndent indent) ++ (show key) ++ ": \"\"" ++ "\n"
                                                  printImpl ((key, [], Empty):xs) indent = (printIndent indent) ++ (show key) ++ ": \"\"" ++ ",\n" ++ (printImpl xs indent)                                            
    
                                                  -- indentation for objects only, no attributes                                        
                                                  printImpl ((key, [], (NodeSubelements value)):[]) indent = (printIndent indent) ++ (show key) ++ ": " ++ (printJsonImpl (NodeSubelements (value)) indent) ++ "\n"
                                                  printImpl ((key, [], (NodeSubelements value)):xs) indent = (printIndent indent) ++ (show key) ++ ": " ++ (printJsonImpl (NodeSubelements (value)) indent) ++ ",\n" ++ (printImpl xs indent)
    
                                                  -- no data for empty instances                                     
                                                  printImpl ((key, attributes, Empty):[]) indent = (printIndent indent) ++ (show key) ++ ": " ++ (printAttrs attributes) ++ "\n"
                                                  printImpl ((key, attributes, Empty):xs) indent = (printIndent indent) ++ (show key) ++ ": " ++ (printAttrs attributes) ++ ",\n" ++ (printImpl xs indent)                                            
    
                                                  -- indentation for objects only                                        
                                                  printImpl ((key, attributes, (NodeSubelements value)):[]) indent = (printIndent indent) ++ (show key) ++ ": " ++ (printJsonImpl (NodeSubelements ([(getAttrs attributes [])] ++ value)) indent) ++ "\n"
                                                  printImpl ((key, attributes, (NodeSubelements value)):xs) indent = (printIndent indent) ++ (show key) ++ ": " ++ (printJsonImpl (NodeSubelements ([(getAttrs attributes [])] ++ value)) indent) ++ ",\n" ++ (printImpl xs indent)
                                                                                          
                                                  -- simple value types cannot have attributes
                                                  printImpl ((key, [], value):[]) indent = (printIndent indent) ++ (show key) ++ ": " ++ (printJsonImpl value 0) ++ "\n"
                                                  printImpl ((key, [], value):xs) indent = (printIndent indent) ++ (show key) ++ ": " ++ (printJsonImpl value 0) ++ ",\n" ++ (printImpl xs indent)                                
                                                in 
                                                  "{\n" ++ (printImpl x (indent + 1)) ++ (printIndent indent) ++ "}"
    
    printXml :: Node -> Int -> String
    printXml x numberOfSpaces = printXmlImpl x 0 where
      printIndent length = replicate (numberOfSpaces * length) ' '
    
      printXmlImpl (NumericContent x) indent = (printIndent indent) ++ (show x)
    
      printXmlImpl (TextContent x) indent = (printIndent indent) ++ x
    
      printXmlImpl (BoolContent x) indent
        | True  = (printIndent indent) ++ "true"
        | False = (printIndent indent) ++ "false"
    
      printXmlImpl (NullValue ()) indent = (printIndent indent) ++ "null"
    
      printXmlImpl (NodesArray x) indent = let
                                            tag = "item"
                                            printImpl (x:[]) = (printIndent indent) ++ "<" ++ tag ++ ">\n" ++ (printXmlImpl x (indent + 1)) ++ "\n" ++ (printIndent indent) ++ "</" ++ tag ++ ">"
                                            printImpl (x:xs) = (printIndent indent) ++ "<" ++ tag ++ ">\n" ++ (printXmlImpl x (indent + 1)) ++ "\n" ++ (printIndent indent) ++ "</" ++ tag ++ ">\n" ++ (printImpl xs)
                                          in
                                            printImpl x
    
      printXmlImpl (NodeSubelements []) indent = ""
                                        
      printXmlImpl (NodeSubelements x) indent = let
                                                printAttrs [] = ""
                                                printAttrs ((key, val):[]) = " " ++ key ++ "=\"" ++ val ++ "\""
                                                printAttrs ((key, val):xs) = " " ++ key ++ "=\"" ++ val ++ "\"" ++ printAttrs xs
    
                                                printImpl [] indent = printIndent indent 
    
                                                -- print empty elements                                       
                                                printImpl ((key, attributes, Empty):[]) indent = (printIndent indent) ++ "<" ++ key ++ (printAttrs attributes) ++ "/>"
                                                printImpl ((key, attributes, Empty):xs) indent = (printIndent indent) ++ "<" ++ key ++ (printAttrs attributes) ++ "/>\n" ++ (printImpl xs indent)                           
                                              
                                                -- print non-empty elements                                       
                                                printImpl ((key, attributes, value):[]) indent = (printIndent indent) ++ "<" ++ key ++ (printAttrs attributes) ++ ">\n" ++ (printXmlImpl value (indent + 1)) ++ "\n" ++ (printIndent indent) ++ "</" ++ key ++ ">"
                                                printImpl ((key, attributes, value):xs) indent = (printIndent indent) ++ "<" ++ key ++ (printAttrs attributes) ++ ">\n" ++ (printXmlImpl value (indent + 1)) ++ "\n" ++ (printIndent indent) ++ "</" ++ key ++ ">\n" ++ (printImpl xs indent)                           
                                              in 
                                                printImpl x indent    