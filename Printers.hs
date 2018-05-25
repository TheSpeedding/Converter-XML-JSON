module Printers where

    import Parsers
    import Data.List
    import Data.Maybe

    printJson :: Node -> String
    printJson x = printJsonImpl x where

      printJsonImpl (NumericContent x) = show x
    
      printJsonImpl (TextContent x) =  show x
    
      printJsonImpl (BoolContent x)
        | True  = "true"
        | False = "false"
    
      printJsonImpl (NullValue ()) = "null"

      printJsonImpl Empty = show ""
    
      printJsonImpl (NodesArray x) = "[ " ++ (intercalate ", " (map (\x -> printJsonImpl x) x)) ++ " ]"
    
      printJsonImpl (NodeSubelements x) = let
                                            -- treat attribute as an object, convert key as a key for object and value as a text content of the object
                                            getAttrs :: [Attribute] -> Maybe Element
                                            getAttrs attributes 
                                              | length attributes == 0 = Nothing
                                              | otherwise = Just ("attributes", [], NodeSubelements (map (\(key, val) -> (key, [], (TextContent val))) attributes))                                                 

                                            printKeyVal :: String -> Node -> String
                                            printKeyVal key val = (show key) ++ ": " ++ printJsonImpl val
   
                                            printImpl (key, attributes, val) = let
                                                                                 attrsAsElems = getAttrs attributes
                                                                                 newVals
                                                                                  | isNothing attrsAsElems = val -- otherwise it's just
                                                                                  | isEmpty val = NodeSubelements [fromJust attrsAsElems] -- otherwise it must be node subelement
                                                                                  | otherwise = NodeSubelements ([fromJust attrsAsElems] ++ (getElems val))
                                                                               in
                                                                                printKeyVal key newVals                             
                                          in 
                                            "{\n" ++ (intercalate ", \n" (map (\x -> printImpl x) x)) ++ "\n}"
    
    printXml :: Node -> String
    printXml x = printXmlImpl x where    

      printOneAttribute :: Attribute -> String
      printOneAttribute (key, val) = key ++ "=" ++ (show val)

      printAttributes :: [Attribute] -> String
      printAttributes [] = "" -- intercalate can handle this, but then there would be one redundant space <element >
      printAttributes x = " " ++ (intercalate " " (map (\x -> printOneAttribute x) x))

      -- print(Open|Close|Empty)Tag :: String -> [Attribute] -> String
      printOpenTag  tag attributes = "<"  ++ tag ++ (printAttributes attributes) ++  ">"
      printCloseTag tag            = "</" ++ tag                                        ++  ">"
      printEmptyTag tag attributes = "<"  ++ tag ++ (printAttributes attributes) ++ "/>"
      
      printOneElement :: Element -> String
      printOneElement (tag, attributes, content)
        | isEmpty content = printEmptyTag tag attributes
        | otherwise = (printOpenTag tag attributes) ++ "\n" ++ (printXmlImpl content) ++ "\n" ++ (printCloseTag tag)

      printXmlImpl (NumericContent x) = show x
    
      printXmlImpl (TextContent x) = x
    
      printXmlImpl (BoolContent x)
        | True  = "true"
        | False = "false"
    
      printXmlImpl (NullValue ()) = "null"

      printXmlImpl Empty = ""
    
      printXmlImpl (NodesArray x) = intercalate "\n" (map (\x -> printOneElement ("item", [], x)) x)
                                        
      printXmlImpl (NodeSubelements x) = intercalate "\n" (map (\x -> printOneElement x) x)