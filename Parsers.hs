module Parsers where

    import PidiParsec
    import Control.Applicative
    import Data.List.Split

    type Attribute = (String, String)
    type Element = (String, [Attribute], Node) -- in json, attributes are empty list

    data Node = NumericContent Int | TextContent String | NodesArray [Node] | BoolContent Bool | NullValue () | NodeSubelements [Element] | Empty


    parseJson :: Parser Node
    parseJson = parseJsonNumber <|> parseJsonString <|> parseJsonArray <|> parseJsonBool <|> parseJsonNull <|> parseJsonObject where
    parseJsonNumber = NumericContent . (read :: String -> Int) <$> pMany1 (pOneOf ['0'..'9'])
    
    parseJsonString = let 
                        innerChar = pOneOf " !#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
                        innerString = pMany innerChar
                        in
                        TextContent <$> pDelim "\"" "\"" innerString
    
    parseJsonArray = NodesArray <$> pBracketsIgnore (pCommaDelimited parseJson)

    parseJsonBool = BoolContent <$> pBool

    parseJsonNull = NullValue <$> pNull

    parseJsonObject = NodeSubelements <$> pBracesIgnore (pCommaDelimited objItem) where 
                        objItem = do 
                                    TextContent key <- pToken parseJsonString
                                    pToken $ pChar ':'
                                    ((,,) key []) <$> parseJson


    parseXml :: Parser Node
    parseXml = parseXmlElement <|> parseXmlInnerContent where
    parseXmlInnerContent = let 
                                innerChar = pOneOf " !#$%&'()*+,-_./0123456789:;?@ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
                                innerString = pMany innerChar
                            in
                                TextContent <$> innerString

    parseXmlAttrOrElName = pMany1 $ pOneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])

    parseXmlAttribute = do
                            key <- pToken parseXmlAttrOrElName
                            pToken $ pStr "=\""
                            TextContent val <- pToken $ parseXmlInnerContent
                            pToken $ pChar '\"'
                            return (key, val)

    parseXmlElement = NodeSubelements <$> (pMany1 element) where 
                        element = do
                                    pToken $ pChar '<'
                                    name <- parseXmlAttrOrElName
                                    attr <- pToken $ pMany parseXmlAttribute
                                    close <- (pStr "/>" <|> pStr ">")
                                    if (length close) == 2 then return (name, attr, Empty) -- empty element
                                    else do 
                                            body <- pToken parseXml -- content of the element
                                            pToken $ pStr "</"
                                            pStr name
                                            pStr ">"
                                            return (name, attr, body)