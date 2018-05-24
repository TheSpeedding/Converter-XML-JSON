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
                                    pWhiteSpace
                                    TextContent key <- parseJsonString
                                    pWhiteSpace
                                    pChar ':'
                                    pWhiteSpace
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
                            pWhiteSpace
                            key <- parseXmlAttrOrElName
                            pWhiteSpace
                            pStr "=\""
                            pWhiteSpace
                            TextContent val <- parseXmlInnerContent
                            pChar '\"'
                            return (key, val)

    parseXmlElement = NodeSubelements <$> (pMany1 element) where 
                        element = do
                                    pWhiteSpace
                                    pChar '<'
                                    name <- parseXmlAttrOrElName
                                    attr <- pMany parseXmlAttribute
                                    close <- (pStr "/>" <|> pStr ">")
                                    pWhiteSpace
                                    if (length close) == 2 then return (name, attr, Empty) -- empty element
                                    else do 
                                            pWhiteSpace
                                            body <- parseXml -- content of the element
                                            pWhiteSpace
                                            pStr "</"
                                            pStr name
                                            pStr ">"
                                            pWhiteSpace
                                            return (name, attr, body)