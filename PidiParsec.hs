-- Library author: https://github.com/exaexa
-- Slightly modified: Ignoring whitespaces, parsing booleans, parsing null values.

import Control.Applicative
import Control.Monad.State.Strict

import Data.Maybe
import System.IO
import Data.List.Split

data Parser a = Parser (State String (Maybe a))

-- monad instance from the bottom up
instance Functor Parser where
  fmap f (Parser s) = Parser $ fmap (fmap f) s
instance Applicative Parser where
  pure a = Parser . pure . pure $ a
  Parser l <*> Parser r = Parser (liftA2 (<*>) l r) 
instance Monad Parser where
  return = pure
  Parser l >>= p = Parser $ do a <- l
                               case a of
                                 Nothing -> return Nothing
                                 Just a -> let Parser r = p a in r
instance Alternative Parser where
  empty = Parser $ pure empty -- empty is Nothing in this case
  Parser l <|> Parser r = Parser $ do s0 <- get
                                      ll <- l  -- try to get the result of l
                                      case ll of
                                        Nothing -> put s0 >> r -- if it failed, restore original state and run r
                                        a -> return a  -- otherwise return whatever it returned

parseFail = empty -- fail hard
parseGet = Parser $ Just <$> get -- get the rest of the parsed string
parseSet s = Parser $ Just <$> put s -- set the rest of parsed string

-- parse out the EOF (a.k.a. verify we're on the end of the string)
pEof = do s <- parseGet
          case s of
            "" -> return () -- looks okay
            _ -> parseFail -- certainly not on the end

-- runners
doParse (Parser p) = runState p
doParseEof p = fst . doParse (p >>= \r -> pEof >> return r)

-- parse out any character (and return it monadically)
pAny = do s <- parseGet
          case s of
            (i:ss) -> parseSet ss >> return i
            _ -> parseFail

pCharCond f = pAny >>= \c -> if f c then return c
                                    else parseFail
                                    
-- parse out a character from a limited range of characters
pOneOf cs = pCharCond (`elem` cs)
pAnyExcept cs = pCharCond $ not.(`elem` cs)
pChar c = pCharCond (== c)

-- parse out an exact string
pStr s = mapM pChar s

-- parse out a boolean
pBool = do
          s <- parseGet
          case s of 
            ('t':'r':'u':'e':ss)     -> parseSet ss >> return True
            ('f':'a':'l':'s':'e':ss) -> parseSet ss >> return False
            _ -> parseFail

-- parse out a null value
pNull = do
          s <- parseGet
          case s of
             ('n':'u':'l':'l':ss) -> parseSet ss >> return ()
             _ -> parseFail

-- parse out one given thing at least once (and return the results concatenated to a list)
pMany1 :: Parser a -> Parser [a]
pMany1 p = do x <- p
              xs <- pMany p
              return (x:xs)

-- kleene star
pMany :: Parser a -> Parser [a]
pMany p = pMany1 p <|> pure []


pWhiteSpace = pMany (pOneOf " \n\t\r")

-- parse out something bracketed from left and right, ignoring whitespaces
pBracketedIgnore l r p = do pWhiteSpace
                            l
                            pWhiteSpace
                            res <- p
                            pWhiteSpace
                            r
                            pWhiteSpace
                            return res

-- parse out something bracketed from left and right
pBracketed l r p = do l
                      res <- p
                      r
                      return res

pDelim l r = pBracketed (pStr l) (pStr r)
pBrackets = pDelim "[" "]"
pBraces = pDelim "{" "}"

pDelimIgnore l r = pBracketedIgnore (pStr l) (pStr r)
pBracketsIgnore = pDelimIgnore "[" "]"
pBracesIgnore = pDelimIgnore "{" "}"

pQuoted q = pDelim q q . pMany $ pAnyExcept q

-- an useful tool: Just 1 <:> Just [2,3] == Just (1:[2,3]) == Just [1,2,3]
infixr 4 <:>
a <:> b = (:) <$> a <*> b

-- a more useful tool: many1 with separator
-- pSep (pChar ',') (pStr "asd") parses "asd,asd,asd,..."
pSep1 s p = p <:> (pWhiteSpace >> s >> pWhiteSpace >> pSep1 s p)
            <|>
            (:[]) <$> p

-- maw function (:[]) is the same as (\x -> [x]).
-- (Also same as 'pure' in this context, but let's avoid too much polymorphism here.)

pSep s p = pSep1 s p <|> return []

pCommaDelimited = pSep (pChar ',')

pWSDelimited = pSep pWhiteSpace









type Attribute = (String, String)
type Element = (String, [Attribute], Node) -- in json, attributes are empty list

data Node = NumericContent Int | TextContent String | NodesArray [Node] | BoolContent Bool | NullValue () | NodeSubelements [Element] | Empty deriving Show

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
                                              getAttrs [] [] = undefined -- should not be reached (debug cases)
                                              getAttrs [] objectContent = ("_attributes", [], NodeSubelements objectContent)
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
                                        body <- parseXml -- content of the element
                                        pStr "</"
                                        pStr name
                                        pStr ">"
                                        pWhiteSpace
                                        return (name, attr, body)
                      
                                                              
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