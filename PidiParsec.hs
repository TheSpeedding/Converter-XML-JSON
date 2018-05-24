-- Library author: https://github.com/exaexa

import Control.Applicative
import Control.Monad.State.Strict
import Data.Maybe

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

-- parse out something bracketed from left and right
pBracketed l r p = do l
                      res <- p
                      r
                      return res

pDelim l r = pBracketed (pStr l) (pStr r)
pBrackets = pDelim "[" "]"
pBraces = pDelim "{" "}"

pQuoted q = pDelim q q . pMany $ pAnyExcept q

-- an useful tool: Just 1 <:> Just [2,3] == Just (1:[2,3]) == Just [1,2,3]
infixr 4 <:>
a <:> b = (:) <$> a <*> b

-- a more useful tool: many1 with separator
-- pSep (pChar ',') (pStr "asd") parses "asd,asd,asd,..."
pSep1 s p = p <:> (s >> pSep1 s p)
            <|>
            (:[]) <$> p

-- maw function (:[]) is the same as (\x -> [x]).
-- (Also same as 'pure' in this context, but let's avoid too much polymorphism here.)

pSep s p = pSep1 s p <|> return []

pCommaDelimited = pSep (pChar ',')

numberOfSpaces = 2

printIndent length = putStr $ replicate (numberOfSpaces * length) ' '

data Json = JsonNumber Int | JsonString String | JsonArray [Json] | JsonBool Bool | JsonNull () | JsonObject [(String, Json)]

printJson :: Json -> IO ()
printJson x = printJsonImpl x 0 where
  printJsonImpl (JsonNumber x) indent = do 
                                         printIndent indent
                                         putStr $ show x

  printJsonImpl (JsonString x) indent = do 
                                         printIndent indent
                                         putStr $ show x

  printJsonImpl (JsonBool x) indent
    | True  = do 
               printIndent indent
               putStr "true"
    | False = do
               printIndent indent
               putStr "false"

  printJsonImpl (JsonNull ()) indent = do
                                        printIndent indent
                                        putStr "null"

  printJsonImpl (JsonArray x) indent = let
                                        printImpl (x:[]) = printJsonImpl x 0
                                        printImpl (x:xs) = do
                                                            printJsonImpl x 0
                                                            putStr ", "
                                                            printImpl xs
                                       in
                                        do
                                          printIndent indent
                                          putStr "[ "
                                          printImpl x
                                          putStr " ]" 

  printJsonImpl (JsonObject x) indent = let
                                          printImpl [] indent = printIndent indent
                                          printImpl ((key, value):[]) indent = do
                                                                                printIndent indent
                                                                                putStr $ show key
                                                                                putStr ":"
                                                                                printJsonImpl value 0
                                                                                putStrLn ""
                                          printImpl ((key, value):xs) indent = do
                                                                                printIndent indent
                                                                                putStr $ show key
                                                                                putStr ":"
                                                                                printJsonImpl value 0
                                                                                putStrLn ","
                                                                                printImpl xs indent
                                        in 
                                          do
                                            printIndent indent
                                            putStrLn "{"
                                            printImpl x (indent + 1)
                                            putStr "}"

parseJson = parseJsonNumber <|> parseJsonString <|> parseJsonArray <|> parseJsonBool <|> parseJsonNull <|> parseJsonObject where
  parseJsonNumber = JsonNumber . (read :: String -> Int) <$> pMany1 (pOneOf ['0'..'9'])
  
  parseJsonString = let 
                  innerChar = pOneOf (['a'..'z'] ++ ['A'..'Z'] ++ " .,/?!" ++ ['0'..'9'])
                  innerString = pMany innerChar
                in
                  JsonString <$> pDelim "\"" "\"" innerString
  
  parseJsonArray = JsonArray <$> pBrackets (pCommaDelimited parseJson)

  parseJsonBool = JsonBool <$> pBool

  parseJsonNull = JsonNull <$> pNull

  parseJsonObject = JsonObject <$> pBraces (pCommaDelimited objItem) where 
                objItem = do 
                            JsonString key <- parseJsonString
                            pChar ':'
                            ((,) key) <$> parseJson
                unusedObjItemApplicativeStyle = (\(JsonString key) val -> (key,val)) <$> parseJsonString <*> (pChar ':' >> parseJson)

demo = doParseEof parseJson "{\"asd\":\"sracka\",\"list\":[1,2,3],\"q\":{}}"