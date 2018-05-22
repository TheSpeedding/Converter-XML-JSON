module PidiParsec
( Parser
, parseFail
, parseGet
, parseSet
, pEof
, doParse
, doParseEof
, pAny
, pOneOf
, pChar
, pStr
, pMany
, pBracketed
) where

import Control.Applicative -- operator <|>
import Control.Monad.State.Strict -- official State implementation

data Parser a = Parser (State String (Maybe a))

-- monad instance from the bottom up
instance Functor Parser where
  fmap f (Parser s) = Parser $ fmap (fmap f) s
instance Applicative Parser where
  pure a = Parser . pure . pure $ a
  Parser l <*> Parser r = Parser ((<*>) <$> l <*> r) 
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
            "" -> return ()  --looks okay
            _ -> parseFail -- certainly not on the end

-- runners
doParse (Parser p) = runState p
doParseEof p = fst . doParse (p >>= \r -> pEof >> return r)

-- parse out any character (and return it monadically)
pAny = do s <- parseGet
          case s of
            (i:ss) -> parseSet ss >> return i
            _ -> parseFail

-- parse out a character from a limited range of characters
pOneOf cs = do s <- parseGet
               case s of
                 (i:ss) -> if i `elem` cs
                           then parseSet ss >> return i
                           else parseFail
                 _ -> parseFail

-- parse out a single specified character
pChar c = pOneOf [c]

-- parse out an exact string
pStr s = mapM pChar s

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