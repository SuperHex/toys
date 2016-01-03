{-# LANGUAGE OverloadedStrings #-}

module XML where

import Text.Parsec
import Text.Parsec.Text
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map  as M

data XML = XML Node [XML]
         | Empty
         deriving (Show, Eq)

data Node = Node
           { labelName :: T.Text
           , attribute :: M.Map T.Text T.Text
           , content   :: Maybe T.Text
           } deriving (Show, Eq)

type Name = T.Text
type Val = T.Text
type Attr = (Name, Val)

xmlHeader :: Parser Node
xmlHeader = do
    _ <- string "<?xml"
    attr <- do
        try $ many xmlAttribute
        <|> return []
    spaces
    _ <- string "?>"
    return $ Node { labelName = "XMLHeader"
                   , attribute = M.fromList attr
                   , content = Nothing
                   }


xmlAttribute :: Parser Attr
xmlAttribute = do
    _ <- many1 $ string " "
    k:v:[] <- (many $ noneOf "<>=? ") `sepBy` (string "=")
    return $ (T.pack k,T.pack v)

-- TODO: support comment parsing
xmlTagStart :: Parser Node
xmlTagStart = do
    _ <- string "<"
    name <- (many1 (noneOf "<>=? ")) <?> "tag name!"
    attr <- do
        try $ many xmlAttribute
        <|> return []
    spaces
    _ <- string ">" <?> "Tag not close!"
    return $ Node { labelName = T.pack name
                  , attribute = M.fromList attr
                  , content   = Nothing
                  }

xmlContent :: Parser T.Text
xmlContent = do
    -- we dont consider escape now, just return what it is.
    txt <- many $ noneOf "<>"
    return $ T.pack txt

xmlTagEnd :: Parser T.Text
xmlTagEnd = do
    _ <- string "</"
    name <- (many1 $ noneOf "<>=? ")
    return $ T.pack name

xmlNode :: Parser Node
xmlNode = do
    node <- xmlTagStart
    txt  <- xmlContent
    end  <- xmlTagEnd
    if (labelName node == end)
        then do
            case (T.null txt) of
                True   -> return node
                False  -> return $ node { content = Just $ txt}
        else error "Tag doesn't close correct!"

xmlNestNode :: Parser XML
xmlNestNode = undefined

-- Here is the difficult point: How to parse a nested tag?
-- which means, we must maintain a stack holds all tag who is not closed.
-- Seems we have to use a monad transformer here, since the parsing and push/pop
-- action is in parelell.
