{-# LANGUAGE OverloadedStrings #-}

module XML where

import Text.Parsec
import Text.Parsec.Text
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map  as M

data XML = Node Label [XML]
         deriving (Show, Eq)

data Label = Label
           { labelName :: T.Text
           , attribute :: M.Map T.Text T.Text
           , content   :: Maybe T.Text
           } deriving (Show, Eq)

type Key = T.Text
type Val = T.Text
type Attr = (Key, Val)

xmlHeader :: Parser Label
xmlHeader = do
    _ <- string "<?xml"
    attr <- do
        try $ many xmlAttribute
        <|> return []
    spaces
    _ <- string "?>"
    return $ Label { labelName = "XMLHeader"
                   , attribute = M.fromList attr
                   , content = Nothing
                   }


xmlAttribute :: Parser Attr
xmlAttribute = do
    _ <- many1 $ string " "
    k:v:[] <- (many $ noneOf "<>=? ") `sepBy` (string "=")
    return $ (T.pack k,T.pack v)
