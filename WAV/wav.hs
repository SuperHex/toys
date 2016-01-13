{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.State
import System.Environment
import qualified Data.ByteString as B
import Data.Word
import Data.Bits

main :: IO ()
main = do
    [args] <- getArgs
    file <- B.readFile args
    let header = runParser (takeByte 78) file
        bin    = runParser defaultParser file
    B.writeFile "a.wav" (header `B.append` bin)
    --forM_ [1] $ do
        -- \_ -> B.writeFile "wav.txt" B.empty
        -- \_ -> appendFile "wav.txt" (fmap ((++",") . init . tail . show) B.unpack (header `B.append` bin))

data Header = Header
            { }

data ParseState = ParseState
                { offset :: Int
                , content :: B.ByteString
                }

type Parser a = State ParseState a

takeByte :: Int -> Parser B.ByteString
takeByte n = do
    s <- get
    let str = B.take n . B.drop (offset s) $ (content s)
    skip n
    return str

peek :: Parser [Word8]
peek = do
    s <- get
    let b = B.take 1 . B.drop (offset s) $ content s
    return $ B.unpack b

skip :: Int -> Parser ()
skip n = do
    modify (\s -> s { offset = offset s + n })
    return ()

skipTo :: Int -> Parser ()
skipTo n = do
    s <- get
    skip (n - offset s)

runParser :: Parser a -> B.ByteString -> a
runParser p s = evalState p (defaultState s)
            where defaultState = \str -> ParseState
                                    { offset = 0
                                    , content = str
                                    }

encode16 :: [Word8] -> Word16
encode16 (l:r:[]) = (fromIntegral l) `shiftL` 8 .|. (fromIntegral r)
encode16 _        = error "Not exactly 2 Bytes"

encode32 :: [Word8] -> Word32
encode32 (a:b:c:d:[]) = (fromIntegral (encode16 [a,b])) `shiftL` 16 .|. fromIntegral (encode16 [c,d])
encode32 _            = error "Not exactly 4 Bytes"

encode16' :: [Word8] -> Word16
encode16' = encode16 . reverse

encode32' :: [Word8] -> Word32
encode32' = encode32 . reverse

f :: [Word8] -> Int ->  Word8
f []     _ = zeroBits
f (x:xs) n = zeroBits `flatBit` n .|. f xs (n-1)
        where flatBit = if x >= 128 then setBit else clearBit

defaultParser :: Parser B.ByteString
defaultParser = do
    header <- takeByte 74
    rawsize <- takeByte 4
    let size = fromIntegral $ encode32' (B.unpack rawsize)
    s <- takeByte size
    let s' = B.take 10000000 . B.drop 4900000 . B.dropWhile (==0) $ s
    return s'
