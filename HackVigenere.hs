{-# LANGUAGE FlexibleContexts #-}

import LetterFrequency (p)
import Data.Char (ord)
import Data.Array (elems, assocs)
import Data.Array.ST (runSTArray, newArray, readArray, writeArray)
import Data.List (sortBy, find)
import Data.Bits (xor)
import Data.Char (chr)
import Debug.Trace
import Numeric (readHex)

printDescription = do
  putStrLn "Usage: Input the text to hack. The program will list suggestions for the key length based on the input data. You will have to decide which key length to use."
  putStrLn "For every character that looks out of place, input the 1-indexed index of that character until the data looks good. If you can't make it look good, restart the program and try another key length. When the data looks good, enter 0 to print the result in an easy to read way."

characterFrequency string = runSTArray $ do
  arr <- newArray (0, 255) 0
  mapM_ (\x -> do
    let index = ord x
    count <- readArray arr index
    writeArray arr index (count + 1)) string
  return arr

hexToBin [] = []
hexToBin (x:x2:xs) = (chr $ fst $ (readHex $ x:x2:[]) !! 0) : hexToBin xs

-- Key length
distrobution string = let freq = (elems $ characterFrequency string); calculatedFreq = map (\x -> (x / (fromIntegral $ length string)) ^ 2) freq in sum calculatedFreq

distrobution2 strings = let x = map distrobution strings in (sum x) / (fromIntegral $ length x)

getNthCharacters _ [] = []
getNthCharacters n arr = (head arr:getNthCharacters n (drop n arr))

getNthCharactersAndOffsets _ [] = [[]]
getNthCharactersAndOffsets n arr = map (\x -> getNthCharacters n (drop x arr)) [0..(n - 1)]

getPossibleKeyLengths arr = map (\x -> (x, distrobution2 $ getNthCharactersAndOffsets x arr)) [1..14]

getBestKeyLengthList arr = let lengths = getPossibleKeyLengths arr in
                               reverse $ sortBy (\(_, x) (_, y) -> if x < y then LT
                                                                            else if x > y then GT
                                                                            else EQ) lengths

getBestKeyLength arr = let ((x, _):_) = getBestKeyLengthList arr in x
                                                                                
-- Key hacking
isValid x = let x' = ord x in x' >= 32 && x' <= 127

hasInvalid string = case find (\x -> not $ isValid x) string of
                         Just _ -> True
                         Nothing -> False

filterInvalid strings = filter (\(_, x) -> not $ hasInvalid x) strings

onlyValids = filter (\x -> p x > 0.0)

rankDecryptedEnglish string = let str2 = onlyValids string ; freq = assocs $ characterFrequency str2; calculatedDist = map (\(x, count) -> ((count / (fromIntegral $ length str2)) - (p $ chr x)) ^ 2) freq in sum calculatedDist

rankAllDecryptedEnglish = map (\(val, str) -> (val, str, rankDecryptedEnglish str))

getBestKeyMatchList strings = sortBy (\(_, _, rank1) (_, _, rank2) -> if rank1 < rank2 then LT
                                                                                         else if rank1 > rank2 then GT
                                                                                         else EQ) strings

bruteForce string = map (\x -> (x, map (\y -> chr $ xor (ord y) x) string)) [0..255]

findAndSortKeys keyLength encData = do
  group <- getNthCharactersAndOffsets keyLength encData
  let validFromFirstGroup = filterInvalid $ bruteForce group
  let rankedEnglish = rankAllDecryptedEnglish validFromFirstGroup
  return $ map (\(x, _, _) -> x) $ getBestKeyMatchList rankedEnglish

-- Decrypt
decrypt string key = map (\x -> chr $ xor (ord $ string !! x) (key !! (mod x (length key)))) [0..(length string) - 1]

-- Wirecode
getIgnoreList x = do
  putStrLn $ "Input ignore value for index " ++ (show x)
  inputval <- getLine 
  return (read inputval :: Int)

getIndexes keys indexes = zipWith (\keys index -> keys !! index) keys indexes

replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

interlace [] _ _ = []
interlace list n chr = let (before, after) = splitAt n list in before ++ (chr:interlace after n chr)

--findCorrectKey :: [[Int]] -> [Char] -> [Int] -> IO ()
findCorrectKey keys encryptedData ignoreList = do
  let keyLength = length keys
  let key = getIndexes keys ignoreList
  let decrypted = (decrypt encryptedData key)
  putStrLn $ "Key is " ++ (show key)
  putStrLn $ interlace decrypted keyLength '|'
  putStrLn $ "Write an index to change it (prefix with minus to change back)"
  putStrLn $ "Write 0 to print without interlacing"
  inputval <- getLine
  let changeInput = (read inputval :: Int)
  let changeIndex = (abs changeInput) - 1
  let changeBy = if changeInput > 0 then 1 else -1
  if changeInput == 0 then putStrLn decrypted
    else findCorrectKey keys encryptedData (replaceNth changeIndex ((ignoreList !! changeIndex) + changeBy) ignoreList)

main = do
  printDescription
  putStrLn "Input the data to hack in hex encoding"
  encryptedData_ <- getLine
  let encryptedData  = hexToBin encryptedData_
  let keyLengthList = getBestKeyLengthList encryptedData
  putStrLn $ "Probable key sizes are in order " ++ (show keyLengthList)
  putStrLn "Choose a key length"
  inputval <- getLine
  let keyLen = read inputval :: Int
  putStrLn $ show keyLen
  let keys = findAndSortKeys keyLen encryptedData
  findCorrectKey keys encryptedData (replicate (length keys) 0)
