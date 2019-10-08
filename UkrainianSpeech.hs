{-
MIT License
Copyright (c) 2019 OleksandrZhabenko
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

module Main (
-- * Main Function
  main, 
-- * Functions used in the main function itself
  createSoundsForSyllable, endS, combineSoundsLs3, words2, 
-- * Functions used for preprocessing
  concatPauseRecords, assimilationFirst, isSpecialNonSpace, separatePunct, softAssociate, ukrainianLast2, ukrainianJottedLast, ukrainianJotted1, changeAssimilative, 
    separatePunct0, ukrainianToMoreSounding, changeH2X, change2BS, firstChange, readEnglishWithUkrainian, numberSounds, isFiltered, 
-- * Functions used to create syllables or special symbol sequences
  pFunctionP, pFunctionP0, hDivideMonths2, concatSoftSign, bGroups, hFunctionH, combineSoundsLs, createSyllablesReady, accountEmphasis, isVowelL, zeroSyllablePart, 
    createSyllablesMultiLast2, divideToUnits, isSimPRecord, concatPunct, convertSyllableToLanguage, createSyllablesMulti, divideToListOfSoundGroupsAsLists, listOfFrames, 
      divideConsonants, prepareToSyllables, listOfPoints, createSoundL, createSoundGroups, amountOfPartsForKthSyl, createSoundLChar, isSimilar, isConsonantL, isFilteredForLast2, 
-- * Functions that are used for sound creation itself
  punctuationPauseLength, punctuationPauseLength1, endE, addSoftSign, punctL, punctOpt, punctL11, punctL1, isDigitOrDot, stringToInteger, createZeroSyllable, createSoftSign, 
-- * Other functions that are used internally in these ones
  endOfExecutable, isSimilarPauseRecords, specialConcatBS, isVowelOrPunctuation, isVowelEG, dropWithFirst, isDigitOrDash, 
    isPunctOrSpaceB, oneToTuple2, isPunctOrSpace, takeWithFirst, changeToEsperanto, verbSpecialConv, isSpecial, changeToDecoded, continueLang, mapLS
) where

import System.CPUTime (getCPUTime)
import System.Process (callCommand)
import System.Directory (findExecutable)
import System.Environment (getArgs)
import qualified Data.List as L (groupBy)
import qualified Data.Char as DC (toLower, isDigit, isAlpha, isPunctuation, isSpace)
import Data.Maybe (Maybe(Just,Nothing),isJust,isNothing)
import qualified Data.ByteString.Lazy.Char8 as C (ByteString,length,dropWhile,takeWhile,drop,unpack,pack,cons,empty,singleton,head,tail,
  null,filter,groupBy,concat,any,span,foldr,readInteger,all)
import Prelude (Double,String,Char,Bool(True,False),Int,Integer,IO,FilePath,($!),($),(.),(==),(/=),(/),(*),(^),foldl1,(<),(<=),(>),(>=),(&&),(||),not,null,any,notElem,
  fst,snd,show,error,putStr,putStrLn,(+),(-),div,mod,(++),foldr,map,zip,zipWith,take,drop,takeWhile,concat,concatMap,toInteger,return,last,init,
    mapM_,filter,getContents,elem,last,head,tail,length,fromInteger,fromIntegral,otherwise,and,sum,all,words,unwords,fromIntegral,iterate,dropWhile)

-- | Main function
-- Головна функція
main :: IO ()
main = do
   args <- getArgs
   if (not . null $ args) && ((head args == "h") || (head args == "-h"))
     then do
       putStr "Введіть рядок українського тексту. За замовчуванням для багатоскладових слів наголос падатиме на передостанній склад у слові. "
       putStr "Якщо Ви бажаєте змінити наголос, тоді перед словом злитно з ним напишіть натуральне число, яке є порядковим номером складу,"
       putStr "на який падає наголос, починаючи з першого складу. Наприклад, \"3мальовнИчого\" означатиме, що наголошеним буде склад з \"И\". "
       putStrLn "Не ставте дефісів або інших розділювачів (у т. ч. пробілів). Не хвилюйтеся, ці числа НЕ будуть озвучені програмою. "
       putStrLn " "
       putStr "Якщо бажаєте, щоб програма швидше читала деякі склади або літери, то можете ввести як аргументи командного рядка "
       putStr "одне з чисел 1, або 2, або 3 (кожне наступне з яких дещо прискорює вимову деяких складів та літер порівняно з попереднім, "
       putStrLn "втім різниця не така велика). "
       putStrLn " "
       putStrLn "Якщо Ви бажаєте, щоб усі склади розбивалися для озвучування на звуки, то введіть як аргумент командного рядка \"-W\" або \"W\". "
       putStrLn ""
       putStr "Якщо бажаєте задати свою тривалість паузи, можете задати додаткову тривалість паузи, вставивши в текст на місці паузи набір символів "
       putStr "\"!1.253\", де цифри через крапку є звичайним записом числа типу Double, яке є бажаною додатковою тривалістю паузи, більшою за 0.1 секунди. "
       putStrLn "Інакше буде додано паузу 0.1 секунди. "
     else do
       createSoftSign
       nI2 <- getContents
       let pauseW x = do
                        mapM_ (createSoundsForSyllable getCPUTime args) x
                        zz <- getCPUTime
                        let t1 = 10000000000 + (zz `div` 10000000) in 
                          do
                            eS <- endS
                            callCommand $ "sox" ++ eS ++ " --multi-threaded  -n -r 22.05k -c 1 -b 16 " ++ show t1 ++ ".ee.wav delay 0.015 trim 0 0.014"
                              in mapM_ (pauseW . combineSoundsLs3 (if elem "W" args || elem "-W" args then 0 else 1)) (words2 nI2)
                              
-- | Function that is used instead of System.Info.os to check whether the eSpeak and SoX executables end in .exe
-- Функція, яка використовується замість System.Info.os, щоб перевірити, чи eSpeak і SoX програми закінчуються на .exe                            
endOfExecutable :: String -> IO String                              
endOfExecutable ys = do
                       xs <- findExecutable ys
                       if isJust xs 
                         then return ""
                         else do
                                zs <- findExecutable (ys ++ ".exe")
                                if isJust zs
                                  then return ".exe"
                                  else error ("Please, install the executable " ++ ys ++ " into the directory in the PATH variable! \n")
                                  
-- | Function that is used to find out the ending of eSpeak executable installed if any
-- Функція, яка використовується для того, щоб знайти закінчення eSpeak програми, якщо така встановлена в системі                                  
endE :: IO String
endE = endOfExecutable "espeak"

-- | Function that is used to find out the ending of SoX executable installed if any
-- Функція, яка використовується для того, щоб знайти закінчення SoX програми, якщо така встановлена в системі
endS :: IO String
endS = endOfExecutable "sox"
                              
-- | Function that checks the eSpeak and SoX executables existence and is used for soft sign sound creation
-- Функція, що перевіряє існування eSpeak і SoX додатків у системі та використовується для створення звуку для м'якого знаку
createSoftSign :: IO ()
createSoftSign = do
    eE <- endE
    eS <- endS
    callCommand $ "espeak" ++ eE ++ " -v esperanto -g 0 -w ь.wav -z \"j\"" 
    callCommand $ "sox" ++ eS ++ " --multi-threaded  ь.wav j.wav trim 0.02 0.037"

-- | Function that for the Ukrainian syllable represented as ((String, String),(String,Integer)) creates sounds
-- Функція, що для українського складу представленого як ((String, String),(String,Integer)) створює звуки
createSoundsForSyllable :: IO Integer -> [String] -> ((String, String),(String,Integer)) -> IO ()
createSoundsForSyllable time args ((xs, ys),(zs, k)) = case k of
    0 -> do
           t <- time
           let t1 = 10000000000 + (t `div` 10000000) in if null args
                                                          then punctuationPauseLength t1 xs ys zs args
                                                          else punctuationPauseLength1 t1 xs ys zs args
    _ -> do
           t <- time
           let t1 = 10000000000 + (t `div` 10000000) in
             if last xs == 'q'
                        then do
                               eE <- endE
                               callCommand $ "espeak" ++ eE ++ " -v " ++ ys ++ " " ++ zs ++ " -w " ++ show t1 ++ "." ++ filter (/= 'q') xs ++ ".wav \"" ++ filter (/= 'q') xs ++ "\"" 
                               addSoftSign $ show t1 ++ "." ++ filter (/= 'q') xs ++ ".wav"
                        else do
                               eE <- endE
                               callCommand $ "espeak" ++ eE ++ " -v " ++ ys ++ " " ++ zs ++ " -w " ++ show t1 ++ "." ++ filter (/= 'q') xs ++ ".wav \"" ++ filter (/= 'q') xs ++ "\"" 

-- | Function that prepares a String for processing by the eSpeak and SoX for non-zero-syllable words
-- Функція, яка готує слово з голосним для подальшої обробки  eSpeak та SoX
combineSoundsLs3 :: Int -> C.ByteString -> [((String, String), (String, Integer))]
combineSoundsLs3 k | k == 0 = combineSoundsLs30 pFunctionP0  
                   | otherwise = combineSoundsLs30 pFunctionP
  where combineSoundsLs30 p = concatMap hDivideMonths2 . concatSoftSign . bGroups p hFunctionH . combineSoundsLs
  
-- | Function that produces the list of Ukrainian strings from the primary Ukrainian string which can be further easily processed
-- Функція, яка створює список українських рядків з початкового українського рядка, які можуть бути легко оброблені далі
words2 :: String -> [C.ByteString]
words2 [] = []
words2 xs = concatPauseRecords . filter (not . C.null) . assimilationFirst . map (C.filter isSpecialNonSpace) . separatePunct . softAssociate . ukrainianLast2 . ukrainianJottedLast . ukrainianJotted1 .
  changeAssimilative . separatePunct0 . ukrainianToMoreSounding . changeH2X . C.pack . change2BS . filter isFiltered . map firstChange . concatMap readEnglishWithUkrainian . unwords . 
    map (\x -> if all DC.isDigit x then concatMap numberSounds x else x) . words $ xs

-- | Function-predicate to filter the characters used for further processment 
-- Функція-предикат, яка використовується для фільтрування потрібних символів для наступної обробки
isFiltered :: Char -> Bool
isFiltered x | x >= '\x044C' = 
  if x >= '\x02028'
    then if x >= '\x20AC'
           then x == '\x20AC' || x == '\x20B4' || x == '\x2103' || x == '\x2109' || x == '\x2122'
           else x <= '\x2029' || (x >= '\x2047' && x <= '\x2049') || (x >= '\x20A0' && x <= '\x20A4') 
    else if x <= '\x0457'
           then if x >= '\x0454'
                  then x /= '\x0455' && x <= '\x0457'
                  else x /= '\x044D' && x <= '\x044F'
           else x == '\x0491' || (x >= '\x2002' && x <= '\x2014') || x == '\x2026'
             | otherwise = 
  if x >= '\x003F'
    then if x >= '\x007B'
           then if x >= '\x00BB'
                  then x == '\x00BB' || (x >= '\x0430' && x <= '\x0449')
                  else (x >= '\x007B' && x <= '\x007F') || x == '\x00A0' || x == '\x00AB'
           else x == '\x003F' || x == '\x0040' || x == '\x005C' || x == '\x005F' || x == '\x0060'
    else if x <= '\x000F'
           then x /= '\x000C' && x /= '\x0007'
           else (x >= '\x0017' && x <= '\x001F') || (x >= '\x0020' && x <= '\x0022') || (x >= '\x0026' && x <= '\x003B')
              
-- | Function-predicate to check whether its argument is from the special volatile punctuation group that sets the duration of the additional pause
-- Функція-предикат, яка перевіряє, чи її аргумент є зі спеціальної довільної групи, яка встановлює тривалість додаткової паузи
isSimPRecord :: Char -> Bool
isSimPRecord x = (x == '\x0021') || isDigitOrDot x

-- | Function-predicate to check whether its arguments are both from the special volatile punctuation group that sets the duration of the additional pause (is used to group them by)
-- Функція-предикат, яка перевіряє, чи її аргументи обидва є зі спеціальної довільної групи, яка встановлює тривалість додаткової паузи (використовується, щоб їх згрупувати)
isSimilarPauseRecords :: C.ByteString -> C.ByteString -> Bool
isSimilarPauseRecords x y = C.all isSimPRecord x && C.all isSimPRecord y

-- | Function that concatenates a list of ByteString into the single ByteString and is used to set the duration of the additional pause
-- Функція, яка ппоєднує список ByteString у один ByteString і яка використовується для встановлення тривалості додаткової паузи
specialConcatBS :: [C.ByteString] -> C.ByteString
specialConcatBS xs | not . null $ xs =
  if null . tail $ xs 
    then head xs 
    else C.concat xs
                   | otherwise = C.empty

-- | Function that is used for creation of the volatile pauses
-- Функція, яка використовується для створення довільних пауз
concatPauseRecords :: [C.ByteString] -> [C.ByteString]
concatPauseRecords xs | not . null $ xs =
  map specialConcatBS . L.groupBy isSimilarPauseRecords $ xs
                      | otherwise = []

-- | Function-predicate used for filtering and indicating that a Char '\x000C' has not space semantics after encoding to ByteString
-- Функція-предикат, яка використовується для фільтрування та вказівки на те, що символ '\x000C' має не семантику пробільного символу після кодування в ByteString
isSpecialNonSpace :: Char -> Bool
isSpecialNonSpace x = case x of
  '\x000C' -> True
  _        -> not . DC.isSpace $ x

-- | Function that is used to create punctuation pauses
-- Функція, що використовується для створення пунктуаційних пауз
punctuationPauseLength :: Integer -> String -> String -> String -> [String] -> IO ()
punctuationPauseLength t1 xs = punctL (punctOpt xs) t1 xs 

-- | Function that is used to create punctuation pauses if args include "1", or "2", or "3"
-- Функція, що використовується для створення пунктуаційних пауз, якщо args включають "1", або "2", або "3"
punctuationPauseLength1 :: Integer -> String -> String -> String -> [String] -> IO ()
punctuationPauseLength1 t1 xs = punctL11 (punctOpt xs) t1 xs 

-- | Function that checks the eSpeak and SoX executables existence and is used for soft sign sound appending to the syllable or word
-- Функція, що перевіряє існування eSpeak і SoX додатків у системі та використовується для додавання м'якого знаку до кінцевого приголосного у слові чи складі
addSoftSign :: FilePath -> IO ()
addSoftSign file =
  do
    eS <- endS
    callCommand $ "sox" ++ eS ++ " --multi-threaded  " ++ file ++ " m." ++  file ++ " trim 0 -0.01"
    callCommand $ "sox" ++ eS ++ " --multi-threaded  m." ++ file ++ " j.wav " ++ file

-- | Function that divides wrongly sounding syllables for abbeviations of esperanto months into parts
-- Функція, яка ділить неправильно озвучувані склади для абревіатур назв місяців мовою есперанто на дві частини
hDivideMonths2:: ((String, String), (String, Integer)) -> [((String, String), (String, Integer))]
hDivideMonths2 ((xs, ys), (zs, k)) = let y = head xs in if y >= 'j'
  then if y `elem` "jmos"
         then if y == 'j'
                then case xs of
                  "jan" -> [(("ja", ys), (zs, k)),(("n", ys), (zs, 0))]
                  "jul" -> [(("ju", ys), (zs, k)),(("l", ys), (zs, 0))]
                  "jun" -> [(("ju", ys), (zs, k)),(("n", ys), (zs, 0))]
                  _     -> [((xs, ys), (zs, k))]
                else if xs >= "okt"
                       then case xs of
                         "okt" -> [(("ok", ys), (zs, k)),(("t", ys), (zs, 0))]
                         "sept" -> [(("sep", ys), (zs, k)),(("t", ys), (zs, 0))]
                         _     -> [((xs, ys), (zs, k))]
                       else case xs of
                         "maj" -> [(("ma", ys), (zs, k)),(("j", ys), (zs, 0))]
                         "mar" -> [(("ma", ys), (zs, k)),(("r", ys), (zs, 0))]
                         _     -> [((xs, ys), (zs, k))]
         else [((xs, ys), (zs, k))]
  else if y `elem` "adf"
         then case xs of
           "apr" -> [(("ap", ys), (zs, k)),(("r", ys), (zs, 0))]
           "dec" -> [(("de", ys), (zs, k)),(("c", ys), (zs, 0))]
           "feb" -> [(("fe", ys), (zs, k)),(("b", ys), (zs, 0))]
           _     -> [((xs, ys), (zs, k))]
         else [((xs, ys), (zs, k))]

-- | Function that concatenates alone soft sign with the previous letter (Esperanto or Greek)
-- Функція, яка з'єднує ізольований м'який знак з попереднім приголосним (есперанто чи грецькою)
concatSoftSign ::  [((String, String), (String, Integer))] ->  [((String, String), (String, Integer))]
concatSoftSign (x:xs) = if null xs
  then [x]
  else case fst . fst . head $ xs of
    "q" | (fst . fst $ x) == "γ" -> x:concatSoftSign (tail xs)
        | otherwise -> (((fst . fst $ x) ++ "q", "esperanto" ), snd x):concatSoftSign (tail xs)
    _ -> x:concatSoftSign xs
concatSoftSign [] = []    

-- | Function that converts zero-syllable groups of consonant sounds into separate sounds for further processing
-- Функція, що перетворює безголосні групи приголосних у окремі звуки для подальшої обробки
hFunctionH :: ((String, String), (String, Integer)) -> [((String, String), (String, Integer))]
hFunctionH (([x, y], ys), (zs, _)) | x == 'd' = case y of
  'z' -> [(("dz", ys), (zs, 0))]
  'ĵ' -> [(("dĵ", ys), (zs, 0))]
  _   -> [(("d", ys), (zs, 0)), (([y], ys), (zs, 0))]
                                   | otherwise = [(([x], ys), (zs, 0)), (([y], ys), (zs, 0))]
hFunctionH ((u:t:ts, ys), (zs, _)) | u == 'd' = case t of
  'z' -> (("dz", ys), (zs, 0)):hFunctionH ((ts, ys), (zs, 0))
  'ĵ' -> (("dĵ", ys), (zs, 0)):hFunctionH ((ts, ys), (zs, 0))
  _   -> (("d", ys), (zs, 0)):hFunctionH  ((t:ts, ys), (zs, 0))
                                   | otherwise = (([u], ys), (zs, 0)):hFunctionH ((t:ts, ys), (zs, 0))
hFunctionH (([x], ys), (zs, _)) = [(([x], ys), (zs, 0))]
hFunctionH  (([], _), (_, _)) = []                                     

-- | Function that combines the emphasis and dividing into sound groups into languages
-- Функція, що поєднує наголос і поділ на групи звуків для мов
combineSoundsLs :: C.ByteString -> [((String, String), (String, Integer))]
combineSoundsLs xs | C.null xs = []
                   | otherwise = createSyllablesReady . accountEmphasis $ xs

-- | Function that applies additional function h to a if p is True on a
-- Функція, що застосовує додаткову функцію h до a, якщо p є Істина на a
bGroups :: (a -> Bool) -> (a -> [a]) -> [a] -> [a]
bGroups p h = concatMap (\x -> if p x then h x else [x])

-- | Function-predicate that checks whether the hFunctionH must be applied to the ((String, String), (String, Integer))
-- Функція-предикат, яка перевіряє, чи має бути застосована hFunctionH до ((String, String), (String, Integer))
pFunctionP :: ((String, String), (String, Integer)) -> Bool
pFunctionP ((xs, _), (_, _)) = ((not . any isVowelOrPunctuation $ xs) || ((length . takeWhile (not . isVowelEG) $ xs) > 3) || ((length . dropWithFirst isVowelEG $ xs ) > 3)) && (not . any DC.isPunctuation $ xs)

-- | Function-predicate that checks whether the hFunctionH must be applied to the ((String, String), (String, Integer)) for args containing -W or W
-- Функція-предикат, яка перевіряє, чи має бути застосована hFunctionH до ((String, String), (String, Integer)) для args, які містять -W або W
pFunctionP0 :: ((String, String), (String, Integer)) -> Bool
pFunctionP0 ((xs, _), (_, _)) = not . any DC.isPunctuation $ xs

-- | Additional function to take into account assimilation rules that depend on the position in the word of the group of Ukrainian sounds
-- Додаткова функція, щоб врахувати правила асиміляції, які залежать від положення у слові групи українських звуків
assimilationFirst :: [C.ByteString] -> [C.ByteString]
assimilationFirst [] = []
assimilationFirst xs = map (\x -> let z = C.dropWhile isDigitOrDash x in if C.null z
  then C.takeWhile isDigitOrDash x
  else case C.head z of
    '\x007A' -> if C.null . C.tail $ z
       then x
       else case C.head . C.tail $ z of
         '\x0049' -> C.concat [C.takeWhile isDigitOrDash x, '\x0042' `C.cons` C.singleton '\x0049', C.drop 2 z]
         '\x0042' -> C.concat [C.takeWhile isDigitOrDash x, '\x0042' `C.cons` C.singleton '\x0042', C.drop 2 z]
         _  -> x
    _  -> x) xs

-- | Function that separates punctuation from the words for further processing
-- Функція, що відділяє пунктуацію від слів для подальшої обробки
separatePunct :: C.ByteString -> [C.ByteString]
separatePunct xs | C.null xs = [C.empty]
                 | (not . C.null . C.tail $ xs) && C.head xs == '!' = let z2 = C.span (\x -> isPunctOrSpaceB x || DC.isDigit x) xs in C.singleton ' ':fst z2:separatePunct (snd z2)
                 | isPunctOrSpaceB . C.head $ xs = let z = C.span isPunctOrSpaceB xs in C.singleton ' ':fst z:separatePunct (snd z)
                 | otherwise = let zN = C.span (not . isPunctOrSpaceB) xs in fst zN:separatePunct (snd zN)

-- | Additional function to check whether its argument takes into account that soft sign is considered as a part of a consonant
-- Додаткова функція, яка перевіряє, чи її аргумент бере до уваги, що м'який знак вважається частиною приголосного
softAssociate :: C.ByteString -> C.ByteString
softAssociate xs | C.null xs =
  C.empty
                 | C.null . C.tail $ xs =
  C.singleton z
                 | y == '\x0071' =
  if z >= '\x006B'
        then if z >= '\x0073'
          then if z >= '\x0077'
            then case z of
              '\x0077' -> '\x0050' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x007A' -> '\x0055' `C.cons` softAssociate (C.tail . C.tail $ xs)
              _        -> z `C.cons` softAssociate (C.tail . C.tail $ xs)
            else case z of
              '\x0073' -> '\x000C' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x0074' -> '\x0010' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x0076' -> '\x0078' `C.cons` softAssociate (C.tail . C.tail $ xs)
              _        -> z `C.cons` softAssociate (C.tail . C.tail $ xs)
          else if z <= '\x006D'
            then case z of
              '\x006B' -> '\x0056' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x006C' -> '\x0057' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x006D' -> '\x0058' `C.cons` softAssociate (C.tail . C.tail $ xs)
              _        -> z `C.cons` softAssociate (C.tail . C.tail $ xs)
            else case z of
              '\x006E' -> '\x0059' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x0070' -> '\x005A' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x0072' -> '\x0007' `C.cons` softAssociate (C.tail . C.tail $ xs)
              _        -> z `C.cons` softAssociate (C.tail . C.tail $ xs)
        else if z <= '\x004F'
          then if z >= '\x0049'
            then case z of
              '\x0049' -> '\x0014' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x004F' -> '\x0051' `C.cons` softAssociate (C.tail . C.tail $ xs)
              _        -> z `C.cons` softAssociate (C.tail . C.tail $ xs)
            else case z of
              '\x0042' -> '\x0015' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x0043' -> '\x0016' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x0045' -> '\x0054' `C.cons` softAssociate (C.tail . C.tail $ xs)
              _        -> z `C.cons` softAssociate (C.tail . C.tail $ xs)
          else if z <= '\x0064'
            then case z of
              '\x0062' -> '\x005E' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x0063' -> '\x0013' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x0064' -> '\x0053' `C.cons` softAssociate (C.tail . C.tail $ xs)
              _        -> z `C.cons` softAssociate (C.tail . C.tail $ xs)
            else case z of
              '\x0066' -> '\x0011' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x0067' -> '\x0052' `C.cons` softAssociate (C.tail . C.tail $ xs)
              '\x0068' -> '\x0012' `C.cons` softAssociate (C.tail . C.tail $ xs)
              _        -> z `C.cons` softAssociate (C.tail . C.tail $ xs)
                 | otherwise = z `C.cons` y `C.cons` softAssociate (C.tail . C.tail $ xs)
    where z = C.head xs
          y = C.head . C.tail $ xs

-- | Function that applies assimilation rules to the Ukrainian preprocessed string
-- Функція, яка застосовує правила асиміляції до українського попередньо обробленого рядка
ukrainianLast2 :: C.ByteString -> C.ByteString
ukrainianLast2 = fst . C.foldr f v
  where v = (C.empty, C.empty)
        f x (zs, xs) = case x of
          '\x0045' -> if and [not . C.null $ xs, not . C.null . C.tail $ xs, C.head xs == '\x0073' || C.head xs == '\x0063' || C.head xs == '\x0013',
            (C.head . C.tail $ xs) == '\x0071' || (C.head . C.tail $ xs) == '\x0069']
                        then ('\x0055' `C.cons` zs, x `C.cons` xs)
                        else ('\x0045' `C.cons` zs, x `C.cons` xs)
          '\x0042' -> if and [not . C.null $ xs, not . C.null . C.tail $ xs, C.head xs == '\x0073' || C.head xs == '\x0063' || C.head xs == '\x0013',
            (C.head . C.tail $ xs) == '\x0071' || (C.head . C.tail $ xs) == '\x0069']
                        then ('\x000C' `C.cons` zs, x `C.cons` xs)
                        else ('\x0042' `C.cons` zs, x `C.cons` xs)
          '\x0049' -> if and [not . C.null $ xs, not . C.null . C.tail $ xs, C.head xs == '\x0073' || C.head xs == '\x0063' || C.head xs == '\x0013',
            (C.head . C.tail $ xs) == '\x0071' || (C.head . C.tail $ xs) == '\x0069']
                        then ('\x0013' `C.cons` zs, x `C.cons` xs)
                        else ('\x0049' `C.cons` zs, x `C.cons` xs)
          _   -> let ys = C.dropWhile (not . isFilteredForLast2) xs in if x < '\x0074'
                   then case x of
                     '\x0064' | (not . C.null $ ys) && elem (C.head ys) "\x0073\x007A\x0063" ->
                        ('\x0064' `C.cons` '\x007A' `C.cons` zs, x `C.cons` xs)
                              | (not . C.null $ ys) && elem (C.head ys) "\x0045\x0042\x0049" ->
                        ('\x0064' `C.cons` '\x0045' `C.cons` zs, x `C.cons` xs)
                              | otherwise -> ('\x0064' `C.cons` zs, x `C.cons` xs)
                     '\x0073' | (not . C.null $ ys) && (C.head ys == '\x0042') ->
                        ('\x0042' `C.cons` zs, x `C.cons` xs)
                              | otherwise -> ('\x0073' `C.cons` zs, x `C.cons` xs)
                     _        -> (x `C.cons` zs, x `C.cons` xs)
                   else case x of
                     '\x0074' | (not . C.null $ ys) && (C.head ys == '\x0063') ->
                        ('\x0063' `C.cons` zs, x `C.cons` xs)
                              | (not . C.null $ ys) && elem (C.head ys) "\x0042\x0049" ->
                        ('\x0049' `C.cons` zs, x `C.cons` xs)
                              | otherwise -> ('\x0074' `C.cons` zs, x `C.cons` xs)
                     '\x007A' | (not . C.null $ ys) && elem (C.head ys) "\x0042\x0045\x0049" ->
                        ('\x0045' `C.cons` zs, x `C.cons` xs)
                              | (not . C.null $ ys) && (C.head ys == '\x0064') && 
                        (not . C.null . C.tail $ ys) && ((C.head . C.tail $ ys) == '\x0045') ->
                          ('\x0045' `C.cons` zs, x `C.cons` xs)
                              | otherwise -> ('\x007A' `C.cons` zs, x `C.cons` xs)
                     _        -> (x `C.cons` zs, x `C.cons` xs)

-- | Function-predicate that is used to eliminate the quantity of ukrainianLast2 applications
-- Функція-предикат, яка використовується для зменшення кількості застосувань функції ukrainianLast2
isFilteredForLast2 :: Char -> Bool
isFilteredForLast2 z | z <= '\x005A' =
  if z <= '\x0016'
    then z >= '\x0010' || (z == '\x0007' || z == '\x000C')
    else z >= '\x0041' && z /= '\x0048'
                     | z <= '\x00F9' =
  if z <= '\x007A'
    then z >= '\x0061' || z == '\x005E'
    else z >= '\x00E1' && z <= '\x00F9'
                     | otherwise = DC.isPunctuation z                     

-- | Function to convert Ukrainian "я", "ю", "є" and "ї" into some other String for syllables processing
-- Функція для перетворення українських "я", "ю", "є" та "ї" на деякі інші рядки для обробки складів
ukrainianJottedLast :: C.ByteString -> C.ByteString
ukrainianJottedLast xs | C.null xs =
  C.empty
                       | otherwise =
  case C.head xs of
    '\x0047' -> '\x0071' `C.cons` '\x0061' `C.cons` ukrainianJottedLast (C.tail xs)
    '\x004A' -> '\x0071' `C.cons` '\x0075' `C.cons` ukrainianJottedLast (C.tail xs)
    '\x0046' -> '\x0071' `C.cons` '\x0065' `C.cons` ukrainianJottedLast (C.tail xs)
    '\x0044' -> '\x006A' `C.cons` '\x0069' `C.cons` ukrainianJottedLast (C.tail xs)
    '\x0074' -> if C.null . C.tail $ xs
                  then C.singleton '\x0074'
                  else case C.head . C.tail $ xs of
                    '\x0073' -> '\x0063' `C.cons` ukrainianJottedLast (C.tail . C.tail $ xs)
                    _        -> '\x0074' `C.cons` ukrainianJottedLast (C.tail xs)
    _  -> C.head xs `C.cons` ukrainianJottedLast (C.tail xs)

-- | Optimized function to convert Ukrainian "я", "ю", "є" and "ї" into some other String for syllables processing
-- Оптимізована функція для перетворення українських "я", "ю", "є" та "ї" на деякі інші рядки для обробки складів
ukrainianJotted1 :: C.ByteString -> C.ByteString
ukrainianJotted1 = fst . C.foldr f v
  where v = (C.empty, C.empty)
        f x (ys,xs) | C.null xs =
           (C.singleton x, x `C.cons` xs)
                    | otherwise =
           if k == '\x0044'
             then (x `C.cons` '\x006A' `C.cons` '\x0069' `C.cons` C.tail ys, x `C.cons` xs)
             else if k == '\x0047' || k == '\x0046' || k == '\x004A'
               then if x >= '\x0030' && x <= '\x0039'
                 then (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                 else if x >= '\x004B'
                   then if x >= '\x006F'
                     then case x of
                       '\x006F' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                       '\x0075' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                       '\x0079' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                       _        -> (x `C.cons` '\x0071' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs) 
                     else if x <= '\x0061'
                       then case x of
                         '\x004B' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                         '\x0061' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                         _        -> (x `C.cons` '\x0071' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs) 
                       else case x of
                         '\x0065' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                         '\x0069' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                         _        -> (x `C.cons` '\x0071' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs) 
                   else if x <= '\x0044'
                     then if x >= '\x002D'
                       then case x of
                         '\x002D' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                         '\x0044' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                         _        -> (x `C.cons` '\x0071' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs) 
                       else case x of
                         '\x0020' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                         '\x0027' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                         _        -> (x `C.cons` '\x0071' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs) 
                     else case x of
                       '\x0046' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                       '\x0047' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                       '\x004A' -> (x `C.cons` '\x006A' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs)
                       _        -> (x `C.cons` '\x0071' `C.cons` jC k `C.cons` C.tail ys, x `C.cons` xs) 
               else (x `C.cons` ys, x `C.cons` xs)
                           where k = C.head xs
                                 jC y | y == '\x0047' = '\x0061'
                                      | y == '\x004A' = '\x0075'
                                      | y == '\x0046' = '\x0065'
                                      | otherwise = y

-- | Function that makes some assimilation changes for correct Ukrainian pronunciation
-- Функція, що робить деякі асиміляційні зміни для правильної української вимови
changeAssimilative :: C.ByteString -> C.ByteString
changeAssimilative xs | C.null xs = C.empty
                      | C.head xs == '\x006E' =
  if C.null . C.tail $ xs
    then xs
    else case C.head . C.tail $ xs of
      '\x0074' -> if C.null . C.tail . C.tail $ xs
        then xs
        else case C.head . C.tail . C.tail $ xs of
          '\x0073' -> if C.null . C.tail . C.tail . C.tail $ xs
            then xs
            else case C.head . C.tail . C.tail . C.tail $ xs of
              '\x0074' -> '\x006E' `C.cons` '\x0073' `C.cons` '\x0074' `C.cons` changeAssimilative (C.tail . C.tail . C.tail . C.tail $ xs)
              _  -> '\x006E' `C.cons` '\x0063' `C.cons` changeAssimilative (C.tail . C.tail . C.tail $ xs)
          '\x000C' -> if C.null . C.tail . C.tail . C.tail $ xs
            then xs
            else case C.head . C.tail . C.tail . C.tail $ xs of
              '\x006B' -> '\x0059' `C.cons` '\x000C' `C.cons` '\x0068' `C.cons` changeAssimilative (C.tail . C.tail . C.tail . C.tail $ xs)
              _  -> '\x006E' `C.cons` '\x0013' `C.cons` changeAssimilative (C.tail . C.tail . C.tail $ xs)
          _  -> '\x006E' `C.cons` '\x0074' `C.cons` changeAssimilative (C.tail . C.tail $ xs)
      _        -> '\x006E' `C.cons` changeAssimilative (C.tail xs)
                      | C.head xs == '\x0073' = 
  if C.null . C.tail $ xs
    then xs
    else case C.head . C.tail $ xs of
      '\x0074' -> if C.null . C.tail . C.tail $ xs
        then xs
        else case C.head . C.tail . C.tail $ xs of
          '\x000C' -> if C.null . C.tail . C.tail . C.tail $ xs
            then xs
            else case C.head . C.tail . C.tail . C.tail $ xs of
              '\x006B' -> '\x000C' `C.cons` '\x000C' `C.cons` '\x006B' `C.cons` changeAssimilative (C.tail . C.tail . C.tail . C.tail $ xs)
              _  -> '\x0073' `C.cons` '\x0074' `C.cons` '\x000C' `C.cons` changeAssimilative (C.tail . C.tail . C.tail $ xs)
          '\x0013' -> if C.null . C.tail . C.tail . C.tail $ xs
            then xs
            else '\x000C' `C.cons` '\x0013' `C.cons` '\x0013' `C.cons` changeAssimilative (C.tail . C.tail . C.tail $ xs)
          '\x0049' -> if C.null . C.tail . C.tail . C.tail $ xs
            then xs
            else '\x0042' `C.cons` '\x0049' `C.cons` changeAssimilative (C.tail . C.tail . C.tail $ xs)
          '\x0064' -> if C.null . C.tail . C.tail . C.tail $ xs
            then xs
            else '\x007A' `C.cons` '\x0064' `C.cons` changeAssimilative (C.tail . C.tail . C.tail $ xs)
          '\x0073' -> if C.null . C.tail . C.tail . C.tail $ xs
            then xs
            else '\x0073' `C.cons` '\x0073' `C.cons` changeAssimilative (C.tail . C.tail . C.tail $ xs)
          _  -> '\x0073' `C.cons` '\x0074' `C.cons` changeAssimilative (C.tail . C.tail $ xs)
      _        -> '\x0073' `C.cons` changeAssimilative (C.tail xs)
                      | C.head xs == '\x0047' = 
  if C.null . C.tail $ xs
    then xs
    else case C.head . C.tail $ xs of
      '\x0074' -> if C.null . C.tail . C.tail $ xs
        then xs
        else case C.head . C.tail . C.tail $ xs of
          '\x0064' -> if C.null . C.tail . C.tail . C.tail $ xs
            then xs
            else case C.head . C.tail . C.tail . C.tail $ xs of
              '\x0065' -> if C.null . C.tail . C.tail . C.tail . C.tail $ xs
                then xs
                else case C.head . C.tail . C.tail . C.tail . C.tail $ xs of
                  '\x0073' -> if C.null . C.tail . C.tail . C.tail . C.tail . C.tail $ xs
                    then xs
                    else case C.head . C.tail . C.tail . C.tail . C.tail . C.tail $ xs of
                      '\x0047' -> if C.null . C.tail . C.tail . C.tail . C.tail . C.tail . C.tail $ xs
                        then xs
                        else case C.head . C.tail . C.tail . C.tail . C.tail . C.tail . C.tail $ xs of
                          '\x0074' -> '\x006A' `C.cons` '\x0061' `C.cons` '\x0064' `C.cons` '\x0065' `C.cons` '\x000C' `C.cons` '\x0061' `C.cons` '\x0074' `C.cons`
                            changeAssimilative (C.tail . C.tail . C.tail . C.tail . C.tail . C.tail . C.tail $ xs)
                          _  -> '\x0047' `C.cons` '\x0074' `C.cons` '\x0064' `C.cons` '\x0065' `C.cons` '\x0073' `C.cons` '\x0047' `C.cons` 
                            changeAssimilative (C.tail . C.tail . C.tail . C.tail . C.tail . C.tail $ xs)
                      _        -> '\x0047' `C.cons` '\x0074' `C.cons` '\x0064' `C.cons` '\x0065' `C.cons` '\x0073'`C.cons`
                        changeAssimilative (C.tail . C.tail . C.tail . C.tail . C.tail $ xs)
                  _  -> '\x0047' `C.cons` '\x0074' `C.cons` '\x0064' `C.cons` '\x0065' `C.cons` changeAssimilative (C.tail . C.tail . C.tail . C.tail $ xs)
              _  -> '\x0047' `C.cons` '\x0074' `C.cons` '\x0064' `C.cons` changeAssimilative (C.tail . C.tail . C.tail $ xs)
          _        -> '\x0047' `C.cons` '\x0074' `C.cons` changeAssimilative (C.tail . C.tail $ xs)
      _         -> '\x0047' `C.cons` changeAssimilative (C.tail xs)
                      | otherwise = C.head xs `C.cons` changeAssimilative (C.tail xs)

-- | Function that separates punctuation marks from the words
-- Функція, яка відділяє пунктуаційні знаки від слів
separatePunct0 :: C.ByteString -> C.ByteString
separatePunct0 = C.foldr f v
  where v = C.empty
        f x ys | DC.isPunctuation x || (x >= '\x00E1' && x <= '\x00F9') || (x >= '\x0030' && x <= '\x0039') = ' ' `C.cons` x `C.cons` ' ' `C.cons` ys
               | otherwise = x `C.cons` ys

-- | Function that primarily converts Ukrainian line into more sounds-based line and more oriented to more using prosodical information
-- Функція, що початково перетворює український рядок на більш орінтований на звуки рядок і більш орієнтований на використання просодійної інформації
ukrainianToMoreSounding :: C.ByteString -> C.ByteString
ukrainianToMoreSounding xs = if C.null xs
                               then C.empty
                               else let f ys = if not . C.null . C.tail $ ys
                                                 then case C.head . C.tail $ ys of
                                                   '\x0047' -> C.head ys `C.cons` '\x006A' `C.cons` '\x0061' `C.cons` ukrainianToMoreSounding (C.tail . C.tail $ ys)
                                                   '\x004A' -> C.head ys `C.cons` '\x006A' `C.cons` '\x0075' `C.cons` ukrainianToMoreSounding (C.tail . C.tail $ ys)
                                                   '\x0046' -> C.head ys `C.cons` '\x006A' `C.cons` '\x0065' `C.cons` ukrainianToMoreSounding (C.tail . C.tail $ ys)
                                                   '\x0044' -> C.head ys `C.cons` '\x006A' `C.cons` '\x0069' `C.cons` ukrainianToMoreSounding (C.tail . C.tail $ ys)
                                                   _  -> C.head ys `C.cons` ukrainianToMoreSounding (C.tail ys)
                                                 else C.singleton . C.head $ ys in let zz = C.head xs in let z2 zs = C.head zs `C.cons` ukrainianToMoreSounding (C.tail zs) in if zz >= '\x0051'
                                                   then if zz >= '\x0058'
                                                          then if zz >= '\x005E'
                                                                 then case zz of
                                                                   '\x005E' -> f xs
                                                                   '\x0071' -> f xs
                                                                   '\x0078' -> f xs
                                                                   _        -> z2 xs
                                                                 else case zz of
                                                                   '\x0058' -> f xs
                                                                   '\x0059' -> f xs
                                                                   '\x005A' -> f xs
                                                                   _        -> z2 xs
                                                          else if zz >= '\x0054'
                                                                 then if zz >= '\x0056'
                                                                        then case zz of
                                                                          '\x0056' -> f xs
                                                                          '\x0057' -> f xs
                                                                          _        -> z2 xs
                                                                        else case zz of
                                                                          '\x0054' -> f xs
                                                                          '\x0055' -> f xs
                                                                          _        -> z2 xs
                                                                 else case zz of
                                                                   '\x0051' -> f xs
                                                                   '\x0052' -> f xs
                                                                   '\x0053' -> f xs
                                                                   _        -> z2 xs
                                                   else if zz <= '\x0013'
                                                          then if zz <= '\x0010'
                                                                 then case zz of
                                                                   '\x0007' -> f xs
                                                                   '\x000C' -> f xs
                                                                   '\x0010' -> if not . C.null . C.tail $ xs
                                                                                 then case C.head . C.tail $ xs of
                                                                                   '\x0073' -> if not . C.null . C.tail . C.tail $ xs
                                                                                                 then if (C.head . C.tail . C.tail $ xs) == '\x0047'
                                                                                                        then '\x0013' `C.cons` '\x0013' `C.cons` '\x0061' `C.cons`
                                                                                                          ukrainianToMoreSounding (C.tail . C.tail . C.tail $ xs)
                                                                                                        else '\x0010' `C.cons` '\x0073' `C.cons` ukrainianToMoreSounding (C.tail . C.tail $ xs)
                                                                                                 else '\x0010' `C.cons` C.singleton '\x0073'
                                                                                   _  -> '\x0010' `C.cons` ukrainianToMoreSounding (C.tail xs)
                                                                                 else C.singleton '\x0010'
                                                                   _        -> z2 xs
                                                                 else case zz of
                                                                   '\x0011' -> f xs
                                                                   '\x0012' -> f xs
                                                                   '\x0013' -> f xs
                                                                   _        -> z2 xs
                                                          else if zz <= '\x0020'
                                                                 then if zz <= '\x0015'
                                                                        then case zz of
                                                                          '\x0014' -> f xs
                                                                          '\x0015' -> f xs
                                                                          _        -> z2 xs
                                                                        else case zz of
                                                                          '\x0016' -> f xs
                                                                          '\x0020' -> if not . C.null . C.tail $ xs
                                                                                        then case C.head . C.tail $ xs of
                                                                                          '-' -> if not . C.null . C.tail . C.tail $ xs
                                                                                                   then if (C.head . C.tail . C.tail $ xs) == '-'
                                                                                                          then ' ' `C.cons` ukrainianToMoreSounding (C.tail . C.tail . C.tail $ xs)
                                                                                                          else ' ' `C.cons` '-' `C.cons` ukrainianToMoreSounding (C.tail xs)
                                                                                                   else C.singleton ' '
                                                                                          _  -> ' ' `C.cons` ukrainianToMoreSounding (C.tail xs)
                                                                                        else C.singleton ' '
                                                                          _        -> z2 xs
                                                                 else if zz >= '\x004B'
                                                                        then case zz of
                                                                          '\x004B' -> if not . C.null . C.tail $ xs
                                                                                        then case C.head . C.tail $ xs of
                                                                                          '\x0047' -> '\x006A' `C.cons` '\x006A' `C.cons` '\x0061' `C.cons` ukrainianToMoreSounding (C.tail . C.tail $ xs)
                                                                                          '\x004A' -> '\x006A' `C.cons` '\x006A' `C.cons` '\x0075' `C.cons` ukrainianToMoreSounding (C.tail . C.tail $ xs)
                                                                                          '\x0046' -> '\x006A' `C.cons` '\x006A' `C.cons` '\x0065' `C.cons` ukrainianToMoreSounding (C.tail . C.tail $ xs)
                                                                                          '\x0044' -> '\x006A' `C.cons` '\x006A' `C.cons` '\x0069' `C.cons` ukrainianToMoreSounding (C.tail . C.tail $ xs)
                                                                                          _  -> C.head xs `C.cons` ukrainianToMoreSounding (C.tail xs)
                                                                                        else C.empty
                                                                          '\x0050' -> f xs
                                                                          _        -> z2 xs
                                                                        else case zz of
                                                                          '\x002D' -> if not . C.null . C.tail $ xs
                                                                                        then case C.head . C.tail $ xs of
                                                                                          '-' -> if not . C.null . C.tail . C.tail $ xs
                                                                                                   then if (C.head . C.tail . C.tail $ xs) == ' '
                                                                                                          then ' ' `C.cons` ukrainianToMoreSounding (C.tail . C.tail . C.tail $ xs)
                                                                                                          else ' ' `C.cons` ukrainianToMoreSounding (C.tail . C.tail $ xs)
                                                                                                   else C.singleton ' '
                                                                                          _  -> ' ' `C.cons` ukrainianToMoreSounding (C.tail xs)
                                                                                        else C.singleton ' '
                                                                          '\x0043' -> '\x0042' `C.cons` '\x0049' `C.cons` ukrainianToMoreSounding (C.tail xs)
                                                                          _        -> z2 xs

-- | Function for special Ukrainian words where "г" sounds approximately as "х"
-- Функція для спеціальних українських слів, де звук "г" звучить близько до "х"
changeH2X :: C.ByteString -> C.ByteString
changeH2X xs | C.null xs = C.empty
             | C.head xs == '\x0076' = if C.null . C.tail $ xs
                then xs
                else case C.head . C.tail $ xs of
                  '\x006F' -> if C.null . C.tail . C.tail $ xs
                                then xs
                                else case C.head . C.tail . C.tail $ xs of
                                  '\x0041' -> if C.null . C.tail . C.tail . C.tail $ xs
                                                then xs
                                                else case C.head . C.tail . C.tail . C.tail $ xs of
                                                  '\x006B' -> '\x0076' `C.cons` '\x006F' `C.cons` '\x0068' `C.cons` '\x006B' `C.cons` changeH2X (C.tail . C.tail . C.tail . C.tail $ xs)
                                                  _  -> '\x0076' `C.cons` '\x006F' `C.cons` '\x0041' `C.cons` (C.head . C.tail . C.tail . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail . C.tail . C.tail $ xs)
                                  _        -> '\x0076' `C.cons` '\x006F' `C.cons` (C.head . C.tail . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail . C.tail $ xs)
                  _        -> '\x0076' `C.cons` (C.head . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail $ xs)
             | C.head xs == '\x006C' = if C.null . C.tail $ xs
                then xs
                else case C.head . C.tail $ xs of
                  '\x0065' -> if C.null . C.tail . C.tail $ xs
                                then xs
                                else case C.head . C.tail . C.tail $ xs of
                                  '\x0041' -> if C.null . C.tail . C.tail . C.tail $ xs
                                                then xs
                                                else case C.head . C.tail . C.tail . C.tail $ xs of
                                                  '\x006B' -> '\x006C' `C.cons` '\x0065' `C.cons` '\x0068' `C.cons` '\x006B' `C.cons` changeH2X (C.tail . C.tail . C.tail . C.tail $ xs)
                                                  _  -> '\x006C' `C.cons` '\x0065' `C.cons` '\x0041' `C.cons` (C.head . C.tail . C.tail . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail . C.tail . C.tail $ xs)
                                  _        -> '\x006C' `C.cons` '\x0065' `C.cons` (C.head . C.tail . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail . C.tail $ xs)
                  _        -> '\x006C' `C.cons` (C.head . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail $ xs)
             | C.head xs == '\x006B' = if C.null . C.tail $ xs
                then xs
                else case C.head . C.tail $ xs of
                  '\x0069' -> if C.null . C.tail . C.tail $ xs
                                then xs
                                else case C.head . C.tail . C.tail $ xs of
                                  '\x0041' -> if C.null . C.tail . C.tail . C.tail $ xs
                                                then xs
                                                else case C.head . C.tail . C.tail . C.tail $ xs of
                                                  '\x0074' -> '\x006B' `C.cons` '\x0069' `C.cons` '\x0068' `C.cons` '\x0074' `C.cons` changeH2X (C.tail . C.tail . C.tail . C.tail $ xs)
                                                  _  -> '\x006B' `C.cons` '\x0069' `C.cons` '\x0041' `C.cons` (C.head . C.tail . C.tail . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail . C.tail . C.tail $ xs)
                                  _        -> '\x006B' `C.cons` '\x0069' `C.cons` (C.head . C.tail . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail . C.tail $ xs)
                  _        -> '\x006B' `C.cons` (C.head . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail $ xs)
             | C.head xs == '\x006E' = if C.null . C.tail $ xs
                then xs
                else case C.head . C.tail $ xs of
                  '\x0069' -> if C.null . C.tail . C.tail $ xs
                                then xs
                                else case C.head . C.tail . C.tail $ xs of
                                  '\x0041' -> if C.null . C.tail . C.tail . C.tail $ xs
                                                then xs
                                                else case C.head . C.tail . C.tail . C.tail $ xs of
                                                  '\x0074' -> '\x006E' `C.cons` '\x0069' `C.cons` '\x0068' `C.cons` '\x0074' `C.cons` changeH2X (C.tail . C.tail . C.tail . C.tail $ xs)
                                                  _  -> '\x006E' `C.cons` '\x0069' `C.cons` '\x0041' `C.cons` (C.head . C.tail . C.tail . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail . C.tail . C.tail $ xs)
                                  _        -> '\x006E' `C.cons` '\x0069' `C.cons` (C.head . C.tail . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail . C.tail $ xs)
                  _        -> '\x006E' `C.cons` (C.head . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail $ xs)
             | C.head xs == '\x0064' = if C.null . C.tail $ xs
                then xs
                else case C.head . C.tail $ xs of
                  '\x0069' -> if C.null . C.tail . C.tail $ xs
                                then xs
                                else case C.head . C.tail . C.tail $ xs of
                                  '\x0041' -> if C.null . C.tail . C.tail . C.tail $ xs
                                                then xs
                                                else case C.head . C.tail . C.tail . C.tail $ xs of
                                                  '\x0074' -> if C.null . C.tail . C.tail . C.tail . C.tail $ xs
                                                                then xs
                                                                else case C.head . C.tail . C.tail . C.tail . C.tail $ xs of
                                                                  '\x0047' -> if C.null . C.tail . C.tail . C.tail . C.tail . C.tail $ xs
                                                                                then xs
                                                                                else case C.head . C.tail . C.tail . C.tail . C.tail . C.tail $ xs of
                                                                                  '\x0072' -> '\x0064' `C.cons` '\x0069' `C.cons` '\x0068' `C.cons` '\x0074' `C.cons` '\x0047' `C.cons`
                                                                                                '\x0072' `C.cons` changeH2X (C.tail . C.tail . C.tail . C.tail . C.tail . C.tail $ xs)
                                                                                  _  -> '\x0064' `C.cons` '\x0069' `C.cons` '\x0041' `C.cons` '\x0074' `C.cons` '\x0047' `C.cons`
                                                                                          (C.head . C.tail . C.tail . C.tail . C.tail . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail . C.tail . C.tail . C.tail . C.tail $ xs)
                                                                  _        -> '\x0064' `C.cons` '\x0069' `C.cons` '\x0041' `C.cons` '\x0074' `C.cons` (C.head . C.tail . C.tail . C.tail . C.tail $ xs) `C.cons`
                                                                                changeH2X (C.tail . C.tail . C.tail . C.tail . C.tail $ xs)
                                                  _         -> '\x0064' `C.cons` '\x0069' `C.cons` '\x0041' `C.cons` (C.head . C.tail . C.tail . C.tail $ xs) `C.cons`
                                                                    changeH2X (C.tail . C.tail . C.tail . C.tail $ xs)
                                  _        -> '\x0064' `C.cons` '\x0069' `C.cons` (C.head . C.tail . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail . C.tail $ xs)
                  _         -> '\x0064' `C.cons` (C.head . C.tail $ xs) `C.cons` changeH2X (C.tail . C.tail $ xs)
              | otherwise = C.head xs `C.cons` changeH2X (C.tail xs)

-- | Function that encode the Unicode characters from '\x0430' to '\x2122' for using in the Data.ByteString.Lazy.Char8 functions
-- Функція, що кодує Unicode символи з '\x0430' по '\x2122' для використання у Data.ByteString.Lazy.Char8 функціях
change2BS :: String -> String
change2BS (x : (y : (z : xs)))
  | y == '\1100' =
    if x >= '\1088' then
      if x >= '\1094' then
        if x >= '\1097' then
          case x of
              '\1097' -> '\SYN' : change2BS (z : xs)
              '\1169' -> 'R' : change2BS (z : xs)
              _ -> change2BS (x : z : xs)
          else
          case x of
              '\1094' -> '\DC3' : change2BS (z : xs)
              '\1095' -> '\DC4' : change2BS (z : xs)
              '\1096' -> '\NAK' : change2BS (z : xs)
              _ -> change2BS (x : z : xs)
        else
        if x >= '\1092' then
          case x of
              '\1092' -> '\DC1' : change2BS (z : xs)
              '\1093' -> '\DC2' : change2BS (z : xs)
              _ -> change2BS (x : z : xs)
          else
          case x of
              '\1088' -> '\a' : change2BS (z : xs)
              '\1089' -> '\f' : change2BS (z : xs)
              '\1090' -> '\DLE' : change2BS (z : xs)
              _ -> change2BS (x : z : xs)
      else
      if x <= '\1078' then
        if x <= '\1075' then
          case x of
              '\1073' -> '^' : change2BS (z : xs)
              '\1074' -> 'x' : change2BS (z : xs)
              '\1075' -> '\US' : change2BS (z : xs)
              _ -> change2BS (x : z : xs)
          else
          case x of
              '\1076' -> 'S' : change2BS (z : xs)
              '\1078' -> 'T' : change2BS (z : xs)
              _ -> change2BS (x : z : xs)
        else
        if x >= '\1084' then
          case x of
              '\1084' -> 'X' : change2BS (z : xs)
              '\1085' -> 'Y' : change2BS (z : xs)
              '\1087' -> 'Z' : change2BS (z : xs)
              _ -> change2BS (x : z : xs)
          else
          case x of
              '\1079' -> 'U' : change2BS (z : xs)
              '\1082' -> 'V' : change2BS (z : xs)
              '\1083' -> 'W' : change2BS (z : xs)
              _ -> change2BS (x : z : xs)
  | x == '\1076' =
    case y of
        '\1079' -> if z == '\1100' then 'Q' : change2BS xs else
                     'O' : change2BS (z : xs)
        '\1078' -> if z == '\1100' then 'P' : change2BS xs else
                     'w' : change2BS (z : xs)
        _ -> 'd' : change2BS (y : z : xs)
  | x >= '\8195' =
    if x >= '\8212' then
      if x >= '\8354' then
        if x >= '\8372' then
          if x >= '\8457' then
            case x of
                '\8457' -> '\162' : change2BS (y : z : xs)
                '\8482' -> '\168' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
            else
            case x of
                '\8372' -> '\255' : change2BS (y : z : xs)
                '\8451' -> '\165' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
          else
          if x <= '\8355' then
            case x of
                '\8354' -> '\252' : change2BS (y : z : xs)
                '\8355' -> '\253' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
            else
            case x of
                '\8356' -> '\254' : change2BS (y : z : xs)
                '\8364' -> '\181' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
        else
        if x <= '\8263' then
          if x >= '\8233' then
            case x of
                '\8233' -> '\246' : change2BS (y : z : xs)
                '\8263' -> '\247' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
            else
            case x of
                '\8212' -> 'M' : change2BS (y : z : xs)
                '\8230' -> '.' : '.' : '.' : change2BS (y : z : xs)
                '\8232' -> '\245' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
          else
          if x <= '\8265' then
            case x of
                '\8264' -> '\248' : change2BS (y : z : xs)
                '\8265' -> '\249' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
            else
            case x of
                '\8352' -> '\250' : change2BS (y : z : xs)
                '\8353' -> '\251' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
      else
      if x <= '\8202' then
        if x >= '\8199' then
          if x >= '\8201' then
            case x of
                '\8201' -> '\232' : change2BS (y : z : xs)
                '\8202' -> '\233' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
            else
            case x of
                '\8199' -> '\230' : change2BS (y : z : xs)
                '\8200' -> '\231' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
          else
          if x <= '\8196' then
            case x of
                '\8195' -> '\226' : change2BS (y : z : xs)
                '\8196' -> '\227' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
            else
            case x of
                '\8197' -> '\228' : change2BS (y : z : xs)
                '\8198' -> '\229' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
        else
        if x <= '\8206' then
          if x >= '\8205' then
            case x of
                '\8205' -> '\236' : change2BS (y : z : xs)
                '\8206' -> '\237' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
            else
            case x of
                '\8203' -> '\234' : change2BS (y : z : xs)
                '\8204' -> '\235' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
          else
          if x <= '\8209' then
            case x of
                '\8207' -> '\238' : change2BS (y : z : xs)
                '\8208' -> '\239' : change2BS (y : z : xs)
                '\8209' -> '\240' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
            else
            case x of
                '\8210' -> '\241' : change2BS (y : z : xs)
                '\8211' -> 'L' : change2BS (y : z : xs)
                _ -> x : change2BS (y : z : xs)
  | x <= '\1088' =
    if x >= '\1080' then
      if x >= '\1085' then
        if x >= '\1087' then
          case x of
              '\1087' -> 'p' : change2BS (y : z : xs)
              '\1088' -> 'r' : change2BS (y : z : xs)
              _ -> x : change2BS (y : z : xs)
          else
          case x of
              '\1085' -> 'n' : change2BS (y : z : xs)
              '\1086' -> 'o' : change2BS (y : z : xs)
              _ -> x : change2BS (y : z : xs)
        else
        if x <= '\1082' then
          case x of
              '\1080' -> 'y' : change2BS (y : z : xs)
              '\1081' -> 'j' : change2BS (y : z : xs)
              '\1082' -> 'k' : change2BS (y : z : xs)
              _ -> x : change2BS (y : z : xs)
          else
          case x of
              '\1083' -> 'l' : change2BS (y : z : xs)
              '\1084' -> 'm' : change2BS (y : z : xs)
              _ -> x : change2BS (y : z : xs)
      else
      if x <= '\1075' then
        if x >= '\1074' then
          case x of
              '\1074' -> 'v' : change2BS (y : z : xs)
              '\1075' -> 'A' : change2BS (y : z : xs)
              _ -> x : change2BS (y : z : xs)
          else
          case x of
              '\'' -> 'K' : change2BS (y : z : xs)
              '\1072' -> 'a' : change2BS (y : z : xs)
              '\1073' -> 'b' : change2BS (y : z : xs)
              _ -> x : change2BS (y : z : xs)
        else
        if x <= '\1077' then
          case x of
              '\1076' -> 'd' : change2BS (y : z : xs)
              '\1077' -> 'e' : change2BS (y : z : xs)
              _ -> x : change2BS (y : z : xs)
          else
          case x of
              '\1078' -> 'E' : change2BS (y : z : xs)
              '\1079' -> 'z' : change2BS (y : z : xs)
              _ -> x : change2BS (y : z : xs)
  | x <= '\1097' =
    if x >= '\1093' then
      if x >= '\1096' then
        case x of
            '\1096' -> 'B' : change2BS (y : z : xs)
            '\1097' -> 'C' : change2BS (y : z : xs)
            _ -> x : change2BS (y : z : xs)
        else
        case x of
            '\1093' -> 'h' : change2BS (y : z : xs)
            '\1094' -> 'c' : change2BS (y : z : xs)
            '\1095' -> 'I' : change2BS (y : z : xs)
            _ -> x : change2BS (y : z : xs)
      else
      if x <= '\1090' then
        case x of
            '\1089' -> 's' : change2BS (y : z : xs)
            '\1090' -> 't' : change2BS (y : z : xs)
            _ -> x : change2BS (y : z : xs)
        else
        case x of
            '\1091' -> 'u' : change2BS (y : z : xs)
            '\1092' -> 'f' : change2BS (y : z : xs)
            _ -> x : change2BS (y : z : xs)
  | x <= '\1108' =
    if x >= '\1103' then
      case x of
          '\1103' -> 'G' : change2BS (y : z : xs)
          '\1108' -> 'F' : change2BS (y : z : xs)
          _ -> x : change2BS (y : z : xs)
      else
      case x of
          '\1100' -> 'q' : change2BS (y : z : xs)
          '\1102' -> 'J' : change2BS (y : z : xs)
          _ -> x : change2BS (y : z : xs)
  | x <= '\1111' =
    case x of
        '\1110' -> 'i' : change2BS (y : z : xs)
        '\1111' -> 'D' : change2BS (y : z : xs)
        _ -> x : change2BS (y : z : xs)
  | otherwise =
    case x of
        '\1169' -> 'g' : change2BS (y : z : xs)
        '\8194' -> '\225' : change2BS (y : z : xs)
        _ -> x : change2BS (y : z : xs)
change2BS [x, y]
  | y == '\1100' =
    if x >= '\1088' then
      if x >= '\1094' then
        if x >= '\1096' then
          case x of
              '\1097' -> "\SYN"
              '\1169' -> "R"
              _ -> change2BS [x]
          else
          case x of
              '\1094' -> "\DC3"
              '\1095' -> "\DC4"
              '\1096' -> "\NAK"
              _ -> change2BS [x]
        else
        if x <= '\1090' then
          case x of
              '\1088' -> "\a"
              '\1089' -> "\f"
              '\1090' -> "\DLE"
              _ -> change2BS [x]
          else
          case x of
              '\1092' -> "\DC1"
              '\1093' -> "\DC2"
              _ -> change2BS [x]
      else
      if x <= '\1078' then
        if x >= '\1076' then
          case x of
              '\1076' -> "S"
              '\1078' -> "T"
              _ -> change2BS [x]
          else
          case x of
              '\1073' -> "^"
              '\1074' -> "x"
              '\1075' -> "\US"
              _ -> change2BS [x]
        else
        if x <= '\1083' then
          case x of
              '\1079' -> "U"
              '\1082' -> "V"
              '\1083' -> "W"
              _ -> change2BS [x]
          else
          case x of
              '\1084' -> "X"
              '\1085' -> "Y"
              '\1087' -> "Z"
              _ -> change2BS [x]
  | x == '\1076' =
    case y of
        '\1079' -> "O"
        '\1078' -> "w"
        _ -> 'd' : change2BS [y]
  | x >= '\8195' =
    if x >= '\8212' then
      if x >= '\8354' then
        if x >= '\8372' then
          if x >= '\8457' then
            case x of
                '\8457' -> '\162' : change2BS [y]
                '\8482' -> '\168' : change2BS [y]
                _ -> x : change2BS [y]
            else
            case x of
                '\8372' -> '\255' : change2BS [y]
                '\8451' -> '\165' : change2BS [y]
                _ -> x : change2BS [y]
          else
          if x <= '\8355' then
            case x of
                '\8354' -> '\252' : change2BS [y]
                '\8355' -> '\253' : change2BS [y]
                _ -> x : change2BS [y]
            else
            case x of
                '\8356' -> '\254' : change2BS [y]
                '\8364' -> '\181' : change2BS [y]
                _ -> x : change2BS [y]
        else
        if x <= '\8263' then
          if x >= '\8233' then
            case x of
                '\8233' -> '\246' : change2BS [y]
                '\8263' -> '\247' : change2BS [y]
                _ -> x : change2BS [y]
            else
            case x of
                '\8212' -> 'M' : change2BS [y]
                '\8230' -> '.' : '.' : '.' : change2BS [y]
                '\8232' -> '\245' : change2BS [y]
                _ -> x : change2BS [y]
          else
          if x <= '\8265' then
            case x of
                '\8264' -> '\248' : change2BS [y]
                '\8265' -> '\249' : change2BS [y]
                _ -> x : change2BS [y]
            else
            case x of
                '\8352' -> '\250' : change2BS [y]
                '\8353' -> '\251' : change2BS [y]
                _ -> x : change2BS [y]
      else
      if x <= '\8202' then
        if x >= '\8199' then
          if x >= '\8201' then
            case x of
                '\8201' -> '\232' : change2BS [y]
                '\8202' -> '\233' : change2BS [y]
                _ -> x : change2BS [y]
            else
            case x of
                '\8199' -> '\230' : change2BS [y]
                '\8200' -> '\231' : change2BS [y]
                _ -> x : change2BS [y]
          else
          if x <= '\8196' then
            case x of
                '\8195' -> '\226' : change2BS [y]
                '\8196' -> '\227' : change2BS [y]
                _ -> x : change2BS [y]
            else
            case x of
                '\8197' -> '\228' : change2BS [y]
                '\8198' -> '\229' : change2BS [y]
                _ -> x : change2BS [y]
        else
        if x <= '\8206' then
          if x >= '\8205' then
            case x of
                '\8205' -> '\236' : change2BS [y]
                '\8206' -> '\237' : change2BS [y]
                _ -> x : change2BS [y]
            else
            case x of
                '\8203' -> '\234' : change2BS [y]
                '\8204' -> '\235' : change2BS [y]
                _ -> x : change2BS [y]
          else
          if x <= '\8209' then
            case x of
                '\8207' -> '\238' : change2BS [y]
                '\8208' -> '\239' : change2BS [y]
                '\8209' -> '\240' : change2BS [y]
                _ -> x : change2BS [y]
            else
            case x of
                '\8210' -> '\241' : change2BS [y]
                '\8211' -> 'L' : change2BS [y]
                _ -> x : change2BS [y]
  | x <= '\1088' =
    if x >= '\1080' then
      if x >= '\1085' then
        if x >= '\1087' then
          case x of
              '\1087' -> 'p' : change2BS [y]
              '\1088' -> 'r' : change2BS [y]
              _ -> x : change2BS [y]
          else
          case x of
              '\1085' -> 'n' : change2BS [y]
              '\1086' -> 'o' : change2BS [y]
              _ -> x : change2BS [y]
        else
        if x <= '\1082' then
          case x of
              '\1080' -> 'y' : change2BS [y]
              '\1081' -> 'j' : change2BS [y]
              '\1082' -> 'k' : change2BS [y]
              _ -> x : change2BS [y]
          else
          case x of
              '\1083' -> 'l' : change2BS [y]
              '\1084' -> 'm' : change2BS [y]
              _ -> x : change2BS [y]
      else
      if x <= '\1075' then
        if x >= '\1074' then
          case x of
              '\1074' -> 'v' : change2BS [y]
              '\1075' -> 'A' : change2BS [y]
              _ -> x : change2BS [y]
          else
          case x of
              '\'' -> 'K' : change2BS [y]
              '\1072' -> 'a' : change2BS [y]
              '\1073' -> 'b' : change2BS [y]
              _ -> x : change2BS [y]
        else
        if x <= '\1077' then
          case x of
              '\1076' -> 'd' : change2BS [y]
              '\1077' -> 'e' : change2BS [y]
              _ -> x : change2BS [y]
          else
          case x of
              '\1078' -> 'E' : change2BS [y]
              '\1079' -> 'z' : change2BS [y]
              _ -> x : change2BS [y]
  | x <= '\1097' =
    if x >= '\1093' then
      if x >= '\1096' then
        case x of
            '\1096' -> 'B' : change2BS [y]
            '\1097' -> 'C' : change2BS [y]
            _ -> x : change2BS [y]
        else
        case x of
            '\1093' -> 'h' : change2BS [y]
            '\1094' -> 'c' : change2BS [y]
            '\1095' -> 'I' : change2BS [y]
            _ -> x : change2BS [y]
      else
      if x <= '\1090' then
        case x of
            '\1089' -> 's' : change2BS [y]
            '\1090' -> 't' : change2BS [y]
            _ -> x : change2BS [y]
        else
        case x of
            '\1091' -> 'u' : change2BS [y]
            '\1092' -> 'f' : change2BS [y]
            _ -> x : change2BS [y]
  | x <= '\1108' =
    if x >= '\1103' then
      case x of
          '\1103' -> 'G' : change2BS [y]
          '\1108' -> 'F' : change2BS [y]
          _ -> x : change2BS [y]
      else
      case x of
          '\1100' -> 'q' : change2BS [y]
          '\1102' -> 'J' : change2BS [y]
          _ -> x : change2BS [y]
  | x <= '\1111' =
    case x of
        '\1110' -> 'i' : change2BS [y]
        '\1111' -> 'D' : change2BS [y]
        _ -> x : change2BS [y]
  | otherwise =
    case x of
        '\1169' -> 'g' : change2BS [y]
        '\8194' -> '\225' : change2BS [y]
        _ -> x : change2BS [y]
change2BS [x]
  | x >= '\8195' =
    if x >= '\8212' then
      if x >= '\8354' then
        if x >= '\8372' then
          if x >= '\8457' then
            case x of
                '\8457' -> "\162"
                '\8482' -> "\168"
                _ -> [x]
            else
            case x of
                '\8372' -> "\255"
                '\8451' -> "\165"
                _ -> [x]
          else
          if x <= '\8355' then
            case x of
                '\8354' -> "\252"
                '\8355' -> "\253"
                _ -> [x]
            else
            case x of
                '\8356' -> "\254"
                '\8364' -> "\181"
                _ -> [x]
        else
        if x <= '\8263' then
          if x >= '\8233' then
            case x of
                '\8233' -> "\246"
                '\8263' -> "\247"
                _ -> [x]
            else
            case x of
                '\8212' -> "M"
                '\8230' -> "..."
                '\8232' -> "\245"
                _ -> [x]
          else
          if x <= '\8265' then
            case x of
                '\8264' -> "\248"
                '\8265' -> "\249"
                _ -> [x]
            else
            case x of
                '\8352' -> "\250"
                '\8353' -> "\251"
                _ -> [x]
      else
      if x <= '\8202' then
        if x >= '\8199' then
          if x >= '\8201' then
            case x of
                '\8201' -> "\232"
                '\8202' -> "\233"
                _ -> [x]
            else
            case x of
                '\8199' -> "\230"
                '\8200' -> "\231"
                _ -> [x]
          else
          if x <= '\8196' then
            case x of
                '\8195' -> "\226"
                '\8196' -> "\227"
                _ -> [x]
            else
            case x of
                '\8197' -> "\228"
                '\8198' -> "\229"
                _ -> [x]
        else
        if x <= '\8206' then
          if x >= '\8205' then
            case x of
                '\8205' -> "\236"
                '\8206' -> "\237"
                _ -> [x]
            else
            case x of
                '\8203' -> "\234"
                '\8204' -> "\235"
                _ -> [x]
          else
          if x <= '\8209' then
            case x of
                '\8207' -> "\238"
                '\8208' -> "\239"
                '\8209' -> "\240"
                _ -> [x]
            else
            case x of
                '\8210' -> "\241"
                '\8211' -> "L"
                _ -> [x]
  | x <= '\1088' =
    if x >= '\1080' then
      if x >= '\1085' then
        if x >= '\1087' then
          case x of
              '\1087' -> "p"
              '\1088' -> "r"
              _ -> [x]
          else
          case x of
              '\1085' -> "n"
              '\1086' -> "o"
              _ -> [x]
        else
        if x <= '\1082' then
          case x of
              '\1080' -> "y"
              '\1081' -> "j"
              '\1082' -> "k"
              _ -> [x]
          else
          case x of
              '\1083' -> "l"
              '\1084' -> "m"
              _ -> [x]
      else
      if x <= '\1075' then
        if x >= '\1074' then
          case x of
              '\1074' -> "v"
              '\1075' -> "A"
              _ -> [x]
          else
          case x of
              '\'' -> "K"
              '\1072' -> "a"
              '\1073' -> "b"
              _ -> [x]
        else
        if x <= '\1077' then
          case x of
              '\1076' -> "d"
              '\1077' -> "e"
              _ -> [x]
          else
          case x of
              '\1078' -> "E"
              '\1079' -> "z"
              _ -> [x]
  | x <= '\1097' =
    if x >= '\1093' then
      if x >= '\1096' then
        case x of
            '\1096' -> "B"
            '\1097' -> "C"
            _ -> [x]
        else
        case x of
            '\1093' -> "h"
            '\1094' -> "c"
            '\1095' -> "I"
            _ -> [x]
      else
      if x <= '\1090' then
        case x of
            '\1089' -> "s"
            '\1090' -> "t"
            _ -> [x]
        else
        case x of
            '\1091' -> "u"
            '\1092' -> "f"
            _ -> [x]
  | x <= '\1108' =
    if x >= '\1103' then
      case x of
          '\1103' -> "G"
          '\1108' -> "F"
          _ -> [x]
      else
      case x of
          '\1100' -> "q"
          '\1102' -> "J"
          _ -> [x]
  | x <= '\1111' =
    case x of
        '\1110' -> "i"
        '\1111' -> "D"
        _ -> [x]
  | otherwise =
    case x of
        '\1169' -> "g"
        '\8194' -> "\225"
        _ -> [x]
change2BS [] = []

-- | Function that converts Latin text into English-sounding letters in Ukrainian
-- Функція, яка конвертує текст латиницею в літери, які звучать по-англійськи, записані українською
readEnglishWithUkrainian :: Char -> String
readEnglishWithUkrainian x | x >= '\x0061' && x <= '\x007A' =
  if x >= '\x006E'
    then if x >= '\x0075'
      then if x >= '\x0078'
        then case x of
          '\x0078' -> "екс"
          '\x0079' -> "уай"
          _        -> "зед"
        else case x of
          '\x0075' -> "ю"
          '\x0076' -> "ві"
          _        -> "даблййу"
      else if x <= '\x0071'
        then if x >= '\x0070'
          then case x of
            '\x0070' -> "пі"
            _        -> "кййу"
          else case x of
            '\x006E' -> "ен"
            _        -> "оу"
        else case x of
          '\x0072' -> "ар"
          '\x0073' -> "ес"
          _        -> "ті"
    else if x <= '\x0067'
      then if x >= '\x0065'
        then case x of
          '\x0065' -> "і"
          '\x0066' -> "еф"
          _        -> "джи"
        else if x <= '\x0062'
          then case x of
            '\x0061' -> "ей"
            _        -> "бі"
          else case x of
            '\x0063' -> "сі"
            _        -> "ді"
      else if x <= '\x006A'
        then case x of
          '\x0068' -> "ейч"
          '\x0069' -> "ай"
          _        -> "джей"
        else case x of
          '\x006B' -> "кей"
          '\x006C' -> "ел"
          _        -> "ем"
                            | x >= '\x0041' && x <= '\x005A' =       
  if x >= '\x004E'
    then if x >= '\x0055'
      then if x >= '\x0058'
        then case x of
          '\x0058' -> "екс"
          '\x0059' -> "уай"
          _        -> "зед"
        else case x of
          '\x0055' -> "ю"
          '\x0056' -> "ві"
          _        -> "даблййу"
      else if x <= '\x0051'
        then if x >= '\x0050'
          then case x of
            '\x0050' -> "пі"
            _        -> "кййу"
          else case x of
            '\x004E' -> "ен"
            _        -> "оу"
        else case x of
          '\x0052' -> "ар"
          '\x0053' -> "ес"
          _        -> "ті"
    else if x <= '\x0047'
      then if x >= '\x0045'
        then case x of
          '\x0045' -> "і"
          '\x0046' -> "еф"
          _        -> "джи"
        else if x <= '\x0042'
          then case x of
            '\x0041' -> "ей"
            _        -> "бі"
          else case x of
            '\x0043' -> "сі"
            _        -> "ді"
      else if x <= '\x004A'
        then case x of
          '\x0048' -> "ейч"
          '\x0049' -> "ай"
          _        -> "джей"
        else case x of
          '\x004B' -> "кей"
          '\x004C' -> "ел"
          _        -> "ем"                       
                           | otherwise = [x]

-- | Function that converts letters to lower case letters and makes some additional punctuation changes
-- Функція, що конвертує літери в прописні літери та робить додаткові пунктуаційні зміни
firstChange :: Char -> Char
firstChange y = case y of
  '[' -> '('
  ']' -> ')'
  '\x203C' -> '!'
  _  -> DC.toLower y

-- | Function that sounds separated with whitespaces numbers digit by digit
-- Функція, яка озвучує відокремлені пробілами числа цифра за цифрою
numberSounds :: Char -> String
numberSounds x | x >= '\x0035' =
  if x >= '\x0038'
    then case x of
      '\x0038' -> " вісім "
      _        -> " девййать "
    else case x of
      '\x0035' -> " пййать "
      '\x0036' -> " шість "
      _        -> " сім "
               | otherwise =
  if x <= '\x0032'
    then case x of
      '\x0030' -> " нуль "
      '\x0031' -> " 2один "
      _        -> " два "
    else case x of
      '\x0033' -> " три "
      _        -> " чотири "

-- | Function that considers a number of punctuation marks for proper pause creation
-- Функція, яка бере до уваги кількість пунктуаційних знаків для правильного створення пауз
punctL :: Integer -> Integer -> String -> String -> String -> [String] -> IO ()
punctL k t1 xs ys zs _ | k >= 10000000000 =
  punctL1 (let r = fromIntegral k / 100000000000000 in if r >= 0.1 then r - 0.014 else 0.086) t1 xs
                       | DC.isPunctuation . head $ xs =
  if k >= 1000200
    then if k >= 10000000
      then if k >= 12000000
        then case k of
          12000000 -> punctL1 0.9 t1 xs
          100000000 -> punctL1 0.5 t1 xs
          1000000000 -> punctL1 0.3 t1 xs
          _          -> punctL1 0.7 t1 xs
        else case k of
          10000000 -> punctL1 0.4 t1 xs
          10000001 -> punctL1 0.95 t1 xs
          10000010 -> punctL1 0.95 t1 xs
          _        -> punctL1 0.7 t1 xs
      else if k >= 2000100
        then case k of
          2000100 -> punctL1 1.0 t1 xs
          2001000 -> punctL1 1.0 t1 xs
          3000000 -> punctL1 0.9 t1 xs
          _       -> punctL1 0.7 t1 xs
        else case k of
          1000200 -> punctL1 1.2 t1 xs
          1001100 -> punctL1 1.3 t1 xs
          1002000 -> punctL1 1.2 t1 xs
          _       -> punctL1 0.7 t1 xs
    else if k >= 1100
      then if k >= 10000
        then case k of
          10000 -> punctL1 0.6 t1 xs
          100000 -> punctL1 0.7 t1 xs
          1000000 -> punctL1 0.8 t1 xs
          _       -> punctL1 0.7 t1 xs
        else case k of
          1100 -> punctL1 1.2 t1 xs
          2000 -> punctL1 1.1 t1 xs
          3000 -> punctL1 1.3 t1 xs
          _    -> punctL1 0.7 t1 xs
      else if k <= 100
        then case k of
          1 -> punctL1 0.85 t1 xs
          10 -> punctL1 0.75 t1 xs
          100 -> punctL1 0.9 t1 xs
          _   -> punctL1 0.7 t1 xs
        else case k of
          200 -> punctL1 1.1 t1 xs
          300 -> punctL1 1.3 t1 xs
          1000 -> punctL1 0.9 t1 xs
          _    -> punctL1 0.7 t1 xs
                       | otherwise = 
  let (ts, us) = oneToTuple2 . filter (/= 'q') $ xs in if last xs == 'q'
    then do
           eE <- endE
           eS <- endS
           callCommand ("espeak" ++ eE ++ " -v " ++ ys ++ " " ++ zs ++ " -w m." ++ show t1 ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") 
           callCommand ("sox" ++ eS ++ " --multi-threaded  " ++  "m." ++ show t1 ++ "." ++ ts ++ ".wav " ++  show t1 ++ "." ++ ts ++ ".wav " ++ us) 
           addSoftSign $ show t1 ++ "." ++ ts ++ ".wav"
    else do
           eE <- endE
           eS <- endS
           callCommand ("espeak" ++ eE ++ " -v " ++ ys ++ " " ++ zs ++ " -w m." ++ show t1 ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") 
           callCommand $ "sox" ++ eS ++ " --multi-threaded  " ++  "m." ++ show t1 ++ "." ++ xs ++ ".wav " ++ show t1 ++ "." ++ xs ++ ".wav " ++ us ++ (if xs == "y" then " tempo -s 0.7" else "") 

-- | Function-predicate to check whether its argument is a digit or dot
-- Функція-предикат, яка перевіряє, чи є її аргумент цифра чи крапка
isDigitOrDot :: Char -> Bool
isDigitOrDot x = DC.isDigit x || (x == '\x002E') 

-- | Function that converts a String with digits into an Integer
-- Функція, що конвертує String з цифрами в Integer
stringToInteger :: String -> Integer
stringToInteger xs | null xs = 0
                   | otherwise = 
  foldl1 ((+) . (*10)) $! map charToDigit xs
    where charToDigit x = case x of
            '1' -> 1
            '2' -> 2
            '3' -> 3
            '4' -> 4
            '5' -> 5
            '6' -> 6
            '7' -> 7
            '8' -> 8
            '9' -> 9
            '0' -> 0
            _ -> error "Character Is Not a Digit!\n"

-- | Additional function that is used for optimization of the punctL and punctL11 functions
-- Додаткова функція, яка використовується для оптимізації функцій punctL і punctL11
punctOpt :: String -> Integer
punctOpt xs | (not . null . tail $ xs) && head xs == '!' && (DC.isDigit . head . tail $ xs) =
  let z = dropWhile (== '\x0030') . (\x -> if last x == '\x002E' then init x else x) . filter isDigitOrDot $ xs in 
    if head z == '\x002E'
      then let zk1 = take 3 . tail $ z in 
             let zk2 = length zk1 in case zk2 of
                 0 -> stringToInteger z * 10000000000000
                 _ -> stringToInteger zk1 * 10^(14-zk2)
      else let zk1 = take 3 . dropWithFirst (/= '\x002E') $ z in 
             let z0 = takeWhile (/= '\x002E') z in
               let zk2 = length zk1 in 
                 let zk3 = z0 ++ zk1 in case zk2 of
                   0 -> stringToInteger z * 10000000000000
                   _ -> stringToInteger zk3 * 10^(14-zk2)             
            | otherwise = 
  sum . map (\x -> if x >= '\x003A'
    then if x >= '\x004F'
      then case x of
        '\x004C' -> 10
        '\x004D' -> 1
        _        -> 0
      else case x of
        '\x003A' -> 100000
        '\x003B' -> 10000
        '\x003F' -> 100
        _        -> 0
    else if x >= '\x002C'
      then case x of
        '\x002C' -> 10000000
        '\x002E' -> 1000000
        _        -> 0
      else case x of
        '\x0021' -> 1000
        '\x0028' -> 1000000000
        '\x0029' -> 100000000
        _        -> 0) $ xs

-- | Function that considers a number of punctuation marks for proper pause creation
-- Функція, яка бере до уваги кількість пунктуаційних знаків для правильного створення пауз
punctL11 :: Integer -> Integer -> String -> String -> String -> [String] -> IO ()
punctL11 k t1 xs ys zs args | k >= 10000000000 =
  punctL1 (let r = fromIntegral k / 100000000000000 in if r >= 0.1 then r - 0.014 else 0.086) t1 xs
                            | DC.isPunctuation . head $ xs =
  if k >= 1000200
    then if k >= 10000000
      then if k >= 12000000
        then case k of
          12000000 -> punctL1 0.9 t1 xs
          100000000 -> punctL1 0.5 t1 xs
          1000000000 -> punctL1 0.3 t1 xs
          _          -> punctL1 0.7 t1 xs
        else case k of
          10000000 -> punctL1 0.4 t1 xs
          10000001 -> punctL1 0.95 t1 xs
          10000010 -> punctL1 0.95 t1 xs
          _        -> punctL1 0.7 t1 xs
      else if k >= 2000100
        then case k of
          2000100 -> punctL1 1.0 t1 xs
          2001000 -> punctL1 1.0 t1 xs
          3000000 -> punctL1 0.9 t1 xs
          _       -> punctL1 0.7 t1 xs
        else case k of
          1000200 -> punctL1 1.2 t1 xs
          1001100 -> punctL1 1.3 t1 xs
          1002000 -> punctL1 1.2 t1 xs
          _       -> punctL1 0.7 t1 xs
    else if k >= 1100
      then if k >= 10000
        then case k of
          10000 -> punctL1 0.6 t1 xs
          100000 -> punctL1 0.7 t1 xs
          1000000 -> punctL1 0.8 t1 xs
          _       -> punctL1 0.7 t1 xs
        else case k of
          1100 -> punctL1 1.2 t1 xs
          2000 -> punctL1 1.1 t1 xs
          3000 -> punctL1 1.3 t1 xs
          _    -> punctL1 0.7 t1 xs
      else if k <= 100
        then case k of
          1 -> punctL1 0.85 t1 xs
          10 -> punctL1 0.75 t1 xs
          100 -> punctL1 0.9 t1 xs
          _   -> punctL1 0.7 t1 xs
        else case k of
          200 -> punctL1 1.1 t1 xs
          300 -> punctL1 1.3 t1 xs
          1000 -> punctL1 0.9 t1 xs
          _    -> punctL1 0.7 t1 xs
                            | otherwise =
  let (ts, us) = oneToTuple2 . filter (/= 'q') $ xs in case head args of
      "1" -> createZeroSyllable (0.7, t1, xs, ys, zs, ts, us)
      "2" -> createZeroSyllable (0.8, t1, xs, ys, zs, ts, us)
      "3" -> createZeroSyllable (0.9, t1, xs, ys, zs, ts, us)
      _   -> createZeroSyllable (0.65, t1, xs, ys, zs, ts, us)

-- | Function that creates Ukrainian syllables and groups them with some parameters to be then processed by the eSpeak and SoX executables
-- Функція, що створює українські склади та групує їх з деякими параметрами, щоб потім вони були оброблені програмами eSpeak і SoX
createSyllablesReady :: (C.ByteString, (Maybe Integer, Integer)) -> [((String, String),(String,Integer))]
createSyllablesReady x = case snd . snd $ x of
  0 -> zeroSyllablePart . fst $ x
  _ -> createSyllablesMultiLast2 (divideToUnits . fst $ x) (snd x)

-- | Function that creates from a Ukrainian pre-processed ByteString a data of the type (ByteString, (Maybe Integer, Integer)) that takes into account a word emphasis
-- Функція, що створює з попередньо обробленого українського ByteString дані типу (ByteString, (Maybe Integer, Integer)), що враховують наголос у складі
accountEmphasis :: C.ByteString -> (C.ByteString, (Maybe Integer, Integer))
accountEmphasis xs | C.null xs = (C.empty, (Nothing, 0))
                   | otherwise = let z = fromIntegral . C.length . C.filter isVowelL $ xs in case C.readInteger xs of
  Nothing -> (xs, (Nothing, z))
  Just (k, ys) -> (ys, (Just (k `mod` z), z))

-- | Function-predicate that checks whether its argument is either a vowel or a punctuation mark
-- Функція-предикат, яка перевіряє, чи є її аргумент голосним або знаком пунктуації
isVowelOrPunctuation :: Char -> Bool
isVowelOrPunctuation x = DC.isPunctuation x || isVowelEG x

-- | Function-predicate to check whether its argument is a Vowel sound letter in Esperanto or Greek respesentation of the group of sounds
-- Функція-предикат, щоб визначити, чи її аргумент є літерою, що позначає голосний у представленні грецькою мовою чи есперанто
isVowelEG :: Char -> Bool
isVowelEG x | x >= '\x006F' =
  case x of
    '\x006F' -> True
    '\x0075' -> True
    _        -> False
            | otherwise =
  case x of
    '\x0061' -> True
    '\x0065' -> True
    '\x0069' -> True
    _        -> False

-- | Optimized function to take elements of the list after the first occasion of the wrong predicate p excluding the first occurance
-- Оптимізована функція, щоб узяти елементи списку після першої появи хибності у предикаті, виключаючи саму цю першу появу
dropWithFirst :: (a -> Bool) -> ([a] -> [a])
dropWithFirst p = fst . foldr f v
  where
    f x (ys,xs) = (if p x then ys else xs,x:xs)
    v = ([],[])

-- | Function-predicate that checks whether its argument is either a digit character or a dash
-- Функція-предикат, що перевіряє, чи її аргумент є символом цифри чи дефісу
isDigitOrDash :: Char -> Bool
isDigitOrDash x = DC.isDigit x || x == '\x002D'

-- | Function-predicate that checks whether its argument is a punctuation mark or a whitespace encoded as ByteString
-- Функція-предикат, яка перевіряє, чи є її аргумент пунктуаційним знаком чи пробілом, закодованим як ByteString
isPunctOrSpaceB :: Char -> Bool
isPunctOrSpaceB x = case x of
  '\x000C' -> False
  _ -> DC.isSpace x || DC.isPunctuation x || (x >= '\x00E1' && x <= '\x00F9')

-- | Additional function that is used for pause creation into the functions punctL and punctL11
-- Додаткова функція, яка використовується всередині функцій punctL і punctL11 для створення пауз
punctL1 :: Double -> Integer -> String -> IO ()
punctL1 x t1 xs = do
  eS <- endS
  callCommand ("sox" ++ eS ++ " --multi-threaded  -n -r 22.05k -c 1 -b 16 " ++ show t1 ++ "." ++ xs ++ ".wav delay " ++ show (x + 0.001) ++ " trim 0 " ++ show x) 
  
-- | Function that is used to convert single letters to a respective syllables for sounding
-- Функція, що використовується, щоб перетворити окремі літери на відповідні склади для озвучування
oneToTuple2 :: String -> (String, String)
oneToTuple2 [] = ([], [])
oneToTuple2 xs = let x = head xs in if x == 'd'
  then if null . tail $ xs
         then ("ed", " trim -0.070")
         else case head . tail $ xs of
                'z' -> ("edz", " trim -0.178")
                'ĵ' -> ("edĵ", " trim -0.166")
                _ -> ("ed", " trim -0.070")
  else if not. DC.isAlpha $ x
         then (xs, "")
         else if x >= 'p'
                then if x >= 'z'
                       then if x >= 'ĵ'
                              then case x of
                                'ĵ' -> ("eĵ", " trim -0.132")
                                'ŝ' -> ("eŝ", " trim -0.121")
                                'γ' -> ("αγ", " trim -0.117")
                                _   -> ([x], "")
                              else case x of
                                'z' -> ("ez", " trim -0.157")
                                'ĉ' -> ("eĉ", " trim -0.114")
                                'ĥ' -> ("eĥ", " trim -0.118")
                                _   -> ([x], "")
                       else if x <= 't'
                              then if x >= 's'
                                     then case x of
                                       's' -> ("es", " trim -0.121")
                                       't' -> ("et", " trim -0.071")
                                       _   -> ([x], "")
                                     else case x of
                                       'p' -> ("ep", " trim -0.103")
                                       'r' -> ("er", " trim -0.099")
                                       _   -> ([x], "")
                              else case x of
                                'u' -> ("u", "")
                                'v' -> ("ev", " trim -0.134")
                                'y' -> ("yy", "")
                                _   -> ([x], "")
                else if x <= 'h'
                       then if x >= 'f'
                              then case x of
                                'f' -> ("ef", " trim -0.112")
                                'g' -> ("eg", " trim -0.081")
                                'h' -> ("eh", " trim -0.083")
                                _   -> ([x], "")
                              else if x <= 'b'
                                     then case x of
                                       'a' -> ("a", "")
                                       'b' -> ("eb", " trim -0.070")
                                       _   -> ([x], "")
                                     else case x of
                                       'c' -> ("ec", " trim -0.115")
                                       'e' -> ("e", "")
                                       _   -> ([x], "")
                       else if x <= 'l'
                              then if x >= 'k'
                                     then case x of
                                       'k' -> ("ek", " trim -0.071")
                                       'l' -> ("el", " trim -0.105")
                                       _   -> ([x], "")
                                     else case x of
                                       'i' -> ("i", "")
                                       'j' -> ("ej", " trim -0.133")
                                       _   -> ([x], "")
                              else case x of
                                'm' -> ("em", " trim -0.146")
                                'n' -> ("en", " trim -0.136")
                                'o' -> ("o", "")
                                _   -> ([x], "")

-- | Function that is used for zero-syllable creation varied from OS and duration
-- Функція, яка використовується для створення слів без голосних і варіюється в залежності від ОС та тривалості
createZeroSyllable :: (Double, Integer, String, String, String, String, String) -> IO ()
createZeroSyllable (v, t1, xs, ys, zs, ts, us) | last xs == 'q' =
  do
    eE <- endE
    eS <- endS
    callCommand ("espeak" ++ eE ++ " -v " ++ ys ++ " " ++ zs ++ " -w m." ++ show t1 ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") 
    callCommand ("sox" ++ eS ++ " --multi-threaded  " ++  "m." ++ show t1 ++ "." ++ ts ++ ".wav " ++  show t1 ++ "." ++ ts ++ ".wav " ++ us ++ " tempo -s " ++ show v) 
    addSoftSign $ show t1 ++ "." ++ ts ++ ".wav"
                                               | otherwise = 
  do
    eE <- endE
    eS <- endS
    callCommand ("espeak" ++ eE ++ " -v " ++ ys ++ " " ++ zs ++ " -w m." ++ show t1 ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") 
    callCommand ("sox" ++ eS ++ " --multi-threaded  " ++  "m." ++ show t1 ++ "." ++ xs ++ ".wav " ++ show t1 ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s " ++ show v) 

-- | Function that takes a Ukrainian String and converts it to the data of the type ((String, String), (String, Integer)) that is used for zero-vowel words
-- Функція, що отримує на вхід український String і конвертує його на дані типу ((String, String), (String, Integer)), що використовується для слів без голосних
zeroSyllablePart :: C.ByteString ->  [((String, String), (String, Integer))]
zeroSyllablePart xs | C.all isSimPRecord xs =
  [((C.unpack xs, "esperanto"), ("-z",0))]
                    | otherwise =
  filter (not . null . fst . fst) . concatPunct . map (\x -> if x == "γ"
    then (("γ", "greek"), ("-z", 0))
    else if x == "y"
      then (("y", "polish"), ("-z", 0))
      else ((x, "esperanto"), ("-z", 0))) . convertSyllableToLanguage $ xs

-- | Function that creates data of the type [((String, String),(String,Integer))] for non-zero-syllable words
-- Функція, що створює дані типу [((String, String),(String,Integer))] для слів з голосними
createSyllablesMultiLast2 :: [[String]] -> (Maybe Integer, Integer) -> [((String, String),(String,Integer))]
createSyllablesMultiLast2 xss (Just y, z) | null xss = []
                                          | otherwise = case z of
    1 -> case y of
       0 -> concatMap (map (\x -> case x of
         "γ" -> (("γ", "greek"), ("-z", 1))
         "y" -> (("y", "polish"), ("-z", 0))
         _   -> ((x, "esperanto"), ("-z", 1)))) xss
       _ -> concatMap (map (\x -> case x of
         "γ" -> (("γ", "greek"), ("-a 110 -z", 1))
         "y" -> (("y", "polish"), ("-a 110 -z", 0))
         _   -> ((x, "esperanto"), ("-a 110 -z", 1)))) xss
    _ -> if y /= 0
           then concat [concatMap (map (\x -> case x of
             "γ" -> (("γ", "greek"), ("-z", z))
             "y" -> (("y", "polish"), ("-z", 0))
             _   -> ((x, "esperanto"), ("-z", z)))) . take (fromInteger (y - 1)) $ xss, concatMap (map (\x -> case x of
               "γ" -> (("γ", "greek"), ("-a 110 -z", z))
               "y" -> (("y", "polish"), ("-a 110 -z", 0))
               _   -> ((x, "esperanto"), ("-a 110 -z", z)))) [head $ drop (fromInteger (y - 1)) xss], concatMap (map (\x -> case x of
                 "γ" -> (("γ", "greek"), ("-z", z))
                 "y" -> (("y", "polish"), ("-z", 0))
                 _   -> ((x, "esperanto"), ("-z", z)))) $ drop (fromInteger y) xss]
           else (concatMap (map (\x -> case x of
             "γ" -> (("γ", "greek"), ("-z", z))
             "y" -> (("y", "polish"), ("-z", 0))
             _   -> ((x, "esperanto"), ("-z", z)))) . take (fromInteger (z - 1)) $ xss) ++ concatMap (map (\x -> case x of
               "γ" -> (("γ", "greek"), ("-a 110 -z", z))
               "y" -> (("y", "polish"), ("-a 110 -z", 0))
               _   -> ((x, "esperanto"), ("-a 110 -z", z)))) [last xss]
createSyllablesMultiLast2 xss (Nothing, z) | null xss = []
                                           | otherwise = createSyllablesMultiLast2 xss (Just (z - 1), z)

-- | Additional function for dividing into units for further processing
-- Додаткова функція для поділу на одиниці для подальшої обробки
divideToUnits :: C.ByteString -> [[String]]
divideToUnits xs | C.null xs = []
                 | otherwise = map convertSyllableToLanguage . createSyllablesMulti $ xs

-- | Function-predicate that checks whether its argument is a Ukrainian vowel letter
-- Функція-предикат, що перевіряє, чи її аргумент є українською голосною літерою
isVowelL :: Char -> Bool
isVowelL x | x >= '\x0065' =
  if x >= '\x0075'
    then case x of
      '\x0075' -> True
      '\x0079' -> True
      _        -> False
    else case x of
      '\x0065' -> True
      '\x0069' -> True
      '\x006F' -> True
      _        -> False
           | otherwise = 
  if x <= '\x0047'
    then case x of
      '\x0044' -> True
      '\x0046' -> True
      '\x0047' -> True
      _        -> False
    else case x of
      '\x004A' -> True
      '\x0061' -> True
      _        -> False

-- | Optimized function that is applied to the zero-syllable parts to concatenate the punctuation into the one sample of sounding
-- Оптимізована функція, що застосовується до нуль-складових слів, щоб з'єднати пунктуаційні знаки в одну частину озвучування
concatPunct :: [((String, String), (String, Integer))] -> [((String, String), (String, Integer))]
concatPunct = fst . foldr f v
  where v = ([], [])
        f x (ys, xs) | (not . null . fst . fst $ x) && isPunctOrSpace (head . fst . fst $ x) =
          ([((filter (not . DC.isSpace) (takeWhile isPunctOrSpace . concatMap (fst . fst) $ (x:xs)), "esperanto"), ("-z", 0))], x:xs)
                     | otherwise = (x:ys, x:xs)

-- | Additional function that is used inside the createSoundsForSyllable function to convert Ukrainian syllables into Esperanto, or Polish, or Greek ones
-- Додаткова функція, що використовується всередині createSoundsForSyllable для конвертування українського складу в склад мовою есперанто, польською чи грецькою
convertSyllableToLanguage ::  C.ByteString ->  [String]
convertSyllableToLanguage xs | C.null xs = []
                             | otherwise = 
  let chEs = takeWhile (not . null) . map (takeWithFirst (/= 'q')) . iterate (dropWithFirst (/= 'q')) . changeToEsperanto in if C.any (\x -> (x == '\x0041') || (x == '\x0079')) xs
    then concatMap (L.groupBy isSpecial) . chEs $ xs
    else chEs xs

-- | Function to create a list of syllables from the Ukrainian multi-syllable word
-- Функція, що утворює список складів з українського багатоскладового слова
createSyllablesMulti :: C.ByteString -> [C.ByteString]
createSyllablesMulti xs = let zss = divideToListOfSoundGroupsAsLists xs in
  map (\y -> C.concat . map fst . concat . take (snd y) . drop (fst y) $ zss) (listOfFrames xs)

-- | Function-predicate to check whether its argument is either a space or a punctuation sign
-- Функція-предикат для перевірки, чи її аргумент є чи пробілом, чи знаком пунктуації
isPunctOrSpace :: Char -> Bool
isPunctOrSpace x = DC.isSpace x || DC.isPunctuation x

-- | Optimized function to take elements of the list till the first occasion of the wrong predicate p including the first occurance
-- Оптимізована функція, щоб узяти елементи списку до першої появи хибності у предикаті, включаючи саму цю першу появу
takeWithFirst :: (a -> Bool) -> ([a] -> [a])
takeWithFirst p = fst . foldr f v
  where
    f x (ys,xs) = (if p x then x:ys else [x],x:xs)
    v = ([],[])

-- | Function that actually converts a Ukrainian word written as an encoded ByteString to the Esperanto string for further reading
-- Функція, що власне перетворює українське слово, записане як закодований ByteString, у Esperanto рядок для подальшого озвучування
changeToEsperanto :: C.ByteString -> String
changeToEsperanto xs | C.null xs = []
                     | otherwise = changeToDecoded . C.unpack . verbSpecialConv $ xs

-- | Function that is used to convert "ться" into the wright letter combination in Esperanto
-- Функція, що використовується для перетворення "ться" на правильну комбінацію літер есперанто
verbSpecialConv :: C.ByteString -> C.ByteString
verbSpecialConv xs | C.null xs = C.empty
                   | C.null . C.tail $ xs = xs
                   | not . C.null . C.tail . C.tail $ xs = if (C.head xs == '\x0013') && ((C.head . C.tail $ xs) == '\x0013') && ((C.head . C.tail . C.tail $ xs) == '\x0061')
  then 'c' `C.cons` 's' `C.cons` 'j' `C.cons` 'a' `C.cons` verbSpecialConv (C.drop 3 xs)
  else C.head xs `C.cons` verbSpecialConv (C.tail xs)
                   | otherwise = C.head xs `C.cons` verbSpecialConv (C.tail xs)

-- | Function-predicate to group only not special letters
-- Функція-предикат для групування лише не спеціальних літер
isSpecial :: Char -> Char -> Bool
isSpecial x y = continueLang x && continueLang y

-- | Intermediate function for creating a Ukrainian syllables
-- Проміжна функція для створення українських складів
divideToListOfSoundGroupsAsLists :: C.ByteString -> [[(C.ByteString, Char)]]
divideToListOfSoundGroupsAsLists xs = concatMap divideConsonants (takeWhile (not . null) $ prepareToSyllables xs)

-- | Function to create a list of frames for producing syllables in multi-syllable word
-- Функція, щоб створити список обмежень для генерування складів у багатоскладовому слові
listOfFrames :: C.ByteString -> [(Int,Int)]
listOfFrames xs = let ys = listOfPoints xs in zip (0:ys) (zipWith (-) ys (0:ys))

-- | Function that is used for decoding of the String
-- Функція, яка використовується для декодування String
changeToDecoded :: String -> String
changeToDecoded (y:ys) | null ys =
  if y >= '\x0063'
    then if y >= '\x0074'
      then if y >= '\x00A8'
        then if y >= '\x00FC'
          then if y >= '\x00FE'
            then case y of
              '\x00FE' -> "\x20A4"
              '\x00FF' -> "\x20B4"
              _        -> [y]
            else case y of
              '\x00FC' -> "\x20A2"
              '\x00FD' -> "\x20A3"
              _        -> [y]
          else if y <= '\x00B5'
            then case y of
              '\x00A8' -> "\x2122"
              '\x00B5' -> "\x20AC"
              _        -> [y]
            else case y of
              '\x00FA' -> "\x20A0"
              '\x00FB' -> "\x20A1"
              _        -> [y]
        else if y <= '\x0078'
          then if y >= '\x0077'
            then case y of
              '\x0077' -> "dĵ"
              '\x0078' -> "vq"
              _        -> [y]
            else case y of
              '\x0074' -> "t"
              '\x0075' -> "u"
              '\x0076' -> "v"
              _        -> [y]
          else if y <= '\x007A'
            then case y of
              '\x0079' -> "y"
              '\x007A' -> "z"
              _        -> [y]
            else case y of
              '\x00A2' -> "\x2109"
              '\x00A5' -> "\x2103"
              _        -> [y]
      else if y <= '\x006B'
        then if y >= '\x0068'
          then if y >= '\x006A'
            then case y of
              '\x006A' -> "j"
              '\x006B' -> "k"
              _        -> [y]
            else case y of
              '\x0068' -> "h"
              '\x0069' -> "i"
              _        -> [y]
          else if y <= '\x0065'
            then case y of
              '\x0063' -> "c"
              '\x0064' -> "d"
              '\x0065' -> "e"
              _        -> [y]
            else case y of
              '\x0066' -> "f"
              '\x0067' -> "g"
              _        -> [y]
        else if y <= '\x006F'
          then if y >= '\x006E'
            then case y of
              '\x006E' -> "n"
              '\x006F' -> "o"
              _        -> [y]
            else case y of
              '\x006C' -> "l"
              '\x006D' -> "m"
              _        -> [y]
          else if y <= '\x0071'
            then case y of
              '\x0070' -> "p"
              '\x0071' -> ""
              _        -> [y]
            else case y of
              '\x0072' -> "r"
              '\x0073' -> "s"
              _        -> [y]
    else if y <= '\x0049'
      then if y >= '\x0016'
        then if y >= '\x0045'
          then if y >= '\x0047'
            then case y of
              '\x0047' -> "ja"
              '\x0049' -> "ĉ"
              _        -> [y]
            else case y of
              '\x0045' -> "ĵ"
              '\x0046' -> "je"
              _        -> [y]
          else if y <= '\x0042'
            then case y of
              '\x0016' -> "ŝĉq"
              '\x0041' -> "γ"
              '\x0042' -> "ŝ"
              _        -> [y]
            else case y of
              '\x0043' -> "ŝĉ"
              '\x0044' -> "ji"
              _        -> [y]
        else if y <= '\x0011'
          then if y >= '\x0010'
            then case y of
              '\x0010' -> "tq"
              '\x0011' -> "fq"
              _        -> [y]
            else case y of
              '\x0007' -> "rq"
              '\x000C' -> "sq"
              _        -> [y]
          else if y <= '\x0013'
            then case y of
              '\x0012' -> "hq"
              '\x0013' -> "cq"
              _        -> [y]
            else case y of
              '\x0014' -> "ĉq"
              '\x0015' -> "ŝq"
              _        -> [y]
      else if y <= '\x0055'
        then if y >= '\x0052'
          then if y >= '\x0054'
            then case y of
              '\x0054' -> "ĵ"
              '\x0055' -> "zq"
              _        -> [y]
            else case y of
              '\x0052' -> "gq"
              '\x0053' -> "dq"
              _        -> [y]
          else if y <= '\x004F'
            then if y >= '\x004D'
              then case y of
                '\x004D' -> "\x2014"
                '\x004E' -> "..."
                '\x004F' -> "dz"
                _        -> [y]
              else case y of
                '\x004A' -> "ju"
                '\x004B' -> ""
                '\x004C' -> "\x2013"
                _        -> [y]
            else case y of
              '\x0050' -> "dĵq"
              '\x0051' -> "dzq"
              _        -> [y]
        else if y <= '\x0059'
          then if y >= '\x0058'
            then case y of
              '\x0058' -> "mq"
              '\x0059' -> "nq"
              _        -> [y]
            else case y of
              '\x0056' -> "kq"
              '\x0057' -> "lq"
              _        -> [y]
          else if y <= '\x005E'
            then case y of
              '\x005A' -> "pq"
              '\x005E' -> "bq"
              _        -> [y]
            else case y of
              '\x0061' -> "a"
              '\x0062' -> "b"
              _        -> [y]
                       | otherwise = 
  let zy zs = if head zs `notElem` "aojue"
      then 'q'
      else 'j' in
        if y >= '\x0063'
          then if y >= '\x0074'
            then if y >= '\x00A8'
              then if y >= '\x00FC'
                then if y >= '\x00FE'
                  then case y of
                    '\x00FE' -> '\x20A4':changeToDecoded ys
                    '\x00FF' -> '\x20B4':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x00FC' -> '\x20A2':changeToDecoded ys
                    '\x00FD' -> '\x20A3':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                else if y <= '\x00B5'
                  then case y of
                    '\x00A8' -> '\x2122':changeToDecoded ys
                    '\x00B5' -> '\x20AC':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x00FA' -> '\x20A0':changeToDecoded ys
                    '\x00FB' -> '\x20A1':changeToDecoded ys
                    _        -> y:changeToDecoded ys
              else if y <= '\x0078'
                then if y >= '\x0077'
                  then case y of
                    '\x0077' -> 'd':'ĵ':changeToDecoded ys
                    '\x0078' -> 'v':zy ys:changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x0074' -> 't':changeToDecoded ys
                    '\x0075' -> 'u':changeToDecoded ys
                    '\x0076' -> 'v':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                else if y <= '\x007A'
                  then case y of
                    '\x0079' -> 'y':changeToDecoded ys
                    '\x007A' -> 'z':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x00A2' -> '\x2109':changeToDecoded ys
                    '\x00A5' -> '\x2103':changeToDecoded ys
                    _        -> y:changeToDecoded ys
            else if y <= '\x006B'
              then if y >= '\x0068'
                then if y >= '\x006A'
                  then case y of
                    '\x006A' -> 'j':changeToDecoded ys
                    '\x006B' -> 'k':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x0068' -> 'h':changeToDecoded ys
                    '\x0069' -> 'i':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                else if y <= '\x0065'
                  then case y of
                    '\x0063' -> 'c':changeToDecoded ys
                    '\x0064' -> 'd':changeToDecoded ys
                    '\x0065' -> 'e':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x0066' -> 'f':changeToDecoded ys
                    '\x0067' -> 'g':changeToDecoded ys
                    _        -> y:changeToDecoded ys
              else if y <= '\x006F'
                then if y >= '\x006E'
                  then case y of
                    '\x006E' -> 'n':changeToDecoded ys
                    '\x006F' -> 'o':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x006C' -> 'l':changeToDecoded ys
                    '\x006D' -> 'm':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                else if y <= '\x0071'
                  then case y of
                    '\x0070' -> 'p':changeToDecoded ys
                    '\x0071' -> changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x0072' -> 'r':changeToDecoded ys
                    '\x0073' -> 's':changeToDecoded ys
                    _        -> y:changeToDecoded ys
          else if y <= '\x0049'
            then if y >= '\x0016'
              then if y >= '\x0045'
                then if y >= '\x0047'
                  then case y of
                    '\x0047' -> 'j':'a':changeToDecoded ys
                    '\x0049' -> 'ĉ':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x0045' -> 'ĵ':changeToDecoded ys
                    '\x0046' -> 'j':'e':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                else if y <= '\x0042'
                  then case y of
                    '\x0016' -> 'ŝ':'ĉ':zy ys:changeToDecoded ys
                    '\x0041' -> 'γ':changeToDecoded ys
                    '\x0042' -> 'ŝ':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x0043' -> 'ŝ':'ĉ':changeToDecoded ys
                    '\x0044' -> 'j':'i':changeToDecoded ys
                    _        -> y:changeToDecoded ys
              else if y <= '\x0011'
                then if y >= '\x0010'
                  then case y of
                    '\x0010' -> 't':zy ys:changeToDecoded ys
                    '\x0011' -> 'f':zy ys:changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x0007' -> 'r':zy ys:changeToDecoded ys
                    '\x000C' -> 's':zy ys:changeToDecoded ys
                    _        -> y:changeToDecoded ys
                else if y <= '\x0013'
                  then case y of
                    '\x0012' -> 'h':zy ys:changeToDecoded ys
                    '\x0013' -> 'c':zy ys:changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x0014' -> 'ĉ':zy ys:changeToDecoded ys
                    '\x0015' -> 'ŝ':zy ys:changeToDecoded ys
                    _        -> y:changeToDecoded ys
            else if y <= '\x0055'
              then if y >= '\x0052'
                then if y >= '\x0054'
                  then case y of
                    '\x0054' -> 'ĵ':changeToDecoded ys
                    '\x0055' -> 'z':zy ys:changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x0052' -> 'g':zy ys:changeToDecoded ys
                    '\x0053' -> 'd':zy ys:changeToDecoded ys
                    _        -> y:changeToDecoded ys
                else if y <= '\x004F'
                  then case y of
                    '\x004A' -> 'j':'u':changeToDecoded ys
                    '\x004B' -> changeToDecoded ys
                    '\x004F' -> 'd':'z':changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x0050' -> 'd':'ĵ':zy ys:changeToDecoded ys
                    '\x0051' -> 'd':'z':zy ys:changeToDecoded ys
                    _        -> y:changeToDecoded ys
              else if y <= '\x0059'
                then if y >= '\x0058'
                  then case y of
                    '\x0058' -> 'm':zy ys:changeToDecoded ys
                    '\x0059' -> 'n':zy ys:changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x0056' -> 'k':zy ys:changeToDecoded ys
                    '\x0057' -> 'l':zy ys:changeToDecoded ys
                    _        -> y:changeToDecoded ys
                else if y <= '\x005E'
                  then case y of
                    '\x005A' -> 'p':zy ys:changeToDecoded ys
                    '\x005E' -> 'b':zy ys:changeToDecoded ys
                    _        -> y:changeToDecoded ys
                  else case y of
                    '\x0061' -> 'a':changeToDecoded ys
                    '\x0062' -> 'b':changeToDecoded ys
                    _        -> y:changeToDecoded ys
changeToDecoded _ = []

-- | Additional function-predicate to check if its argument is a sound that converts to another language than the default Esperanto one
-- Додаткова функція-предикат, щоб перевірити, чи її аргумент є звуком, що конверується до іншої мови, ніж звичайна есперанто
continueLang :: Char -> Bool
continueLang x = case x of
  'y' -> False
  'γ' -> False
  _  -> True

-- | Function that divides a list of data of the type (String, Char) representing the Ukrainian consonants into two groups for further syllable constuction
-- Функція, що ділить список даних типу (String, Char), що представляють українські приголосні, на дві групи для подальшого конструювання складів
divideConsonants :: [(C.ByteString, Char)] -> [[(C.ByteString,Char)]]
divideConsonants xs = let y = length xs in case y of
  1 -> [xs]
  2 | (elem (snd . head $ xs) "rq" && head xs /= last xs) || (elem (snd . head $ xs) "di" && elem (snd . head . tail $ xs) "sa") -> [[head xs], tail xs] 
    | otherwise -> [xs]
  3 | (snd . head $ xs) `elem` "rq" -> [[head xs], tail xs] 
    | (snd . head . tail $ xs) `elem` "rq" -> [[head xs, head . tail $ xs], [last xs]] 
    | otherwise -> [xs]
  _ | (snd . head $ xs) `elem` "rqdi" -> [[head xs], tail xs] 
    | otherwise -> [xs]

-- | Function that prepares a Ukrainian word to be divided into syllables
-- Функція, що готує українське слово для поділу на склади
prepareToSyllables :: C.ByteString -> [[(C.ByteString, Char)]]
prepareToSyllables xs = map createSoundL (createSoundGroups xs)

-- | Additional list of the amount of parts to be taken to create syllables
-- Додаткова список кількостей частин, які потрібно узяти, щоб створити склади
listOfPoints :: C.ByteString -> [Int]
listOfPoints xs = let countSoundGroups = length . divideToListOfSoundGroupsAsLists $ xs in
  let w1 = fromIntegral . C.length . C.filter isVowelL $ xs in take w1 . map (amountOfPartsForKthSyl xs) $ [1..countSoundGroups]

-- | Additional function that is used to divide a Ukrainian word into syllables, it creates a list of data of the type (String, Char)
-- Додаткова функція, що використовується для поділу українського слова на склади, створює список даних типу (String, Char)
createSoundL :: C.ByteString -> [(C.ByteString, Char)]
createSoundL xs | C.null xs = []
                | otherwise = (createSoundLChar . C.head $ xs):(createSoundL . C.tail $ xs)

-- | Additional function that is used to divide a Ukrainian word into syllables
-- Додаткова функція, що використовується, щоб поділити українське слово на склади
createSoundGroups :: C.ByteString -> [C.ByteString]
createSoundGroups = C.groupBy isSimilar

-- | Additional function to find out the amount of parts to be taken for the k-th syllable
-- Додаткова функція, щоб визначити, яку кількість частин потрібно взяти для k-го складу
amountOfPartsForKthSyl :: C.ByteString -> Int -> Int
amountOfPartsForKthSyl xs k = let u = length . divideToListOfSoundGroupsAsLists $ xs in let w = mapLS xs [1..u] in let t = length . takeWhile (== k) . dropWhile (< k) $ w in
  let w2 = toInteger . C.length . C.filter isVowelL $ xs in if w2 == 1
    then u
-- Multisyllable word
    else case t of
      3 -> if toInteger k < w2
-- There is a next vowel in the word
             then (length . takeWhile (<= k) $ w) - 1
             else u
      2 -> if toInteger k < w2
-- There is a next vowel in the word
             then (length . takeWhile (<= k) $ w) - 1
             else u
      _ -> if toInteger k < w2
             then length . takeWhile (<= k) $ w
             else u

-- | Function that converts encoded Char into a tuple (ByteString, Char) for further usage in syllable segmentation
-- Функція, що конвертує кодований символ у кортеж (ByteString, Char) для подальшого використання у поділі на склади
createSoundLChar :: Char -> (C.ByteString, Char)
createSoundLChar x = (C.singleton x, y)
  where y | isVowelL x = 'w'
          | x <= '\x005A' = if x >= '\x0045' 
             then if x >= '\x0056'
                    then if x >= '\x0057' && x <= '\x0059' 
                          then 'q'
                          else case x of 
                            '\x0056' -> 'a'
                            '\x005A' -> 'a'
                            _        -> '0'
                    else if x <= '\x0049'
                           then case x of
                             '\x0045' -> 'd'
                             '\x0049' -> 's'
                             _        -> '0'
                           else if x >= '\x0050'  
                                  then 'i'
                                  else case x of 
                                    '\x004F' -> 'd'
                                    _        -> '0'
             else if x >= '\x0014'
                    then if x >= '\x0042'
                           then case x of
                             '\x0042' -> 's'
                             '\x0043' -> 's'
                             _        -> '0'
                           else if x <= '\x0016' 
                                  then 'a'
                                  else case x of 
                                    '\x0041' -> 'd'
                                    _        -> '0'
                    else if x >= '\x0010'
                           then if x <= '\x0012' 
                                  then 'a' 
                                  else case x of 
                                    '\x0013' -> 'i'
                                    _        -> '0'
                           else case x of
                             '\x0007' -> 'q'
                             '\x000C' -> 'a'
                             _        -> '0'
          | x >= '\x005E' = if x >= '\x0070'
             then if x >= '\x0077'
                    then case x of
                      '\x0077' -> 'd'
                      '\x0078' -> 'q'
                      '\x007A' -> 'd'
                      _        -> '0'
                    else if x <= '\x0072'
                           then case x of 
                             '\x0070' -> 's'
                             '\x0072' -> 'r'
                             _        -> '0'
                           else if x == '\x0073' || x == '\x0074' 
                                  then 's'
                                  else case x of 
                                    '\x0076' -> 'r'
                                    _        -> '0'
             else if x <= '\x0067'
                    then if x <= '\x0064'
                           then if x >= '\x0062' 
                                  then 'd'
                                  else case x of 
                                    '\x005E' -> 'i'
                                    _        -> '0'
                           else case x of 
                             '\x0066' -> 's'
                             '\x0067' -> 'd'
                             _        -> '0'
                    else if x <= '\x006A'
                           then case x of 
                             '\x0068' -> 's'
                             '\x006A' -> 'r'
                             _        -> '0'
                           else if x >= '\x006C' && x <= '\x006E' 
                                  then 'r'
                                  else case x of 
                                    '\x006B' -> 's'
                                    _        -> '0'
          | otherwise = '0'    

-- | Function that checks whether its arguments are both consonants
-- Функція, що перевіряє, чи є обидва її аргументи приголосні
isSimilar :: Char -> Char -> Bool
isSimilar x y = isConsonantL x && isConsonantL y

-- | Function-predicate to check whether its argument is a consonant
-- Функція-предикат, яка перевіряє, чи є її аргумент приголосним
isConsonantL :: Char -> Bool
isConsonantL x | x <= '\x005E' = if x <= '\x0043'
                                   then if x <= '\x0043'
                                          then (x >= '\x0010' && x <= '\x0016') || x >= '\x0041'
                                          else x == '\x0007' || x == '\x000C'
                                   else if x >= '\x004F'
                                          then x <= '\x005A'
                                          else x == '\x005E' || (x == '\x0045' || x == '\x0049')
               | otherwise = 
  if x >= '\x0070'
    then (x >= '\x0070' && x <= '\x0078' && x /= '\x0075') || x == '\x007A'
    else (x >= '\x0062' && x <= '\x0068' && x /= '\x0065') || (x >= '\x006B' && x <= '\x006E')

-- | Function to create a list of Int that is used for dividing into syllables for words with one or more vowels
-- Функція, щоб створити список Int, який використовується для поділу на склади для слів з одним чи більше голосним
mapLS :: C.ByteString -> [Int] -> [Int]
mapLS xs ks = let zss = divideToListOfSoundGroupsAsLists xs in map (\x -> sum1 (map ((== 'w') . snd) $ concat $ take x zss) $! 0) ks 
  where sum1 ys accum | not . null $ ys = if head ys then sum1 (tail ys) $! (accum + 1) else sum1 (tail ys) accum 
                      | otherwise = accum
