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

import Text.Replace 
import System.CPUTime
import System.Process (callCommand)
import System.Directory (findExecutable)
import System.Info (os)

-- A new type for presentation of the Ukrainian text for eSpeak in the form it can process directly
-- Новий тип даних для представлення українського тексту для eSpeak у формі, яку він може прямо обробляти
data Language a = Polish a | Esperanto a
   deriving (Eq,Ord,Show)

-- Function that 'extracts' inner value from the data type Language a
-- Функція, яка "витягує" вкладене значення у дані Language a
getThrough :: Language a -> a
getThrough (Polish x) = x
getThrough (Esperanto x) = x

-- Function-predicate that checks whether a Language data is actually the Esperanto one
-- Функція-предикат, яка перевіряє, чи є дані типу Language власне Esperanto
isEsperanto :: Language a -> Bool
isEsperanto (Esperanto _) = True
isEsperanto _ = False


-- Function that primarily converts Ukrainian line into more sounds-based line
-- Функція, що початково перетворює український рядок на більш орінтований на звуки рядок
ukrainianToMoreSounding :: String -> String
ukrainianToMoreSounding xs = replaceWithList [Replace (string'fromString "ться") "цьцьа", Replace (string'fromString " --") " ", Replace (string'fromString "-- ") " ", Replace (string'fromString " -") " ",  Replace (string'fromString "- ") " ", Replace (string'fromString "ья") "ьйа", Replace (string'fromString "ьє") "ьйе", Replace (string'fromString "ью") "ьйу", Replace (string'fromString "ьї") "ьйі", Replace (string'fromString "’я") "ййа", Replace (string'fromString "’є") "ййе", Replace (string'fromString "’ю") "ййу", Replace (string'fromString "’ї") "ййі",  Replace (string'fromString "щ") "шч"] xs

-- Function to convert Ukrainian "я", "ю", "є" and "ї" into some other String for syllables processing
-- Функція для перетворення українських "я", "ю", "є" та "ї" на деякі інші рядки для обробки складів
ukrainianJotted2 :: String -> String
ukrainianJotted2 xs = replaceWithList [Replace (string'fromString "Я") "Йа", Replace (string'fromString "Ю") "Йу", Replace (string'fromString "Є") "Йе", Replace (string'fromString "Ї") "Йі", Replace (string'fromString "ї") "йі"] xs

-- Function to convert Ukrainian "я", "ю", "є" and "ї" into some other String for syllables processing
-- Функція для перетворення українських "я", "ю", "є" та "ї" на деякі інші рядки для обробки складів
ukrainianJotted1 :: String -> String
ukrainianJotted1 (x:y:xs) = case y of
  'я' -> if elem x "- ЯЮЄЇяюєїаоуеиіАОУЕИІ"
    then x:'й':(ukrainianJotted1 ('а':xs))
    else x:'ь':(ukrainianJotted1  ('а':xs))
  'ю' -> if elem x "- ЯЮЄЇяюєїаоуеиіАОУЕИІ"
    then x:'й':(ukrainianJotted1 ('у':xs))
    else x:'ь':(ukrainianJotted1 ('у':xs))
  'є' -> if elem x "- ЯЮЄЇяюєїаоуеиіАОУЕИІ"
    then x:'й':(ukrainianJotted1 ('е':xs))
    else x:'ь':(ukrainianJotted1 ('е':xs))
  'ї' -> x:'й':(ukrainianJotted1 ('і':xs))
  _ -> x:(ukrainianJotted1 (y:xs))
ukrainianJotted1 [] = []
ukrainianJotted1 [x] = [x]

-- Function to convert Ukrainian "я", "ю", "є" and "ї" into some other String for syllables processing
-- Функція для перетворення українських "я", "ю", "є" та "ї" на деякі інші рядки для обробки складів
ukrainianJottedLast :: String -> String
ukrainianJottedLast xs = replaceWithList [Replace (string'fromString "я") "йа", Replace (string'fromString "ю") "йу", Replace (string'fromString "є") "йе", Replace (string'fromString "ї") "йі"] xs

-- Function that append the wrong sounding separate one-letter words to the next word to obtain more proper sounding
-- Функція, що додає неправильно озвучувані однолітерні слова до наступного слова, щоб отримати більш правильне озвучування
concatUkrainian :: [String] -> [String]
concatUkrainian [] = []
concatUkrainian (x:xs) | not . null $ xs = if ((x == "в") || (x == "В") ||(x == "з") ||(x == "З") || (x == "Й") || (x == "й")) 
                                              then (x ++ (head xs)):(concatUkrainian . tail $ xs)
                                              else x:(concatUkrainian xs)
                       | otherwise = [x]

-- Function that append the wrong sounding separate one-letter word "ж" or "б" to the previous word to obtain more proper sounding
-- Функція, що додає неправильно озвучуване однолітерне слово "ж" або "б" до попереднього слова, щоб отримати більш правильне озвучування
concatUkrainianZhOrB :: [String] -> [String]
concatUkrainianZhOrB [] = []
concatUkrainianZhOrB xs = zipWith togetherZh xs (mappend (tail xs) [" "])
               where togetherZh ys zs | (zs == "ж") = ys ++ "ж"
                                      | (zs == "б") = ys ++ "б"
                                      | (ys == "ж") = ""
                                      | (ys == "б") = ""
                                      | otherwise = ys

-- Function that produces the list of Ukrainian strings from the primary Ukrainian string which can be further easily processed
-- Функція, яка створює список українських рядків з початкового українського рядка, які можуть бути легко оброблені далі
words2 :: String -> [String]
words2 [] = [""]
words2 xs = concatUkrainian . concatUkrainianZhOrB . words . ukrainianJottedLast . ukrainianJotted2 . ukrainianJotted1 .  ukrainianToMoreSounding $ xs

-- Function-predicate that checks whether its argument is either a digit character or a dash
-- Функція-предикат, що перевіряє, чи її аргумент є символом цифри чи дефісу
isDigitOrDash :: Char -> Bool
isDigitOrDash x = case x of
  '0' -> True
  '1' -> True
  '2' -> True
  '3' -> True
  '4' -> True
  '5' -> True
  '6' -> True
  '7' -> True
  '8' -> True
  '9' -> True
  '-' -> True
  _   -> False

-- Function-predicate that checks whether its argument is a digit character
-- Функція-предикат, що перевіряє, чи її аргумент є символом цифри
isDigit :: Char -> Bool
isDigit x = case x of
  '0' -> True
  '1' -> True
  '2' -> True
  '3' -> True
  '4' -> True
  '5' -> True
  '6' -> True
  '7' -> True
  '8' -> True
  '9' -> True
  _   -> False


-- Function-predicate that checks whether its argument is neither a digit character nor a dash
-- Функція-предикат, що перевіряє, чи її аргумент не є символом цифри чи дефісу
isNotDigitOrDash :: Char -> Bool
isNotDigitOrDash = not . isDigitOrDash

-- Function-predicate that checks whether its argument is a Ukrainian vowel letter
-- Функція-предикат, що перевіряє, чи її аргумент є українською голосною літерою
isVowelL :: Char -> Bool
isVowelL x = case x of
  'а' -> True
  'о' -> True
  'е' -> True
  'и' -> True
  'і' -> True
  'у' -> True
  'А' -> True
  'О' -> True
  'Е' -> True
  'И' -> True
  'І' -> True
  'У' -> True
  'я' -> True
  'ю' -> True
  'є' -> True
  'ї' -> True
  'Я' -> True
  'Ю' -> True
  'Є' -> True
  'Ї' -> True
  _   -> False

-- Function that creates from a Ukrainian pre-processed String a data of the type (String, (Maybe Integer, Integer)) that takes into account a word emphasis
-- Функція, що створює з попередньо обробленого українського String дані типу (String, (Maybe Integer, Integer)), що враховують наголос у складі
accountEmphasis :: String -> (String, (Maybe Integer, Integer))
accountEmphasis [] = ([], (Nothing, toInteger 0))
accountEmphasis y@(x:xs) = let z = toInteger . length . filter (isVowelL) $ y in if isDigitOrDash x
  then if (stringToInteger . dropWhile (== '0') . filter (isDigit) $ y) `mod` z == 0
         then (filter (isNotDigitOrDash) $ y, (Just z, z))
         else (filter (isNotDigitOrDash) $ y, (Just ((stringToInteger . dropWhile (== '0') . filter (isDigit) $ y) `mod` z), z))
  else (y, (Nothing, z))

-- Additional function that is used to divide a Ukrainian word into syllables
-- Додаткова функція, що використовується, щоб поділити українське слово на склади
createSoundGroups :: String -> [String]
createSoundGroups [] = [[]]
createSoundGroups y@(x:xs) = if isVowelL x
  then [x]:(createSoundGroups xs)
  else let k = (takeWhile (not . isVowelL) $ y) in if k /= y
    then concat [[k, [head . dropWhile (not . isVowelL) $ y]], createSoundGroups . tail . dropWhile (not . isVowelL) $ y]
    else [y]

-- Additional function that is used to divide a Ukrainian word into syllables, it creates a list of data of the type (String, Char)
-- Додаткова функція, що використовується для поділу українського слова на склади, створює список даних типу (String, Char)
createSoundL :: String -> [(String, Char)]
createSoundL [] = []
createSoundL (x:y:z:xs) = if isVowelL x
  then ([x], 'w'):(createSoundL (y:z:xs))
  else if y == 'ь'
    then if elem x "вмнлрВМНЛР"
           then ([x,y],'q'):(createSoundL (z:xs))
           else if ((x == 'й') || (x == 'Й'))
                  then ([x],'r'):(createSoundL (y:z:xs))
                  else if elem x "бдзжгґБДЗЖГҐ"
                         then ([x,y],'i'):(createSoundL (z:xs))
                         else ([x,y],'a'):(createSoundL (z:xs))
    else if isVowelL y
           then if elem x "вмнлрйВМНЛРЙ"
                  then ([x],'r'):([y],'w'):(createSoundL (z:xs))
                  else if elem x "бзжгґБЗЖГҐ"
                         then ([x],'d'):([y],'w'):(createSoundL (z:xs))
                         else ([x],'s'):([y],'w'):(createSoundL (z:xs))
           else if ((x == 'д') || (x == 'Д'))
                  then if ((y == 'з') || (y == 'ж'))
                         then if z == 'ь'
                                then ([x,y,z], 'i'):(createSoundL xs)
                                else ([x,y], 'd'):(createSoundL (z:xs))
                         else ([x], 'd'):(createSoundL (y:z:xs))
                  else if elem x "вмнлрйВМНЛРЙ"
                          then ([x], 'r'):(createSoundL (y:z:xs))
                          else if elem x "бзжгґБЗЖГҐ"
                                  then ([x], 'd'):(createSoundL (y:z:xs))
                                  else ([x], 's'):(createSoundL (y:z:xs))
createSoundL [x,y] = if isVowelL x
  then ([x], 'w'):(createSoundL [y])
  else if y == 'ь'
    then if elem x "вмнлрВМНЛР"
           then [([x,y],'q')]
           else if elem x "бдзжгґБДЗЖГҐ"
                         then [([x,y],'i')]
                         else [([x,y],'a')]
    else if isVowelL y
           then if elem x "вмнлрйВМНЛРЙ"
                  then ([x],'r'):[([y],'w')]
                  else if elem x "бзжгґБЗЖГҐ"
                         then ([x],'d'):[([y],'w')]
                         else ([x],'s'):[([y],'w')]
           else if ((x == 'д') || (x == 'Д'))
                  then if ((y == 'з') || (y == 'ж'))
                         then [([x,y], 'd')]
                         else ([x], 'd'):(createSoundL [y])
                  else if elem x "вмнлрйВМНЛРЙ"
                          then ([x], 'r'):(createSoundL [y])
                          else if elem x "бзжгґБЗЖГҐ"
                                  then ([x], 'd'):(createSoundL [y])
                                  else ([x], 's'):(createSoundL [y])
createSoundL [x] = if isVowelL x
  then [([x], 'w')]
  else if elem x "вмнлрйВМНЛРЙ"
    then [([x], 'r')]
    else if elem x "бдзжгґБДЗЖГҐ"
           then [([x],'d')]
           else [([x],'s')]

-- Additional function-predicate to check whether its argument begins with a data of the type (String, Char) representing a consonant Ukrainian sound
-- Додаткова функція-предикат, що перевіряє, чи її аргумент починається з даних типу (String, Char), що представляють приголосний український звук
beginsWithC :: [(String, Char)] -> Bool
beginsWithC [] = False
beginsWithC xs = if (snd (head xs) /= 'w') then True else False

-- Additional function to make possible application of the simple functions to divide into syllables the Ukrainian word
-- Додаткова функція, що робить можливим застосування простих функцій для поділу на склади українського слова
endApplicationToListOfLists :: ([t] -> [a]) -> ([t] -> [[a]]) -> [[t]] -> Int -> [[a]]
endApplicationToListOfLists g h [[a]] k = concat [[take (k-1) (concatMap g [[a]])],h $ head $ drop (k-1) [[a]],[drop k (concatMap g [[a]])]]

-- Function that prepares a Ukrainian word to be divided into syllables
-- Функція, що готує українське слово для поділу на склади
prepareToSyllables :: String -> [[(String, Char)]]
prepareToSyllables xs = map createSoundL (createSoundGroups xs)

-- Function that divides a list of data of the type (String, Char) representing the Ukrainian consonants into two groups for further syllable constuction
-- Функція, що ділить список даних типу (String, Char), що представляють українські приголосні, на дві групи для подальшого конструювання складів
divideConsonants :: [(String, Char)] -> [[(String,Char)]]
divideConsonants xs = let y = length xs in  case y of
  1 -> [xs]
  2 -> if (elem (snd $ head xs) "rq" && head xs /= last xs) || (elem (snd $ head xs) "di" && elem (snd $ head $ tail xs) "sa") then [[head xs], tail xs] else [xs]
  3 -> if elem (snd $ head xs) "rq" then [[head xs], tail xs] else if elem (snd $ head $ tail xs) "rq" then [[head xs, head $ tail xs], [last xs]] else [xs]
  4 -> if elem (snd $ head xs) "rqdi" then [[head xs], tail xs] else [xs]

-- Intermediate function for creating a Ukrainian syllables
-- Проміжна функція для створення українських складів
divideToSyllables1 :: String -> [[(String, Char)]]
divideToSyllables1 xs = concatMap divideConsonants (takeWhile (not . null) (prepareToSyllables xs))

-- Function that takes a Ukrainian String and converts it to the data of the type ((String, String), (String, Integer)) that is used for zero-vowel words
-- Функція, що отримує на вхід український String і конвертує його на дані типу ((String, String), (String, Integer)), що використовується для слів без голосних
zeroSyllablePart :: String ->  ((String, String), (String, Integer))
zeroSyllablePart xs =  ((xs, "esperanto"), ("-g 0 -s 100 -z", toInteger 0))

mapLS xs ks = let zss = divideToSyllables1 xs in map (\x -> length . filter (== 'w') . map snd $ concat $ take x zss) ks

-- Additional function to find out the amount of parts to be taken for the k-th syllable
-- Додаткова функція, щоб визначити, яку кількість частин потрібно взяти для k-го складу
rr xs k = let u = length $ divideToSyllables1 xs in let t = length $ filter (== k) $ mapLS xs [1..u] in case t of
  3 -> if k < u then (length $ filter (<= k) $ mapLS xs [1..u]) - 1 else u
  2 -> if k < u then (length $ filter (<= k) $ mapLS xs [1..u]) - 1 else u
  1 -> length $ filter (<= k) $ mapLS xs [1..u]
  otherwise -> if k < u then k else u

-- Additional list of the amount of parts to be taken to create syllables
-- Додаткова список кількостей частин, які потрібно узяти, щоб створити склади
listOfPoints :: String -> [Int]
listOfPoints xs = let u = length $ divideToSyllables1 xs in
  let w = length $ filter isVowelL xs in
    concat [init $ take w $ map (\x -> rr xs x) [1..u], [u]]

-- Function to create a list of frames for producing syllables in multi-syllable word
-- Функція, щоб створити список обмежень для генерування складів у багатоскладовому слові
listOfFrames :: String -> [(Int,Int)]
listOfFrames xs = let ys = listOfPoints xs in
  zip (0:ys) (zipWith (-) ys (0:ys))

-- Function to create a list of syllables from the Ukrainian multi-syllable word
-- Функція, що утворює список складів з українського багатоскладового слова
createSyllablesMulti :: String -> [String]
createSyllablesMulti xs = let zss = divideToSyllables1 xs in
  map (\y -> concatMap fst $ concat $ take (snd y) $ drop (fst y) zss) (listOfFrames xs)

-- Function that creates data of the type [((String, String),(String,Integer))] for multi-syllable words
-- Функція, що створює дані типу [((String, String),(String,Integer))] для багатоскладових слів
createSyllablesMultiLast :: [String] -> (Maybe Integer, Integer) -> [((String, String),(String,Integer))]
createSyllablesMultiLast xs (Just y, z) | y < z = concat [let u = take (fromInteger (y-1)) xs in  if (not . null $ u) then map (\x -> ((x, if elem 'и' x || elem 'И' x then "polish" else "esperanto"), ("-g 0 -z", z))) $ u else [],let u = head $ drop (fromInteger (y-1)) xs in map (\x -> ((x, if elem 'и' x || elem 'И' x then "polish" else "esperanto"), ("-g 0 -z -a 110", z))) [u],let u = take (fromInteger (z-y-1)) $ drop (fromInteger y) xs in if (not . null $ u) then map (\x -> ((x, if elem 'и' x || elem 'И' x then "polish" else "esperanto"), ("-g 0 -z", z))) $ u else [],map (\x -> ((x, if elem 'и' x || elem 'И' x then "polish" else "esperanto"), ("-g 0", z))) [last xs]]
                                        | otherwise = concat [map (\x -> ((x, if elem 'и' x || elem 'И' x then "polish" else "esperanto"), ("-g 0 -z", z))) $ init xs,map (\x -> ((x, if elem 'и' x || elem 'И' x then "polish" else "esperanto"), ("-g 0 -a 110", z))) [last xs]]
createSyllablesMultiLast xs (Nothing, z) = createSyllablesMultiLast xs (Just (z - 1), z)                                          

-- Function that creates Ukrainian syllables and groups them with some parameters to be then processed by the eSpeak and SoX executables
-- Функція, що створює українські склади та групує їх з деякими параметрами, щоб потім вони були оброблені програмами eSpeak і SoX
createSyllablesReady :: (String, (Maybe Integer, Integer)) -> [((String, String),(String,Integer))]
createSyllablesReady x = case snd . snd $ x of
  0 -> map (zeroSyllablePart . fst) $ createSoundL $ filter isNotDigitOrDash $ fst x
  1 -> [((fst x, if elem 'и' (fst x) || elem 'И' (fst x) then "polish" else "esperanto"), ("-g 0", 1))]
  otherwise -> createSyllablesMultiLast (createSyllablesMulti $ fst x) (fst . snd $ x, snd . snd $ x)

-- Function that for the Ukrainian syllable represented as ((String, String),(String,Integer)) creates sounds
-- Функція, що для українського складу представленого як ((String, String),(String,Integer)) створює звуки
createSoundsForSyllable :: IO Integer -> ((String, String),(String,Integer)) -> IO ()
createSoundsForSyllable time ((xs, ys),(zs, k)) = case k of
    0 -> do
                   t <- time
                   let t1 = 10000000000 + (t `div` 10000000) in
                     if (os == "Windows") 
                       then do 
                           x <- findExecutable "espeak.exe"
                           y <- findExecutable "sox.exe"
                           if (x /= Nothing) && (y /= Nothing)
                              then do
                                let ts = convertSyllableToLanguage xs in if last xs == 'ь'
                                  then do
                                    return ("espeak.exe -z -v " ++ ys ++ " " ++ zs ++ " -w " ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                    return ("sox.exe " ++ (show t1) ++ "." ++ ts ++ ".wav " ++ (show t1) ++ "." ++ ts ++ ".wav ") >>= callCommand
                                    addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                  else do
                                    return ("espeak.exe -z -v " ++ ys ++ " " ++ zs ++ " -w " ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                    return ("sox.exe " ++ (show t1) ++ "." ++ ts ++ ".wav " ++ (show t1) ++ "." ++ ts ++ ".wav ") >>= callCommand                                    
                              else error "Please, install eSpeak executable espeak.exe and SoX executable sox.exe into the directories mentioned in the variable PATH!\n"
                       else do 
                           x <- findExecutable "espeak"
                           y <- findExecutable "sox"
                           if (x /= Nothing) && (y /= Nothing)
                              then do
                                let ts = convertSyllableToLanguage xs in if last xs == 'ь'
                                  then do
                                    return ("espeak -z -v " ++ ys ++ " " ++ zs ++ " -w " ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                    return ("sox " ++ (show t1) ++ "." ++ ts ++ ".wav " ++ (show t1) ++ "." ++ ts ++ ".wav trim 0 0.045") >>= callCommand
                                    addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                  else do
                                    return ("espeak -z -v " ++ ys ++ " " ++ zs ++ " -w " ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                    return ("sox " ++ (show t1) ++ "." ++ ts ++ ".wav " ++ (show t1) ++ "." ++ ts ++ ".wav trim 0 0.045") >>= callCommand                                    
                              else error "Please, install eSpeak executable espeak and SoX executable sox into the directories mentioned in the variable PATH!\n"
    otherwise -> do
                   t <- time
                   let t1 = 10000000000 + (t `div` 10000000) in
                     if (os == "Windows") 
                       then do 
                           x <- findExecutable "espeak.exe"
                           y <- findExecutable "sox.exe"
                           if (x /= Nothing) && (y /= Nothing)
                              then do
                                let ts = convertSyllableToLanguage xs in if last xs == 'ь'
                                  then do
                                    return ("espeak.exe -z -v " ++ ys ++ " " ++ zs ++ " -w " ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"" ) >>= callCommand
                                    addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                  else do
                                    return ("espeak.exe -z -v " ++ ys ++ " " ++ zs ++ " -w " ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"" ) >>= callCommand                             
                              else error "Please, install eSpeak executable espeak.exe and SoX executable sox.exe into the directories mentioned in the variable PATH!\n"
                       else do 
                           x <- findExecutable "espeak"
                           y <- findExecutable "sox"
                           if (x /= Nothing) && (y /= Nothing)
                              then do
                                let ts = convertSyllableToLanguage xs in if last xs == 'ь'
                                  then do
                                    return ("espeak -z -v " ++ ys ++ " " ++ zs ++ " -w " ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"" ) >>= callCommand
                                    addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                  else do
                                    return ("espeak -z -v " ++ ys ++ " " ++ zs ++ " -w " ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"" ) >>= callCommand                             
                              else error "Please, install eSpeak executable espeak and SoX executable sox into the directories mentioned in the variable PATH!\n"

-- Additional function that is used inside the createSoundsForSyllable function to convert Ukrainian syllables into Esperanto or Polish ones
-- Додаткова функція, що використовується всередині createSoundsForSyllable для конвертування українського складу в склад мовою есперанто чи польською
convertSyllableToLanguage ::  String ->  String
convertSyllableToLanguage xs =  if elem 'и' xs || elem 'И' xs then changeToPolish xs else changeToEsperanto xs

-- Function that creates a pause between words using SoX executable
-- Функція, що створює паузи між словами
createWordPause :: IO Integer -> IO ()
createWordPause time = do
  t <- time
  let t1 = 10000000000 + (t `div` 10000000) in
    if (os == "Windows")
      then do 
        y <- findExecutable "sox.exe"
        if y /= Nothing
          then do
            return ("sox.exe --rate 22,1k -n " ++ (show t1) ++ ".wav  trim 0 0.06") >>= callCommand
          else error "Please, install SoX executable sox.exe into the directory mentioned in the variable PATH!\n"
      else do
        y <- findExecutable "sox"
        if y /= Nothing
          then do
            return ("sox --rate 22,1k -n " ++ (show t1) ++ ".wav  trim 0 0.06") >>= callCommand
          else error "Please, install SoX executable sox into the directory mentioned in the variable PATH!\n"

-- Function that converts a String with digits into an Integer
-- Функція, що конвертує String з цифрами в Integer
stringToInteger :: String -> Integer
stringToInteger [] = toInteger 0
stringToInteger xs = foldl1 ((+) . (*10)) $! (map (charToDigit) $ xs)
    where charToDigit x = case x of
            '1' -> toInteger 1
 	    '2' -> toInteger 2
	    '3' -> toInteger 3
	    '4' -> toInteger 4
	    '5' -> toInteger 5
	    '6' -> toInteger 6
	    '7' -> toInteger 7
	    '8' -> toInteger 8
	    '9' -> toInteger 9
            '0' -> toInteger 0
	    _ -> error "Character Is Not a Digit!\n"
  
-- Function that actually converts a Ukrainian word to the Polish string for further reading
-- Функція, що власне перетворює українське слово у Polish рядок для подальшого озвучування
changeToPolish :: String -> String
changeToPolish [] = []
changeToPolish x = concatMap change3 ( replaceWithList [Replace (string'fromString "нь") "ń", Replace (string'fromString "сі") "sji", Replace (string'fromString "Сі") "Sji", Replace (string'fromString "Ці") "Cji", Replace (string'fromString "ці") "cji", Replace (string'fromString "цьцьа") "csja", Replace (string'fromString "рх") "rch", Replace (string'fromString "ьо") "jo", Replace (string'fromString "ьй") "jj", Replace (string'fromString "зі") "zji", Replace (string'fromString "Зі") "Zji"] x)

-- Function that actually converts a Ukrainian word to the Esperanto string for further reading
-- Функція, що власне перетворює українське слово у Esperanto рядок для подальшого озвучування                        
changeToEsperanto :: String -> String
changeToEsperanto [] = []
changeToEsperanto x = concatMap change2 ( replaceWithList [Replace (string'fromString "цьцьа") "csja", Replace (string'fromString "ьо") "jo", Replace (string'fromString "ьй") "jj"] x)

-- Function that converts a list of Ukrainian strings to the list of Language String data with respect to the rules of sounding
-- Функція, що перетворює список українських рядків на список даних типу Language String з врахуванням правил озвучування
polishChecker :: [String] -> [Language String]
polishChecker [] = []
polishChecker (x:xs) = if ((elem 'и' x) || (elem 'И' x)) 
                             then (Polish (changeToPolish x)):(polishChecker xs)
                             else (Esperanto (changeToEsperanto x)):(polishChecker xs)

-- Function to convert char-by-char the rest of the preprocessed Ukrainian word into the Esperanto sounding string
-- Функція для перетворення символ за символом решти попердньо обробленого українського слова у радок Esperanto для озвучування
change2 :: Char -> String
change2 x | x == 'ж' = "ĵ"
          | x == 'Ж' = "ĵ"
          | x == 'ш' = "ŝ"
          | x == 'Ш' = "ŝ"
          | x == 'ч' = "ĉ"
          | x == 'Ч' = "ĉ"
          | x == 'ц' = "c"
          | x == 'Ц' = "C"
          | x == 'ь' = ""
          | x == 'ґ' = "g"
          | x == 'Ґ' = "G"
          | x == 'я' = "ja"
          | x == 'Я' = "Ja"
          | x == 'є' = "je"
          | x == 'Є' = "Je"
          | x == 'ю' = "ju"
          | x == 'Ю' = "Ju"
          | x == 'ї' = "ji"
          | x == 'Ї' = "Ji"
          | x == 'а' = "a"
          | x == 'А' = "A"
          | x == 'о' = "o"
          | x == 'О' = "O"
          | x == 'е' = "e"
          | x == 'Е' = "E"
          | x == 'у' = "u"
          | x == 'У' = "U"
          | x == 'і' = "i"
          | x == 'І' = "I"
          | x == 'й' = "j"
          | x == 'Й' = "J"
          | x == 'к' = "k"
          | x == 'К' = "K"
          | x == 'б' = "b"
          | x == 'Б' = "B"
          | x == 'м' = "m"
          | x == 'М' = "M"
          | x == 'щ' = "ŝĉ"
          | x == 'Щ' = "ŝĉ"
          | x == 'в' = "v"
          | x == 'В' = "V"
          | x == 'з' = "z"
          | x == 'З' = "Z"
          | x == 'с' = "s"
          | x == 'С' = "S"
          | x == 'н' = "n"
          | x == 'Н' = "N"
          | x == 'т' = "t"
          | x == 'Т' = "T"
          | x == 'х' = "ĥ"
          | x == 'Х' = "ĥ"
          | x == 'д' = "d"
          | x == 'Д' = "D"
          | x == 'п' = "p"
          | x == 'П' = "P"
          | x == 'ф' = "f"
          | x == 'Ф' = "F"
          | x == 'г' = "h"
          | x == 'Г' = "H"
          | x == 'ц' = "c"
          | x == 'Ц' = "C"
          | x == 'р' = "r"
          | x == 'Р' = "R"
          | x == 'л' = "l"
          | x == 'Л' = "L"
          | x == '’' = ""
          | x == '-' = ""
          | otherwise = [x]

-- Function to convert char-by-char the rest of the preprocessed Ukrainian word into the Polish sounding string
-- Функція для перетворення символ за символом решти попердньо обробленого українського слова у радок Polish для озвучування
change3 :: Char -> String
change3 x | x == 'ж' = "ż"
          | x == 'Ж' = "ż"
          | x == 'ш' = "sz"
          | x == 'Ш' = "Sz"
          | x == 'ч' = "cz"
          | x == 'Ч' = "Cz"
          | x == 'ц' = "c"
          | x == 'Ц' = "C"
          | x == 'ь' = ""
          | x == 'ґ' = "g"
          | x == 'Ґ' = "G"
          | x == 'я' = "ja"
          | x == 'Я' = "Ja"
          | x == 'є' = "je"
          | x == 'Є' = "Je"
          | x == 'ю' = "ju"
          | x == 'Ю' = "Ju"
          | x == 'ї' = "ji"
          | x == 'Ї' = "Ji"
          | x == 'а' = "a"
          | x == 'А' = "A"
          | x == 'о' = "o"
          | x == 'О' = "O"
          | x == 'е' = "e"
          | x == 'Е' = "E"
          | x == 'у' = "u"
          | x == 'У' = "U"
          | x == 'і' = "i"
          | x == 'І' = "I"
          | x == 'й' = "j"
          | x == 'Й' = "J"
          | x == 'к' = "k"
          | x == 'К' = "K"
          | x == 'б' = "b"
          | x == 'Б' = "B"
          | x == 'м' = "m"
          | x == 'М' = "M"
          | x == 'щ' = "szcz"
          | x == 'Щ' = "Szcz"
          | x == 'в' = "w"
          | x == 'В' = "W"
          | x == 'з' = "z"
          | x == 'З' = "Z"
          | x == 'с' = "s"
          | x == 'С' = "S"
          | x == 'н' = "n"
          | x == 'Н' = "N"
          | x == 'т' = "t"
          | x == 'Т' = "T"
          | x == 'х' = "cĥ"
          | x == 'Х' = "Cĥ"
          | x == 'д' = "d"
          | x == 'Д' = "D"
          | x == 'п' = "p"
          | x == 'П' = "P"
          | x == 'ф' = "f"
          | x == 'Ф' = "F"
          | x == 'г' = "h"
          | x == 'Г' = "H"
          | x == 'р' = "r"
          | x == 'Р' = "R"
          | x == 'л' = "l"
          | x == 'Л' = "L"
          | x == '’' = ""
          | x == '-' = ""
          | x == 'и' = "y"
          | x == 'И' = "Y"
          | otherwise = [x]

-- Function that checks the eSpeak and SoX executables existence and is used for soft sign sound creation
-- Функція, що перевіряє існування eSpeak і SoX додатків у системі та використовується для створення звуку для м'якого знаку
createSoftSign :: IO ()
createSoftSign = do
                   if (os == "Windows") 
                      then do 
                           x <- findExecutable "espeak.exe"
                           y <- findExecutable "sox.exe"
                           if (x /= Nothing) && (y /= Nothing)
                              then do
                                   return "espeak.exe -v esperanto -g 0 -w ь.wav -z \"j\"" >>= callCommand
                                   return "sox.exe ь.wav j.wav trim 0.02 0.037" >>= callCommand
                              else error "Please, install eSpeak executable espeak.exe and SoX executable sox.exe into the directories mentioned in the variable PATH!\n"
                      else do 
                           x <- findExecutable "espeak"
                           y <- findExecutable "sox"
                           if (x /= Nothing) && (y /= Nothing)
                              then do
                                   return "espeak -v esperanto -g 0 -w ь.wav -z \"j\"" >>= callCommand
                                   return "sox ь.wav j.wav trim 0.02 0.037" >>= callCommand
                              else error "Please, install eSpeak executable espeak and SoX executable sox into the directories mentioned in the variable PATH!\n"

-- Function that checks the eSpeak and SoX executables existence and is used for soft sign sound appending to the syllable or word
-- Функція, що перевіряє існування eSpeak і SoX додатків у системі та використовується для додавання м'якого знаку до кінцевого приголосного у слові чи складі
addSoftSign :: FilePath -> IO ()
addSoftSign file = do
                     if (os == "Windows") 
                         then do 
                           x <- findExecutable "espeak.exe"
                           y <- findExecutable "sox.exe"
                           if (x /= Nothing) && (y /= Nothing)
                              then do
                                   return ("sox.exe " ++ file ++ " m." ++  file ++ " trim 0 -0.02") >>= callCommand
                                   return ("sox.exe m." ++ file ++ " j.wav " ++ file) >>= callCommand
                              else error "Please, install SoX executable sox.exe into the directory mentioned in the variable PATH!\n"
                         else do 
                           x <- findExecutable "espeak"
                           y <- findExecutable "sox"
                           if (x /= Nothing) && (y /= Nothing)
                              then do
                                   return ("sox " ++ file ++ " m." ++ file ++ " trim 0 -0.02") >>= callCommand
                                   return ("sox m." ++ file ++ " j.wav " ++ file) >>= callCommand
                              else error "Please, install SoX executable sox into the directory mentioned in the variable PATH!\n"

-- Main program
-- Головна програма
main = do 
   putStrLn "Введіть рядок українського тексту. За замовчуванням для багатоскладових слів наголос падатиме на передостанній склад у слові."
   putStrLn "Якщо Ви бажаєте змінити наголос, тоді перед словом злитно з ним напишіть натуральне число, яке є порядковим номером складу,"
   putStrLn "на який падає наголос, починаючи з першого складу. Наприклад, \"3мальовнИчого\" означатиме, що наголошеним буде склад з \"И\"."
   putStrLn "Не ставте дефісів або інших розділювачів (у т. ч. пробілів). Не хвилюйтеся, ці числа НЕ будуть озвучені програмою.\n"
   createSoftSign
   nI2 <- getContents
   mapM_ (mapM_ (createSoundsForSyllable getCPUTime)) (map (createSyllablesReady . accountEmphasis) $ words2 nI2)
