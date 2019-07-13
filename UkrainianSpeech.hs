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
import System.Environment (getArgs)

-- Function that primarily converts Ukrainian line into more sounds-based line
-- Функція, що початково перетворює український рядок на більш орінтований на звуки рядок
ukrainianToMoreSounding :: String -> String
ukrainianToMoreSounding xs = replaceWithList [Replace (string'fromString "ться") "цьцьа", Replace (string'fromString " --") " ", 
  Replace (string'fromString "-- ") " ", Replace (string'fromString " -") " ",  Replace (string'fromString "- ") " ", 
    Replace (string'fromString "ья") "ьйа", Replace (string'fromString "ьє") "ьйе", Replace (string'fromString "ью") "ьйу", 
      Replace (string'fromString "ьї") "ьйі", Replace (string'fromString "’я") "ййа", Replace (string'fromString "’є") "ййе", 
        Replace (string'fromString "’ю") "ййу", Replace (string'fromString "’ї") "ййі",  Replace (string'fromString "щ") "шч"] xs

-- Function to convert Ukrainian "я", "ю", "є" and "ї" into some other String for syllables processing
-- Функція для перетворення українських "я", "ю", "є" та "ї" на деякі інші рядки для обробки складів
ukrainianJotted2 :: String -> String
ukrainianJotted2 xs = replaceWithList [Replace (string'fromString "Я") "Йа", Replace (string'fromString "Ю") "Йу", 
  Replace (string'fromString "Є") "Йе", Replace (string'fromString "Ї") "Йі", Replace (string'fromString "ї") "йі"] xs

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
ukrainianJottedLast xs = replaceWithList [Replace (string'fromString "я") "ьа", Replace (string'fromString "ю") "ьу", 
  Replace (string'fromString "є") "ье", Replace (string'fromString "ї") "йі"] xs

-- Function that makes some assimilation changes for correct Ukrainian pronunciation
-- Функція, що робить деякі асиміляційні зміни для правильної української вимови
changeAssimilative :: String -> String
changeAssimilative [] = []
changeAssimilative xs = replaceWithList [Replace (string'fromString "нтськ") "ньськ", Replace (string'fromString "стськ") "сьськ", 
  Replace (string'fromString "нтст") "нст", Replace (string'fromString "стць") "сьцьць", Replace (string'fromString "стч") "шч", 
    Replace (string'fromString "стд") "зд", Replace (string'fromString "стс") "сс", Replace (string'fromString "ятдесят") "йадесьат"] xs

-- Additional function to take into account assimilation rules that depend on the position in the word of the group of Ukrainian sounds
-- Додаткова функція, щоб врахувати правила асиміляції, які залежать від положення у слові групи українських звуків
assimilationFirst :: [String] -> [String]
assimilationFirst [] = []
assimilationFirst xs = map (\x -> let z = dropWhile isDigitOrDash x in if take 2 z == "зч" || take 2 z == "Зч"
                                           then takeWhile isDigitOrDash x ++ "шч" ++ drop 2 z
                                           else if take 2 z == "зш" || take 2 z == "Зш"
                                                  then takeWhile isDigitOrDash x ++ "шш" ++ drop 2 x
                                                  else x) xs

-- Function for special Ukrainian words where "г" sounds approximately as "х"
-- Функція для спеціальних українських слів, де звук "г" звучить близько до "х"
changeH2X :: String -> String
changeH2X [] = []
changeH2X xs = replaceWithList [Replace (string'fromString "вогк") "вохк", Replace (string'fromString "легк") "лехк", 
  Replace (string'fromString "кігт") "кіхт", Replace (string'fromString "нігт") "ніхт", Replace (string'fromString "дігтяр") "діхтяр", 
    Replace (string'fromString "Вогк") "вохк", Replace (string'fromString "Легк") "лехк", Replace (string'fromString "Кігт") "кіхт", 
      Replace (string'fromString "Нігт") "ніхт", Replace (string'fromString "Дігтяр") "діхтяр"] xs

-- Function that produces the list of Ukrainian strings from the primary Ukrainian string which can be further easily processed
-- Функція, яка створює список українських рядків з початкового українського рядка, які можуть бути легко оброблені далі
words2 :: String -> [String]
words2 [] = []
words2 xs = assimilationFirst . words . ukrainianJottedLast . ukrainianJotted2 . ukrainianJotted1 .  changeAssimilative . ukrainianToMoreSounding . changeH2X $ xs
--  concatUkrainian . concatUkrainianZhOrB .

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
createSoundGroups [] = []
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
  then ([x], 'w'):createSoundL (y:z:xs)
  else if y == 'ь'
    then if elem x "вмнлрВМНЛР"
           then ([x,y],'q'):createSoundL (z:xs)
           else if ((x == 'й') || (x == 'Й'))
                  then ([x],'r'):createSoundL (y:z:xs)
                  else if elem x "бдзжгґБДЗЖГҐ"
                         then ([x,y],'i'):createSoundL (z:xs)
                         else ([x,y],'a'):createSoundL (z:xs)
    else if isVowelL y
           then if elem x "вмнлрйВМНЛРЙ"
                  then ([x],'r'):([y],'w'):createSoundL (z:xs)
                  else if elem x "бзжгґБЗЖГҐ"
                         then ([x],'d'):([y],'w'):createSoundL (z:xs)
                         else ([x],'s'):([y],'w'):createSoundL (z:xs)
           else if ((x == 'д') || (x == 'Д'))
                  then if ((y == 'з') || (y == 'ж'))
                         then if z == 'ь'
                                then ([x,y,z], 'i'):createSoundL xs
                                else ([x,y], 'd'):createSoundL (z:xs)
                         else ([x], 'd'):createSoundL (y:z:xs)
                  else if elem x "вмнлрйВМНЛРЙ"
                          then ([x], 'r'):createSoundL (y:z:xs)
                          else if elem x "бзжгґБЗЖГҐ"
                                  then ([x], 'd'):createSoundL (y:z:xs)
                                  else ([x], 's'):createSoundL (y:z:xs)
createSoundL [x,y] = if isVowelL x
  then ([x], 'w'):createSoundL [y]
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
                         else ([x], 'd'):createSoundL [y]
                  else if elem x "вмнлрйВМНЛРЙ"
                          then ([x], 'r'):createSoundL [y]
                          else if elem x "бзжгґБЗЖГҐ"
                                  then ([x], 'd'):createSoundL [y]
                                  else ([x], 's'):createSoundL [y]
createSoundL [x] = if isVowelL x
  then [([x], 'w')]
  else if elem x "вмнлрйВМНЛРЙ"
    then [([x], 'r')]
    else if elem x "бдзжгґБДЗЖГҐ"
           then [([x],'d')]
           else [([x],'s')]

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
zeroSyllablePart :: String ->  [((String, String), (String, Integer))]
zeroSyllablePart xs = map (\x -> if x == "γ" 
  then (("γ", "greek"), ("-z", 0)) 
  else if x == "y" 
    then (("y", "polish"), ("-z", 0)) 
    else ((x, "esperanto"), ("-z", 0))) $ convertSyllableToLanguage xs

-- Function to create a list of Int that is used for dividing into syllables for more than one syllable words
-- Функція, щоб створити список Int, який використовується для поділу на склади для більш, ніж односкладових слів
mapLS :: String -> [Int] -> [Int]
mapLS xs ks = let zss = divideToSyllables1 xs in map (\x -> length . filter (== 'w') . map snd $ concat $ take x zss) ks

-- Additional function to find out the amount of parts to be taken for the k-th syllable
-- Додаткова функція, щоб визначити, яку кількість частин потрібно взяти для k-го складу
rr :: String -> Int -> Int
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

-- Additional function for dividing into units for further processing
-- Додаткова функція для поділу на одиниці для подальшої обробки
divideToUnits :: String -> [[String]]
divideToUnits [] = []
divideToUnits xs = map convertSyllableToLanguage $ createSyllablesMulti xs

-- Function that creates data of the type [((String, String),(String,Integer))] for non-zero-syllable words
-- Функція, що створює дані типу [((String, String),(String,Integer))] для слів з голосними
createSyllablesMultiLast2 :: [[String]] -> (Maybe Integer, Integer) -> [((String, String),(String,Integer))]
createSyllablesMultiLast2 [] _ = []
createSyllablesMultiLast2 xss (Just y, z) | z == 1 = case y of
  1 -> concatMap (map (\x -> if x == "γ" then (("γ", "greek"), ("-a 110 -z", 1)) else if x == "y" then (("y", "polish"), ("-a 110 -z", 0)) else ((x, "esperanto"), ("-a 110 -z", 1)))) $ xss
  0 -> concatMap (map (\x -> if x == "γ" then (("γ", "greek"), ("-z", 1)) else if x == "y" then (("y", "polish"), ("-z", 0)) else ((x, "esperanto"), ("-z", 1)))) $ xss
  _ -> concatMap (map (\x -> if x == "γ" then (("γ", "greek"), ("-a 110 -z", 1)) else if x == "y" then (("y", "polish"), ("-a 110 -z", 0)) else ((x, "esperanto"), ("-a 110 -z", 1)))) $ xss
                                          | z > 1 = if y < z
                                              then concat [concatMap (map (\x -> if x == "γ" 
                                                then (("γ", "greek"), ("-z", z)) 
                                                else if x == "y" 
                                                  then (("y", "polish"), ("-z", 0)) 
                                                  else ((x, "esperanto"), ("-z", z)))) $ take (fromInteger (y - 1)) xss, concatMap (map (\x -> if x == "γ" 
                                                    then (("γ", "greek"), ("-a 110 -z", z)) 
                                                    else if x == "y" 
                                                      then (("y", "polish"), ("-a 110 -z", 0)) 
                                                      else ((x, "esperanto"), ("-a 110 -z", z)))) $ [head $ drop (fromInteger (y - 1)) xss], concatMap (map (\x -> if x == "γ" 
                                                        then (("γ", "greek"), ("-z", z)) 
                                                        else if x == "y" 
                                                          then (("y", "polish"), ("-z", 0)) 
                                                          else ((x, "esperanto"), ("-z", z)))) $ drop (fromInteger y) xss]
                                              else concat [concatMap (map (\x -> if x == "γ" 
                                                then (("γ", "greek"), ("-z", z)) 
                                                else if x == "y" 
                                                  then (("y", "polish"), ("-z", 0)) 
                                                  else ((x, "esperanto"), ("-z", z)))) $ take (fromInteger (y - 1)) xss, concatMap (map (\x -> if x == "γ" 
                                                    then (("γ", "greek"), ("-a 110 -z", z)) 
                                                    else if x == "y" 
                                                      then (("y", "polish"), ("-a 110 -z", 0)) 
                                                      else ((x, "esperanto"), ("-a 110 -z", z)))) $ [last xss]]
createSyllablesMultiLast2 xss (Nothing, z) = createSyllablesMultiLast2 xss (Just (z - 1), z)

-- Function that creates Ukrainian syllables and groups them with some parameters to be then processed by the eSpeak and SoX executables
-- Функція, що створює українські склади та групує їх з деякими параметрами, щоб потім вони були оброблені програмами eSpeak і SoX
createSyllablesReady :: (String, (Maybe Integer, Integer)) -> [((String, String),(String,Integer))]
createSyllablesReady x = case snd . snd $ x of
  0 -> zeroSyllablePart . fst $ x
  otherwise -> createSyllablesMultiLast2 (divideToUnits $ fst x) (fst . snd $ x, snd . snd $ x)

-- Function that combines the emphasis and dividing into sound groups into languages
-- Функція, що поєднує наголос і поділ на групи звуків для мов
combineSoundsLs :: String -> [((String, String), (String, Integer))]
combineSoundsLs [] = []
combineSoundsLs xs = createSyllablesReady . accountEmphasis $ xs

-- Function that is used to make pauses between words
-- Функція, що використовується для того, щоб були паузи між словами
createPausesW :: [((String, String), (String, Integer))] -> [((String, String), (String, Integer))]
createPausesW [] = []
createPausesW [x] = [(fst x, (take ((length . fst . snd $ x) - 3) (fst . snd $ x), snd . snd $ x))]
createPausesW xs = let (y, ys) = (last xs, init xs) in concat [ys, [(fst y, (take ((length . fst . snd $ y) - 3) (fst . snd $ y), snd . snd $ y))]]

-- Function that applies additional function h to a if p is True on a
-- Функція, що застосовує додаткову функцію h до a, якщо p є Істина на a
bGroups :: (a -> Bool) -> (a -> [a]) -> [a] -> [a]
bGroups p h xs = concatMap (\x -> if p x then h x else [x]) xs

-- Function-predicate to check whether its argument is a Vowel sound letter in Esperanto or Greek respesentation of the group of sounds
-- Функція-предикат, щоб визначити, чи її аргумент є літерою, що позначає голосний у представленні грецькою мовою чи есперанто
isVowelEG :: Char -> Bool
isVowelEG x = case x of
  'a' -> True
  'A' -> True
  'o' -> True
  'O' -> True
  'e' -> True
  'E' -> True
  'u' -> True
  'U' -> True
  'i' -> True
  'I' -> True
  _   -> False

-- Function-predicate that checks whether the hFunctionH must be applied to the [((String, String), (String, Integer))]
-- Функція-предикат, яка перевіряє, чи має бути застосована hFunctionH до [((String, String), (String, Integer))]
pFunctionP :: ((String, String), (String, Integer)) -> Bool
pFunctionP ((xs, ys), (zs, k)) = null $ filter isVowelEG xs

-- Function that converts zero-syllable groups of sounds into separate sounds for further processing
-- Функція, що перетворює безголосні групи приголосних у окремі звуки для подальшої обробки
hFunctionH :: ((String, String), (String, Integer)) -> [((String, String), (String, Integer))]
hFunctionH  (([], ys), (zs, k)) = []
hFunctionH (([x], ys), (zs, k)) = [(([x], ys), (zs, 0))]
hFunctionH (([x, y], ys), (zs, k)) | x == 'd' || x == 'D' = case y of
                                       'z' -> [(("dz", ys), (zs, 0))]
                                       'ĵ' -> [(("dĵ", ys), (zs, 0))]
                                       otherwise   -> [(("d", ys), (zs, 0)), (([y], ys), (zs, 0))]
                                   | otherwise = [(([x], ys), (zs, 0)), (([y], ys), (zs, 0))]
hFunctionH (((u:t:ts), ys), (zs, k)) | u == 'd' || u == 'D' = case t of
                                          'z' -> (("dz", ys), (zs, 0)):hFunctionH ((ts, ys), (zs, 0))
                                          'ĵ' -> (("dĵ", ys), (zs, 0)):hFunctionH ((ts, ys), (zs, 0))
                                          otherwise   -> (("d", ys), (zs, 0)):hFunctionH  (((t:ts), ys), (zs, 0))
                                     | otherwise = (([u], ys), (zs, 0)):hFunctionH (((t:ts), ys), (zs, 0))

-- Function that prepares a String for processing by the eSpeak and SoX for non-zero-syllable words
-- Функція, яка готує слово з голосним для подальшої обробки  eSpeak та SoX
combineSoundsLs3 :: String -> [((String, String), (String, Integer))]
combineSoundsLs3 xs = concatSoftSign $ createPausesW $ bGroups pFunctionP hFunctionH $ combineSoundsLs xs

-- Function that concatenates alone soft sign with the previous letter (Esperanto or Greek)
-- Функція, яка з'єднує ізольований м'який знак з попереднім приголосним (есперанто чи грецькою)
concatSoftSign ::  [((String, String), (String, Integer))] ->  [((String, String), (String, Integer))]
concatSoftSign [] = []
concatSoftSign [x] = [x]
concatSoftSign (x:y:xs) = case fst . fst $ y of
  "q" -> if (fst . fst $ x) == "γ"
           then x:concatSoftSign xs
           else (((fst . fst $ x) ++ (fst . fst $ y), "esperanto" ), snd x):concatSoftSign xs
  otherwise -> x:concatSoftSign (y:xs)
              
-- Function that is used to create String that is a parameter effect to get a needed duration of the sound
-- Функція, що використовується для створення String, що є параметричним ефектом, щоб отримати звук потрібної тривалості
stringDurationLim :: String -> String
stringDurationLim [] = []
stringDurationLim xs = case xs of
  "A" -> " trim -0.265"
  "a" -> " trim -0.265"
  "eb" -> " trim -0.070"
  "ec" -> " trim -0.115"
  "eĉ" -> " trim -0.114"
  "edĵ" -> " trim -0.166"
  "ed" -> " trim -0.070"
  "edz" -> " trim -0.178"
  "ef" -> " trim -0.112"
  "eg" -> " trim -0.081"
  "eh" -> " trim -0.083"
  "eĥ" -> " trim -0.118"
  "eĵ" -> " trim -0.132"
  "ej" -> " trim -0.133"
  "ek" -> " trim -0.071"
  "el" -> " trim -0.105"
  "em" -> " trim -0.146"
  "en" -> " trim -0.136"
  "ep" -> " trim -0.103"
  "er" -> " trim -0.099"
  "eŝĉ" -> " trim -0.198"
  "es" -> " trim -0.121"
  "eŝ" -> " trim -0.121"
  "E" -> " trim -0.254"
  "e" -> " trim -0.254"
  "et" -> " trim -0.071"
  "ev" -> " trim -0.134"
  "ez" -> " trim -0.157"
  "I" -> " trim -0.254"
  "i" -> " trim -0.254"
  "ja" -> " trim -0.350"
  "je" -> " trim -0.348"
  "ji" -> " trim -0.329"
  "ju" -> " trim -0.315"
  "O" -> " trim -0.241"
  "o" -> " trim -0.241"
  "yy" -> ""
  "U" -> " trim -0.241"
  "u" -> " trim -0.241"
  "αγ" -> " trim -0.117"
  otherwise -> []

-- Function that is used to convert single letters to a respective syllables for sounding
-- Функція, що використовується, щоб перетворити окремі літери на відповідні склади для озвучування
oneToSyllable :: String -> String
oneToSyllable [] = []
oneToSyllable xs = if head xs == 'd' || head xs == 'D'
  then if null . tail $ xs
         then "ed"
         else case head . tail $ xs of
                'z' -> "edz"
                'ĵ' -> "edĵ"
  else case head xs of
    'ĵ' -> "eĵ"
    'ŝ' -> "eŝ"
    'ĉ' -> "eĉ"
    'c' -> "ec"
    'C' -> "ec"    
    'e' -> "e"
    'g' -> "eg"
    'G' -> "eg"    
    'a' -> "a"
    'A' -> "A"
    'o' -> "o"
    'O' -> "O"
    'E' -> "E"
    'u' -> "u"
    'U' -> "U"
    'i' -> "i"
    'I' -> "I"
    'j' -> "ej"
    'J' -> "ej"    
    'k' -> "ek"
    'K' -> "ek"    
    'b' -> "eb"
    'B' -> "eb"    
    'm' -> "em"
    'M' -> "em"    
    'v' -> "ev"
    'V' -> "ev"    
    'z' -> "ez"
    'Z' -> "ez"    
    's' -> "es"
    'S' -> "es"    
    'n' -> "en"
    'N' -> "en"    
    't' -> "et"
    'T' -> "et"    
    'ĥ' -> "eĥ"
    'p' -> "ep"
    'P' -> "ep"    
    'f' -> "ef"
    'F' -> "ef"    
    'h' -> "eh"
    'H' -> "eh"    
    'r' -> "er"
    'R' -> "er"    
    'l' -> "el"
    'L' -> "el"    
    'γ' -> "αγ"
    'y' -> "yy"
    'Y' -> "yy"
    otherwise -> [head xs]

-- Function that for the Ukrainian syllable represented as ((String, String),(String,Integer)) creates sounds
-- Функція, що для українського складу представленого як ((String, String),(String,Integer)) створює звуки
createSoundsForSyllable :: IO Integer -> [String] -> ((String, String),(String,Integer)) -> IO ()
createSoundsForSyllable time args ((xs, ys),(zs, k)) = case k of
    0 -> do
                   t <- time
                   let t1 = 10000000000 + (t `div` 10000000) in
                     if (os == "Windows") 
                       then do 
                           if null args
                                  then let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in if last xs == 'q'
                                         then do
                                           return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++ "m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                           return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us) >>= callCommand
                                           addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                         else do
                                           if xs == "y"
                                             then do
                                               return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                               return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                             else do
                                               return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                               return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us) >>= callCommand
                                  else case head args of
                                         "1" -> let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in if last xs == 'q'
                                                  then do
                                                    return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++ "m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                    return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                                    addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                                  else do
                                                    if xs == "y"
                                                      then do
                                                        return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                        return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                                      else do
                                                        return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                        return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                         "2" -> let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in if last xs == 'q'
                                                  then do
                                                    return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++ "m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                    return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us ++ " tempo -s 0.8") >>= callCommand
                                                    addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                                  else do
                                                    if xs == "y"
                                                      then do
                                                        return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                        return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                                      else do
                                                        return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                        return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.8") >>= callCommand
                                         "3" -> let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in if last xs == 'q'
                                                  then do
                                                    return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++ "m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                    return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us ++ " tempo -s 0.9") >>= callCommand
                                                    addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                                  else do
                                                    if xs == "y"
                                                      then do
                                                        return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                        return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                                      else do
                                                        return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                        return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.9") >>= callCommand
                                         otherwise -> let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in if last xs == 'q'
                                                        then do
                                                          return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++ "m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                          return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                                          addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                                        else do
                                                          if xs == "y"
                                                            then do
                                                              return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                              return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                                            else do
                                                              return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                              return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                       else do 
                         if null args
                                  then let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in if last xs == 'q'
                                         then do
                                           return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++ "m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                           return ("sox " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us) >>= callCommand
                                           addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                         else do
                                           if xs == "y"
                                             then do
                                               return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                               return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                             else do
                                               return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                               return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us) >>= callCommand
                                  else case head args of
                                         "1" -> let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in if last xs == 'q'
                                                  then do
                                                    return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++ "m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                    return ("sox " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                                    addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                                  else do
                                                    if xs == "y"
                                                      then do
                                                        return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                        return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                                      else do
                                                        return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                        return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                         "2" -> let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in if last xs == 'q'
                                                  then do
                                                    return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++ "m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                    return ("sox " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us ++ " tempo -s 0.8") >>= callCommand
                                                    addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                                  else do
                                                    if xs == "y"
                                                      then do
                                                        return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                        return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                                      else do
                                                        return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                        return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.8") >>= callCommand
                                         "3" -> let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in if last xs == 'q'
                                                  then do
                                                    return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++ "m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                    return ("sox " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us ++ " tempo -s 0.9") >>= callCommand
                                                    addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                                  else do
                                                    if xs == "y"
                                                      then do
                                                        return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                        return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                                      else do
                                                        return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                        return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.9") >>= callCommand
                                         otherwise -> let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in if last xs == 'q'
                                                        then do
                                                          return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++ "m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                          return ("sox " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                                          addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                                        else do
                                                          if xs == "y"
                                                            then do
                                                              return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                              return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
                                                            else do
                                                              return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                              return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s 0.7") >>= callCommand
    otherwise -> do
                   t <- time
                   let t1 = 10000000000 + (t `div` 10000000) in
                     if (os == "Windows") 
                       then do 
                         if last xs == 'q'
                                  then do
                                    return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++ (show t1) ++ "." ++ (filter (/= 'q') xs) ++ ".wav \"" ++ (filter (/= 'q') xs) ++ "\"" ) >>= callCommand
                                    addSoftSign $ (show t1) ++ "." ++ (filter (/= 'q') xs) ++ ".wav"
                                  else do
                                    return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w " ++ (show t1) ++ "." ++ (filter (/= 'q') xs) ++ ".wav \"" ++ (filter (/= 'q') xs) ++ "\"" ) >>= callCommand
                       else do 
                         if last xs == 'q'
                                  then do
                                    return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++ (show t1) ++ "." ++ (filter (/= 'q') xs) ++ ".wav \"" ++ (filter (/= 'q') xs) ++ "\"" ) >>= callCommand
                                    addSoftSign $ (show t1) ++ "." ++ (filter (/= 'q') xs) ++ ".wav"
                                  else do
                                    return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w " ++ (show t1) ++ "." ++ (filter (/= 'q') xs) ++ ".wav \"" ++ (filter (/= 'q') xs) ++ "\"" ) >>= callCommand
                       
-- Additional function-predicate to check if its argument is a sound that converts to another language than the default Esperanto one
-- Додаткова функція-предикат, щоб перевірити, чи її аргумент є звуком, що конверується до іншої мови, ніж звичайна есперанто
continueLang :: Char -> Bool
continueLang x = case x of
  'и' -> False
  'И' -> False
  'г' -> False
  'Г' -> False
  _  -> True

-- Additional function that is used inside the createSoundsForSyllable function to convert Ukrainian syllables into Esperanto, or Polish, or Greek ones
-- Додаткова функція, що використовується всередині createSoundsForSyllable для конвертування українського складу в склад мовою есперанто, польською чи грецькою
convertSyllableToLanguage ::  String ->  [String]
convertSyllableToLanguage [] = []
convertSyllableToLanguage xs =  if elem 'и' xs || elem 'И' xs || elem 'г' xs || elem 'Г' xs
         then let z = head $ dropWhile continueLang xs in
                concat . filter (not . null) . map (\x -> [takeWithFirst (/= 'q') x, dropWithFirst (/= 'q') x]) $
                  (changeToEsperanto $ takeWhile continueLang xs):(if z == 'и' || z == 'И'
                                                                      then "y"
                                                                      else "γ"):convertSyllableToLanguage (dropWithFirst continueLang xs)
         else concat . filter (not . null) . map (\x -> [takeWithFirst (/= 'q') x, dropWithFirst (/= 'q') x]) $ [changeToEsperanto xs]

-- Optimized function to take elements of the list till the first occasion of the wrong predicate p including the first occurance
-- Оптимізована функція, щоб узяти елементи списку до першої появи хибності у предикаті, включаючи саму цю першу появу
takeWithFirst :: (a -> Bool) -> ([a] -> [a])
takeWithFirst p = fst . foldr f v
  where
    f x (ys,xs) = (if p x then x:ys else [x],x:xs)
    v = ([],[])

-- Optimized function to take elements of the list after the first occasion of the wrong predicate p excluding the first occurance
-- Оптимізована функція, щоб узяти елементи списку після першої появи хибності у предикаті, виключаючи саму цю першу появу
dropWithFirst :: (a -> Bool) -> ([a] -> [a])
dropWithFirst p = fst . foldr f v
  where
    f x (ys,xs) = (if p x then ys else xs,x:xs)
    v = ([],[])

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
  
-- Function that actually converts a Ukrainian word to the Esperanto string for further reading
-- Функція, що власне перетворює українське слово у Esperanto рядок для подальшого озвучування                        
changeToEsperanto :: String -> String
changeToEsperanto [] = []
changeToEsperanto x = concatMap change2 ( replaceWithList [Replace (string'fromString "цьцьа") "csja", Replace (string'fromString "ьо") "jo", 
  Replace (string'fromString "ьй") "jj", Replace (string'fromString "ьа") "ja", Replace (string'fromString "ьу") "ju", Replace (string'fromString "ье") "je"] x)

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
          | x == 'ь' = "q"
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
                                   return ("sox.exe " ++ file ++ " m." ++  file ++ " trim 0 -0.01") >>= callCommand
                                   return ("sox.exe m." ++ file ++ " j.wav " ++ file) >>= callCommand
                         else do 
                                   return ("sox " ++ file ++ " m." ++ file ++ " trim 0 -0.01") >>= callCommand
                                   return ("sox m." ++ file ++ " j.wav " ++ file) >>= callCommand
                           
-- Main program
-- Головна програма
main = do 
   putStrLn "Введіть рядок українського тексту. За замовчуванням для багатоскладових слів наголос падатиме на передостанній склад у слові."
   putStrLn "Якщо Ви бажаєте змінити наголос, тоді перед словом злитно з ним напишіть натуральне число, яке є порядковим номером складу,"
   putStrLn "на який падає наголос, починаючи з першого складу. Наприклад, \"3мальовнИчого\" означатиме, що наголошеним буде склад з \"И\"."
   putStrLn "Не ставте дефісів або інших розділювачів (у т. ч. пробілів). Не хвилюйтеся, ці числа НЕ будуть озвучені програмою.\n"
   createSoftSign
   args <- getArgs
   nI2 <- getContents
   mapM_ (mapM_ (createSoundsForSyllable getCPUTime args)) (map combineSoundsLs3 $ words2 nI2)
