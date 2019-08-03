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
import qualified Data.Char as DC (toLower, isDigit, isAlpha, isPunctuation, isSpace) 
import Data.Maybe (Maybe(Just,Nothing))
import Prelude (Double,String,Char,Bool(True,False),Int,Integer,IO,FilePath,($),($!),(.),(==),(/=),(<),(<=),(>),(>=),(&&),(||),not,null,
  fst,snd,toInteger,show,return,(>>=),error,putStr,putStrLn,(+),(*),(-),div,mod,(++),foldr,map,zip,zipWith,take,drop,takeWhile,dropWhile,concat,concatMap,
    mapM_,foldl1,filter,getContents,elem,last,head,tail,length,fromInteger,init,otherwise,and,or,span,sum)

-- Function that primarily converts Ukrainian line into more sounds-based line and more oriented to more using prosodical information
-- Функція, що початково перетворює український рядок на більш орінтований на звуки рядок і більш орієнтований на використання просодійної інформації
ukrainianToMoreSounding :: String -> String
ukrainianToMoreSounding xs = replaceWithList [Replace (string'fromString "ться") "цьцьа", Replace (string'fromString " --") " ", 
  Replace (string'fromString "-- ") " ", Replace (string'fromString " -") " ",  Replace (string'fromString "- ") " ", 
    Replace (string'fromString "ья") "ьйа", Replace (string'fromString "ьє") "ьйе", Replace (string'fromString "ью") "ьйу", 
      Replace (string'fromString "ьї") "ьйі", Replace (string'fromString "’я") "ййа", Replace (string'fromString "’є") "ййе", 
        Replace (string'fromString "’ю") "ййу", Replace (string'fromString "’ї") "ййі",  Replace (string'fromString "щ") "шч"] xs
              
-- Function that separates punctuation marks from the words
-- Функція, яка відділяє пунктуаційні знаки від слів
separatePunct0 :: String -> String                       
separatePunct0 = foldr f v
  where v = []
        f x ys = if DC.isPunctuation x 
                          then ' ':x:' ':ys
                          else x:ys

-- Optimized function to convert Ukrainian "я", "ю", "є" and "ї" into some other String for syllables processing
-- Оптимізована функція для перетворення українських "я", "ю", "є" та "ї" на деякі інші рядки для обробки складів
ukrainianJotted1 :: String -> String
ukrainianJotted1 = fst . foldr f v
  where v = ([], [])
        f x (ys,xs) = if null xs 
                         then ([x],x:xs)
                         else if k == 'ї' 
                           then (x:'й':'і':tail ys,x:xs)
                           else if elem k "яює" 
                             then if elem x "аоуеиі- яюєї"
                               then (x:'й':jC k:tail ys,x:xs)
                               else (x:'ь':jC k:tail ys,x:xs)
                             else (x:ys,x:xs)
                           where k = head xs
                                 jC y | y == 'я' = 'а'
                                      | y == 'ю' = 'у'
                                      | y == 'є' = 'е'
                                      | otherwise = y

-- Function to convert Ukrainian "я", "ю", "є" and "ї" into some other String for syllables processing
-- Функція для перетворення українських "я", "ю", "є" та "ї" на деякі інші рядки для обробки складів
ukrainianJottedLast :: String -> String
ukrainianJottedLast xs = replaceWithList [Replace (string'fromString "я") "ьа", Replace (string'fromString "ю") "ьу", 
  Replace (string'fromString "є") "ье", Replace (string'fromString "ї") "йі", Replace (string'fromString "тс") "ц"] xs

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
assimilationFirst xs = map (\x -> let z = dropWhile isDigitOrDash x in if take 2 z == "зч"
                                           then takeWhile isDigitOrDash x ++ "шч" ++ drop 2 z
                                           else if take 2 z == "зш" 
                                                  then takeWhile isDigitOrDash x ++ "шш" ++ drop 2 x
                                                  else x) xs
                                                  
-- Function for special Ukrainian words where "г" sounds approximately as "х"
-- Функція для спеціальних українських слів, де звук "г" звучить близько до "х"
changeH2X :: String -> String
changeH2X [] = []
changeH2X xs = replaceWithList [Replace (string'fromString "вогк") "вохк", Replace (string'fromString "легк") "лехк", 
  Replace (string'fromString "кігт") "кіхт", Replace (string'fromString "нігт") "ніхт", Replace (string'fromString "дігтяр") "діхтяр"] xs
  
-- Function that applies assimilation rules to the Ukrainian preprocessed string
-- Функція, яка застосовує правила асиміляції до українського попередньо обробленого рядка             
ukrainianLast2 :: String -> String
ukrainianLast2 = fst . foldr f v 
  where v = ([], [])
        f x (zs, xs) = case x of 
          'ж' -> if and [not . null $ xs, not . null . tail $ xs, elem (head xs) "сц", elem (head . tail $ xs) "іь"]
                   then ('з':'ь':zs,x:xs)
                   else ('ж':zs,x:xs)
          'ш' -> if and [not . null $ xs, not . null . tail $ xs, elem (head xs) "сц", elem (head . tail $ xs) "іь"]
                   then ('с':'ь':zs,x:xs)
                   else ('ш':zs,x:xs)
          'ч' -> if and [not . null $ xs, not . null . tail $ xs, elem (head xs) "сц", elem (head . tail $ xs) "іь"]
                   then ('ц':'ь':zs,x:xs)
                   else ('ч':zs,x:xs)
          _   -> let ys = dropWhile (not . (\z -> DC.isAlpha z || DC.isPunctuation z)) xs in case x of
                   'с' -> if (not . null $ ys) && (head ys == 'ш')
                            then ('ш':zs,x:xs)
                            else ('с':zs,x:xs)
                   'з' -> if (not . null $ ys) && elem (head ys) "шжч"
                            then ('ж':zs,x:xs)
                            else if (not . null $ ys) && (head ys == 'д')
                                   then if (not . null . tail $ ys) && ((head . tail $ ys) == 'ж')
                                          then ('ж':zs,x:xs)
                                          else ('з':zs,x:xs)
                                   else ('з':zs,x:xs)
                   'д' -> if (not . null $ ys) && elem (head ys) "сзц"
                            then ('д':'з':zs,x:xs)
                            else if (not . null $ ys) && elem (head ys) "жшч"
                                   then ('д':'ж':zs,x:xs)
                                   else ('д':zs,x:xs)
                   'т' -> if (not . null $ ys) && ((head ys) == 'ц')
                            then ('ц':zs,x:xs)
                            else if (not . null $ ys) && elem (head ys) "шч"
                                   then ('ч':zs,x:xs)
                                   else ('т':zs,x:xs)
                   _   -> (x:zs,x:xs)

-- Function that produces the list of Ukrainian strings from the primary Ukrainian string which can be further easily processed
-- Функція, яка створює список українських рядків з початкового українського рядка, які можуть бути легко оброблені далі
words2 :: String -> [String]
words2 [] = []
words2 xs = filter (not . null) $ assimilationFirst $ map (filter (not . DC.isSpace)) $ separatePunct . ukrainianLast2 . ukrainianJottedLast . ukrainianJotted1 .  
  changeAssimilative . separatePunct0 . ukrainianToMoreSounding . changeH2X $ filter (\x -> or [x >= '\x0020' && x <= '\x0022', x >= '\x0026' && x <= '\x003B', 
    x >= '\x003F' && x <= '\x005B', x == '\x005D', x >= '\x0061' && x <= '\x007D', x >= '\x0008' && x <= '\x000D', x == '\x00AB', x == '\x00BB', x == '\x0404', 
      x >= '\x0406' && x <= '\x0407', x >= '\x0410' && x <= '\x0429', x == '\x042C', x >= '\x042E' && x <= '\x0449', x== '\x044C', x >= '\x044E' && x <= '\x044F', 
        x == '\x0454', x >= '\x0456' && x <= '\x0457', x >= '\x2002' && x <= '\x2014', x == '\x2026', x >= '\x2028' && x <= '\x2029', x >= '\x2047' && x <= '\x2049', 
          x >= '\x20A0' && x <= '\x20B9', x == '\x2103', x == '\x2109', x == '\x2122']) $ map (\y -> case y of 
            '[' -> '(' 
            ']' -> ')'
            _  -> DC.toLower y) xs

-- Function-predicate that checks whether its argument is a punctuation mark or a whitespace
-- Функція-предикат, яка перевіряє, чи є її аргумент пунктуаційним знаком чи пробілом
isPunctOrSpace :: Char -> Bool
isPunctOrSpace x = DC.isPunctuation x || DC.isSpace x

-- Function that separates punctuation from the words for further processing
-- Функція, що відділяє пунктуацію від слів для подальшої обробки
separatePunct :: String -> [String]
separatePunct xs = if null xs 
                     then []
                     else if isPunctOrSpace $ head xs
                            then let z = span isPunctOrSpace xs in (' ':(fst z)):(separatePunct $ snd z)
                            else let zN = span (not . isPunctOrSpace) xs in fst zN:(separatePunct $ snd zN)

-- Function-predicate that checks whether its argument is either a digit character or a dash
-- Функція-предикат, що перевіряє, чи її аргумент є символом цифри чи дефісу
isDigitOrDash :: Char -> Bool
isDigitOrDash x = DC.isDigit x || x == '-'

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
  'я' -> True
  'ю' -> True
  'є' -> True
  'ї' -> True
  _   -> False

-- Function that creates from a Ukrainian pre-processed String a data of the type (String, (Maybe Integer, Integer)) that takes into account a word emphasis
-- Функція, що створює з попередньо обробленого українського String дані типу (String, (Maybe Integer, Integer)), що враховують наголос у складі
accountEmphasis :: String -> (String, (Maybe Integer, Integer))
accountEmphasis [] = ([], (Nothing, 0))
accountEmphasis ys = let z = toInteger . length . filter (isVowelL) $ ys in if isDigitOrDash . head $ ys
  then if (stringToInteger . dropWhile (== '0') . filter (DC.isDigit) $ ys) `mod` z == 0
         then (filter (isNotDigitOrDash) $ ys, (Just z, z))
         else (filter (isNotDigitOrDash) $ ys, (Just ((stringToInteger . dropWhile (== '0') . filter (DC.isDigit) $ ys) `mod` z), z))
  else (ys, (Nothing, z))

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
    then if elem x "вмнлр"
           then ([x,y],'q'):createSoundL (z:xs)
           else if (x == 'й')
                  then ([x],'r'):createSoundL (y:z:xs)
                  else if elem x "бдзжгґ"
                         then ([x,y],'i'):createSoundL (z:xs)
                         else ([x,y],'a'):createSoundL (z:xs)
    else if isVowelL y
           then if elem x "вмнлрй"
                  then ([x],'r'):([y],'w'):createSoundL (z:xs)
                  else if elem x "бзжгґ"
                         then ([x],'d'):([y],'w'):createSoundL (z:xs)
                         else ([x],'s'):([y],'w'):createSoundL (z:xs)
           else if (x == 'д')
                  then if ((y == 'з') || (y == 'ж'))
                         then if z == 'ь'
                                then ([x,y,z], 'i'):createSoundL xs
                                else ([x,y], 'd'):createSoundL (z:xs)
                         else ([x], 'd'):createSoundL (y:z:xs)
                  else if elem x "вмнлрй"
                          then ([x], 'r'):createSoundL (y:z:xs)
                          else if elem x "бзжгґ"
                                  then ([x], 'd'):createSoundL (y:z:xs)
                                  else ([x], 's'):createSoundL (y:z:xs)
createSoundL [x,y] = if isVowelL x
  then ([x], 'w'):createSoundL [y]
  else if y == 'ь'
    then if elem x "вмнлр"
           then [([x,y],'q')]
           else if elem x "бдзжгґ"
                         then [([x,y],'i')]
                         else [([x,y],'a')]
    else if isVowelL y
           then if elem x "вмнлрй"
                  then ([x],'r'):[([y],'w')]
                  else if elem x "бзжгґ"
                         then ([x],'d'):[([y],'w')]
                         else ([x],'s'):[([y],'w')]
           else if (x == 'д')
                  then if ((y == 'з') || (y == 'ж'))
                         then [([x,y], 'd')]
                         else ([x], 'd'):createSoundL [y]
                  else if elem x "вмнлрй"
                          then ([x], 'r'):createSoundL [y]
                          else if elem x "бзжгґ"
                                  then ([x], 'd'):createSoundL [y]
                                  else ([x], 's'):createSoundL [y]
createSoundL [x] = if isVowelL x
  then [([x], 'w')]
  else if elem x "вмнлрй"
    then [([x], 'r')]
    else if elem x "бдзжгґ"
           then [([x],'d')]
           else [([x],'s')]

-- Function that prepares a Ukrainian word to be divided into syllables
-- Функція, що готує українське слово для поділу на склади
prepareToSyllables :: String -> [[(String, Char)]]
prepareToSyllables xs = map createSoundL (createSoundGroups xs)

-- Function that divides a list of data of the type (String, Char) representing the Ukrainian consonants into two groups for further syllable constuction
-- Функція, що ділить список даних типу (String, Char), що представляють українські приголосні, на дві групи для подальшого конструювання складів
divideConsonants :: [(String, Char)] -> [[(String,Char)]]
divideConsonants xs = let y = length xs in case y of
  1 -> [xs]
  2 -> if (elem (snd . head $ xs) "rq" && head xs /= last xs) || (elem (snd . head $ xs) "di" && elem (snd . head . tail $ xs) "sa") then [[head xs], tail xs] else [xs]
  3 -> if elem (snd . head $ xs) "rq" then [[head xs], tail xs] else if elem (snd . head . tail $ xs) "rq" then [[head xs, head . tail $ xs], [last xs]] else [xs]
  _ -> if elem (snd . head $ xs) "rqdi" then [[head xs], tail xs] else [xs]

-- Intermediate function for creating a Ukrainian syllables
-- Проміжна функція для створення українських складів
divideToSyllables1 :: String -> [[(String, Char)]]
divideToSyllables1 xs = concatMap divideConsonants (takeWhile (not . null) (prepareToSyllables xs))
              
-- Optimized function that is applied to the zero-syllable parts to concatenate the punctuation into the one sample of sounding
-- Оптимізована функція, що застосовується до нуль-складових слів, щоб з'єднати пунктуаційні знаки в одну частину озвучування
concatPunct :: [((String, String), (String, Integer))] -> [((String, String), (String, Integer))]
concatPunct = fst . foldr f v
  where v = ([], [])
        f x (ys, xs) = if not . null . fst . fst $ x 
                         then if isPunctOrSpace (head . fst . fst $ x)
                                then ([((filter (not . DC.isSpace) (takeWhile isPunctOrSpace $ concatMap (fst . fst) (x:xs)), "esperanto"), ("-z", 0))], x:xs)
                                else (x:ys, x:xs)
                         else (x:ys, x:xs)
                         
-- Function that takes a Ukrainian String and converts it to the data of the type ((String, String), (String, Integer)) that is used for zero-vowel words
-- Функція, що отримує на вхід український String і конвертує його на дані типу ((String, String), (String, Integer)), що використовується для слів без голосних
zeroSyllablePart :: String ->  [((String, String), (String, Integer))]
zeroSyllablePart xs =  filter (not . null . fst . fst) $ concatPunct $ map (\x -> if x == "γ" 
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
  _ -> if k < u then k else u

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
createSyllablesMultiLast2 xss (Just y, z) | null xss = []
                                          | otherwise = case z of 
    1 -> case y of
       1 -> concatMap (map (\x -> if x == "γ" then (("γ", "greek"), ("-a 110 -z", 1)) else if x == "y" then (("y", "polish"), ("-a 110 -z", 0)) else ((x, "esperanto"), ("-a 110 -z", 1)))) $ xss
       0 -> concatMap (map (\x -> if x == "γ" then (("γ", "greek"), ("-z", 1)) else if x == "y" then (("y", "polish"), ("-z", 0)) else ((x, "esperanto"), ("-z", 1)))) $ xss
       _ -> concatMap (map (\x -> if x == "γ" then (("γ", "greek"), ("-a 110 -z", 1)) else if x == "y" then (("y", "polish"), ("-a 110 -z", 0)) else ((x, "esperanto"), ("-a 110 -z", 1)))) $ xss
    _ -> if y < z
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
createSyllablesMultiLast2 xss (Nothing, z) | null xss = []
                                           | otherwise = createSyllablesMultiLast2 xss (Just (z - 1), z)

-- Function that creates Ukrainian syllables and groups them with some parameters to be then processed by the eSpeak and SoX executables
-- Функція, що створює українські склади та групує їх з деякими параметрами, щоб потім вони були оброблені програмами eSpeak і SoX
createSyllablesReady :: (String, (Maybe Integer, Integer)) -> [((String, String),(String,Integer))]
createSyllablesReady x = case snd . snd $ x of
  0 -> zeroSyllablePart . fst $ x
  _ -> createSyllablesMultiLast2 (divideToUnits $ fst x) (fst . snd $ x, snd . snd $ x)

-- Function that combines the emphasis and dividing into sound groups into languages
-- Функція, що поєднує наголос і поділ на групи звуків для мов
combineSoundsLs :: String -> [((String, String), (String, Integer))]
combineSoundsLs [] = []
combineSoundsLs xs = createSyllablesReady . accountEmphasis $ xs

-- Function that applies additional function h to a if p is True on a
-- Функція, що застосовує додаткову функцію h до a, якщо p є Істина на a
bGroups :: (a -> Bool) -> (a -> [a]) -> [a] -> [a]
bGroups p h xs = concatMap (\x -> if p x then h x else [x]) xs

-- Function that applies additional function h to a if args include "0" or "-0"
-- Функція, що застосовує додаткову функцію h до a, якщо args включають "0" або "-0"
bGroups0 :: (a -> [a]) -> [a] -> [a]
bGroups0 h xs = concatMap h xs

-- Function-predicate to check whether its argument is a Vowel sound letter in Esperanto or Greek respesentation of the group of sounds
-- Функція-предикат, щоб визначити, чи її аргумент є літерою, що позначає голосний у представленні грецькою мовою чи есперанто
isVowelEG :: Char -> Bool
isVowelEG x = case x of
  'a' -> True
  'o' -> True
  'e' -> True
  'u' -> True
  'i' -> True
  _   -> False
  
-- Function-predicate that checks whether its argument is either a vowel or a punctuation mark
-- Функція-предикат, яка перевіряє, чи є її аргумент голосним або знаком пунктуації
isVowelOrPunctuation :: Char -> Bool
isVowelOrPunctuation x = DC.isPunctuation x || isVowelEG x

-- Function-predicate that checks whether the hFunctionH must be applied to the ((String, String), (String, Integer))
-- Функція-предикат, яка перевіряє, чи має бути застосована hFunctionH до ((String, String), (String, Integer))
pFunctionP :: ((String, String), (String, Integer)) -> Bool
pFunctionP ((xs, _), (_, _)) = (null $ filter isVowelOrPunctuation xs) || ((length $ takeWhile (not . isVowelEG) xs) > 3) || ((length $ dropWithFirst isVowelEG xs) > 3)

-- Function that converts zero-syllable groups of consonant sounds into separate sounds for further processing
-- Функція, що перетворює безголосні групи приголосних у окремі звуки для подальшої обробки
hFunctionH :: ((String, String), (String, Integer)) -> [((String, String), (String, Integer))]
hFunctionH  (([], _), (_, _)) = []
hFunctionH (([x], ys), (zs, _)) = [(([x], ys), (zs, 0))]
hFunctionH (([x, y], ys), (zs, _)) | x == 'd' = case y of
                                       'z' -> [(("dz", ys), (zs, 0))]
                                       'ĵ' -> [(("dĵ", ys), (zs, 0))]
                                       _   -> [(("d", ys), (zs, 0)), (([y], ys), (zs, 0))]
                                   | otherwise = [(([x], ys), (zs, 0)), (([y], ys), (zs, 0))]
hFunctionH (((u:t:ts), ys), (zs, _)) | u == 'd' = case t of
                                          'z' -> (("dz", ys), (zs, 0)):hFunctionH ((ts, ys), (zs, 0))
                                          'ĵ' -> (("dĵ", ys), (zs, 0)):hFunctionH ((ts, ys), (zs, 0))
                                          _   -> (("d", ys), (zs, 0)):hFunctionH  (((t:ts), ys), (zs, 0))
                                     | otherwise = (([u], ys), (zs, 0)):hFunctionH (((t:ts), ys), (zs, 0))

-- Function that divides wrongly sounding syllables for abbeviations of esperanto months into parts
-- Функція, яка ділить неправильно озвучувані склади для абревіатур назв місяців мовою есперанто на дві частини
hDivideMonths2:: ((String, String), (String, Integer)) -> [((String, String), (String, Integer))]
hDivideMonths2 ((xs, ys), (zs, k)) = case xs of
  "jan" -> [(("ja", ys), (zs, k)),(("n", ys), (zs, 0))]
  "feb" -> [(("fe", ys), (zs, k)),(("b", ys), (zs, 0))]
  "mar" -> [(("ma", ys), (zs, k)),(("r", ys), (zs, 0))]
  "apr" -> [(("ap", ys), (zs, k)),(("r", ys), (zs, 0))]
  "maj" -> [(("ma", ys), (zs, k)),(("j", ys), (zs, 0))]
  "jun" -> [(("ju", ys), (zs, k)),(("n", ys), (zs, 0))]
  "jul" -> [(("ju", ys), (zs, k)),(("l", ys), (zs, 0))]
  "sept" -> [(("sep", ys), (zs, k)),(("t", ys), (zs, 0))]
  "okt" -> [(("ok", ys), (zs, k)),(("t", ys), (zs, 0))]
  "dec" -> [(("de", ys), (zs, k)),(("c", ys), (zs, 0))]
  _ -> [((xs, ys), (zs, k))]

-- Function that prepares a String for processing by the eSpeak and SoX for non-zero-syllable words
-- Функція, яка готує слово з голосним для подальшої обробки  eSpeak та SoX
combineSoundsLs3 :: String -> [((String, String), (String, Integer))]
combineSoundsLs3 xs = concatMap hDivideMonths2 $ concatSoftSign $ bGroups pFunctionP hFunctionH $ combineSoundsLs xs

-- Function that prepares a String for processing by the eSpeak and SoX if args include "0" or "-0"
-- Функція, яка готує слово з голосним для подальшої обробки  eSpeak та SoX, якщо args включають "0" або "-0"
combineSoundsLs30 :: String -> [((String, String), (String, Integer))]
combineSoundsLs30 xs = concatMap hDivideMonths2 $ concatSoftSign $ bGroups0 hFunctionH $ combineSoundsLs xs

-- Function that concatenates alone soft sign with the previous letter (Esperanto or Greek)
-- Функція, яка з'єднує ізольований м'який знак з попереднім приголосним (есперанто чи грецькою)
concatSoftSign ::  [((String, String), (String, Integer))] ->  [((String, String), (String, Integer))]
concatSoftSign [] = []
concatSoftSign (x:xs) = if null xs 
  then [x]
  else case fst . fst . head $ xs of
    "q" -> if (fst . fst $ x) == "γ"
             then x:concatSoftSign (tail xs)
             else (((fst . fst $ x) ++ "q", "esperanto" ), snd x):concatSoftSign (tail xs)
    _ -> x:concatSoftSign xs
              
-- Function that is used to create String that is a parameter effect to get a needed duration of the sound
-- Функція, що використовується для створення String, що є параметричним ефектом, щоб отримати звук потрібної тривалості
stringDurationLim :: String -> String
stringDurationLim [] = []
stringDurationLim xs = case xs of
  "ep" -> " trim -0.103"
  "er" -> " trim -0.099"
  "ev" -> " trim -0.134"
  "el" -> " trim -0.105"
  "edĵ" -> " trim -0.166"
  "ed" -> " trim -0.070"
  "edz" -> " trim -0.178"
  "eĵ" -> " trim -0.132"
  "ef" -> " trim -0.112"
  "αγ" -> " trim -0.117"
  "en" -> " trim -0.136"
  "ek" -> " trim -0.071"
  "ej" -> " trim -0.133"
  "es" -> " trim -0.121"
  "em" -> " trim -0.146"
  "et" -> " trim -0.071"                            
  "eb" -> " trim -0.070"
  "ec" -> " trim -0.115"
  "eĉ" -> " trim -0.114"
  "eg" -> " trim -0.081"
  "eh" -> " trim -0.083"
  "eĥ" -> " trim -0.118"
  "eŝ" -> " trim -0.121"
  "ez" -> " trim -0.157"
  "ja" -> " trim -0.350"
  "je" -> " trim -0.348"
  "ji" -> " trim -0.329"
  "ju" -> " trim -0.315"
  "eŝĉ" -> " trim -0.198"
  _ -> []

-- Function that is used to convert single letters to a respective syllables for sounding
-- Функція, що використовується, щоб перетворити окремі літери на відповідні склади для озвучування
oneToSyllable :: String -> String
oneToSyllable [] = []
oneToSyllable xs = if head xs == 'd'
  then if null . tail $ xs
         then "ed"
         else case head . tail $ xs of
                'z' -> "edz"
                'ĵ' -> "edĵ"
                _ -> "ed"
  else if DC.isPunctuation . head $ xs 
         then xs
         else case head xs of
                'p' -> "ep"
                'r' -> "er"
                'a' -> "a"
                'o' -> "o"
                'v' -> "ev"
                'l' -> "el"
                'i' -> "i"
                'γ' -> "αγ"
                'n' -> "en"
                'e' -> "e"
                'k' -> "ek"
                'u' -> "u"
                's' -> "es"
                'm' -> "em"
                'y' -> "yy"
                't' -> "et"                                                    
                'ĵ' -> "eĵ"
                'ŝ' -> "eŝ"
                'ĉ' -> "eĉ"
                'c' -> "ec"
                'g' -> "eg"
                'j' -> "ej"
                'b' -> "eb"
                'z' -> "ez"
                'ĥ' -> "eĥ"
                'f' -> "ef"
                'h' -> "eh"
                _ -> [head xs]
                
{-
______________________________________________________________________________________________________
(   )   ,   .   :    ;   ...    !   ?    –    —  |   y    |  y + 0.01  |  11 variables respectively   |
------------------------------------------------------------------------------------------------------
x0  x1  x2  x3  x4  x5   x6     x7  x8  x9   xA  |        |            |  from 0 to A (== 11)         |
------------------------------------------------------------------------------------------------------
0   0    0   0   0   0    0      0   0   0    0      0        0
1   0    0   0   0   0    0      0   0   0    0     0.3      0.301
0   1    0   0   0   0    0      0   0   0    0     0.5      0.501  
0   0    1   0   0   0    0      0   0   0    0     0.4      0.401
0   0    0   1   0   0    0      0   0   0    0     0.8      0.801
0   0    0   0   1   0    0      0   0   0    0     0.7      0.701
0   0    0   0   0   1    0      0   0   0    0     0.6      0.601
0   0    0   0   0   0    1      0   0   0    0     0.9      0.901
0   0    0   0   0   0    0      1   0   0    0     0.9      0.901
0   0    0   0   0   0    0      0   1   0    0     0.9      0.901
0   0    0   0   0   0    0      0   0   1    0     0.75     0.751
0   0    0   0   0   0    0      0   0   0    1     0.85     0.851
0   0    1   2   0   0    0      0   0   0    0     0.9      0.901
0   0    0   0   0   0    0      2   0   0    0     1.1      1.101
0   0    0   1   0   0    0      2   0   0    0     1.2      1.201
0   0    0   0   0   0    0      3   0   0    0     1.3      1.301
0   0    0   2   0   0    0      1   0   0    0     1.0      1.001
0   0    0   0   0   0    0      0   2   0    0     1.1      1.101
0   0    0   1   0   0    0      0   2   0    0     1.2      1.201
0   0    0   0   0   0    0      0   3   0    0     1.3      1.301
0   0    0   2   0   0    0      0   1   0    0     1.0      1.001
0   0    0   0   0   0    0      1   1   0    0     1.2      1.201
0   0    0   1   0   0    0      1   1   0    0     1.3      1.301
0   0    1   0   0   0    0      0   0   1    0     0.95     0.951
0   0    1   0   0   0    0      0   0   0    1     0.95     0.951
      ^  
     / \
    _____
      |
      |   

______________________________________________________________________________________________________
(   )   ,   .   :    ; |  !      ?    –   —     |   y    |  y + 0.01  |  10 variables respectively   |
------------------------------------------------------------------------------------------------------
x0  x1  x2  x3  x4  x5 |  x6     x7  x8  x9     |        |            |  from 0 to 9                 |
------------------------------------------------------------------------------------------------------
0   0    0   0   0   0 |   0      0   0   0          0        0
1   0    0   0   0   0 |   0      0   0   0         0.3      0.301
0   1    0   0   0   0 |   0      0   0   0         0.5      0.501  
0   0    1   0   0   0 |   0      0   0   0         0.4      0.401
0   0    0   1   0   0 |   0      0   0   0         0.8      0.801
0   0    0   0   1   0 |   0      0   0   0         0.7      0.701
0   0    0   0   0   1 |   0      0   0   0         0.6      0.601
______________________________________________________________________________________________________
0   0    0   3   0   0     0      0   0   0         0.9      0.901                                   |
------------------------------------------------------------------------------------------------------
0   0    0   0   0   0 |   1      0   0   0         0.9      0.901
0   0    0   0   0   0 |   0      1   0   0         0.9      0.901
0   0    0   0   0   0 |   0      0   1   0         0.75     0.751
0   0    0   0   0   0 |   0      0   0   1         0.85     0.851
0   0    1   2   0   0 |   0      0   0   0         0.9      0.901
0   0    0   0   0   0 |   2      0   0   0         1.1      1.101
0   0    0   1   0   0 |   2      0   0   0         1.2      1.201
0   0    0   0   0   0 |   3      0   0   0         1.3      1.301
0   0    0   2   0   0 |   1      0   0   0         1.0      1.001
0   0    0   0   0   0 |   0      2   0   0         1.1      1.101
0   0    0   1   0   0 |   0      2   0   0         1.2      1.201
0   0    0   0   0   0 |   0      3   0   0         1.3      1.301
0   0    0   2   0   0 |   0      1   0   0         1.0      1.001
0   0    0   0   0   0 |   1      1   0   0         1.2      1.201
0   0    0   1   0   0 |   1      1   0   0         1.3      1.301
0   0    1   0   0   0 |   0      0   1   0         0.95     0.951
0   0    1   0   0   0 |   0      0   0   1         0.95     0.951
-}

-- Additional function that is used for optimization of the punctL and punctL11 functions
-- Додаткова функція, яка використовується для оптимізації функцій punctL і punctL11
punctOpt :: String -> Integer
punctOpt = sum . map (\x -> case x of 
                              '(' -> 1000000000
                              ')' -> 100000000
                              ',' -> 10000000
                              '.' -> 1000000
                              ':' -> 100000
                              ';' -> 10000
                              '!' -> 1000
                              '?' -> 100
                              '–' -> 10
                              '—' -> 1
                              _   -> 0)

-- Function that is used to create punctuation pauses
-- Функція, що використовується для створення пунктуаційних пауз
punctuationPauseLength :: Integer -> String -> String -> String -> [String] -> IO ()
punctuationPauseLength t1 xs ys zs args = punctL (punctOpt xs) t1 xs ys zs args
          
-- Function that is used to create punctuation pauses if args include "1", or "2", or "3"
-- Функція, що використовується для створення пунктуаційних пауз, якщо args включають "1", або "2", або "3"
punctuationPauseLength1 :: Integer -> String -> String -> String -> [String] -> IO ()
punctuationPauseLength1 t1 xs ys zs args = punctL11 (punctOpt xs) t1 xs ys zs args
        
-- Additional function that is used for pause creation into the functions punctL and punctL11
-- Додаткова функція, яка використовується всередині функцій punctL і punctL11 для створення пауз
punctL1 :: Double -> Integer -> String -> IO ()
punctL1 x t1 xs = do 
                          if os == "Windows"
                            then return ("sox.exe -n -r 22.05k -c 1 -b 16 " ++ (show t1) ++ "." ++ xs ++ ".wav delay " ++ 
                              show (x + 0.001) ++ " trim 0 " ++ show x) >>= callCommand
                            else return ("sox -n -r 22.05k -c 1 -b 16 " ++ (show t1) ++ "." ++ xs ++ ".wav delay " ++ 
                              show (x + 0.001) ++ " trim 0 " ++ show x) >>= callCommand

-- Function that considers a number of punctuation marks for proper pause creation
-- Функція, яка бере до уваги кількість пунктуаційних знаків для правильного створення пауз        
punctL :: Integer -> Integer -> String -> String -> String -> [String] -> IO ()
punctL k t1 xs ys zs _ = if DC.isPunctuation . head $ xs 
                                then case k of
                                       1000000000 -> punctL1 0.3 t1 xs
                                       100000000 -> punctL1 0.5 t1 xs
                                       10000000 -> punctL1 0.4 t1 xs
                                       1000000 -> punctL1 0.8 t1 xs
                                       100000 -> punctL1 0.7 t1 xs
                                       10000 -> punctL1 0.6 t1 xs
                                       3000000 -> punctL1 0.9 t1 xs
                                       1000 -> punctL1 0.9 t1 xs
                                       100 -> punctL1 0.9 t1 xs
                                       10 -> punctL1 0.75 t1 xs
                                       1 -> punctL1 0.85 t1 xs
                                       12000000 -> punctL1 0.9 t1 xs
                                       2000 -> punctL1 1.1 t1 xs
                                       1002000 -> punctL1 1.2 t1 xs
                                       3000 -> punctL1 1.3 t1 xs
                                       2001000 -> punctL1 1.0 t1 xs
                                       200 -> punctL1 1.1 t1 xs
                                       1000200 -> punctL1 1.2 t1 xs
                                       300 -> punctL1 1.3 t1 xs
                                       2000100 -> punctL1 1.0 t1 xs
                                       1100 -> punctL1 1.2 t1 xs
                                       1001100 -> punctL1 1.3 t1 xs
                                       10000010 -> punctL1 0.95 t1 xs
                                       10000001 -> punctL1 0.95 t1 xs
                                       _ -> punctL1 0.7 t1 xs
                                else if os == "Windows"
                                       then let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in if last xs == 'q'
                                              then do
                                                     return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                     return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us) >>= callCommand
                                                     addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                              else do
                                                     if xs == "y"
                                                       then do
                                                         return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                         return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ 
                                                           " tempo -s 0.7") >>= callCommand
                                                       else do
                                                         return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                         return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us) >>= callCommand
                                       else let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in if last xs == 'q'
                                              then do
                                                     return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                     return ("sox " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us) >>= callCommand
                                                     addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                              else do
                                                     if xs == "y"
                                                       then do
                                                         return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                         return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ 
                                                           " tempo -s 0.7") >>= callCommand
                                                       else do
                                                         return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                         return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us) >>= callCommand                                                          
                                                         
-- Function that is used for zero-syllable creation varied from OS and duration
-- Функція, яка використовується для створення слів без голосних і варіюється в залежності від ОС та тривалості
createZeroSyllable :: (Char, Double, Integer, String, String, String, String, String) -> IO ()
createZeroSyllable ('w', v, t1, xs, ys, zs, ts, us) = if last xs == 'q'
                                                then do
                                                  return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                  return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us ++ " tempo -s " ++ (show v)) >>= callCommand
                                                  addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                                else do
                                                  if xs == "y"
                                                    then do
                                                      return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                      return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s " ++ (show v)) >>= callCommand
                                                    else do
                                                      return ("espeak.exe -v " ++ ys ++ " " ++ zs ++ " -w m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                      return ("sox.exe " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s " ++ (show v)) >>= callCommand                                                         
createZeroSyllable (_, v, t1, xs, ys, zs, ts, us) = if last xs == 'q'
                                              then do
                                                return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w m." ++ (show t1) ++ "." ++ ts ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                return ("sox " ++  "m." ++ (show t1) ++ "." ++ ts ++ ".wav " ++  (show t1) ++ "." ++ ts ++ ".wav " ++ us ++ " tempo -s " ++ (show v)) >>= callCommand
                                                addSoftSign $ (show t1) ++ "." ++ ts ++ ".wav"
                                              else do
                                                if xs == "y"
                                                  then do
                                                    return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                    return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s " ++ (show v)) >>= callCommand
                                                  else do
                                                    return ("espeak -v " ++ ys ++ " " ++ zs ++ " -w m." ++ (show t1) ++ "." ++ xs ++ ".wav \"" ++ ts ++ "\"") >>= callCommand
                                                    return ("sox " ++  "m." ++ (show t1) ++ "." ++ xs ++ ".wav " ++ (show t1) ++ "." ++ xs ++ ".wav " ++ us ++ " tempo -s " ++ (show v)) >>= callCommand                                                             

-- Function that considers a number of punctuation marks for proper pause creation
-- Функція, яка бере до уваги кількість пунктуаційних знаків для правильного створення пауз                                                                         
punctL11 :: Integer -> Integer -> String -> String -> String -> [String] -> IO ()
punctL11 k t1 xs ys zs args = if DC.isPunctuation . head $ xs 
                                then case k of
                                       1000000000 -> punctL1 0.3 t1 xs
                                       100000000 -> punctL1 0.5 t1 xs
                                       10000000 -> punctL1 0.4 t1 xs
                                       1000000 -> punctL1 0.8 t1 xs
                                       100000 -> punctL1 0.7 t1 xs
                                       10000 -> punctL1 0.6 t1 xs
                                       3000000 -> punctL1 0.9 t1 xs
                                       1000 -> punctL1 0.9 t1 xs
                                       100 -> punctL1 0.9 t1 xs
                                       10 -> punctL1 0.75 t1 xs
                                       1 -> punctL1 0.85 t1 xs
                                       12000000 -> punctL1 0.9 t1 xs
                                       2000 -> punctL1 1.1 t1 xs
                                       1002000 -> punctL1 1.2 t1 xs
                                       3000 -> punctL1 1.3 t1 xs
                                       2001000 -> punctL1 1.0 t1 xs
                                       200 -> punctL1 1.1 t1 xs
                                       1000200 -> punctL1 1.2 t1 xs
                                       300 -> punctL1 1.3 t1 xs
                                       2000100 -> punctL1 1.0 t1 xs
                                       1100 -> punctL1 1.2 t1 xs
                                       1001100 -> punctL1 1.3 t1 xs
                                       10000010 -> punctL1 0.95 t1 xs
                                       10000001 -> punctL1 0.95 t1 xs
                                       _ -> punctL1 0.7 t1 xs
                                else if os == "Windows"
                                       then let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in case head args of
                                              "1" -> createZeroSyllable ('w', 0.7, t1, xs, ys, zs, ts, us)
                                              "2" -> createZeroSyllable ('w', 0.8, t1, xs, ys, zs, ts, us) 
                                              "3" -> createZeroSyllable ('w', 0.9, t1, xs, ys, zs, ts, us)
                                              _   -> createZeroSyllable ('w', 0.65, t1, xs, ys, zs, ts, us)
                                       else let ts = oneToSyllable $ filter (/= 'q') xs in let us = stringDurationLim ts in case head args of
                                              "1" -> createZeroSyllable ('u', 0.7, t1, xs, ys, zs, ts, us)
                                              "2" -> createZeroSyllable ('u', 0.8, t1, xs, ys, zs, ts, us) 
                                              "3" -> createZeroSyllable ('u', 0.9, t1, xs, ys, zs, ts, us)
                                              _   -> createZeroSyllable ('u', 0.65, t1, xs, ys, zs, ts, us)
                
-- Function that for the Ukrainian syllable represented as ((String, String),(String,Integer)) creates sounds
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
  'г' -> False
  _  -> True

-- Additional function that is used inside the createSoundsForSyllable function to convert Ukrainian syllables into Esperanto, or Polish, or Greek ones
-- Додаткова функція, що використовується всередині createSoundsForSyllable для конвертування українського складу в склад мовою есперанто, польською чи грецькою
convertSyllableToLanguage ::  String ->  [String]
convertSyllableToLanguage [] = []
convertSyllableToLanguage xs =  if elem 'и' xs || elem 'г' xs
         then let z = head $ dropWhile continueLang xs in
                concat . filter (not . null) . map (\x -> [takeWithFirst (/= 'q') x, dropWithFirst (/= 'q') x]) $
                  (changeToEsperanto $ takeWhile continueLang xs):(if z == 'и'
                                                                      then "y"
                                                                      else "γ"):convertSyllableToLanguage (dropWithFirst continueLang xs)
         else (concat . filter (not . null) . map (\x -> [takeWithFirst (/= 'q') x, dropWithFirst (/= 'q') x])) [changeToEsperanto xs]

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
stringToInteger [] = 0
stringToInteger xs = foldl1 ((+) . (*10)) $! (map (charToDigit) $ xs)
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
            _ -> if os == "Windows" 
                   then error "Character Is Not a Digit!\r\n"
                   else error "Character Is Not a Digit!\n"
  
-- Function that actually converts a Ukrainian word to the Esperanto string for further reading
-- Функція, що власне перетворює українське слово у Esperanto рядок для подальшого озвучування                        
changeToEsperanto :: String -> String
changeToEsperanto [] = []
changeToEsperanto xs = concatMap change2 $ replaceWithList [Replace (string'fromString "цьцьа") "csja", Replace (string'fromString "ьо") "jo", 
  Replace (string'fromString "ьй") "jj", Replace (string'fromString "ьа") "ja", Replace (string'fromString "ьу") "ju", Replace (string'fromString "ье") "je"] xs

-- Function to convert char-by-char the rest of the preprocessed Ukrainian word into the Esperanto sounding string
-- Функція для перетворення символ за символом решти попердньо обробленого українського слова у радок Esperanto для озвучування
change2 :: Char -> String
change2 x | x == 'п' = "p"
          | x == 'р' = "r"
          | x == 'а' = "a"
          | x == 'о' = "o"
          | x == 'в' = "v"
          | x == 'л' = "l"
          | x == 'і' = "i"
          | x == 'д' = "d"
          | x == 'ф' = "f"
          | x == 'ж' = "ĵ"
          | x == 'г' = "h"
          | x == 'н' = "n"
          | x == 'е' = "e"
          | x == 'к' = "k"
          | x == 'й' = "j"
          | x == 'ь' = "q"
          | x == 'у' = "u"
          | x == 'с' = "s"
          | x == 'м' = "m"
          | x == 'т' = "t"
          | x == 'б' = "b"
          | x == 'ш' = "ŝ"
          | x == 'ч' = "ĉ"
          | x == 'ц' = "c"
          | x == 'ґ' = "g"
          | x == 'з' = "z"
          | x == 'х' = "ĥ"
          | x == 'ц' = "c"
          | x == 'я' = "ja"
          | x == 'є' = "je"
          | x == 'ю' = "ju"
          | x == 'ї' = "ji"
          | x == '’' = ""
          | x == '-' = ""
          | x == 'щ' = "ŝĉ"          
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
                              else error "Please, install eSpeak executable espeak.exe and SoX executable sox.exe into the directories mentioned in the variable PATH!\r\n"
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
     else do
       createSoftSign
       nI2 <- getContents
       if elem "W" args || elem "-W" args 
         then let pauseW0 x = do
                               mapM_ (createSoundsForSyllable getCPUTime args) x
                               zz <- getCPUTime
                               let t1 = 10000000000 + (zz `div` 10000000) in
                                 if (os == "Windows") 
                                   then do 
                                          return ("sox.exe -n -r 22.05k -c 1 -b 16 " ++ (show t1) ++ ".ee.wav delay 0.015 trim 0 0.014") >>= callCommand
                                   else do
                                          return ("sox -n -r 22.05k -c 1 -b 16 " ++ (show t1) ++ ".ee.wav delay 0.015 trim 0 0.014") >>= callCommand
                                 in mapM_ pauseW0 (map combineSoundsLs30 $ words2 nI2)
         else let pauseW x = do
                               mapM_ (createSoundsForSyllable getCPUTime args) x
                               zz <- getCPUTime
                               let t1 = 10000000000 + (zz `div` 10000000) in
                                 if (os == "Windows") 
                                   then do 
                                          return ("sox.exe -n -r 22.05k -c 1 -b 16 " ++ (show t1) ++ ".ee.wav delay 0.015 trim 0 0.014") >>= callCommand
                                   else do
                                          return ("sox -n -r 22.05k -c 1 -b 16 " ++ (show t1) ++ ".ee.wav delay 0.015 trim 0 0.014") >>= callCommand
                                 in mapM_ pauseW (map combineSoundsLs3 $ words2 nI2)
