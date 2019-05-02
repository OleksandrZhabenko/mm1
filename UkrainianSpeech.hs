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

data Language a = Polish a | Esperanto a
   deriving (Eq,Ord,Show)
   
getThrough :: Language a -> a
getThrough (Polish x) = x
getThrough (Esperanto x) = x

isPolish :: Language a -> Bool
isPolish (Polish _) = True
isPolish _ = False

isEsperanto :: Language a -> Bool
isEsperanto (Esperanto _) = True
isEsperanto _ = False

isTheSamePair :: [Language a] -> Bool
isTheSamePair [x,y] | ((isEsperanto x && isEsperanto y) || (isPolish x && isPolish y)) = True
                    | otherwise = False
isTheSamePair _ = False

concatUkrainian :: [String] -> [String]
concatUkrainian [] = []
concatUkrainian (x:xs) | not $ null xs = if ((x == "в") || (x == "В") ||(x == "з") ||(x == "З") || (x == "Й") || (x == "й") || (x == "ж")) 
                                             then (x ++ (head xs)):(concatUkrainian $ tail xs)
                                             else x:(concatUkrainian xs)
                       | otherwise = [x]


words2 :: String -> [String]
words2 [] = [""]
words2 xs = concatUkrainian $ words xs


changeToPolish :: String -> String
changeToPolish [] = []
changeToPolish x = concatMap change3 ( replaceWithList [Replace (string'fromString " --") "; ", Replace (string'fromString "-- ") "; ", Replace (string'fromString " -") " ;",  Replace (string'fromString "- ") "; ", Replace (string'fromString "сі") "sji", Replace (string'fromString "Сі") "Sji", Replace (string'fromString "Ці") "Cji", Replace (string'fromString "ці") "cji", Replace (string'fromString "ться") "tsja", Replace (string'fromString "рх") "rch", Replace (string'fromString "ьо") "jo", Replace (string'fromString "ья") "jja", Replace (string'fromString "ьє") "jje", Replace (string'fromString "ью") "jju", Replace (string'fromString "ьї") "jji", Replace (string'fromString "’я") "jja", Replace (string'fromString "’є") "jje", Replace (string'fromString "’ю") "jju", Replace (string'fromString "’ї") "jji"] x)
                        
changeToEsperanto :: String -> String
changeToEsperanto [] = []
changeToEsperanto x = concatMap change2 ( replaceWithList [Replace (string'fromString " --") "; ", Replace (string'fromString "-- ") "; ", Replace (string'fromString " -") " ;",  Replace (string'fromString "- ") "; ", Replace (string'fromString "ться") "tsja", Replace (string'fromString "ьо") "jo", Replace (string'fromString "ья") "jja", Replace (string'fromString "ьє") "jje", Replace (string'fromString "ью") "jju", Replace (string'fromString "ьї") "jji", Replace (string'fromString "’я") "jja", Replace (string'fromString "’є") "jje", Replace (string'fromString "’ю") "jju", Replace (string'fromString "’ї") "jji"] x)

polishChecker :: [String] -> [Language String]
polishChecker [] = []
polishChecker (x:xs) = if ((elem 'и' x) || (elem 'И' x)) 
                             then (Polish (changeToPolish x)):(polishChecker xs)
                             else (Esperanto (changeToEsperanto x)):(polishChecker xs)
                             
concatSameL3 :: [Language String] -> [Language String]
concatSameL3 [] = []
concatSameL3 (x:xs) | not $ null xs = if (isTheSamePair [x,head xs]) 
                                             then if isEsperanto x 
                                                       then (Esperanto ((getThrough x) ++ (getThrough (head xs)))):(concatSameL3 $ tail xs)
                                                       else (Polish ((getThrough x) ++ (getThrough (head xs)))):(concatSameL3 $ tail xs)
                                             else x:(concatSameL3 xs)
                    | otherwise = [x]

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
          | x == '’' = "j"
          | otherwise = [x]

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
          | x == '’' = "j"
          | x == 'и' = "y"
          | x == 'И' = "Y"
          | otherwise = [x]

nI :: String -> [Language String]
nI line = concatSameL3 $ polishChecker $ words2 line 

createLine :: IO Integer -> Language String -> IO String
createLine time line = do
                t <- time
                let t1 = 10000000000 + (t `div` 10000000) in
                  if (os == "Windows") 
                    then do 
                           x <- findExecutable "espeak.exe"
                           if (x /= Nothing)
                              then return ("espeak.exe -v " ++ (if (isEsperanto line) then "esperanto -w " else "polish -w ") ++ (show t1) ++ ".wav -z " ++ (getThrough line) ) 
                              else error "Please, install eSpeak executable  espeak.exe into the directory mentioned in the variable PATH!\n"
                    else do 
                           x <- findExecutable "espeak"
                           if (x /= Nothing)
                              then return ("espeak -v " ++ (if (isEsperanto line) then "esperanto -w " else "polish -w ") ++ (show t1) ++ ".wav -z " ++ (getThrough line) ) 
                              else error "Please, install eSpeak executable  espeak into the directory mentioned in the variable PATH!\n"

doCreation xs = mapM_ (>>= callCommand) (fmap (createLine getCPUTime) xs)

main = do 
   nI2 <- getLine
   let ni3 = nI nI2 in 
     doCreation ni3
