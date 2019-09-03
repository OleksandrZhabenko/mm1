# Revision history for mm1

## 0.1.0.0  -- 2019-05-02

* First version. Released on an unsuspecting world.

## 0.1.1.0  -- 2019-05-03

* Second version. Imroved some details in the program. Added parallel Ukrainian and English versions to the files.


## 0.1.1.1  -- 2019-05-03

* Second version. Some minor changes to files.

## 0.1.1.2  -- 2019-05-03

* Second version. Some minor changes to files.

## 0.2.0.0  -- 2019-06-11

* Third version. Program is rewritten so that now it uses also SoX. As a basic unit, now the syllable.is used, not a word as earlier.

## 0.2.1.0  -- 2019-06-12

* Third revised version. Fixed an issue with the letters 'я', 'ю' and 'є'.

## 0.2.1.1  -- 2019-06-12

* Third again revised version. Fixed an issue with the letters 'я', 'ю' and 'є'.

## 0.2.1.2  -- 2019-06-12

* Third again revised C version. Fixed an issue with the INSTALL files.

## 0.2.2.0  -- 2019-06-18

* Fourth alpha version. Fixed some issues with Polish and added some rules for assimilation.

## 0.2.3.0  -- 2019-06-18

* Fourth version. Now the program uses both syllabies and sounds approach together. Added support for letter "г".

## 0.2.4.0  -- 2019-06-25

* Fifth version. Added support for letter "ь". Added possibility to use command line arguments and to change 
the duration of the singular letters.

## 0.2.4.1  -- 2019-07-08

* Fifth little revised version. Some minor changes to avoid overprocessment.

## 0.2.5.0  -- 2019-07-14

* Fifth revised version. Fixed some issues with the letters "ь" and "г".

## 0.2.6.0  -- 2019-07-14

* Sixth version. Changed approach to making pauses between words. Some additional minor changes.

## 0.2.6.1  -- 2019-07-15

* Sixth version revised A. Some changes to making pauses between words. Fixed an issue with "maj\Maj" syllable. Some minor changes.

## 0.2.7.0  -- 2019-07-25

* Sixth revised B. Added assimilation rules for consonants to the program. Fixed an issue with "mar\maj" syllable. Some optimisations added. The output files have no capital letters in them. Some other minor changes.

## 0.2.8.0  -- 2019-07-26

* Seventh version. Added more proper command line arguments handling and punctuation pauses. The program now can work in 
two different regimes: as help menu and common usage. Fixed an issue with the Esperanto months abbreviations 
sounding wrongly. Some minor fixes.

## 0.2.8.1  -- 2019-07-29

* Seventh version revised A. Fixed an issue with empty list in the command line arguments checking.

## 0.2.8.2  -- 2019-07-29

* Seventh version revised B. Fixed an issue with empty list. Added most common punctuation patterns.

## 0.2.9.0  -- 2019-07-31

* Eigth version. Added opportunity to read text by sounds. You can now choose whether it would be better to read it 
by syllables and sounds or only by sounds (internally, the program divides words into syllables in both cases). Fixed an issue 
with wrongly sounding long syllables. Added a punctuation constraint for assimilation rules. The function that creates 
sounds and punctuation is completely rewritten using functions with multiple arguments.

## 0.2.9.1  -- 2019-07-31

* Eigth version revised A. Fixed an issue with missed checking for OS. 

## 0.2.9.2  -- 2019-08-01

* Eigth version revised B. Added filter for Unicode symbols to prevent from some extra symbols being substituted. 
Fixed an issues with wrongly sounding parts of syllables. Added a special conversion for '\[' and '\]'. Partially 
optimized a function for punctuation pauses.

## 0.2.9.3  -- 2019-08-03

* Eigth version revised C. Made some optimizations and shortened the code length to make it more readable and shorter.

## 0.3.0.0  -- 2019-08-23

* Ninth version. The program were completely rewritten with performance and further improvements and additions in mind. Removed Text.Replace as a dependency, added Data.ByteSring as a dependency. The program now uses a special conversion to 8bit characters and a lot of functions were rewritten. Added the possibility to read separate numbers in the text by naming 
their separate digits in Ukrainian Nominative clause. Moreover, the program now supports reading by letters the written in 
Latin-1 words asseming the English language for them.

## 0.3.1.0  -- 2019-08-23

* Tenth version. Added some extra documentation and more to the existing one.

## 0.3.2.0  -- 2019-08-24

* Eleventh version. Changed the mechanism how it it checked whether the .exe suffix is needed for espeak ond sox executables. Earlier there could be some difficulties for users with this issue. Now, it should work for all enough good
conditions.

## 0.3.3.0  -- 2019-09-02

* Twelfth version. You can now add voluntary pauses by using a " !Double " pattern
where Double means a normally written Double numeral with (or without) a dot. 
Made documentation more complete and used a module declaration.

## 0.3.4.0  -- 2019-09-03

* Twelfth version revised A. Made some optimizations for predicate functions, 
filtering and voluntary pause creation. 

