{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Enum, Bounded, Show)

showDigit ::
  Digit
  -> Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq, Show)

-- Possibly convert a character to a digit.
fromChar ::
  Char
  -> Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars
  -> Chars
dollars string = dn ++ " and " ++ cn
  where
    (d, c) = toDot string
    dn     = case d of
      Nil           -> "zero dollars"
      (Zero :. Nil) -> "zero dollars"
      (One :. Nil)  -> "one dollar"
      _             -> translateDigits d ++ " dollars"
    cn     = case c of
      Nil                  -> "zero cents"
      (Zero :. One :. Nil) -> "one cent"
      (c' :. Nil)          -> showDigit3 (D2 c' Zero) ++ " cents"
      (c' :. c'' :. _)     -> showDigit3 (D2 c' c'') ++ " cents"

-- helper functions

translateDigits :: List Digit -> Chars
translateDigits digits = translateDigitsHelper Nil (digitsToDigit3s $ reverseDigits digits) illion
  where
  translateDigitsHelper :: List Chars -> List Digit3 -> List Chars -> Chars
  translateDigitsHelper accum Nil _                               = unwords accum
  translateDigitsHelper _ _ Nil                                   = error "Number too big"
  translateDigitsHelper accum (D1 Zero :. ds) (_ :. is)           = translateDigitsHelper accum ds is
  translateDigitsHelper accum (D2 Zero Zero :. ds) (_ :. is)      = translateDigitsHelper accum ds is
  translateDigitsHelper accum (D3 Zero Zero Zero :. ds) (_ :. is) = translateDigitsHelper accum ds is
  translateDigitsHelper accum (d :. ds) (i :. is)                 = translateDigitsHelper ((showDigit3 d ++ space i) :. accum) ds is

  space :: Chars -> Chars
  space "" = ""
  space s  = ' ' :. s

  digitsToDigit3s :: List Digit -> List Digit3
  digitsToDigit3s Nil                = Nil
  digitsToDigit3s (a :. Nil)         = D1 a :. Nil
  digitsToDigit3s (a :. b :. Nil)    = D2 a b :. Nil
  digitsToDigit3s (a :. b :. c :. d) = D3 a b c :. digitsToDigit3s d

  reverseDigits :: List Digit -> List Digit
  reverseDigits = reverseDigitsHelper . reverse
    where
      reverseDigitsHelper Nil                = Nil
      reverseDigitsHelper (a :. Nil)         = a :. Nil
      reverseDigitsHelper (a :. b :. Nil)    = b :. a :. Nil
      reverseDigitsHelper (a :. b :. c :. d) = c :. b :. a :. reverseDigitsHelper d

toDot :: Chars -> (List Digit, List Digit)
toDot string =
  case c of
    Nil -> (stringToDigits d, Nil)
    (_ :. c') -> (stringToDigits d, stringToDigits c')
  where
    (d, c) = break (== '.') string

    stringToDigits :: Chars -> List Digit
    stringToDigits Nil    = Nil
    stringToDigits string' = string' >>= helper
      where
        helper c'' = case fromChar c'' of
          Empty  -> Nil
          Full d' -> d' :. Nil

showDigit3 :: Digit3 -> Chars
showDigit3 d = case d of
  (D1 d') -> showDigit d'
  (D2 Zero d') -> showDigit d'
  (D2 One d') -> case d' of
    Zero  -> "ten"
    One   -> "eleven"
    Two   -> "twelve"
    Three -> "thirteen"
    Four  -> "fourteen"
    Five  -> "fifteen"
    Six   -> "sixteen"
    Seven -> "seventeen"
    Eight -> "eighteen"
    Nine  -> "nineteen"
  (D2 Two d') -> "twenty" .++. d'
  (D2 Three d') -> "thirty" .++. d'
  (D2 Four d') -> "forty" .++. d'
  (D2 Five d') -> "fifty" .++. d'
  (D2 Six d') -> "sixty" .++. d'
  (D2 Seven d') -> "seventy" .++. d'
  (D2 Eight d') -> "eighty" .++. d'
  (D2 Nine d') -> "ninety" .++. d'
  (D3 Zero Zero Zero) -> ""
  (D3 Zero d' d'') -> showDigit3 $ D2 d' d''
  (D3 d' Zero Zero) -> showDigit d' ++ " hundred"
  (D3 d' d'' d''') -> showDigit d' ++ " hundred and " ++ showDigit3 (D2 d'' d''')
  where
    prefix .++. digit = case digit of
      Zero -> prefix
      _    -> prefix ++ "-" ++ showDigit digit
