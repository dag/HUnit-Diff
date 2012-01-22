module Test.HUnit.Diff where

import Data.Functor        ( (<$>) )

import Data.Algorithm.Diff ( getDiff, DI(B, F, S) )
import System.Console.ANSI ( Color(Green, Red), ColorIntensity(Dull)
                           , ConsoleLayer(Foreground), SGR(SetColor, Reset)
                           , setSGRCode )
import Test.HUnit          ( Assertion, assertBool )
import Text.Groom          ( groom )

(@?==) :: (Eq a, Show a) => a -> a -> Assertion
x @?== y =
    assertBool ('\n':msg) (x == y)
  where
    msg       = unlines $ fmt <$> getDiff (lines . groom $ x)
                                          (lines . groom $ y)
    fmt (B,s) =               ' ' : s
    fmt (F,s) = color Green $ '+' : s
    fmt (S,s) = color Red   $ '-' : s
    color c s = setSGRCode [SetColor Foreground Dull c]
                ++ s ++
                setSGRCode [Reset]
