module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain = forAll charGen
                                (\c -> (charToMorse c
                                 >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_thereAndBackAgain

-- 14.6 Arbitrary instances
-- Babby's First Arbitrary
data Trivial = Trivial deriving (Eq, Show)
instance Arbitrary Trivial where
    arbitrary = trivialGen

trivialGen :: Gen Trivial
trivialGen = return Trivial

sampTrivialGen :: IO [Trivial]
sampTrivialGen = sample' trivialGen

-- Identity Crisis
data Identity a = Identity a deriving (Eq, Show)
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-- Arbitrary Products
data Pair a b = Pair a b deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Pair a b) where
     arbitrary = pairGen

pairGen :: (Arbitrary a, Arbitrary b)
        => Gen (Pair a b)
pairGen = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

-- Greater than the sum of its parts
data Sum a b = First a | Second b deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b)
            => Gen (Sum a b)
sumGenEqual = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

sumGenFirstPls :: (Arbitrary a, Arbitrary b)
               => Gen (Sum a b)
sumGenFirstPls = do
    a <- arbitrary
    b <- arbitrary
    frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

-- Hangman Testing
fillInCharacter :: Puzzle -> Char -> Puzzle 
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (c : s)
    where zipper guessed wordChar guessChar = 
            if wordChar == guessed
            then Just wordChar
            else guessChar 
          newFilledInSoFar = 
            let zd = (zipper c)
            in zipWith zd word filledInSoFar



-- CoArbitrary: counterpart to Arbitrary, generation of functions 
-- fitting a particular type
{- 
    {-# LANGUAGE DeriveGeneric #-}
    module CoArbitrary where import GHC.Generics
    import Test.QuickCheck
    
    data Bool' = True' | False' deriving (Generic)
    instance CoArbitrary Bool'    

    trueGen :: Gen Int
    trueGen = coarbitrary True' arbitrary
    
    falseGen :: Gen Int
    falseGen = coarbitrary False' arbitrary
 
 Essentially this lets you randomly generate a function.
 -}

 -- 14.7 Chapter Exercises
 