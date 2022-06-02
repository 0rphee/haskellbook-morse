module Main where
import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)

main :: IO ()
main = do
    quickCheck prop_half
    quickCheck prop_orderedInt
    quickCheck prop_associative
    quickCheck prop_commuttative
    quickCheck prop_multAssociative
    quickCheck prop_multCommuttative
    quickCheck prop_divMody
    quickCheck prop_quotRem
    quickCheck prop_hatAssociative
    quickCheck prop_hatCommutative
    quickCheck prop_revListTwice
    quickCheck prop_dotSign
    quickCheck prop_dollarSign
    quickCheck prop_concy
    quickCheck prop_funcy
    quickCheck prop_sortWords
    quickCheck prop_capWords
-- Using QuickCheck: Test some arithmetic properties
-- 1. test half
prop_half :: Property
prop_half = forAll arbitrary (\x -> x/2 == half x)

half :: Float -> Float
half x = x /2

-- 2. For any list you apply sort to, this property should hold
prop_orderedInt :: Property
prop_orderedInt = forAll (arbitrary::Gen [Int]) (listOrdered . sort )

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go _ status@(_,False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, _) = (Just y, x >= y)

-- 3. Testing associative and commutative properties of addition
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

prop_associative :: Property
prop_associative = forAll (arbitrary::Gen Int) plusAssociative

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

prop_commuttative :: Property
prop_commuttative = forAll (arbitrary::Gen Int) plusCommutative
-- 4. testing commutative and associative properties for multiplication
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

prop_multAssociative :: Property
prop_multAssociative = forAll (arbitrary::Gen Int) multAssociative

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x + y == y + x

prop_multCommuttative :: Property
prop_multCommuttative = forAll (arbitrary::Gen Int) multCommutative

-- 5. Laws quot-rem div-mod
quotRemy :: Integral a => a -> a -> Bool
quotRemy _ 0 = True
quotRemy x y = (quot x y)*y + (rem x y) == x

prop_quotRem :: Property
prop_quotRem = forAll (arbitrary::Gen Int) quotRemy

divMody :: Integral a => a -> a -> Bool
divMody _ 0 = True
divMody x y = (div x y)*y + (mod x y) == x

prop_divMody :: Property
prop_divMody = forAll (arbitrary::Gen Int) divMody

-- 6. Is (^) associative and commmutative?
hatAssos :: Int -> Int -> Int -> Bool
hatAssos x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_hatAssociative :: Property
prop_hatAssociative = forAll arbitrary hatAssos

hatCommut :: Int -> Int -> Bool
hatCommut x y = x ^ y == y ^ x

prop_hatCommutative :: Property
prop_hatCommutative = forAll arbitrary hatCommut

-- 7. a twice reversed list is the same as the identity
revTwice :: Eq a => [a] -> Bool
revTwice l = (reverse . reverse) l == id l

prop_revListTwice :: Property
prop_revListTwice = forAll (arbitrary::Gen [Int]) revTwice

--- 8. Write a property definition of ($)
dollarSign :: (Bool -> Bool) -> Bool -> Bool
dollarSign f a = f $ a == f a

prop_dollarSign :: Property
prop_dollarSign = forAll (arbitrary::Gen Bool) (dollarSign id)

dotSign :: (Bool -> Bool) -> (Bool -> Bool) -> Bool -> Bool
dotSign f g x = (f . g) x== f (g x)

prop_dotSign :: Property
prop_dotSign = forAll (arbitrary::Gen Bool) (dotSign id id)

-- 9. see if two foldrs al equal
funcy :: Eq a => [a] -> [a] -> Bool
funcy lef ri = foldr (:) ri lef == (++) lef ri

prop_funcy :: Property
prop_funcy = forAll (arbitrary::Gen [Char]) funcy

concy :: (Eq a, Foldable t) => t [a] -> Bool
concy l = foldr (++) [] l == concat l

prop_concy :: Property
prop_concy = forAll (arbitrary::Gen [String]) concy

-- Idempotence
twice :: (b -> b) -> b -> b
twice f = f . f
fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

-- 1.
capitalizeWord :: [Char] -> [Char]
capitalizeWord = map toUpper

capWordCheck :: [Char] -> Bool
capWordCheck x = (capitalizeWord x == twice capitalizeWord x)
      &&
      (capitalizeWord x == fourTimes capitalizeWord x)

prop_capWords :: Property
prop_capWords = forAll (arbitrary::Gen [Char]) capWordCheck

-- 2.
f' :: Ord a => [a] -> Bool
f' x = (sort x == twice sort x)
       &&
       (sort x == fourTimes sort x)
    
prop_sortWords :: Property
prop_sortWords = forAll (arbitrary::Gen [Char]) f'

-- Make a Gen random generator for the datatype
-- 1. Equal probabilities for each
data Fool = Fulse | Frue deriving (Eq, Show)    

eqFool :: Gen Fool
eqFool = elements [Fulse, Frue] 

-- 2. 2/3s chance of Fulse, 1/3 chance of Frue
twoThirds :: Gen Fool
twoThirds = frequency [(2, return Fulse), (1,return Frue)]