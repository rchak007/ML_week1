import Data.Monoid

import Test.QuickCheck
import Control.Monad

-- Run as
-- stack ghc --package QuickCheck -- purple-15-monoid-semigroup.hs


-- 15.3 Monoid



m2 = mappend [4, 8, 10] mempty
m3 = mappend mempty [5, 9, 11]


-- 15.4 How Monoid is defined in Haskell
-- The type class Monoid is defined like this:
-- class Semigroup m => Monoid m where
-- mempty :: m
-- mappend :: m -> m -> m
-- mconcat :: [m] -> m
-- mconcat = foldr mappend mempty

-- 15.5 Examples of using Monoid
-- List
m1 = mappend [1, 2, 3] [4, 5, 6]
mc1 = mconcat [[1..3], [4..6]]
plusplus = (++) [3,2,1] [6,5,4]
f = foldr (++) [] [[10..13], [14..16]]
-- https://byorgey.wordpress.com/2012/11/05/foldr-is-made-of-monoids/

f2 =  foldr mappend mempty [[1..3], [4..6]]

-- 15.6 Why Integer doesn’t have a Monoid
-- To resolve the conflict, we have the Sum and Product newtypes to wrap numeric values and signal which Monoid instance we want.

sum1 = mappend (Sum 1) (Sum 5)
prod1 = mappend (Product 5) (Product 5)

-- Why newtype?

-- Prelude> import Data.Monoid
-- Prelude> :info Sum
-- newtype Sum a = Sum {getSum :: a}
-- ...some instances elided...
-- instance Num a => Monoid (Sum a)

-- Prelude> :info Product
-- newtype Product a =
-- Product {getProduct :: a}
-- ...some instances elided...
-- instance Num a => Monoid (Product a)

x = Sum 1
y = Sum 2
z = Sum 3
sum3 = mappend x (mappend y z)


--  (<>) 
-- Prelude Data.Monoid> :t (<>)
-- (<>) :: Monoid m => m -> m -> m
sum4 = Sum 1 <> Sum 5 <> Sum 10

mcon1  =  mconcat [Sum 8, Sum 10, Sum 21]

--Prelude Data.Monoid> :t mconcat
-- mconcat :: Monoid a => [a] -> a

getsum1 = getSum $ mappend x y
yprod = Product  5
getprod1 = getProduct $ mappend yprod yprod

getsum3 = getSum $ mconcat [x, y, z]       -- with $ is just brackets
getsum4 = getSum (mconcat [x, y, z])


-- 15.7 Why bother?

-- Monoids are even strongly associated with the concept of folding or catamorphism—something we do all the time in Haskell. 
-- You’ll see this more explicitly in Chapter 20, but here’s a taste:Monoids are even strongly associated with the concept
-- of folding or catamorphism—something we do all the time in Haskell. You’ll see this more explicitly in Chapter 20, but here’s a taste:

xs = [2, 4, 6] :: [Product Int]
fol1 = foldr mappend mempty xs

ys = [2, 4, 6] :: [Sum Int]
sumfol1 = foldr mappend mempty ys

strList = ["blah", "woot"]
strListFold = foldr mappend mempty strList


-- 15.8 Laws


-- We care about the laws a Monoid instance must adhere to, because we want our programs to be correct wherever possible.
-- Proofs are programs, and programs are proofs. We


-- Monoid instances must abide by the following laws:
-- left identity
-- mappend mempty x = x
-- right identity
-- mappend x mempty = x

-- associativity
-- mappend x (mappend y z) =
-- mappend (mappend x y) z
-- mconcat = foldr mappend mempty

-- Any laws that apply to mappend will also apply to the <> method defined by Semigroup.

-- ********************************  Monoid **********************************************************
-- A monoid is a binary associative operation with an identity.

-- 1. Monoid: The thing we’re talking about—monoids. That’ll end up being the name of our type class.
-- 2. Binary, i.e., two. So, there will be two of something.
-- 3. Associative—this is a property or law that must be satisfied. You’ve seen associativity with addition and multiplication.
--    We’ll explain it more in a moment.

-- 4. Operation—so called because in mathematics, it’s usually used as an infix operator. You can read this interchangeably as “function.” 
-- Note that given the mention of “binary” earlier, we know that this is a function of two arguments.

-- 5. Identity is one of those words in mathematics that pops up a lot. In this context, we can take this to mean there’ll be some value which, 
-- when combined with any other value, will always return that other value. This can be seen most immediately with examples.

-- For lists, we have a binary operator, ++, that joins two lists together. We can also use a function, mappend, 
--  from the Monoid type class to do the same thing
-- *****************************    For lists, the empty list, [], is the identity value: *********************************


-- Here is how the identity law looks in practice:
-- -- left identity
leftI = mappend mempty (Sum 1) -- Sum {getSum = 1}

-- -- right identity
rightI = mappend (Sum 1) mempty -- Sum {getSum = 1}


-- We can demonstrate associativity using the infix operator <> from the Semigroup class. Remember that mappend and <> should be identical in behavior
-- Associativity:
associativity1 = (Sum 1) <> (Sum 2 <> Sum 3)
associativity2 = (Sum 1 <> Sum 2) <> (Sum 3)


-- https://www.splashlearn.com/math-vocabulary/addition/associative-property
-- ********************************  Associatiivity  **********************************************************
-- This property states that when three or more numbers are added (or multiplied), 
-- the sum (or the product) is the same regardless of the grouping of the addends (or the multiplicands).
-- Grouping means the use of parentheses or brackets to group numbers.
-- Associative property involves 3 or more numbers.
-- The numbers that are grouped within a parenthesis or bracket become one unit.
-- Associative property can only be used with addition and multiplication and not with subtraction or division.
-- Example of Associative Property for Addition 
-- 4 + (5+3)  = (4 + 5) + 3 = 12

-- some more examples here for identiy and associativity with Lists



-- 15.9 Different instance, same representation

-- Monoid is somewhat different from other type classes in Haskell, in that many datatypes have more than one valid monoid. 
-- We saw that for numbers, both addition and multiplication are sensible monoids with different behaviors. 
-- When we have more than one potential implementation for Monoid for a datatype, it’s most convenient to use newtypes to distinguish them, 
-- as we did with Sum and Product.

-- We mentioned above that monoids are important to folding and catamorphisms, more generally. 

-- the Monoid instances for Bool
-- Boolean values have two possible monoids—a monoid of conjunction and a monoid of disjunction.

bool1 = All True <> All True
bool2 = All True <> All False
bool3 = Any True <> Any False
bool4 = Any False <> Any False

-- Prelude Data.Monoid> :i <>
-- type Semigroup :: * -> Constraint
-- class Semigroup a where
--  (<>) :: a -> a -> a
--  ...
--  	-- Defined in ‘GHC.Base’
--infixr 6 <>


-- All represents Boolean conjunction: it returns a True if and only if all values it is “appending” are True. 
-- Any is the monoid of Boolean disjunction: it returns a True if any value is True.

-- Prelude Data.Monoid> :t All
-- All :: Bool -> All
-- Prelude Data.Monoid> :t Any
-- Any :: Bool -> Any

-- The Maybe type has more than two possible Monoids. We’ll look at each in turn, but the two that have an obvious relationship
-- are First and Last.

-- First returns the first or leftmost non-Nothing value:
fir1 = First (Just 1)
fir2 = First (Just 2)
first1 = fir1 `mappend` fir2
fir3 = First (Nothing)
fir4 = First (Just 2)
first2 = fir3 `mappend` fir4
fir5 = First (Nothing)
fir6 = First (Nothing)
first3 = fir5 `mappend` fir6
-- printFirst3 :: First a -> IO ()
-- printFirst3 b = if b == First (Nothing)
--                 then return()
--                 else putStrLn (" Success First ")
                

foo :: Int -> IO ()
foo a 
   | a > 100 = return ()    -- Does not print anything
   | otherwise = putStrLn "YES!!!"



-- 15.10 Reusing algebras by asking for algebras
-- We alluded to there being more possible Monoids for Maybe than just First and Last. Let’s write that other Monoid instance.
-- will now be concerned not with choosing one value out of a set of values but of combining the a values contained within the Maybe a type.

-- notice a pattern:
-- instance Monoid b => Monoid (a -> b)
-- instance (Monoid a, Monoid b)
-- => Monoid (a, b)
-- instance (Monoid a, Monoid b, Monoid c)
-- => Monoid (a, b, c)





-- Exercise: Optional Monoid

data Optional a =
       Nada
     | Only a
     deriving (Eq, Show)
instance Monoid a
     => Monoid (Optional a) where
  mempty = Nada -- undefined
  -- mappend = undefined


instance Semigroup a => Semigroup (Optional a) where
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only a') = Only (a <> a')
  Nada <> Nada = Nada

-- Gave error w/o the Semi group.Below link gave solution
-- https://stackoverflow.com/questions/52237895/could-not-deduce-semigroup-optional-a-arising-from-the-superclasses-of-an-in

onlySum = Only (Sum 1)
opt1 = onlySum `mappend` onlySum

onlyFour = Only (Product 4)
onlyTwo = Only (Product 2)
prodOpt1 = onlyFour `mappend` onlyTwo

sumNada = Only (Sum 1) `mappend` Nada

sum1Nada = Only [1] `mappend` Nada
sum1_2 = Only [1] `mappend` Only [2]    -- Mine - these are list examples 

-- 15.11 Madness

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String

madlibbinBetter' e adv noun adj = 
    mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]

madlib1 = madlibbinBetter' "Blacky" "while" "Dodge" "pretty"  


-- 15.12 Better living through QuickCheck

-- Proving laws can be tedious, especially if the code we’re checking is in the middle of changing frequently.
-- Accordingly, having a cheap way to get a sense of whether or not the laws are likely to be obeyed by an instance is pretty useful. 
-- QuickCheck happens to be an excellent way to accomplish this.

-- Validating associativity with QuickCheck

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
            (a <> (b <> c)) == ((a <> b) <> c)
type S = String
type B = Bool
type MA = S -> S -> S -> B

quickcheck1 = quickCheck (monoidAssoc :: MA)   -- +++ OK, passed 100 tests.

-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Eq.html
-- The Eq class defines equality (==) and inequality (/=). All the basic datatypes exported by the Prelude are instances of Eq, 
--     and Eq may be derived for any datatype whose constituents are also instances of Eq.


-- Testing left and right identity


monoidLeftIdentity :: (Eq m, Monoid m)
                   => m 
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidRightIdentity a = (a <> mempty) == a
mli = monoidLeftIdentity
mri = monoidRightIdentity


-- Testing QuickCheck’s patience
-- Let us see an example of QuickCheck catching us out for having an invalid Monoid.
-- We’re going to demonstrate why a Bool Monoid can’t have False as the identity, always returning the value False, and still be a valid Monoid.

data Bull =
    Fools
    | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Semigroup Bull where
    (<>) _ _ = Fools

instance Monoid Bull where
    mempty = Fools


type BullMappend =
    Bull -> Bull -> Bull -> Bool




main :: IO ()
main = do
    putStrLn (" m1 mappend = " ++ show(m1))
    putStrLn (" m2 mappend mempty = " ++ show(m2))
    putStrLn (" m3 mappend mempty = " ++ show(m3))
    putStrLn (" mc1 moncat = " ++ show(mc1))
    let g = " goes well with garlic"
    putStrLn (" mc1 moncat = " ++ show(mappend "Trout" g))
    putStrLn (" ++ = " ++ show(plusplus))
    putStrLn (" foldr ++ = " ++ show(f))
    putStrLn (" foldr mappend = " ++ show(f2))
    putStrLn (" sum = " ++ show(sum1))
    putStrLn (" prod1 = " ++ show(prod1))
    putStrLn (" sum3 3 variables = " ++ show(sum3))
    putStrLn (" sum4 <> operator = " ++ show(sum4))
    putStrLn (" mconcat over sum = " ++ show(mcon1))
    putStrLn (" getsum = " ++ show(getsum1))
    putStrLn (" getProduct = " ++ show(getprod1))
    putStrLn (" getsum concat w $ = " ++ show(getsum3))
    putStrLn (" getsum concat w  brackets = " ++ show(getsum4))
    putStrLn (" prod int declare and fold = " ++ show(fol1))
    putStrLn (" sum int declare and fold = " ++ show(sumfol1))
    putStrLn (" String and fold = " ++ show(strListFold))
    putStrLn (" Left Identity = " ++ show(leftI))
    putStrLn (" Right Identity = " ++ show(rightI))
    putStrLn (" Bool1  = " ++ show(bool1))
    putStrLn (" Bool2  = " ++ show(bool2))
    putStrLn (" Bool3  = " ++ show(bool3))
    putStrLn (" Bool4  = " ++ show(bool4))
    putStrLn (" First1  = " ++ show(first1))
    putStrLn (" First with 1st Nothing and 2nd Just = " ++ show(first2))
    foo 50
    foo 1000   -- Does not print anything 
    putStrLn (" Opt1  = " ++ show(opt1))
    putStrLn ("ProdOpt1  = " ++ show(prodOpt1))
    putStrLn ("sumNada  = " ++ show(sumNada))
    putStrLn ("sum1Nada  = " ++ show(sum1Nada))
    putStrLn ("sum1_2  = " ++ show(sum1_2))
    putStrLn ("madlib  = " ++ show(madlib1))
    quickCheck (monoidAssoc :: MA)
    quickCheck (mli :: String -> Bool)
    quickCheck (mri :: String -> Bool)
    let ma = monoidAssoc
        mli1 = monoidLeftIdentity
        mri1 = monoidRightIdentity
    putStrLn (" Quickcheck  ma :: BullMappend : ")
    quickCheck (ma :: BullMappend)
    putStrLn (" mli1 :: Bull -> Bool : ")
    quickCheck (mli1 :: Bull -> Bool)
    putStrLn (" mli1 :: Bull -> Bool: ")
    quickCheck (mri1 :: Bull -> Bool)
    -- verboseCheck monoidAssoc
    -- putStrLn ("QuickCheck associativity  = " ++ show(quickcheck1))
    -- if first3 == First {getFirst = Nothing}
    --   then return ()
    --   else putStrLn (" Success ")
    -- putStrLn (" First with both Nothing = " ++ show(first3))       -- Errors 
   -- print Nothing -- ERrors 






