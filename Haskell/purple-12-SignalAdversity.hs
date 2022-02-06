
import Data.Maybe
import Data.Typeable
import Data.List

-- https://stackoverflow.com/questions/5217171/how-can-i-get-nth-element-from-a-list/23627631#23627631
-- import Control.Lens -- Did not work

-- https://github.com/mvaldesdeleon/haskell-book -- answers to exercises


-- 12.1 Signaling adversity
--chapter will include:
-- • Nothing, or Just Maybe.
-- • Either left or right, but not both.
-- • Higher-kindedness.
-- • Anamorphisms, but not animorphs



-- 12.2 How to stop worrying and love Nothing
-- definition of Maybe again:
-- data Maybe a = Nothing | Just a

--ifEvenAdd2 :: Integer -> Integer
--ifEvenAdd2 n =
--if even n then n + 2 else ???

-- “Hey, this number isn’t even, so I have nothing for you, my friend?” Instead of promising an Integer result, we can return Maybe Integer:


-- ************************************************************ Maybe / Just / Nothing ************************************************

-- And here’s how we fix it:
ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n =
    if even n then Just (n+2) else Nothing 

-- We have to parenthesize (n + 2), because function application  binds the most tightly in Haskell

-- below is mine
maybeInt :: Maybe Integer -> Integer
maybeInt x = if ifEvenAdd2 (fromJust (x)) == Nothing 
             then 0
             else fromJust (ifEvenAdd2 (fromJust (x)))
-- ************************************************************ fromJust ************************************************


-- Smart constructors for datatypes

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show   --- this makes it print 


-- There are already a few problems here. One is that we could construct a Person with an empty String for a name or make
--     a person who is negative years old.

-- This is no problem to fix with Maybe, though:
mkPerson1 :: Name -> Age -> Maybe Person
mkPerson1 name age
    | name /= "" && age >= 0 =
        Just $ Person name age
    | otherwise = Nothing

p2 = mkPerson1 "John Browning" (-1)

-- ************************************************************ Smart Constructor ************************************************
-- mkPerson is what we call a smart constructor. It allows us to construct values of a type only when they meet certain criteria,

-- This is much better than our original version, but what if we want to know whether it is the name, age, or both that are invalid?
-- Fortunately, we have a datatype for that!


-- 12.3 Bleating either-- 

-- We want a way to express why we don’t get a successful result back from our mkPerson constructor. 
-- To handle that, we’ve got the Either datatype, which

-- data Either a b = Left a | Right b
-- ************************************************************ Either ************************************************
data PersonInvalid = NameEmpty
    | AgeTooLow
    deriving (Eq, Show)
-- This signifies that we’re going to get a Person value if we succeed but a PersonInvalid if it fails.


-- ************************************************************ deriving (Eq, Show) ************************************************
-- By now, you know why we derive Show, but it’s important that we derive Eq, as well, because otherwise we can’t equality check the constructors. 

-- Pattern matching is a case expression, where the data constructor is the condition. Case expressions and 
--   pattern matching will work without an Eq instance, but guards using == will not. As





mkPerson :: Name
    -> Age
    -> Either PersonInvalid Person
--       [1]      [2]         [3]
mkPerson name age
    | name /= "" && age >= 0 =
        Right $ Person name age
--               [4]
    | name == "" = Left NameEmpty
--                     [5]
    | otherwise = Left AgeTooLow


-- 1. Our mkPerson type takes a Name and Age and returns an Either result.
-- 2. The Left result of the Either is an invalid person, when either the name or age is an invalid input.
-- 3. The Right result is a valid Person.
-- 4. The first case of our mkPerson function, then, matches on the Right constructor of the Either and returns a Person result. We could have written:
--            name /= "" && age >= 0 =
--            Right (Person name age)
--        Instead of using the dollar sign operator.

-- 5. The next two cases match on the Left constructor and allow us to tailor our invalid results based on the failure reasons. 
--    We can pattern match on Left, because it’s one of the constructors of Either.


-- We use Left as our invalid or error constructor for a couple of reasons. 
-- The reason has to do with the ordering of type arguments and application of functions
-- Normally, it is your error or invalid result that is going to cause to stop whatever work is being done by your program. 
-- Functor will not map over the left type argument, because it is applied away.
-- Since you normally want to apply functions and map over the case that doesn’t stop your program
-- (that is, not the error case), it has become convention that the Left of Either is used for whatever case is going to cause the work to stop.

typeMkPerson = typeOf (mkPerson "Djali" 5) 
mkPersonDjali = mkPerson "Djali" 5

-- bad data:
badName = mkPerson "" 10
ageLow = mkPerson "Djali" (-1)

-- Notice in the last example that when both the name and the age are wrong, we’re only going to see the result of the first failure case, not both.
-- This is imperfect in one respect, as it doesn’t let us express a list of errors. We can fix this, too! 

-- instead of validating all the data for a Person at once, we’re going to make separate checking functions and then combine the results. 
-- We’ll see means of abstracting patterns like this out later

ageOkay :: Age
     -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
   True -> Right age
   False -> Left [AgeTooLow]

nameOkay :: Name
   -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
   True -> Right name
   False -> Left [NameEmpty]

-- We can nest the PersonInvalid sum type right into the Left position of Either

-- The Name value will only return this invalid result when it’s an empty String.
-- If you try to substitute an Integer for the name, you won’t get a Left result, you’ll get a type error
-- We’re going to return a list of PersonInvalid results. 
-- That will allow us to return both NameEmpty and AgeTooLow in cases where both of those are true.

type ValidatePerson a =
    Either [PersonInvalid] a


mkPersonNew :: Name
    -> Age
    -> ValidatePerson Person
--            [1]       [2]
mkPersonNew name age =
    mkPerson' (nameOkay name) (ageOkay age)
--      [3]        [4]              [5]


mkPerson' :: ValidatePerson Name
    -> ValidatePerson Age
    -> ValidatePerson Person             -- ValidaPerson you see can work on Name, Age or Person 
--            [6]

mkPerson' (Right nameOk) (Right ageOk) =
    Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) =
    Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge

-- 1. A type alias for Either [PersonInvalid] a.
-- 2. This is the a argument to the ValidatePerson type.
-- 3. Our main function now relies on a similarly-named helper function.
-- 4. The first argument to this function is the result of the nameOkay function.
-- 5. The second argument is the result of the ageOkay function.
-- 6. The type relies on the synonym for Either.
-- The rest of our helper function mkPerson' consists of plain old pattern matches.

mkBothErr = mkPersonNew "" (-1)   -- Left [NameEmpty,AgeTooLow]


-- Ahh, that’s more like it. This time, we can tell the user what is incorrect in one go without them having to round-trip each mistake!



-- 12.4 Kinds, a thousand stars in your types
-- Kinds are types one level up. They are used to describe the types of type constructors.
-- ************************************************************ Kinds / higher-kinded types ************************************************
-- ************************************************************ higher order Functions ************************************************
-- One noteworthy feature of Haskell is that it has higher-kinded types. 
-- The term “higher-kinded” derives from higher-order functions, functions that take more functions as arguments. 

-- Type constructors (that is, higherkinded types) are types that take more types as arguments.
-- ************************************************************ type constant - types with no args - ALREADY CONCRETE TYPE **************************
-- The Haskell Report uses the term type constant to refer to types that take no arguments and are already concrete types
-- ************************************************************ type constructor ************************************************
-- In the Report, type constructor is used to refer to a type that must have arguments applied in order to become a concrete type

-- ******************************   examples of type constants:
-- Prelude> :kind Int
-- Int :: *
-- Prelude> :k Bool
-- Bool :: *
-- Prelude> :k Char
-- Char :: *

-- ************************************************************ :: syntax ************************************************
-- The :: syntax usually means “has the type of,” but it is used for kind signatures as well as type signatures.

-- following is an example of a type that has a type constructor rather than a type constant:
--  -- ************************************************************ Example type constructor ************************************************
data Example a = Blah | Woot a
-- Example is a type constructor rather than a constant, because it takes a type argument a that is used with the Woot data constructor.

-- In GHCi, we can query kinds with the :k command:
-- Prelude> :k Example
-- Example :: * -> *

-- mine
-- Prelude> data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq,Show)    --- This is type constant since it does not take argument
-- Prelude> :k PersonInvalid
-- PersonInvalid :: *
-- Prelude> :k Int
-- Int :: *               -- PersonInvalid is simiilar to int in that sense

-- ************************************************** 2-tuple takes two arguments
-- Example has one parameter, so it must be applied to one type in order to become a concrete type represented by a single *.
-- The 2-tuple takes two arguments, so it must be applied to two types to become a concrete type:
-- Prelude> :k (,)
-- (,) :: * -> * -> *
-- Prelude> :k (Int, Int)
-- (Int, Int) :: *

-- ************************************************** Maybe/Either - also have type constructors
-- The Maybe and Either datatypes we’ve just reviewed also have type constructors rather than constants. 
-- They have to be applied to an argument before they become concrete types
-- As with the effect of currying in type signatures, applying Maybe to an a type constructor relieves us of one arrow and 
-- gives it the kind *, or star:

-- https://wiki.haskell.org/Currying
-- Currying is the process of transforming a function that takes multiple arguments in a tuple as its argument, 
-- into a function that takes just a single argument and returns another function which accepts further arguments, one by one, 
-- that the original function would receive in the rest of that tuple.
-- f :: a -> (b -> c)     -- which can also be written as    f :: a -> b -> c
-- is the curried form of
-- g :: (a, b) -> c


-- Prelude> :k Maybe
--Maybe :: * -> *
--Prelude> :k Maybe Int
--Maybe Int :: *

-- ***************************************** Either 2 args
-- On the other hand, Either has to be applied to two arguments, an a and a b, so the kind of Either is “star to star tostar”:
-- Prelude> :k Either
-- Either :: * -> * -> *


-- ***************************************** effects of applying it to arguments
-- And, again, we can query the effects of applying it to arguments:
-- Prelude> :k Either Int
-- Either Int :: * -> *
-- Prelude> :k Either Int String
-- Either Int String :: *


-- **************************************** kind * represents a concrete type - 
-- As we’ve said, the kind * represents a concrete type. There is nothing left awaiting application.

-- ************************************** Lifted and unlifted types ********************************************************************
-- Lifted and unlifted types To be precise, kind * is the kind of all standard lifted types, while types that have the kind # are unlifted.
-- ********************** Kind * is Lifted  ***************************
-- ********************** Kind # is UnLifted  ***************************
-- lifted type, which includes any datatype you could define yourself, is any that can be inhabited by bottom.
-- are represented by a pointer and include most of the datatypes we’ve seen

-- 12.5 Chapter exercises






-- Exercise 1- String processing

string1 = "Rafa nadal is the goat with 21 grand slams. The greatest!"
words1 = words string1

replaceString :: [String] -> [String]
replaceString [] = []
replaceString [x] = [replaceThe x] 
replaceString (x:xs) = replaceThe x : replaceString xs

replaceThe :: String -> String
replaceThe x = if notThe x == Nothing 
               then "a"
               else x

notThe :: String -> Maybe String 
notThe s = if s == "the"
           then Nothing
           else Just s
-- notThe = undefined



-- Exercise 1- String processing
string2 = "this is 2nd exercise the cow the evil cow the greatest the undoubtably greatest the"
words2 = words string2
vowels2 = "aeiou"

vowelInitial :: String -> Bool
vowelInitial [] = False
vowelInitial [c] = elem c vowels2
vowelInitial (c:cs) = elem c vowels2      --- care of onlt 1st letter

-- countTheBeforeVowel :: String -> Integer
-- countTheBeforeVowel s = 
--           let w2 = words s


indices2 = elemIndices "the" words2

countThe :: [String] -> [Int]
countThe [] = [0]
countThe [x] = if notThe x == Nothing
               then [1]
               else []
countThe (x:xs) = if notThe x == Nothing 
               then  (1 : countThe xs )
               else countThe xs

-- indicesNextWork :: [Integer] -> Maybe [Integer]
-- indicesNextWork [] = []
-- indicesNextWork [x] = 

length2 = length words2
indicesNextWord :: [Int] -> [Int]
indicesNextWord [] = []
indicesNextWord [x] = []
indicesNextWord (x:xs) = if (x+1) <= length2
                         then (x+1) : (indicesNextWord xs)
                         else indicesNextWord xs

finalIndices = indicesNextWord indices2

getFinalWords :: [Int] -> [String]
getFinalWords [] = []
getFinalWords [x] = [words2 !! x]
getFinalWords (x:xs) = ((words2 !! x ) : getFinalWords xs)

finalWords = getFinalWords finalIndices



finalCount :: [String] -> [Int]
finalCount [] = [0]
finalCount [x] = if vowelInitial x == True
                 then [1]
                 else [0]
finalCount (x:xs) = if vowelInitial x == True
                 then ( 1 : finalCount xs )
                 else (finalCount xs)

finalVowelCount = sum (finalCount finalWords)


main :: IO ()
main = do 
    putStrLn (" ifEvenAdd2 7 = " ++ show(ifEvenAdd2 7))  -- Nothing since its odd
    putStrLn (" ifEvenAdd2 8 = " ++ show(ifEvenAdd2 8)) -- Just 10 since its even adds 2
    putStrLn (" 10 + maybeInt 20  = " ++ show(10 + (maybeInt (Just 20))))   --  10 + maybeInt 20  = 32 
    putStrLn (" 10 + maybeInt 21  = " ++ show(10 + (maybeInt (Just 21))))  --  10 + maybeInt 21  = 10 
    putStrLn (" mkPerson1 John Browning 160 = " ++ show(mkPerson1 "John Browning" 160))    -- Just (Person "John Browning" 160)   -- 
    putStrLn (" mkPerson1 John Browning 160 = " ++ show(mkPerson1 "John Browning" (-1160))) -- Nothing
    putStrLn (" typeOf (mkPerson Djali 5) = " ++ show(typeMkPerson))
    putStrLn (" mkPerson Djali 5 = " ++ show(mkPersonDjali))
    putStrLn (" mkPerson __ 10 = " ++ show(badName))
    putStrLn (" mkPerson Djali -1 = " ++ show(ageLow))
    putStrLn (" mkPersonNew both error mkPerson empty (-1) = " ++ show(mkBothErr))
    putStrLn ("string1 = " ++ show(string1))
    putStrLn ("string1 to words1 = " ++ show(words1))
    putStrLn (" Replaced String as list = " ++ show(unwords(replaceString words1)))
    putStrLn (" notThe Rafa = " ++ show(notThe "Rafa"))
    putStrLn (" notThe the = " ++ show(notThe "the"))
    putStrLn (" vowelInitial cow = " ++ show(vowelInitial "cow"))
    putStrLn (" vowelInitial \"evil\" = " ++ show(vowelInitial "evil"))
    putStrLn "This sentence is not a \"sentence\" "
    putStrLn (" words2 = " ++ show(words2))
    putStrLn (" CountThe words2 = " ++ show(countThe words2))
    putStrLn (" the Indices in word2 = " ++ show(indices2))
    putStrLn (" the Next Indices of the in word2 = " ++ show(finalIndices))
    putStrLn (" = " ++ show( [1,2,3]!!1))
    putStrLn (" = " ++ show( words2!!1))
    putStrLn (" Final words next of the = " ++ show(finalWords))
    putStrLn (" finalVowelCount = " ++ show(finalVowelCount))












    