



-- 1. Implement Functor instances for Either e

-- instance Functor (Either' a) where

data Either' a b = Left' a | Right' b deriving (Show)

instance Functor (Either' z) where
-- --- --fmap :: (a -> b) -> Either' Int a -> Either' Int b
--    fmap f (Left' n) = Left' n
--    fmap f (Right' a) = Right' (f a)
    fmap l (Left' n) = Left' n
    fmap l (Right' a) = Right' (l a)

-- *Main> fmap (+2) (Right' 4)       
--         [1] [2]    [3]
---   [1] is fmap
--    [2] is the l which here is (+2) Function.
--    [3] is result of instance of Either'
-- Right' 6

-- (Either' z)  - is the Functor



-- 1. Exercise - Implement Functor instances for ((->) e).


data Fun a b = Fun (a -> b) 

instance Functor (Fun a) where   -- 
    fmap f (Fun g) = Fun (\x -> f (g x))
  

instance Show (Fun a b) where 
    show (Fun g) = "function"

-- *Main Data.Char> fmap show (Fun negate)
-- function



-- 2. Implement Functor instances for ((,) e) 


data Tup a b = Tup a b deriving (Show)
instance Functor (Tup c) where
    fmap f (Tup d e ) = Tup d (f e) 
-- *Main> fmap (+5) (Tup 3 4)
-- Tup 3 9
-- *Main> fmap (+5) (Tup "x" 4)
-- Tup "x" 9




-- 2. Implement Functor instances for Pair, defined as data Pair a = Pair a a

data Pair a = Pair a a deriving (Show)

instance Functor (Pair) where
    fmap f (Pair x y) = Pair (f x) (f y) 
--  [1]  [2]  [3]         [4]
-- [1] is always fmap
-- [2] f is the Function you are applying which is like the (+1) we are using in Prelude with fmap 
-- [3] We need to give the concrete type which is actual value eg. Int is type and 3 is value. Here Pair a is like Int but Pair a a is the value. 
-- [4] this is where applying the function inside the container


-- *Main> fmap (+1) (Pair 3 3)
-- Pair 4 4


-- Explain their similarities and differences.

-- Difference in Pair example it had a a - so they are same type. Whereas Tup we had different types.

-- instance Functor (Two a) where
-- fmap f (Two a b) = Two $ (f a) (f b)
-- This won't fly. We’re not supposed to touch anything in the f referenced in the type of fmap, 
-- so we can’t apply the function (named f in our fmap definition) to the a, because the a is now. 
-- So in Tup we can only apply function on the 2nd argument. whereas Pair allows function to be applied both both.





-- 3. Implement a Functor instance for the type ITree, defined as
-- data ITree a = Leaf (Int -> a) 
--              | Node [ITree a]


instance Functor (ITree) where
  fmap f (Leaf g) = Leaf (f.g)    -- fmap f g
  fmap f (Node []) = Node []
  fmap f (Node xs) = Node (map (fmap f) xs)

data ITree a = Leaf (Int -> a) 
             | Node [ITree a] 


exampleTree = Node [ Node [Node[ Leaf (+5), Leaf (+6)] , Node [Leaf (+7) ] ] , Leaf (+1) , Node [Leaf (+3)] ]

applyAll :: Int -> ITree a -> [a]
applyAll n (Leaf f) = [f n]
applyAll n (Node xs) =  concat (fmap ( applyAll n) xs)



-- *Main> applyAll 5 exampleTree       --- does not use Functor was to just display tree
-- [10,11,12,6,8]

-- *Main> applyAll 5 (fmap (even) exampleTree)     -- uses the Functor Itree.
-- [True,False,True,True,True]
---   fmap (even) exampleTree  ---> Lifts the functioon even to a functin between Itree Int and ITree Bool.



-- 4. Give an example of a type of kind * -> * which cannot be made an instance of Functor (without using undefined).
-- I could not come up with this yet.


-- 5. Is this statement true or false?
-- The composition of two Functors is also a Functor.
-- If false, give a counterexample; if true, prove it by exhibiting some appropriate Haskell code.

-- Answer is yes - 
data Func2 f g a = MkFunc2 (f (g a)) deriving Show   -- 

instance (Functor a, Functor b) => Functor (Func2 a b) where
  fmap f (MkFunc2 (x)) = MkFunc2 (fmap (fmap f) x)


-- *Main> fmap (+1) (MkFunc2 [Just 1, Just 5])
-- MkFunc2 [Just 2,Just 6]








-- Extra practice --- below can be ignored - was just some extra practice while i did the exercise.

-- Extra practice Binary tree

-- *Main> fmap (fmap (+1))  [Just 1, Just 2, Just 3]
-- [Just 2,Just 3,Just 4]

--  Just nomral tree but to show it we used instance Show etc. 

data Tree a = TLeaf a
             | TNode a (Tree a) (Tree a)    -- Diff (Tree a, a)    

instance Show a => Show (Tree a) where
    show tree = case tree of 
        TLeaf l ->  "-" ++ show l ++ "\n"
        TNode a (subtree1) (subtree2) -> "+" ++ show a ++ "\n" ++ show subtree1 ++ show subtree2

treeEx1 :: Tree Int
treeEx1 = TNode 5 (TNode 1 (TLeaf 10) (TNode 3 (TLeaf 1) (TLeaf 7))) 
                 (TNode 4 (TNode 21 (TLeaf 4) (TLeaf 45)) (TLeaf 21))

-- *Main> treeEx1
-- +5
-- +1
-- -10
-- +3
-- -1
-- -7
-- +4
-- +21
-- -4
-- -45
-- -21

instance Functor (Tree) where
  fmap f (TLeaf g) = TLeaf (f g)    -- fmap f g
  fmap f (TNode xs n1 n2) = TNode (f xs) (fmap f n1) (fmap f n2)
-- *Main> fmap (+1) treeEx1
-- +6
-- +2
-- -11
-- +4
-- -2
-- -8
-- +5
-- +22
-- -5
-- -46
-- -22




-- Extra practice --- below can be ignored - was just some extra practice while i did the exercise.


-- extra practice

retChar :: Int -> Char
retChar a = if a > 10 
            then 'a'
            else 'b'


--- Usinig existng --  ((->) e)
-- *Main> retStr = fmap show retChar
-- *Main> retStr 10
-- "'b'"
-- *Main> retChar 5
-- 'b'




data Maybe' a  = Just' a | Nothing' deriving (Show)
-- data MaybeL a = [Maybe' a]
instance Functor (Maybe') where
--    fmap :: (a -> b) -> Maybe' a -> Maybe' b
    fmap f (Nothing') = Nothing'
    fmap f (Just' a) = Just' (f a)

-- *Main> funcAddOne (Just' 5)
-- Just' 6
-- *Main> funcAddOne (funcAddOne (Just' 5))
-- Just' 7


-- *Main> fmap (+2) (Just' 5)     ---- (+2) is func Int-Int  -- fmap (+2) lifts to maybe Int to Maybe Int.
-- Just' 7
-- *Main> fmap (+2) (Nothing')
-- Nothing'


-- ** Function Composition example

intToString :: Int -> String 
intToString a = if a > 5
                then "GT 5"
                else "LE 5"

intToInt :: Int -> Int
intToInt a = a + 2

funcComp :: Int -> String
funcComp = intToString.intToInt


-- extra praactice 
funcAddOne :: (Functor f) => f Int -> f Int
funcAddOne x = fmap (+1) x



-- so this works over different container - Lists, Either , Maybe 
-- *Main> funcAddOne (Right 4)
-- Right 5
-- *Main> funcAddOne [1,2,3,5]
-- [2,3,4,6]
-- *Main> funcAddOne (Just 3)
-- Just 4
-- *Main> funcAddOne (Nothing)
-- Nothing




-- it knows Either' a is Functor
-- *Main> funcAddOne (Right' 4)
-- Right' 5


-- instance Functor ((->) e) where   -- already defined just like Either
--fmap :: (a -> b) -> Either' Int a -> Either' Int b
--     fmap f (g) = f.g


-- using standard one
-- *Main> (funcAddOne (\x -> x + 2)) 2
-- 5


-- *Main> (funcAddOne (+2)) 2
-- 5
--   



main :: IO ()
main = do
    putStrLn(" funcComp 2 = " ++ show(funcComp 2))


--instance Functor (func1 a) where
--fmap :: (a -> b) -> Either' Int a -> Either' Int b
--    fmap f (c) = g () 


--func1 :: a -> b -> c 



--  a -> b is the function g
-- Fa -> Fb by using fmap g  --- so we say we lifted g using fmap g;
-- fmap g.h = (fmap g). (fmap h)







