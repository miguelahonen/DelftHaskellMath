 module Sort where
 
 -- natural numbers
 data Zero
 data Succ a
 
 -- booleans
 data True
 data False
 
 -- lists
 data Nil
 data Cons a b
 
 -- shortcuts
 type One   = Succ Zero
 type Two   = Succ One
 type Three = Succ Two
 type Four  = Succ Three
 
 -- example list
 list1 :: Cons Three (Cons Two (Cons Four (Cons One Nil)))
 list1 = undefined
 
 -- utilities
 numPred :: Succ a -> a
 numPred = const undefined

 -- Error point candicate 1? 
 class Number a where
   numValue :: a -> Int
 
 instance Number Zero where
   numValue = const 0
 instance Number x => Number (Succ x) where
   numValue x = numValue (numPred x) + 1
 
 numlHead :: Cons a b -> a
 numlHead = const undefined
 
 numlTail :: Cons a b -> b
 numlTail = const undefined
 
 class NumList l where
   listValue :: l -> [Int]
 
 instance NumList Nil where
   listValue = const []
 instance (Number x, NumList xs) => NumList (Cons x xs) where
   listValue l = numValue (numlHead l) : listValue (numlTail l)
 :q
 -- comparisons
 data Less
 data Equal
 data Greater

-- Too many parameters for class ‘Cmp’ 
 class Cmp x y c | x y -> c
 
 instance Cmp Zero Zero     Equal
 instance Cmp Zero (Succ x) Less
 instance Cmp (Succ x) Zero Greater
 instance Cmp x y c => Cmp (Succ x) (Succ y) c
 
 -- put a value into one of three lists according to a pivot element
 class Pick c x ls eqs gs ls' eqs' gs' | c x ls eqs gs -> ls' eqs' gs'
 instance Pick Less    x ls eqs gs (Cons x ls) eqs gs
 instance Pick Equal   x ls eqs gs ls (Cons x eqs) gs
 instance Pick Greater x ls eqs gs ls eqs (Cons x gs)
 
 -- split a list into three parts according to a pivot element
 class Split n xs ls eqs gs | n xs -> ls eqs gs
 instance Split n Nil Nil Nil Nil
 instance (Split n xs ls' eqs' gs',
           Cmp x n c,
 	  Pick c x ls' eqs' gs' ls eqs gs) =>
          Split n (Cons x xs) ls eqs gs
 
 listSplit :: Split n xs ls eqs gs => (n, xs) -> (ls, eqs, gs)
 listSplit = const (undefined, undefined, undefined)
 
 -- zs = xs ++ ys
 class App xs ys zs | xs ys -> zs
 instance App Nil ys ys
 instance App xs ys zs => App (Cons x xs) ys (Cons x zs)
 
 -- zs = xs ++ [n] ++ ys
 -- this is needed because
 --
 -- class CCons x xs xss | x xs -> xss
 -- instance CCons x xs (Cons x xs)
 --
 -- doesn't work
 
 class App' xs n ys zs | xs n ys -> zs
 instance App' Nil n ys (Cons n ys)
 instance (App' xs n ys zs) => App' (Cons x xs) n ys (Cons x zs)
 
 -- quicksort
 class QSort xs ys | xs -> ys
 instance QSort Nil Nil
 instance (Split x xs ls eqs gs,
           QSort ls ls',
 	  QSort gs gs',
 	  App eqs gs' geqs,
 	  App' ls' x geqs ys) =>
          QSort (Cons x xs) ys
 
 listQSort :: QSort xs ys => xs -> ys
 listQSort = const undefined

