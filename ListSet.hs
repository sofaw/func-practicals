module ListSet (Set, module SetSpec, match, addOrUpdate) where

import SetSpec

type Set a  =  ListSet a

-- invariant: ascending order
data ListSet a  = LS [a]

instance SetSpec ListSet where

  empty                =  LS []

  singleton a          =  LS [a]

  size (LS x)          =  length x

  member _ (LS [])     =  False
  member a (LS (b:x))  =  case compare a b of
                          LT -> False
                          EQ -> True
                          GT -> member a (LS x)

  add a (LS x)         = addOrUpdate a (\n -> n) (LS x)

  delete a (LS x)      = LS (delete' a x)
                         where
                         delete' a [] = []
                         delete' a (b:xs) = case compare a b of
                                           LT -> b : xs
                                           EQ -> xs
                                           GT -> b : delete' a xs  

  LS x `union` LS y    =  LS (x `merge` y)
    where
    []       `merge` x         =  x
    x        `merge` []        =  x
    ax@(a:x) `merge` by@(b:y)  =  case compare a b of
                                  LT -> a : (x  `merge` by)
                                  EQ -> a : (x  `merge` y )
                                  GT -> b : (ax `merge` y )

  LS x `inter` LS y    =  LS (x `like` y)
    where                             
    []       `like` _          =  []
    _        `like` []         =  []
    ax@(a:x) `like` by@(b:y)   =  case compare a b of
                                  LT -> x  `like` by
                                  EQ -> a : (x `like` y)
                                  GT -> ax `like` y

  LS x `diff` LS y     =  LS (x `bar` y)
    where
    []       `bar` _           =  []
    x        `bar` []          =  x
    ax@(a:x) `bar` by@(b:y)    =  case compare a b of
                                  LT -> a : (x `bar` by)
                                  EQ -> x  `bar` y
                                  GT -> ax `bar` y

  elements (LS x)              =  x

-- precondition: 1st arg `member` 2nd
match :: Ord a => a -> Set a -> a
match a (LS (b:x))             =  case compare a b of
                                  EQ -> b
                                  GT -> match a (LS x)

-- precondition: u x == x for all x
addOrUpdate :: Ord a => a -> (a->a) -> Set a -> Set a
addOrUpdate a u (LS x)         =  LS (aOrU x)
         where  aOrU []        =  [a]
                aOrU bx@(b:x)  =  case compare a b of
                                  LT -> a   : bx
                                  EQ -> u b : x
                                  GT -> b   : aOrU x

instance Show a => Show (ListSet a) where
  show = showSet
