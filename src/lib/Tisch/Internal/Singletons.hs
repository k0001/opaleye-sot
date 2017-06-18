-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Tisch.Internal.Singletons
 ( First
 , FirstSym0
 , FirstSym1
 , FirstSym2

 , Second
 , SecondSym0
 , SecondSym1
 , SecondSym2

 , (:&&&)
 , (:&&&$)
 , (:&&&$$)
 , (:&&&$$$)
 , (:&&&$$$$)
 ) where

import           Data.Kind
import           Data.Singletons.TH

type Lambda1 p q r u = Apply (Apply Tuple2Sym0 (Apply p u)) (Apply q u)

data LambdaSym3 a b c l =
  forall d. KindOf (Apply (LambdaSym3 a b c) d) ~ KindOf (Lambda1 a b c d) =>
             LambdaSym3KindInference

type instance Apply (LambdaSym3 a b c) l = Lambda1 a b c l

data LambdaSym2 a b c
  = forall z. KindOf (Apply (LambdaSym2 a b) z) ~ KindOf (LambdaSym3 a b z) =>
    LambdaSym2KindInference

type instance Apply (LambdaSym2 a b) c = LambdaSym3 a b c

data LambdaSym1 l k
  = forall a. KindOf (Apply (LambdaSym1 l) a) ~ KindOf (LambdaSym2 l a) =>
    LambdaSym1KindInference

type instance Apply (LambdaSym1 l) k = LambdaSym2 l k

data LambdaSym0 l
  = forall a1. KindOf (Apply LambdaSym0 a1) ~ KindOf (LambdaSym1 a1) =>
    LambdaSym0KindInference

type instance Apply LambdaSym0 l = LambdaSym1 l

type (:&&&$$$$) (y :: TyFun x y4 -> Type)
                (z :: TyFun x z5 -> Type)
                (a :: x3) = (:&&&) y z a

data (:&&&$$$) (h :: TyFun x y -> Type)
               (i :: TyFun x z -> Type)
               (g :: TyFun x (y, z))
  = forall j. KindOf (Apply ((:&&&$$$) h i) j) ~ KindOf ((:&&&$$$$) h i j) =>
    (:&&&$$$###)

type instance Apply ((:&&&$$$) h i) g = (:&&&$$$$) h i g

data (:&&&$$) (e :: TyFun x y -> Type)
              (d :: TyFun (TyFun x z -> Type) (TyFun x (y, z) -> Type))
  = forall f. KindOf (Apply ((:&&&$$) e) f) ~ KindOf ((:&&&$$$) e f) =>
    (:&&&$$###)

type instance Apply ((:&&&$$) e) d = (:&&&$$$) e d

data (:&&&$)
     (b :: TyFun (TyFun x y -> Type)
                 (TyFun (TyFun x z -> Type)
                        (TyFun x (y, z) -> Type) -> Type))
  = forall a. KindOf (Apply (:&&&$) a) ~ KindOf ((:&&&$$) a) =>
    (:&&&$###)

type instance Apply (:&&&$) b = (:&&&$$) b

type SecondSym2 (e :: TyFun y6 y7 -> Type)
                (f :: (x8, y6)) = Second e f
data SecondSym1 (j :: TyFun y6 y7 -> Type)
                (i :: TyFun (x8, y6) (x8, y7))
  = forall k. KindOf (Apply (SecondSym1 j) k) ~ KindOf (SecondSym2 j k) =>
    SecondSym1KindInference

type instance Apply (SecondSym1 j) i = SecondSym2 j i

data SecondSym0 (g ::
   TyFun (TyFun y6 y7 -> Type) (TyFun (x8, y6) (x8, y7) -> Type)) =
  forall h. KindOf (Apply SecondSym0 h) ~ KindOf (SecondSym1 h) =>
            SecondSym0KindInference

type instance Apply SecondSym0 g = SecondSym1 g

type FirstSym2 (q :: TyFun x9 x10 -> Type)
               (r :: (x9, y11)) = First q r

data FirstSym1 (v :: TyFun x9 x10 -> Type)
               (l :: TyFun (x9, y11) (x10, y11))
  = forall w. KindOf (Apply (FirstSym1 v) w) ~ KindOf (FirstSym2 v w) =>
    FirstSym1KindInference

type instance Apply (FirstSym1 v) l = FirstSym2 v l

data FirstSym0 (s :: TyFun (TyFun x9 x10 -> Type) (TyFun (x9, y11) (x10, y11) -> Type))
  = forall a. KindOf (Apply FirstSym0 a) ~ KindOf (FirstSym1 a) =>
    FirstSym0KindInference

type instance Apply FirstSym0 s = FirstSym1 s

type family (:&&&) (k :: TyFun d e -> Type)
                   (l :: TyFun d f -> Type)
                   (m :: d) :: (e, f) where
  (:&&&) p q r = Apply (Apply (Apply (Apply LambdaSym0 p) q) r) r

type family Second (l :: TyFun g h -> Type)
                   (m :: (i, g)) :: (i, h) where
  Second n '(o, p) = Apply (Apply Tuple2Sym0 o) (Apply n p)

type family First (x :: TyFun j k -> Type)
                  (y :: (j, l)) :: (k, l) where
  First z '(a, b) = Apply (Apply Tuple2Sym0 (Apply z a)) b

infixr 3 :&&&

instance SuppressUnusedWarnings (:&&&$) where
  suppressUnusedWarnings _ = snd ((,) (:&&&$###) ())
instance SuppressUnusedWarnings (:&&&$$) where
  suppressUnusedWarnings _ = snd ((,) (:&&&$$###) ())
instance SuppressUnusedWarnings (:&&&$$$) where
  suppressUnusedWarnings _ = snd ((,) (:&&&$$$###) ())
instance SuppressUnusedWarnings FirstSym0 where
  suppressUnusedWarnings _ = snd ((,) FirstSym0KindInference ())
instance SuppressUnusedWarnings FirstSym1 where
  suppressUnusedWarnings _ = snd ((,) FirstSym1KindInference ())
instance SuppressUnusedWarnings SecondSym0 where
  suppressUnusedWarnings _ = snd ((,) SecondSym0KindInference ())
instance SuppressUnusedWarnings SecondSym1 where
  suppressUnusedWarnings _ = snd ((,) SecondSym1KindInference ())
instance SuppressUnusedWarnings LambdaSym0 where
  suppressUnusedWarnings _ = snd ((,) LambdaSym0KindInference ())
instance SuppressUnusedWarnings LambdaSym1 where
  suppressUnusedWarnings _ = snd ((,) LambdaSym1KindInference ())
instance SuppressUnusedWarnings LambdaSym2 where
  suppressUnusedWarnings _ = snd ((,) LambdaSym2KindInference ())
instance SuppressUnusedWarnings LambdaSym3 where
  suppressUnusedWarnings _ = snd ((,) LambdaSym3KindInference ())
