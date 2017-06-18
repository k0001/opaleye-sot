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

data LambdaSym3 a b c l

type instance Apply (LambdaSym3 a b c) l = Lambda1 a b c l

data LambdaSym2 a b c

type instance Apply (LambdaSym2 a b) c = LambdaSym3 a b c

data LambdaSym1 l k

type instance Apply (LambdaSym1 l) k = LambdaSym2 l k

data LambdaSym0 l

type instance Apply LambdaSym0 l = LambdaSym1 l

type (:&&&$$$$) (y :: TyFun x y4 -> Type)
                (z :: TyFun x z5 -> Type)
                (a :: x3) = (:&&&) y z a

data (:&&&$$$) (h :: TyFun x y -> Type)
               (i :: TyFun x z -> Type)
               (g :: TyFun x (y, z))

type instance Apply ((:&&&$$$) h i) g = (:&&&$$$$) h i g

data (:&&&$$) (e :: TyFun x y -> Type)
              (d :: TyFun (TyFun x z -> Type) (TyFun x (y, z) -> Type))

type instance Apply ((:&&&$$) e) d = (:&&&$$$) e d

data (:&&&$)
     (b :: TyFun (TyFun x y -> Type)
                 (TyFun (TyFun x z -> Type)
                        (TyFun x (y, z) -> Type) -> Type))

type instance Apply (:&&&$) b = (:&&&$$) b

type SecondSym2 (e :: TyFun y6 y7 -> Type)
                (f :: (x8, y6)) = Second e f
data SecondSym1 (j :: TyFun y6 y7 -> Type)
                (i :: TyFun (x8, y6) (x8, y7))

type instance Apply (SecondSym1 j) i = SecondSym2 j i

data SecondSym0 (g ::
   TyFun (TyFun y6 y7 -> Type) (TyFun (x8, y6) (x8, y7) -> Type))

type instance Apply SecondSym0 g = SecondSym1 g

type FirstSym2 (q :: TyFun x9 x10 -> Type)
               (r :: (x9, y11)) = First q r

data FirstSym1 (v :: TyFun x9 x10 -> Type)
               (l :: TyFun (x9, y11) (x10, y11))

type instance Apply (FirstSym1 v) l = FirstSym2 v l

data FirstSym0 (s :: TyFun (TyFun x9 x10 -> Type) (TyFun (x9, y11) (x10, y11) -> Type))

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
