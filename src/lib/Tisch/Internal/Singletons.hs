{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tisch.Internal.Singletons
 ( (:&&&)
 , (:&&&$)
 , (:&&&$$)
 , (:&&&$$$)
 , (:&&&$$$$)
 ) where

import Data.Kind
import Data.Singletons.TH

type (:&&&$$$$) (a :: TyFun x y -> Type)
                (b :: TyFun x z -> Type)
                (c :: x) = (:&&&) a b c

data (:&&&$$$) (h :: TyFun x y -> Type)
               (i :: TyFun x z -> Type)
               (g :: TyFun x (y, z))

data (:&&&$$) (e :: TyFun x y -> Type)
              (d :: TyFun (TyFun x z -> Type) (TyFun x (y, z) -> Type))

data (:&&&$)
     (b :: TyFun (TyFun x y -> Type)
                 (TyFun (TyFun x z -> Type)
                        (TyFun x (y, z) -> Type) -> Type))

type instance Apply (:&&&$) b = (:&&&$$) b
type instance Apply ((:&&&$$) a) b = (:&&&$$$) a b
type instance Apply ((:&&&$$$) a b) c = (:&&&$$$$) a b c

type family (:&&&) (k :: TyFun d e -> Type)
                   (l :: TyFun d f -> Type)
                   (m :: d) :: (e, f) where
  (:&&&) p q r = Apply (Apply Tuple2Sym0 (Apply p r)) (Apply q r)

infixr 3 :&&&
