{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Tisch.Internal.Singletons
 ( (:&&&)
 , (:&&&$)
 , (:&&&$$)
 , (:&&&$$$)
 , (:&&&$$$$)
 ) where

import           Data.Kind
import           Data.Singletons.TH

type (:&&&$$$$) (y :: TyFun x y4 -> Type)
                (z :: TyFun x z5 -> Type)
                (a :: x3) = (:&&&) y z a

data (:&&&$$$) (h :: TyFun x y -> Type)
               (i :: TyFun x z -> Type)
               (g :: TyFun x (y, z))

data (:&&&$$) (e :: TyFun x y -> Type)
              (d :: TyFun (TyFun x z -> Type) (TyFun x (y, z) -> Type))

data (:&&&$)
     (b :: TyFun (TyFun x y -> Type)
                 (TyFun (TyFun x z -> Type)
                        (TyFun x (y, z) -> Type) -> Type))

type family (:&&&) (k :: TyFun d e -> Type)
                   (l :: TyFun d f -> Type)
                   (m :: d) :: (e, f) where
  (:&&&) p q r = Apply (Apply Tuple2Sym0 (Apply p r)) (Apply q r)

infixr 3 :&&&
