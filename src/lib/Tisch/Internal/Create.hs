{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | This module deals with generating database creation statements from a
-- Tisch database description.
module Tisch.Internal.Create
 ( makeCreateStatement
 ) where

import           Data.Proxy
import           Data.Singletons
import           Data.Text                     as T
import           GHC.TypeLits
import qualified Opaleye.PGTypes               as O
import           Text.InterpolatedString.Perl6
import           Tisch
import           Tisch.Internal.Table

-- | Generate a CREATE statement for a given table
makeCreateStatement
  :: forall table
   . ( Tisch.Internal.Table.All ColumnInfo (Columns table)
     , ConsumeTypeList (Columns table)
     , KnownSymbol (TableName table)
     )
  => Table table
  -> Text
makeCreateStatement = colInfosCreateStatement (pack tableName) . colInfos
  where
    tableName = symbolVal (Proxy @(TableName table))

-- | Get the term-level information about a table
colInfos
  :: forall table
   . ( Tisch.Internal.Table.All ColumnInfo (Columns table)
     , ConsumeTypeList (Columns table)
     )
  => Table table
  -> [ColInfo]
colInfos _ = dis (Proxy @ColumnInfo) colInfo (Proxy @(Columns table))

-- TODO: Support default and read only
-- | Get a create statement from a description of a table.
colInfosCreateStatement :: Text -> [ColInfo] -> Text
colInfosCreateStatement tableName is
  = [qc|CREATE TABLE "{tableName}" ({intercalate "," args});|]
  where
    args = colInfoCreateArg <$> is
    colInfoCreateArg :: ColInfo -> Text
    colInfoCreateArg (Column name _wcap rcap unique pgtype ()) =
      let uniqueSuffix :: Text
          uniqueSuffix = case unique of
                           NotUnique -> ""
                           Unique    -> " UNIQUE"
          nnSuffix :: Text
          nnSuffix = case rcap of
                       R  -> " NOT NULL"
                       RN -> ""
      in [qc|"{name}" {unPGType pgtype}{uniqueSuffix}{nnSuffix}|]

-- | A handy synonym for a term level column info represenation
type ColInfo = Column Text WCap RCap Unique PGType ()

-- | A class to reify a Column
class ColumnInfo col where
  colInfo :: proxy col -> ColInfo

instance (KnownSymbol name, SingI wcap, SingI rcap, SingI unique, O.IsSqlType (PgType pgtype))
  => ColumnInfo ('Column name (wcap :: WCap) (rcap :: RCap) (unique :: Unique) pgtype hsType) where
  colInfo _ = Column (pack . symbolVal $ Proxy @name)
                     (fromSing (sing :: Sing wcap))
                     (fromSing (sing :: Sing rcap))
                     (fromSing (sing :: Sing unique))
                     (pgType (Proxy @pgtype))
                     ()

-- | A string representing a postgres type
newtype PGType = PGType { unPGType :: Text }
  deriving (Show)

pgType :: forall a . O.IsSqlType (PgType a) => Proxy a -> PGType
pgType _ = PGType . pack . O.showPGType $ Proxy @(PgType a)

--
-- Misc
--

class ConsumeTypeList (as :: [k]) where
  dis :: Tisch.Internal.Table.All c as
      => Proxy c
      -> (forall x. c x => Proxy (x :: k) -> b)
      -> Proxy as
      -> [b]

instance ConsumeTypeList '[] where
  dis _ _ _ = []

instance ConsumeTypeList as => ConsumeTypeList (a ': as) where
  dis c f _ = f (Proxy @a) : dis c f (Proxy @as)
