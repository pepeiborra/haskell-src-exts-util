{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.Exts.Located
  ( Located(..)
  ) where

import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Language.Haskell.Exts

--------------------------------------------------------------------------
-- | Class of types containing source code locations
class Located a where
  type LocType a

  -- | Traversal providing access to the location details
  location :: forall f . Applicative f => (LocType a -> f (LocType a)) -> a -> f a

instance Located (Alt s) where
  type LocType (Alt s) = s
  location = locationForAnnotatedType

instance Located (Binds s) where
  type LocType (Binds s) = s
  location = locationForAnnotatedType

instance Located (Decl s) where
  type LocType (Decl s) = s
  location = locationForAnnotatedType

instance Located (Exp s) where
  type LocType (Exp s) = s
  location = locationForAnnotatedType

instance Located (IPBind s) where
  type LocType (IPBind s) = s
  location = locationForAnnotatedType

instance Located (Pat s) where
  type LocType (Pat s) = s
  location = locationForAnnotatedType

instance Located (Rhs s) where
  type LocType (Rhs s) = s
  location = locationForAnnotatedType

instance Located (GuardedRhs s) where
  type LocType (GuardedRhs s) = s
  location = locationForAnnotatedType

instance Located (Match s) where
  type LocType (Match s) = s
  location = locationForAnnotatedType

instance Located (Name s) where
  type LocType (Name s) = s
  location = locationForAnnotatedType

instance Located (Stmt s) where
  type LocType (Stmt s) = s
  location = locationForAnnotatedType

instance Located (QOp s) where
  type LocType (QOp s) = s
  location = locationForAnnotatedType

instance Located (QualStmt s) where
  type LocType (QualStmt s) = s
  location = locationForAnnotatedType

instance Located a => Located [a] where
  type LocType [a] = LocType a
  location = traverse.location

instance Located a => Located (Maybe a) where
  type LocType (Maybe a) = LocType a
  location = traverse.location

instance (Located a, Ord a) => Located (Set a) where
  type LocType (Set a) = LocType a
  -- | Valid only if it doesn't remove elements from the 'Set'
  location = traverseSet.location
    where
      traverseSet f s = Set.fromList <$> traverse f (Set.toList s)

instance (Located a, Located b, LocType a ~ LocType b) => Located (a,b) where
  type LocType (a,b) = LocType a
  location f (a,b) = (,) <$> location f a <*> location f b

locationForAnnotatedType
  :: (Functor f, Annotated ast)
  => (t -> f t) -> ast t -> f (ast t)
locationForAnnotatedType f x = (\v -> amap (const v) x) <$> f (ann x)
