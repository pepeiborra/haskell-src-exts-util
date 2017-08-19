{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.Exts.Type
  ( module Language.Haskell.Exts.Type
  ) where

import           Data.Set              (Set)
import           Language.Haskell.Exts

--------------------------------
-- Convenience type synonyms
type S = SrcSpanInfo
type Module_ = Module S
type Decl_ = Decl S
type Exp_ = Exp S
type Pat_ = Pat S
type Type_ = Type S

-----------------------------------
-- Convenience SrcLoc constructors

nullSrcLoc :: SrcLoc
nullSrcLoc = SrcLoc "" 0 0

nullSrcSpan :: SrcSpan
nullSrcSpan = mkSrcSpan nullSrcLoc nullSrcLoc

an :: SrcSpanInfo
an = toSrcInfo nullSrcLoc [] nullSrcLoc

--------------------------------------------------------------------------
-- Relation mapping AST types to their corresponding spurce location type

class HasSrcLoc a where
  type SrcLocType a

instance HasSrcLoc (Alt s) where
  type SrcLocType (Alt s) = s

instance HasSrcLoc (Binds s) where
  type SrcLocType (Binds s) = s

instance HasSrcLoc (Decl s) where
  type SrcLocType (Decl s) = s

instance HasSrcLoc (Exp s) where
  type SrcLocType (Exp s) = s

instance HasSrcLoc (IPBind s) where
  type SrcLocType (IPBind s) = s

instance HasSrcLoc (Pat s) where
  type SrcLocType (Pat s) = s

instance HasSrcLoc (Rhs s) where
  type SrcLocType (Rhs s) = s

instance HasSrcLoc (GuardedRhs s) where
  type SrcLocType (GuardedRhs s) = s

instance HasSrcLoc (Match s) where
  type SrcLocType (Match s) = s

instance HasSrcLoc (Name s) where
  type SrcLocType (Name s) = s

instance HasSrcLoc (Stmt s) where
  type SrcLocType (Stmt s) = s

instance HasSrcLoc (QualStmt s) where
  type SrcLocType (QualStmt s) = s

instance HasSrcLoc a => HasSrcLoc (Set a) where
  type SrcLocType (Set a) = SrcLocType a

instance HasSrcLoc a => HasSrcLoc [a] where
  type SrcLocType [a] = SrcLocType a

instance HasSrcLoc a => HasSrcLoc (Maybe a) where
  type SrcLocType (Maybe a) = SrcLocType a
