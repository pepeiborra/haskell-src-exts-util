module Language.Haskell.Exts.Type
  ( module Language.Haskell.Exts.Type
  ) where

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
