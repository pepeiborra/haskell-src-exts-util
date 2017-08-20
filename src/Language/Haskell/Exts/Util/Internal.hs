{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | The contents of this module originate from module
--  [HSE.Util](https://github.com/ndmitchell/hlint/blob/master/src/HSE/Util.hs)
--  in Neil Mitchell's HLint.
module Language.Haskell.Exts.Util.Internal where

import           Control.Monad
import           Data.Data                   hiding (Fixity)
import           Data.Default
import           Data.Functor
import           Data.Generics.Uniplate.Data
import           Data.List
import           Data.Maybe
import           Language.Haskell.Exts
import           Prelude

---------------------------------------------------------------------
-- ACCESSOR/TESTER

-- is* :: Exp l -> Bool
-- is* :: Decl s -> Bool
isApp App{} = True; isApp _ = False
isAnyApp x = isApp x || isInfixApp x
isInfixApp InfixApp{} = True; isInfixApp _ = False

isDot :: QOp s -> Bool
isDot (QVarOp _ (UnQual _ (Symbol _ "."))) = True
isDot _                                    = False
isSection LeftSection{}  = True
isSection RightSection{} = True
isSection _              = False

isDotApp :: Exp s -> Bool
isDotApp (InfixApp _ _ dot _) | isDot dot = True
isDotApp _                    = False

isLexeme Var{} = True
isLexeme Con{} = True
isLexeme Lit{} = True
isLexeme _     = False

