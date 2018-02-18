{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where

-- Note that 1 vs -1  is one of the few things that matters!!!

import Language.Haskell.Exts
import Language.Haskell.Exts.Util
import Data.Data
import Data.List

main = do
    let dtype = dataTypeOf (undefined :: Exp ())
    let ctors = dataTypeConstrs dtype
    let exps = map (fromConstrB mkDefault) ctors :: [Exp ()]
    let (good, bad) = partition safe exps
    putStrLn $ "FAILS TO ROUND TRIP = " ++ intercalate "; " (map disp bad)
    let (atom, comp) = partition checkAtom good
    putStrLn $ "SHOULD BE ATOM = " ++ intercalate "; " [disp x | x <- atom, not $ isAtom x]
    putStrLn $ "SHOULD BE COMPOSITE = " ++ intercalate "; " [disp x | x <- comp, isAtom x]

    let dtype = dataTypeOf (undefined :: Pat ())
    let ctors = dataTypeConstrs dtype
    let exps = map (fromConstrB mkDefault) ctors :: [Pat ()]
    let (good, bad) = partition safeP exps
    putStrLn $ "FAILS TO ROUND TRIP = " ++ intercalate "; " (map disp bad)
    let (atom, comp) = partition checkAtomP good
    putStrLn $ "SHOULD BE ATOM = " ++ intercalate "; " [disp x | x <- atom, not $ isAtom x]
    putStrLn $ "SHOULD BE COMPOSITE = " ++ intercalate "; " [disp x | x <- comp, isAtom x]

    let dtype = dataTypeOf (undefined :: Type ())
    let ctors = dataTypeConstrs dtype
    let exps = map (fromConstrB mkDefault) ctors :: [Type ()]
    let (good, bad) = partition safeT exps
    putStrLn $ "FAILS TO ROUND TRIP = " ++ intercalate "; " (map disp bad)
    let (atom, comp) = partition checkAtomT good
    putStrLn $ "SHOULD BE ATOM = " ++ intercalate "; " [disp x | x <- atom, not $ isAtom x]
    putStrLn $ "SHOULD BE COMPOSITE = " ++ intercalate "; " [disp x | x <- comp, isAtom x]


disp x = ctor x ++ " " ++ prettyPrint x

ctor x = head . words . show $ x

modes = defaultParseMode{extensions = map EnableExtension [minBound .. maxBound]}

checkAtom x = safe (App () x x)
checkAtomP x = safeP (PApp () (UnQual () $ Ident () "Foo") [x,x])
checkAtomT x = safeT (TyApp () x x)

safe x = case parseExpWithMode modes (prettyPrint x) of
    ParseOk y -> x == fmap (const ()) y
    _ -> False

safeP x = case parsePatWithMode modes (prettyPrint x) of
    ParseOk y -> x == fmap (const ()) y
    _ -> False

safeT x = case parseTypeWithMode modes (prettyPrint x) of
    ParseOk y -> x == fmap (const ()) y
    _ -> False

mkDefault :: forall a . Data a => a
mkDefault
    | Just x <- cast () = x
    | Just x <- cast "foo" = x
    | Just x <- cast qname = x
    | Just x <- cast name = x
    | Just x <- cast $ Signless () = x
    | Just x <- cast (1 :: Integer) = x
    | Just x <- cast (1 :: Int) = x
    | Just x <- cast $ IPDup () "foo" = x
    | Just x <- cast $ Int () 1 "1" = x
    | Just x <- cast exp = x
    | Just x <- cast [exp, exp] = x
    | Just x <- cast [typ, typ] = x
    | Just x <- cast [RPPat () pat] = x
    | Just x <- cast [Just exp, Nothing] = x
    | Just x <- cast [FieldUpdate () qname exp] = x
    | Just x <- cast [PFieldPat () qname pat] = x
    | Just x <- cast [QualStmt () stmt] = x
    | Just x <- cast qop = x
    | Just x <- cast Boxed = x
    | Just x <- cast [pat] = x
    | Just x <- cast $ Just pat = x
    | Just x <- cast typ = x
    | Just x <- cast [[QualStmt () stmt]] = x
    | Just x <- cast $ BDecls () [] = x
    | Just x <- cast [GuardedRhs () [] exp] = x
    | Just x <- cast [Alt () pat rhs Nothing] = x
    | Just x <- cast [stmt] = x
    | Just x <- cast $ ExpBracket () exp = x
    | Just x <- cast $ IdSplice () "foo" = x
    | Just x <- cast xname = x
    | Just x <- cast $ Just exp = x
    | Just x <- cast $ Just [UnkindedVar () $ Ident () "foo"] = x
    | Just x <- cast $ Just $ CxSingle () $ WildCardA () Nothing = x
    | Just x <- cast pat = x
    | Just x <- cast $ PromotedName () qname = x
    | Just x <- cast $ PromotedUnit () = x
    | Just x <- cast $ KindStar () = x
    | Just x <- cast $ BangedTy () = x
    | Just x <- cast $ NoUnpackPragma () = x
    | Just x <- cast $ Just name = x
    | Just x <- cast (1 :: Int, 1 :: Int) = x
    | Just x <- cast [XAttr () xname exp] = x
    | Just x <- cast [PXAttr () xname pat] = x
    | otherwise = error $ "mkDefault doesn't know how to generate value of type " ++ show (typeOf (undefined :: a))
    where
        qname = UnQual () name
        name = Ident () "foo"
        exp = Var () qname
        qop = QVarOp () $ UnQual () $ Ident () "foo"
        pat = PVar () name
        rhs = UnGuardedRhs () exp
        typ = TyVar () name
        stmt = Qualifier () exp
        xname = XName () "foo"
