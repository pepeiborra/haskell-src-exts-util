{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- For a description of this program see http://neilmitchell.blogspot.co.uk/2018/02/atomic-expressions-generically.html
module Main(main) where

-- Note that 1 vs -1  is one of the few things that matters!!!

import Language.Haskell.Exts
import Language.Haskell.Exts.Util
import Control.Monad
import Data.Data
import System.Random
import System.IO
import Control.Exception


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let modes = defaultParseMode{extensions = map EnableExtension [minBound .. maxBound]}
    checkAtomicity (fmap void . parseExpWithMode modes) $ \x -> App () x x
    checkAtomicity (fmap void . parseTypeWithMode modes) $ \x -> TyApp () x x
    checkAtomicity (fmap void . parsePatWithMode modes) $ \x -> PApp () (UnQual () $ Ident () "Foo") [x,x]


deriving instance Eq a => Eq (ParseResult a)

checkAtomicity :: forall a . (Pretty a, Eq a, Show a, Data a) => Brackets a => (String -> ParseResult a) -> (a -> a) -> IO ()
checkAtomicity parse wrap =
    forM_ (dataTypeConstrs $ dataTypeOf (undefined :: a)) $ \ctor -> do
        putStr $ show (typeOf (undefined :: a)) ++ " " ++ show ctor ++ " ... "
        ans <- replicateM 10000 $ do
            handle (\LimitReached -> return False) $ do
                x :: a <- fromConstrM (mkValue 50) ctor
                if parse (prettyPrint x) /= ParseOk x then
                    return False
                else do
                    let seemsAtomic = parse (prettyPrint $ wrap x) == ParseOk (wrap x)
                    let saysAtomic = isAtom x
                    when (not seemsAtomic && isAtom x) $
                        putStrLn $ unlines $
                            [""
                            ,"DIAGREE!:"
                            ,"  " ++ prettyPrint x
                            ,"  " ++ show x
                            ,"  isAtom = " ++ show saysAtomic]
                    return $ seemsAtomic == saysAtomic
        putStrLn $ "agreed for " ++ show (length $ filter id ans)


randomElem :: [a] -> IO a
randomElem xs = do
    when (null xs) $ fail "General.Extra.randomElem called with empty list, can't pick a random element"
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i


data LimitReached = LimitReached deriving Show
instance Exception LimitReached

mkValue :: forall a . Data a => Int -> IO a
mkValue depth
    | Just x <- cast "aA1:+-" = randomElem x
    | Just x <- cast [-1 :: Int, 1] = randomElem x
    | Just x <- cast [-1 :: Integer, 1] = randomElem x
    | AlgRep cs <- dataTypeRep $ dataTypeOf (undefined :: a) =
        if depth <= 0 then throwIO LimitReached else fromConstrM (mkValue $ depth - 1) =<< randomElem cs
    | otherwise = error $ "mkDefault doesn't know how to generate value of type " ++ show (typeOf (undefined :: a))
