module Vendor.FilePath
       ( normaliseEx
       ) where

import qualified System.FilePath as Native
import System.Info.Extra


-- copy-paste from https://hackage.haskell.org/package/shake-0.18.5/docs/src/Development.Shake.FilePath.html#normaliseEx


-- | Normalise a 'FilePath', applying the rules:
--
-- * All 'pathSeparators' become 'pathSeparator' (@\/@ on Linux, @\\@ on Windows)
--
-- * @foo\/bar\/..\/baz@ becomes @foo\/baz@ (not universally true in the presence of symlinks)
--
-- * @foo\/.\/bar@ becomes @foo\/bar@
--
-- * @foo\/\/bar@ becomes @foo\/bar@
--
--   This function is not based on the 'normalise' function from the @filepath@ library, as that function
--   is quite broken.
normaliseEx :: FilePath -> FilePath
normaliseEx xs | a:b:xs6 <- xs, isWindows && sep a && sep b = '/' : f ('/':xs6) -- account for UNC paths being double //
               | otherwise = f xs
    where
        sep = Native.isPathSeparator
        f o = toNative $ deslash o $ (++"/") $ concatMap ('/':) $ reverse $ g 0 $ reverse $ split o

        deslash o x
            | x == "/" = case (pre,pos) of
                (True,True)   -> "/"
                (True,False)  -> "/."
                (False,True)  -> "./"
                (False,False) -> "."
            | otherwise = (if pre then id else tail) $ (if pos then id else init) x
            where pre = sep $ head $ o ++ " "
                  pos = sep $ last $ " " ++ o

        g i []         = replicate i ".."
        g i ("..":xs1) = g (i+1) xs1
        g i (".":xs2)  = g i xs2
        g 0 (x:xs3)    = x : g 0 xs3
        g i (_:xs4)    = g (i-1) xs4 -- equivalent to eliminating ../x

        split xs5 = if null ys then [] else a : split b
            where (a,b) = break sep ys
                  ys = dropWhile sep xs5

-- | Convert to native path separators, namely @\\@ on Windows.
toNative :: FilePath -> FilePath
toNative = if isWindows then map (\x -> if x == '/' then '\\' else x) else id
