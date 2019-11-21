{-# LANGUAGE DataKinds
, TypeOperators
, FlexibleContexts
, FlexibleInstances
, TypeApplications
, LambdaCase
, TypeFamilies
, ScopedTypeVariables #-}

module Tutorial where

import Haskus.Utils.Variant


-- | = Pattern matching

x,y :: V '[String,Int]
x = V "test"
y = V @Int 10

f :: V '[String,Int] -> String
f = \case
   V s -> -- Sometimes Variant can infer the type.
     "Found string: " ++ s
   V (n :: Int) -> -- But sometimes a signature is needed.
     "Found int: " ++ show n
   _ -> undefined -- PITFALL: Totality checking always emits
     -- a warning unless there is a default clause like this.


-- | = Skipped: continuation style
--
-- It makes totality checking nicer, but it's complicated.
-- See section 3 of the Variant manual
-- https://docs.haskus.org/variant/safe_pattern_matching.html


-- | Polymorphism

hollerIfString :: (String :< cs) => V cs -> String
  -- Handy shorthand:
  --   (A :< xs, B :< xs, C :< xs)
  -- is equivalent to:
  --   '[A,B,C] :<< xs.
hollerIfString = \case
  V (_ :: String) -> "It's a String!"
  _               -> "Not a String."

testHoller :: String
testHoller = hollerIfString $
  ( V "merp"  -- PITFALL: without the type signature,
    --  GHC doesn't know what this is, so it won't compile.
    :: V '[String,Int] )


-- | = Use `:<?` and `VMaybe` for types that might not be present

hollerIfString' :: (String :<? cs) => V cs -> String
hollerIfString' = \case
   VMaybe (s :: String) -> "It's a String! " ++ s
   _                    -> "Not a String."

testHoller' :: String
testHoller' = hollerIfString' $
  ( V (3 :: Int)
    :: V '[Int] ) -- Note that `String` is not in the list


-- | consuming types

-- `popVariantMaybe` corresponds to `:<?`.
-- `popVariant` corresponds to `:<`.
filterString :: String :<? cs
  => V cs
  -> V (Remove String cs) -- ^ Like `V cs` but without `String`
filterString v =
  case popVariantMaybe v of
    Right (s :: String) -> error ("Found string: " ++ s)
    Left  v'            -> v'

testFilterString_1 :: V '[Int]
testFilterString_1 = filterString $
  (V "nabu" :: V '[String,Int])

testFilterString_2 :: V '[Int]
testFilterString_2 = filterString $
  ( V @Int 10 -- type applications can save some characters
    :: V '[Int] )


-- | `variantToValue` is for unwrapping.
-- (`V` is for wrapping.)

unwrapped :: Int
unwrapped = variantToValue $ V @Int 10


-- | Skipping: `variantFromEither` and `variantToEither`
--
-- They can be used on `Variant`s with exactly 2 possible types.
-- c.f. https://docs.haskus.org/variant/converting.html


-- | Extending a type

test_prependVariant :: V '[Int,Float,String]
test_prependVariant =
  prependVariant @'[Int,Float]
  ( V "word" :: V '[String] )

-- There is also the `Concat` type family,
-- which is `(++)` for type-level lists.

-- In fact there are lots more list operations.
-- c.f. section 5, "converting variants":
-- https://docs.haskus.org/variant/converting.html


-- | = updating (esp. mapping)

test_mapVariant :: V '[Int, String]
test_mapVariant =
  mapVariant (floor :: Float -> Int)
  ( V "test" :: V '[Float,String] )

test_mapVariant_2 :: V '[Int, String]
test_mapVariant_2 =
  mapVariant (floor :: Float -> Int)
  ( V @Float 3 :: V '[Float, String] )
