-- | A free "monoid sans laws" type (i.e., a "free pointed magma") with an
-- illegal 'Monoid' instance, intended for debugging.
--
-- An example use: We can see that the 'Foldable' instance for Data.Map in
-- @containers-0.5.0.0@ generates a lot of 'mempty's (one per leaf):
--
-- @
-- > 'foldMap' 'N' (M.fromList [(x,x) | x <- [1..5]])
-- (((ε ◇ N 1) ◇ ε) ◇ N 2) ◇ ((((ε ◇ N 3) ◇ ε) ◇ N 4) ◇ ((ε ◇ N 5) ◇ ε))
-- @
--
-- After a discussion with the maintainer, this is improved in
-- @containers-0.5.5.1@:
--
-- @
-- > 'foldMap' 'N' (M.fromList [(x,x) | x <- [1..5]])
-- (N 1 ◇ (N 2 ◇ N 3)) ◇ (N 4 ◇ N 5)
-- @
--
-- But now we can see a discrepancy between the 'Foldable' and 'Traversable'
-- instances:
--
-- @
-- > 'foldMapDefault' 'N' (M.fromList [(x,x) | x <- [1..5]])
-- (((N 1 ◇ N 2) ◇ N 3) ◇ N 4) ◇ N 5
-- @
--
-- This is because an expression like @f '<$>' x '<*>' y '<*>' z@ generates a
-- left-biased tree -- @(x '<>' y) '<>' z@ -- whereas the 'Foldable' instance makes a
-- right-biased tree -- @x '<>' (y '<>' z)@.
--
-- Due to the monoid laws, these sorts of issues are typically invisible unless
-- you look for them. But they can make a performance difference.

module Data.Monoid.Nonfree (N(..), (◇), ε, toN, fromN) where

import Control.Applicative
import Data.Monoid
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse, sequenceA), foldMapDefault, fmapDefault)

-- | Nonfree nonmonoid.
data N a = N a | NEmpty | NAppend (N a) (N a)

instance Monoid (N a) where
  mempty = NEmpty
  mappend = NAppend

instance Traversable N where
  traverse f (N x) = N <$> f x
  traverse f NEmpty = pure NEmpty
  traverse f (NAppend a b) = NAppend <$> traverse f a <*> traverse f b

instance Functor N where
  fmap = fmapDefault

instance Foldable N where
  foldMap = foldMapDefault

-- | The 'Show' instance uses short names to make the append trees readable.
instance Show a => Show (N a) where
  showsPrec n (N a) = showString "N " . showsPrec 11 a
  showsPrec n NEmpty = showString "ε"
  showsPrec n (NAppend x y) = showParen (n > 0) $ showsPrec 1 x . showString " ◇ " . showsPrec 1 y

-- | A synonym for 'mappend' ('<>').
(◇) :: Monoid m => m -> m -> m
(◇) = mappend

-- | A synonym for 'mempty'.
ε :: Monoid m => m
ε = mempty

-- | A version of 'Data.Foldable.toList' that extracts the full monoid append
-- tree rather than flattening it to a list.
toN :: Foldable t => t a -> N a
toN = foldMap N

newtype FromN e a = FromN { runFromN :: N e -> a }

instance Functor (FromN e) where
  -- We can't use fmap = liftA because our Applicative instance is illegal.
  fmap f (FromN k) = FromN (fmap f k)

instance Applicative (FromN e) where
  pure x = FromN $ \r -> case r of
    NEmpty -> x
    _ -> error "FromN pure: invalid structure (expected NEmpty)"
  FromN mf <*> FromN mx = FromN $ \r ->
    case r of
      NAppend a b -> mf a (mx b)
      _ -> error "FromN (<*>): invalid structure (expected NAppend)"

-- | Given a monoid append tree and a 'Traversable' structure with exactly the
-- same shape, put values from the former into the latter. This will fail with
-- an error if the structure isn't identical.
fromN :: Traversable t => N b -> t a -> t b
fromN n = (`runFromN` n) . traverse (\_ -> FromN deleaf)
  where
    deleaf :: N e -> e
    deleaf (N x) = x
    deleaf _ = error "fromN traverse: invalid structure (expected N)"
