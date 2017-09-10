module Data.Nullable.Safe
  ( Nullable
  , class NeverNull

  , null
  , just
  , nullable
  , neverNull

  , mapNullable, (?<$>)
  , applyNullable, (?<*>)
  , pureNullable
  , bindNullable, (?>>=)
  , bindNullableFlipped, (?=<<)
  , composeKleisliNullable, (?>=>)
  , composeKleisliNullableFlipped, (?<=<)

  , fromMaybe
  , toMaybe
  ) where

import Prelude

import Data.Function (on)
import Data.Maybe (Maybe (..), maybe)



-- | `Nullable a` is either `a` or `null`. The runtime representation is either
-- | that of `a` or `null`.
foreign import data Nullable :: Type -> Type

instance eqNullable :: (Eq a) => Eq (Nullable a) where
  eq = eq `on` toMaybe

instance ordNullable :: (Ord a) => Ord (Nullable a) where
  compare = compare `on` toMaybe

instance showNullable :: (Show a) => Show (Nullable a) where
  show = nullable "null" (\x -> "(just " <> show x <> ")")



-- | Do not instantiate this class unless your type has a runtime representation
-- | that is never `null`.
class NeverNull a

instance neverNullVoid :: NeverNull Void
instance neverNullBoolean :: NeverNull Boolean
instance neverNullChar :: NeverNull Char
instance neverNullInt :: NeverNull Int
instance neverNullNumber :: NeverNull Number
instance neverNullString :: NeverNull String
instance neverNullArray :: NeverNull (Array a)
instance neverNullRecord :: NeverNull {| r}
instance neverNullOrdering :: NeverNull Ordering
instance neverNullMaybe :: NeverNull (Maybe a)



-- | `null`.
null :: ∀ a. NeverNull a => Nullable a
null = nullFFI

-- | `just`.
just :: ∀ a. NeverNull a => a -> Nullable a
just = justFFI

-- | Fold a `Nullable`.
nullable :: ∀ a b. b -> (a -> b) -> Nullable a -> b
nullable = nullableFFI

-- | Recover a `NeverNull` evidence from a `Nullable`. The first argument is
-- | ignored.
neverNull :: ∀ a b. Nullable a -> (NeverNull a => b) -> b
neverNull x f = neverNullFFI x f



-- | Change the value inside a `Nullable` using a function.
mapNullable :: ∀ a b. NeverNull b => (a -> b) -> Nullable a -> Nullable b
mapNullable f = nullable null (just <<< f)
infixl 4 mapNullable as ?<$>

-- | Change the value inside a `Nullable` using a function in another
-- | `Nullable`.
applyNullable :: ∀ a b. NeverNull b => Nullable (a -> b) -> Nullable a -> Nullable b
applyNullable f x = nullable null (\f' -> nullable null (just <<< f') x) f
infixl 4 applyNullable as ?<*>

-- | `just`.
pureNullable :: ∀ a. NeverNull a => a -> Nullable a
pureNullable = just

-- | Change the value inside a `Nullable` using a function and flatten.
bindNullable :: ∀ a b. NeverNull b => Nullable a -> (a -> Nullable b) -> Nullable b
bindNullable x k = nullable null k x
infixl 1 bindNullable as ?>>=

bindNullableFlipped :: ∀ a b. NeverNull b => (a -> Nullable b) -> Nullable a -> Nullable b
bindNullableFlipped = flip bindNullable
infixr 1 bindNullableFlipped as ?=<<

-- | Compositional form of `(?>>=)`.
composeKleisliNullable :: ∀ a b c. NeverNull c => (a -> Nullable b) -> (b -> Nullable c) -> a -> Nullable c
composeKleisliNullable k l x = k x ?>>= l
infixr 1 composeKleisliNullable as ?>=>

composeKleisliNullableFlipped :: ∀ a b c. NeverNull c => (b -> Nullable c) -> (a -> Nullable b) -> a -> Nullable c
composeKleisliNullableFlipped = flip composeKleisliNullable
infixr 1 composeKleisliNullableFlipped as ?<=<



-- | Convert a `Maybe` to a `Nullable`.
fromMaybe :: ∀ a. NeverNull a => Maybe a -> Nullable a
fromMaybe = maybe null just

-- | Convert a `Nullable` to a `Maybe`.
toMaybe :: ∀ a. Nullable a -> Maybe a
toMaybe = nullable Nothing Just



foreign import nullFFI
  :: ∀ a
   . NeverNull a
  => Nullable a

foreign import justFFI
  :: ∀ a
   . NeverNull a
  => a
  -> Nullable a

foreign import nullableFFI
  :: ∀ a b
   . b
  -> (a -> b)
  -> Nullable a
  -> b

foreign import neverNullFFI
  :: ∀ a b
   . Nullable a
  -> (NeverNull a => b)
  -> b
