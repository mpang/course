{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

import Course.Traversable
import Data.Foldable
import Data.Monoid

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
  Functor (Compose f g) where
    (<$>) f (Compose fga) = Compose ((f <$>) <$> fga)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
    -- Implement the pure function for an Applicative instance for Compose
    pure = Compose . pure . pure
    -- Implement the (<*>) function for an Applicative instance for Compose
    (<*>) (Compose fga) (Compose fga') = Compose (lift2 (<*>) fga fga')

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
    -- Implement the (=<<) function for a Monad instance for Compose
    (=<<) = error "impossible: Course.Compose (<<=)#instance (Compose f g)"

instance (Foldable f, Foldable g) =>
  Foldable (Compose f g) where
    foldMap f (Compose fga) = foldMap (foldMap f) fga

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
    traverse f (Compose fga) = pure Compose <*> traverse (traverse f) fga
