module Sandbox where

import Control.Applicative
import Control.Monad

{-
Because type class definitions are global, another instances of type classes
that have already been defined on Maybe will cause compiler errors. Therefore,
the data type must be wrapped in a newtype declaration, and then type classes
are defined on the wrapper type.
 -}
newtype MaybeSo a = MaybeSo (Maybe a)

instance Semigroup (MaybeSo a) where
  (MaybeSo Nothing) <> a = a
  a <> _ = a

instance Monoid (MaybeSo a) where
  mempty = MaybeSo Nothing

instance Monad MaybeSo where
  return a = MaybeSo $ return a
  (MaybeSo (Just a)) >>= k = k a
  _ >>= _ = MaybeSo Nothing

{-
Having defined Semigroup, Monoid, and Monad, all the following type classes
can be derived in a generic way that works for any type that's an instance
of the all above three type classes.
 -}
instance Applicative MaybeSo where
  pure = return
  a <*> b = a >>= \x -> b >>= \y -> return (x y)

instance Functor MaybeSo where
  fmap = (<*>) . pure

{-
This just the definition of Monoid with the added constraint of Monad.
-}
instance MonadPlus MaybeSo where
  mplus = (<>)
  mzero = mempty

{-
Similarly, this just the definition of Monoid with the added constraint of
Applicative.
-}
instance Alternative MaybeSo where
  (<|>) = (<>)
  empty = mempty

--{-
guard' :: (Monad m, Monoid (m ())) => Bool -> m ()
guard' True = return ()
guard' False = mempty

maybeGuard :: Bool -> Maybe ()
maybeGuard = guard'
--}

data List a =
    Nil
  | Cons a (List a)

instance Semigroup a => Semigroup (List a) where
  (Cons x xs) <> (Cons y ys) = Cons (x <> y) $ xs <> ys
  Nil <> xs = xs
  xs <> _ = xs

instance Semigroup a => Monoid (List a) where
  mempty = Nil

concatLists (Cons x xs) ys = Cons x $ concatLists xs ys
concatLists _ ys = ys

instance Monad List where
  return a = Cons a Nil
  (Cons x xs) >>= k = concatLists (k x) $ xs >>= k

instance Applicative List where
  pure = return
  a <*> b = a >>= \x -> b >>= \y -> return (x y)

instance Functor List where
  fmap = (<*>) . pure

-- | @since 2.01
instance MonadPlus List where
  mzero = Nil
  mplus = concatLists

instance Alternative List where
  empty = mzero
  (<|>) = mplus
{-
newtype MaybeNot a = MaybeNot (Maybe a)

instance Semigroup a => Semigroup (MaybeNot a) where
  (MaybeNot a) <> (MaybeNot b) = MaybeNot $ a <> b

instance Semigroup a => Monoid (MaybeNot a) where
  mempty = MaybeNot mempty

instance Monad MaybeNot where
  return a = MaybeNot $ return a
  (MaybeNot (Just a)) >>= k = k a
  _ >>= _ = MaybeNot Nothing

{-
Having defined Semigroup, Monoid, and Monad, all the following type classes
can be derived in a generic way that works for any type that's an instance
of the all above three type classes.
 -}
instance Applicative MaybeNot where
  pure = return
  a <*> b = a >>= \x -> b >>= \y -> return (x y)

instance Functor MaybeNot where
  fmap = (<*>) . pure

{-
This just the definition of Monoid with the added constraint of Monad.
instance MonadPlus MaybeNot where
  mplus :: forall a. Semigroup a => MaybeNot a -> MaybeNot a -> MaybeNot a
  mplus = (<>)
  mzero = mempty
-}

{-
Similarly, this just the definition of Monoid with the added constraint of
Applicative.
-}
instance forall a. Semigroup (MaybeNot a) => Alternative MaybeNot where
  (<|>) = (<>)
  empty = mempty
--}
