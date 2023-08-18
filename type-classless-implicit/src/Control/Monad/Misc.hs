{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Control.Monad.Misc where

import Control.MonadOps
import Data.FixOp
import Data.Functor.Identity
import Data.Function ( fix )
import Data.MonoidOps
import Control.Applicative (Const(Const))
import Data.Coerce (coerce)

ioMonadOps :: MonadOps IO
ioMonadOps = monadOps Prelude.return (Prelude.>>=)

ioApplicativeOps :: ApplicativeOps IO
ioApplicativeOps = let ?monadOps = ioMonadOps in monadApplicativeOps

listMonadOps :: MonadOps []
listMonadOps = monadOps (:[]) $ fix
  ( \g accf xs k ->
      case xs of
      x:xs' -> g (accf . (k x ++)) xs' k
      _ -> accf []
  ) id

listFmapOp :: Fmap []
listFmapOp = fix
  ( \g accf f xs ->
      case xs of
      x:xs' -> g (accf . (f x:)) f xs'
      _ -> accf []
  ) id

listApplicativeOps :: ApplicativeOps []
listApplicativeOps = let ?monadOps = listMonadOps in monadApplicativeOps

{-
Could have instead used:
listFunctorOp = functorFromApplicativeOps listApplicativeOps
--}

listAlternativeOps :: AlternativeOps []
listAlternativeOps = AlternativeOps listApplicativeOps (MonoidLiftOps listMonoidOps) undefined undefined

{-
instance Semigroup a => Semigroup [a] where
  (x:xs) <> (y:ys) = x <> y : xs <> ys
  [] <> xs = xs
  xs <> _ = xs

instance Semigroup a => Monoid [a] where
  mempty = []
--}
assocListMonoidOps :: Assoc a -> MonoidOps [a]
assocListMonoidOps assocF =
  MonoidOps
    ( fix
      ( \g accf xs ys ->
          case xs of
          x:xs' ->
            case ys of
            y:ys' -> g (accf . (assocF x y:)) xs' ys'
            _ -> accf xs
          _ -> accf ys
      ) id
    ) []

semigroupListSemigroupOp :: Assoc a -> Assoc [a]
semigroupListSemigroupOp assocF = let ?monoidOps = assocListMonoidOps assocF in assoc

--{-
listFixLiftOp :: MfixOp []
listFixLiftOp = MfixOp $ fix
  ( \mfix f ->
      case fix (f . head) of
      (x:_) -> x : mfix (tail . f)
      _     -> []
  )
--}

{-
instance Applicative ZipList where
    pure x = ZipList (repeat x)
    liftA2 f (ZipList xs) (ZipList ys) = ZipList (zipWith f xs ys)

-- | @since 4.11.0.0
instance Alternative ZipList where
   empty = ZipList []
   ZipList xs <|> ZipList ys = ZipList (xs ++ drop (length xs) ys)
-}
zipListApplicativeOps :: ApplicativeOps []
zipListApplicativeOps = ApplicativeOps repeat (zipWith id) zipWith

identityMonadOps :: MonadOps Identity
identityMonadOps = monadOps Identity (\m k -> k $ runIdentity m)

identityApplicativeOps :: ApplicativeOps Identity
identityApplicativeOps = _applicativeOps identityMonadOps

identityFunctorOp :: Fmap Identity
identityFunctorOp f = Identity . f . runIdentity

semigroupIdentitySemigroupOp :: Assoc a -> Assoc (Identity a)
semigroupIdentitySemigroupOp assocF (Identity x) (Identity y) = Identity (assocF x y)

monoidIdentityMonoidOps :: (?monoidOps :: MonoidOps a) => MonoidOps (Identity a)
monoidIdentityMonoidOps =
  MonoidOps (semigroupIdentitySemigroupOp assoc)
    $ Identity ident

identityFixLiftOp :: MfixOp Identity
identityFixLiftOp = MfixOp $ \ f -> Identity . fix $ runIdentity . f

maybeMonadOps :: MonadOps Maybe
maybeMonadOps = monadOps Just $ \mx f ->
    case mx of
    Just x -> f x
    _ -> Nothing

maybeMonoidLiftOps :: MonoidOps (Maybe a)
maybeMonoidLiftOps = MonoidOps
  ( \ma mb ->
      case ma of
      Nothing -> mb
      _ -> ma
  ) Nothing

{-
instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> b       = b
    a       <> Nothing = a
    Just a  <> Just b  = Just (a <> b)

instance Semigroup a => Monoid (Maybe a) where
    mempty = Nothing
-}
semigroupMaybeMonoidOps :: Assoc a -> MonoidOps (Maybe a)
semigroupMaybeMonoidOps assocF = MonoidOps
    ( \xm ym ->
        case xm of
        Just x ->
          case ym of
          Just y -> Just (assocF x y)
          _ -> xm
        _ -> ym
    ) Nothing

semigroupMaybeSemigroupOp :: Assoc a -> Assoc (Maybe a)
semigroupMaybeSemigroupOp assocF = let ?monoidOps = semigroupMaybeMonoidOps assocF in assoc

{-
https://gitlab.haskell.org/ghc/ghc/-/issues/9588
instance (Error e) => MonadPlus (Either e) where
    mzero            = Left noMsg
    Left _ `mplus` n = n
    m      `mplus` _ = m

Let's define the Either MonadPlus operations so that they're more generalized
so that the Error constraint (nor any other typeclass constraint) is not
needed.
-}
monoidLiftEitherOps :: e -> MonoidOps (Either e a)
monoidLiftEitherOps e = MonoidOps
  ( \b c ->
      case b of
      Left _ -> c
      _ -> b
  ) $ Left e

--semigroupMonoidOps :: SemigroupOp a -> SemigroupOp (m a)
--semigroupMonoidOps sOp = 

{- instance Monoid m => Applicative (Const m) where
  pure _ = Const mempty
  liftA2 _ (Const x) (Const y) = Const (x `mappend` y)
  (<*>) = coerce (mappend :: m -> m -> m) -}
constApplicativeOps :: (?monoidOps :: MonoidOps m) => ApplicativeOps (Const m)
constApplicativeOps = applicativeOps
  (const $ Const ident) $
  Left (
    coerce assoc,
    const $ \ (Const a) (Const b) -> Const $ a `assoc` b)
