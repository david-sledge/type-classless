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
ioMonadOps = pkgMonadOps Prelude.return (Prelude.>>=)

ioApplicativeOps :: ApplicativeOps IO
ioApplicativeOps = _monadApplicativeOps ioMonadOps

listFunctorOps :: FunctorOps []
listFunctorOps = pkgFunctorOps $
  let g accf f (x : xs) = g (accf . (f x:)) f xs
      g accf _ _ = accf []
  in g id

listMonadOps :: MonadOps []
listMonadOps =
  let monadOps = pkgMonadOps (:[]) $
        let g accf (x : xs) k = g (accf . (k x ++)) xs k
            g accf _ _ = accf []
        in g id
  in monadOps {_monadApplicativeOps = (_monadApplicativeOps monadOps) { _applicativeFunctorOps = listFunctorOps }}

listApplicativeOps :: ApplicativeOps []
listApplicativeOps = _monadApplicativeOps listMonadOps

listAlternativeOps :: AlternativeOps []
listAlternativeOps = pkgAlternativeOps listApplicativeOps $ MonoidLiftOps listMonoidOps

--{-
listFixLiftOp :: MfixOp []
listFixLiftOp = MfixOp $ fix
  ( \lfix f ->
      case fix (f . head) of
      x : _ -> x : lfix (tail . f)
      _     -> []
  )
--}

{- instance Applicative ZipList where
    pure x = ZipList (repeat x)
    liftA2 f (ZipList xs) (ZipList ys) = ZipList (zipWith f xs ys) -}
zipListApplicativeOps :: ApplicativeOps []
zipListApplicativeOps = pkgApplicativeOps repeat . Right $ Right zipWith

{- instance Alternative ZipList where
   empty = ZipList []
   ZipList xs <|> ZipList ys = ZipList (xs ++ drop (length xs) ys) -}
zipListAlternative :: AlternativeOps []
zipListAlternative = pkgAlternativeOps zipListApplicativeOps $ MonoidLiftOps zipListMonoidOps

identityMonadOps :: MonadOps Identity
identityMonadOps = pkgMonadOps Identity (\m k -> k $ runIdentity m)

identityApplicativeOps :: ApplicativeOps Identity
identityApplicativeOps = _monadApplicativeOps identityMonadOps

identityFunctorOp :: Fmap Identity
identityFunctorOp f = Identity . f . runIdentity

assocIdentityAssoc :: Assoc a -> Assoc (Identity a)
assocIdentityAssoc assocF (Identity x) (Identity y) = Identity (assocF x y)

monoidIdentityMonoidOps :: MonoidOps a -> MonoidOps (Identity a)
monoidIdentityMonoidOps monoidOps =
  MonoidOps (assocIdentityAssoc $ _assoc monoidOps)
    . Identity $ _ident monoidOps

identityFixLiftOp :: MfixOp Identity
identityFixLiftOp = MfixOp $ \ f -> Identity . fix $ runIdentity . f

maybeMonadOps :: MonadOps Maybe
maybeMonadOps = pkgMonadOps Just $ \mx f ->
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
assocMaybeMonoidOps :: Assoc a -> MonoidOps (Maybe a)
assocMaybeMonoidOps assocF = MonoidOps
    ( \xm ym ->
        case xm of
        Just x ->
          case ym of
          Just y -> Just (assocF x y)
          _ -> xm
        _ -> ym
    ) Nothing

assocMaybeAssoc :: Assoc a -> Assoc (Maybe a)
assocMaybeAssoc assocF = _assoc $ assocMaybeMonoidOps assocF

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
constApplicativeOps :: MonoidOps m -> ApplicativeOps (Const m)
constApplicativeOps monoidOps = pkgApplicativeOps
  (const . Const $ _ident monoidOps) $
  Left (
    coerce $ _assoc monoidOps,
    const $ \ (Const a) (Const b) -> Const $ _assoc monoidOps a b)
