{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Control.MonadOps where

import Prelude hiding (pure, fmap, return, (>>=), (<$>), (<>))
import Data.MonoidOps

infixl 1  >>=
infixl 4  <$>, <$, <*>

{- class Functor f where
    fmap :: (a -> b) -> f a -> f b -}
type Fmap f = forall a b . (a -> b) -> f a -> f b
type Fma f = forall a b . a -> f b -> f a

data FunctorOps f = FunctorOps {
  _fmap :: Fmap f,
  _fma :: Fma f
  }

(<$>) :: (?functorOps :: FunctorOps f) => Fmap f
(<$>) = _fmap ?functorOps

fmap :: (?functorOps :: FunctorOps f) => Fmap f
fmap = (<$>)

(<$) :: (?functorOps :: FunctorOps f) => Fma f
(<$) = _fma ?functorOps

(<&>) :: (?functorOps :: FunctorOps f) => forall a b . f a -> (a -> b) -> f b
(<&>) = flip fmap

pkgFunctorOps :: Fmap f -> FunctorOps f
pkgFunctorOps fmapF = FunctorOps fmapF $ fmapF . const

{- class Functor f => Applicative f where
    pure  :: a -> f a
    ap :: f (a -> b) -> f a -> f b
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c -}
type Pure f = forall a . a -> f a
type Ap f = forall a b . f (a -> b) -> f a -> f b
type LiftA2 f = forall a b c . (a -> b -> c) -> f a -> f b -> f c

data ApplicativeOps f = ApplicativeOps
  { _pure     :: Pure f
  , _ap       :: Ap f
  , _liftA2   :: LiftA2 f
  , _applicativeFunctorOps :: FunctorOps f
  }

ap :: (?applicativeOps :: ApplicativeOps f) => Ap f
ap = _ap ?applicativeOps

(<*>) :: (?applicativeOps :: ApplicativeOps f) => Ap f
(<*>) = ap

pure :: (?applicativeOps :: ApplicativeOps f) => Pure f
pure = _pure ?applicativeOps

liftA2 :: (?applicativeOps :: ApplicativeOps f) => LiftA2 f
liftA2 = _liftA2 ?applicativeOps

applicativeFunctorOps :: (?applicativeOps :: ApplicativeOps f) => FunctorOps f
applicativeFunctorOps = _applicativeFunctorOps ?applicativeOps

-- liftA2 can be derived from ap and fmap
apFmapLiftA2 :: Ap f -> Fmap f -> LiftA2 f
apFmapLiftA2 apF fmapF = (apF .) . fmapF

-- liftA2 can also be derived from ap and pure
apPureLiftA2 :: Ap f -> Pure f -> LiftA2 f
apPureLiftA2 apF pureF = apFmapLiftA2 apF $ apF . pureF

-- ap can be derived from liftA2
liftA2Ap :: LiftA2 f -> Ap f
liftA2Ap liftA2F = liftA2F id

pkgApplicativeOps :: Pure f -> Either (Ap f, LiftA2 f) (Either (Ap f) (LiftA2 f)) -> ApplicativeOps f
pkgApplicativeOps pureF =
  let f = ApplicativeOps pureF in
  either
    (\ (apF, liftA2F) -> f apF liftA2F . pkgFunctorOps $ apF . pureF)
    (either
      (\ apF -> f apF (apPureLiftA2 apF pureF) . pkgFunctorOps $ apF . pureF)
      (\ liftA2F ->
        let apF = liftA2Ap liftA2F in
        f apF liftA2F . pkgFunctorOps $ apF . pureF))

{- class Monad m where
       return :: a -> m a
       (>>=) :: m a -> (a -> m b) -> m b -}
type Bind m = forall a b . m a -> (a -> m b) -> m b

data MonadOps m = MonadOps
  { _bind                :: Bind m
  , _monadApplicativeOps :: ApplicativeOps m
  }

monadApplicativeOps :: (?monadOps :: MonadOps m) => ApplicativeOps m
monadApplicativeOps = _monadApplicativeOps ?monadOps

(>>=) :: (?monadOps :: MonadOps m) => Bind m
(>>=) = _bind ?monadOps

(>=>) :: (?monadOps :: MonadOps m) => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g

bindPureAp :: Bind m -> Pure m -> Ap m
bindPureAp bindF pureF m1 m2 = bindF m1 $ bindF m2 . (pureF .)

-- helper function to create instance with pure and bind implementations
pkgMonadOps :: Pure m -> Bind m -> MonadOps m
pkgMonadOps pureF bindF =
  MonadOps bindF $ pkgApplicativeOps pureF (Right (Left (bindPureAp bindF pureF)))

-- derive applicative and functor operations from monad operations
return :: (?monadOps :: MonadOps f) => Pure f
return = _pure monadApplicativeOps

type MonoidLift m = forall a . MonoidOps (m a)

newtype MonoidLiftOps m = MonoidLiftOps
  { _monoidLiftMonoidOps :: MonoidLift m
  }

monoidLiftMonoidOps :: (?monoidLiftOps :: MonoidLiftOps m) => forall a . MonoidOps (m a)
monoidLiftMonoidOps = _monoidLiftMonoidOps ?monoidLiftOps

empty :: (?monoidLiftOps :: MonoidLiftOps m) => m a
empty = _ident monoidLiftMonoidOps

(<|>) :: (?monoidLiftOps :: MonoidLiftOps m) => m a -> m a -> m a
(<|>) = _assoc monoidLiftMonoidOps

{-
Let's consolidates MonadPlus and Alternative. Anything that's a MonadPlus is
also an Alternative.
-}
{- class Applicative f => Alternative f where
    -- | The identity of '<|>'
    empty :: f a
    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a

    -- | One or more.
    some :: f a -> f [a]
    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v

    -- | Zero or more.
    many :: f a -> f [a]
    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v -}
-- type AlternativeOps m = (ApplicativeOps m, forall a. MonoidOps (m a))
--{-
type VagueAmount m = forall a. m a -> m [a]

data AlternativeOps m = AlternativeOps {
  _alternativeApplicativeOps :: ApplicativeOps m,
  _alternativeMonoidLiftOps :: MonoidLiftOps m,
  _some :: VagueAmount m,
  _many :: VagueAmount m}
--}

some :: (?alternativeOps :: AlternativeOps m) => VagueAmount m
some = _some ?alternativeOps

many :: (?alternativeOps :: AlternativeOps m) => VagueAmount m
many = _many ?alternativeOps

pkgAlternativeOps :: ApplicativeOps m -> MonoidLiftOps m -> AlternativeOps m
pkgAlternativeOps applOps monoidLiftOps =
  let ?applicativeOps = applOps
      ?monoidLiftOps = monoidLiftOps in
  AlternativeOps applOps monoidLiftOps (\ v ->
      let some_v = liftA2 (:) v (some_v <|> pure [])
      in some_v) (\ v ->
      let many_v = liftA2 (:) v many_v <|> pure []
      in many_v)

{- class MonadTrans t where
    -- | Lift a computation from the argument monad to the constructed monad.
    lift :: (Monad m) => m a -> t m a -}
type Lift t = forall m a . (?monadOps::MonadOps m) => m a -> t m a

newtype LiftOp t = LiftOp {
  _lift :: Lift t
}

lift :: (?liftOp :: LiftOp t) => Lift t
lift = _lift ?liftOp
