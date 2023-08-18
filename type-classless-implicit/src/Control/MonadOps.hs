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

functorOps :: Fmap f -> FunctorOps f
functorOps fmapF = FunctorOps fmapF $ fmapF . const

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
  }

ap :: (?applicativeOps :: ApplicativeOps f) => Ap f
ap = _ap ?applicativeOps

(<*>) :: (?applicativeOps :: ApplicativeOps f) => Ap f
(<*>) = ap

pure :: (?applicativeOps :: ApplicativeOps f) => Pure f
pure = _pure ?applicativeOps

liftA2 :: (?applicativeOps :: ApplicativeOps f) => LiftA2 f
liftA2 = _liftA2 ?applicativeOps

-- liftA2 can be derived from ap and fmap
apFmapLiftA2 :: Ap f -> Fmap f -> LiftA2 f
apFmapLiftA2 apF fmapF = (apF .) . fmapF

-- liftA2 can also be derived from ap and pure
apPureLiftA2 :: Ap f -> Pure f -> LiftA2 f
apPureLiftA2 apF pureF = apFmapLiftA2 apF $ apF . pureF

-- ap can be derived from liftA2
liftA2Ap :: LiftA2 f -> Ap f
liftA2Ap liftA2F = liftA2F id

applicativeOps :: Pure f -> Either (Ap f, LiftA2 f) (Either (Ap f) (LiftA2 f)) -> ApplicativeOps f
applicativeOps pureF (Left (apF, liftA2F)) = ApplicativeOps pureF apF liftA2F
applicativeOps pureF (Right (Left apF)) = ApplicativeOps pureF apF $ apPureLiftA2 apF pureF
applicativeOps pureF (Right (Right liftA2F)) = ApplicativeOps pureF (liftA2Ap liftA2F) liftA2F

-- derive fmap from applicative operations
applicativeFmap :: (?applicativeOps :: ApplicativeOps f) => Fmap f
applicativeFmap = ap . pure

applicativeFunctorOps :: (?applicativeOps :: ApplicativeOps f) => FunctorOps f
applicativeFunctorOps = functorOps applicativeFmap

{- class Monad m where
       return :: a -> m a
       (>>=) :: m a -> (a -> m b) -> m b -}
type Bind m = forall a b . m a -> (a -> m b) -> m b

data MonadOps m = MonadOps
  { _bind           :: Bind m
  , _applicativeOps :: ApplicativeOps m
  }

monadApplicativeOps :: (?monadOps :: MonadOps m) => ApplicativeOps m
monadApplicativeOps = _applicativeOps ?monadOps

(>>=) :: (?monadOps :: MonadOps f) => Bind f
(>>=) = _bind ?monadOps

bindPureAp :: Bind m -> Pure m -> Ap m
bindPureAp bindF pureF m1 m2 = bindF m1 $ bindF m2 . (pureF .)

-- helper function to create instance with pure and bind implementations
monadOps :: Pure m -> Bind m -> MonadOps m
monadOps pureF bindF =
  MonadOps bindF . applicativeOps pureF . Right . Left $ bindPureAp bindF pureF

-- derive applicative and functor operations from monad operations
return :: (?monadOps :: MonadOps f) => Pure f
return = _pure $ _applicativeOps ?monadOps

monadAp :: (?monadOps :: MonadOps f) => Ap f
monadAp = _ap $ _applicativeOps ?monadOps

monadFmap :: (?monadOps :: MonadOps f) => Fmap f
monadFmap = let ?applicativeOps = _applicativeOps ?monadOps in applicativeFmap

newtype MonoidLiftOps m = MonoidLiftOps
  { _monoidLiftMonoidOps :: forall a . MonoidOps (m a)
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
type VagueQuantity m = forall a. m a -> m [a]

data AlternativeOps m = AlternativeOps {
  _altApplicativeOps :: ApplicativeOps m,
  _monoidLiftOps :: MonoidLiftOps m,
  _some :: VagueQuantity m,
  _many :: VagueQuantity m}
--}

altApplicativeOps :: (?alternativeOps :: AlternativeOps m) => ApplicativeOps m
altApplicativeOps = _altApplicativeOps ?alternativeOps

monoidLiftOps :: (?alternativeOps :: AlternativeOps m) => MonoidLiftOps m
monoidLiftOps = _monoidLiftOps ?alternativeOps

some :: (?alternativeOps :: AlternativeOps m) => VagueQuantity m
some = _some ?alternativeOps

many :: (?alternativeOps :: AlternativeOps m) => VagueQuantity m
many = _many ?alternativeOps

alternativeOps :: ApplicativeOps m -> MonoidLiftOps m -> AlternativeOps m
alternativeOps applOps monoiLiftOps =
  let ?applicativeOps = applOps
      ?monoidLiftOps = monoiLiftOps in
  AlternativeOps applOps monoiLiftOps (\ v ->
      let some_v = liftA2 (:) v (some_v <|> pure [])
      in some_v) (\ v ->
      let many_v = liftA2 (:) v many_v <|> pure []
      in many_v)
-- type MonadPlusOps m = (MonadOps m, forall a. MonoidOps (m a))

{- class MonadTrans t where
    -- | Lift a computation from the argument monad to the constructed monad.
    lift :: (Monad m) => m a -> t m a -}
-- no need for a data or newtype for a single function (perhaps?)
type Lift t = forall m a . (?monadOps :: MonadOps m) => m a -> t m a
type Liftt t = forall m a . m a -> t m a
