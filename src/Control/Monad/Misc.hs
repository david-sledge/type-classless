module Control.Monad.Misc where

import Control.MonadOp
import Control.Monad.FixOp
import Control.Monad.PlusOp
import Data.Functor.Identity
import Data.Function ( fix )
import Data.MonoidOp
import System.IO

monadFix'IOOp    :: MonadFixOp    IO
monad'IOOp       :: MonadOp       IO
applicative'IOOp :: ApplicativeOp IO
monadFix'IOOp@(MonadFixOp {
    _monad'MFOp = monad'IOOp@(MonadOp {
      _applicative = applicative'IOOp
    })
  }) =
  monadFixOp return (>>=) fixIO

--functor'IOOp     :: FunctorOp     IO
--functor'IOOp = _functor'A applicative'IOOp

monadFix'ListOp    :: MonadFixOp    []
monad'ListOp       :: MonadOp       []
applicative'ListOp :: ApplicativeOp []
monadFix'ListOp@(MonadFixOp {
    _monad'MFOp = monad'ListOp@(MonadOp {
      _applicative = applicative'ListOp
    })
  }) =
  monadFixOp return (>>=) $ fix (\mfix f ->
      case fix (f . head) of
      []    -> []
      (x:_) -> x : mfix (tail . f))

--functor'ListOp     :: FunctorOp     []
--functor'ListOp = _functor'A applicative'ListOp

monoid'ListOp = MonoidOp (++) []

{- instance Monad (Identity) where
       return = Identity
       m >>= k = k $ runIdentity m -}
monadFix'IdentityOp    :: MonadFixOp    Identity
monad'IdentityOp       :: MonadOp       Identity
applicative'IdentityOp :: ApplicativeOp Identity
monadFix'IdentityOp@(MonadFixOp {
    _monad'MFOp = monad'IdentityOp@(MonadOp {
      _applicative = applicative'IdentityOp
    })
  }) =
  monadFixOp Identity (\m k -> k $ runIdentity m) $ \f ->
    Identity . fix $ runIdentity . f

--functor'IdentityOp     :: FunctorOp     Identity
--functor'IdentityOp = _functor'A applicative'IdentityOp

--monadPlus'MaybeOp :: MonadPlusOp Maybe
monadPlus'MaybeOp = monadPlusOp (
      \ma mb ->
        case ma of
        Nothing -> mb
        _ -> ma )
    Nothing Just $ \mx f ->
      case mx of
      Just x -> f x
      _ -> Nothing

--monoid'MaybeOp :: MonoidOp (Maybe a)
monoid'MaybeOp = snd $ monadPlus'MaybeOp

--monad'MaybeOp :: MonadOp Maybe
monad'MaybeOp = fst $ monadPlus'MaybeOp

{-
https://gitlab.haskell.org/ghc/ghc/-/issues/9588
instance (Error e) => MonadPlus (Either e) where
    mzero            = Left noMsg
    Left _ `mplus` n = n
    m      `mplus` _ = m

Let's define the Either MonadPlus operations so that they're more generalized
so that the Error constraint (nor any other typeclass constraint) is not needed.
-}
monadPlusEitherOp :: a -> (MonadOp (Either a), MonoidOp (Either a b))
monadPlusEitherOp a = monadPlusOp
  ( \b c ->
      case b of
      Left _ -> c
      _ -> b
  ) (Left a) Right
  (
    \b k ->
      case b of
      Left l -> Left l
      Right r -> k r
  )
