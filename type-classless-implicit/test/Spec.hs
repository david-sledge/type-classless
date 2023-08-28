{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ImplicitParams #-}

import Prelude hiding ((*>))
import FarmerWolfGoatCabbage ( findSolutions )
import Control.Monad.Misc ( ioApplicativeOps, ioMonadOps )
import Control.MonadOps ( (*>) )

-- | Main
main :: IO ()
main =
  let ?monadOps = ioMonadOps
      ?applicativeOps = ioApplicativeOps
  in
  (print (findSolutions 7) *> print (findSolutions 13))
