module Choose.Distinct (choose) where

import Prelude
import Control.Monad.ST as ST
import Data.Array.ST as STA
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Data.Array as Array
import Data.Traversable (for)
import Effect.Random (randomInt)

type SwapInfo = { i :: Int, j :: Int }


swapInfoArray :: Int -> Int -> Effect (Array SwapInfo)
swapInfoArray n pickN = 
  for (Array.range 0 (pickN-1)) $ \i -> do
    j <- randomInt i (n-1)
    pure { i, j }


swapST :: forall h a. SwapInfo -> STA.STArray h a -> ST.ST h Unit
swapST { i, j } stArr = do
  xiMay <- STA.peek i stArr
  xjMay <- STA.peek j stArr
  case swap <$> xiMay <*> xjMay of
    Nothing ->
      pure unit

    Just res ->
      res

  where
    swap ::  a -> a -> ST.ST h Unit
    swap xi xj = do
       _ <- STA.poke j xi stArr
       _ <- STA.poke i xj stArr
       pure unit


choose :: forall a. Int -> Array a -> Effect (Array a)
choose n xs = do
  infoArr <- swapInfoArray (Array.length xs) n
  let st = STA.withArray (swapMany infoArr) xs :: forall h. ST.ST h (Array a)
  pure $ Array.take n $ ST.run st

  where
    swapMany :: forall h. Array SwapInfo -> STA.STArray h a -> ST.ST h Unit
    swapMany infoArr stArr =
      ST.foreach infoArr (\info -> swapST info stArr)

