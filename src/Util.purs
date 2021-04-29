module Util where

import Prelude
import Data.Unfoldable (unfoldr)
import Data.Array (take, drop)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

split :: forall a. Int -> Array a -> Array (Array a)
split n xs = unfoldr f xs
  where
    f :: Array a -> Maybe (Tuple (Array a) (Array a))
    f [] = Nothing
    f ys = Just $ Tuple (take n ys) (drop n ys)
