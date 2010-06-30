> {-# LANGUAGE TypeFamilies, EmptyDataDecls, ViewPatterns #-}

> import Control.Monad.Identity
> import Data.Map (Map, minViewWithKey, insert, insertWith)
> import Control.Monad.Random
> import Control.Monad

The basic idea is to maintain a queue of cells, sorted by their expiration date.
We simply march forwards through the queue, remove a cell and stick its progeny
back into the queue. We allow arbitrary processes, the only constraint being 
that each cell must make decisions independently, though perhaps based on 
interal degrees of freedom. We represent each division process with a label/tag. 
This is also a good place to put parameters of the model, e.g.:

> data TwoTypeMarkov = TwoTypeMarkov Double   Double
> --                                 ^ gamma  ^r

and a generic framework to handle the potential ensuing mess:

> class DivisionProcess dp where
>   data CellType dp
>   timeToLive :: CellType dp -> Double
>   progeny    :: RandomGen g => dp -> CellType dp -> Rand g [CellType dp]

The structure above should enforce the independence of cells. It is possible 
that we should allow mean-field interactions instead, and have structures:

    data MeanFields dp
    progeny :: RandomGen g => dp -> MeanFields dp -> CellType dp -> Rand g [CellType dp]

For our example two type mark
ov process, we should then have:

> instance DivisionProcess TwoTypeMarkov where
>   data CellType TwoTypeMarkov = A Double | B Double
>   timeToLive (A ttl) = ttl
>   timeToLive (B ttl) = ttl
>   progeny (TwoTypeMarkov gamma r) (A _) = do
>       fate <- getRandomR (0.0, 0.1)
>       if fate < r
>         then pair cA cA
>         else if fate < (1.0 - r)
>           then pair cA cB
>           else pair cB cB
>     where
>       cA = liftM A (exponentialVariable 1.0)
>       cB = liftM B (exponentialVariable gamma)
>       pair c1 c2 = do
>         a <- c1
>         b <- c2
>         return [a, b]
>
>   progeny _ (B _) = return []

where:

> exponentialVariable :: (MonadRandom m) => Double -> m Double
> exponentialVariable lambda = do
>   x <- getRandomR (0.0, 0.1)
>   return $ -(log x) / lambda

We have generic operations which need to be carried out, for any given
population of cells. Most of these are based around a priority queue container,
sorted by death time.

> type Queue dp = Map Double [CellType dp]

> viewQ (minViewWithKey -> Nothing) = Nothing
> viewQ (minViewWithKey -> Just ((_, c:[]), q)) = Just (c, q)
> viewQ (minViewWithKey -> Just ((t, c:cs), q)) = Just (c, insert t cs q)
> addCells q0 offset cs = foldr (\c q -> insertWith (++) (timeToLive c) [c] q) q0 cs
