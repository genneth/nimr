> {-# LANGUAGE TypeFamilies, EmptyDataDecls, ViewPatterns, FlexibleContexts #-}

> module MC0DFramework (
>   DivisionProcess, CellType, MeanFields, timeToLive, progeny, cellType,
>   evalMeanFields,
>   Queue, viewQ, addCells, emptyCulture, initialCell, allDead, nextTime,
>   PopulationState, stepPopulation, runPopulation, runPopulationWithRecording,
>   histogram, count,
>   while, evalRandStateIO, exponentialVariable) where

> import Control.Monad.Random
> import Control.Monad.State.Strict
> import Control.Monad.Writer
> import Data.Map (Map, minViewWithKey, insert, insertWith, singleton)
> import qualified Data.Map as M
> import qualified Data.List as L
> import Control.Monad
> import Control.Arrow
> import Data.IORef

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
>   data MeanFields dp
>   timeToLive :: CellType dp -> Double -- ttl should not depend on parameters
>   progeny    :: MonadRandom m => dp -> MeanFields dp -> CellType dp 
>                                     -> m [CellType dp]
>   cellType   :: CellType dp -> Int -- for histogram purposes
>   evalMeanFields :: PopulationState dp -> MeanFields dp

The structure above should enforce the independence of cells. PopulationState
already contains a copy of dp; see later.

For our example two type markov process, we should then have:

> instance DivisionProcess TwoTypeMarkov where
>   data CellType TwoTypeMarkov = A Double | B Double deriving Show
>   data MeanFields TwoTypeMarkov = TTM -- empty
>   timeToLive (A ttl) = ttl
>   timeToLive (B ttl) = ttl
>   progeny (TwoTypeMarkov gamma r) _ (A _) = do
>       fate <- getRandomR (0.0, 1.0)
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
>   progeny _ _ (B _) = return []
>
>   cellType (A _) = 0
>   cellType (B _) = 1
>   evalMeanFields _ = TTM

where:

> exponentialVariable :: (MonadRandom m) => Double -> m Double
> exponentialVariable lambda = do
>   x <- getRandomR (0.0, 1.0)
>   return $ -(log x) / lambda

We have generic operations which need to be carried out, for any given
population of cells. Most of these are based around a priority queue container,
sorted by death time.

> type Queue dp = Map Double [CellType dp]

> viewQ (minViewWithKey -> Nothing) = Nothing
> viewQ (minViewWithKey -> Just ((t, c:[]), q)) = Just ((t, c), q)
> viewQ (minViewWithKey -> Just ((t, c:cs), q)) = Just ((t, c), insert t cs q)
> addCells q0 offset cs = 
>   foldr (\c q -> insertWith (++) (offset + (timeToLive c)) [c] q) q0 cs
> emptyCulture = M.empty
> initialCell c = singleton 0.0 [c]
> allDead = M.null
> nextTime q = t
>   where Just ((t, _), _) = viewQ q

We can't avoid having state:

> type PopulationState dp = (Double, dp, Queue dp)

Key update step: take off front cell, add progeny. Requires a non-empty.
Return False if we're out of cells afterwards.

> stepPopulation :: (DivisionProcess dp, MonadState (PopulationState dp) m, MonadRandom m) => m ()
> stepPopulation = do
>   (curr_time, process, cells) <- get
>   let Just ((deathday, c), rest) = viewQ cells --- assertion: deathday >= curr_time
>       meanFields = evalMeanFields (curr_time, process, cells)
>   daughters <- progeny process meanFields c
>   let new_cells = addCells rest deathday daughters
>   put (deathday, process, new_cells)

> runPopulation :: (DivisionProcess dp, MonadState (PopulationState dp) m, MonadRandom m) => Double -> m (Double, Queue dp)
> runPopulation max_time = do
>     while good stepPopulation
>     (t, _, cells) <- get
>     return $ (t, cells)
>   where good = do (_,_,cells) <- get
>                   return $ (not (allDead cells)) && ((nextTime cells) < max_time)

> runPopulationWithRecording :: (DivisionProcess dp, MonadState (PopulationState dp) m, MonadRandom m, Monoid w) => Double -> WriterT w m () -> m (Double, Queue dp, w)
> runPopulationWithRecording max_time recorder = do
>   ((final_time, final_cells), log) <- runWriterT (runPopulationWithRecording' max_time recorder)
>   return $ (final_time, final_cells, log)

> runPopulationWithRecording' :: (DivisionProcess dp, MonadState (PopulationState dp) m, MonadRandom m, Monoid w, MonadWriter w m) => Double -> m () -> m (Double, Queue dp)
> runPopulationWithRecording' max_time recorder = do
>     while good (stepPopulation >> recorder)
>     (t, _, cells) <- get
>     return $ (t, cells)
>   where good = do (_,_,cells) <- get
>                   return $ (not (allDead cells)) && ((nextTime cells) < max_time)

At the end of a simulation we often want to get a count of the number of
different types of cells:

> histogram :: DivisionProcess dp => Queue dp -> [(Int, Int)]
> histogram = map (head &&& length) . L.group . L.sort . map cellType . concat . M.elems

> count :: DivisionProcess dp => Int -> Queue dp -> Int
> count t = length . filter (==t) . map cellType . concat . M.elems

Generic helpers: 

> evalRandStateIO f initial_state = do
>   gen <- getStdGen
>   let (res, gen') = evalState (runRandT f gen) initial_state
>   setStdGen gen'
>   return res

> while :: Monad m => m Bool -> m a -> m ()
> while test expr = do
>   t <- test
>   case t of
>     False -> return ()
>     True  -> expr >> while test expr

Rest of example:

> twoTypeMC :: IO ()
> twoTypeMC = do
>   runs <- newIORef 0
>   while (liftM (<100) (readIORef runs)) $ do
>     initialTTL <- evalRandIO (exponentialVariable 1.0)
>     (_, cells) <- evalRandStateIO (runPopulation 10.0)
>                     (0.0, TwoTypeMarkov 1.0 0.2, initialCell (A initialTTL))
>     if not (allDead cells)
>       then do
>         modifyIORef runs (+1)
>         sequence_ $ L.intersperse (putStr " ") (map (putStr . show) $ histogram cells)
>         putStrLn ""
>       else return ()

