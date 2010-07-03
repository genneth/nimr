> {-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}

> import MC0DFramework
> import Control.Monad.Random
> import Control.Monad.State.Strict (get, put)
> import Control.Monad.Writer (tell)
> import Control.Monad

> import Graphics.Rendering.Chart 
> import Graphics.Rendering.Chart.Gtk
> import Data.Accessor
> import Data.Colour
> import Data.Colour.SRGB

import Graphics.Rendering.Chart.Simple

Let's try some models for development. First up, a simple model which has goes
through two phases:

 1. Growth: almost all divisions are symmetric renewals
 2. Differentiation: r_1 is 0.0, r_2 is 0.4: asymmetric differentiation
    dominates.

Let us first assume that division happens stochastically with mean time of one
day, and migration occurs sharply peaked at 7 hours, +- 1 hr, and that these
rates do not change. We then need to specify the functions r_1(t) and r_2(t);
for simplicity, we simply change abruptly at a given t_switch.

> cycleTime = exponentialVariable (1/24.0)

> normalVariable mu sigma = do
>   u <- getRandomR (0.0, 1.0)
>   v <- getRandomR (0.0, 1.0)
>   let x = sqrt(-2 * (log u)) * cos(2 * pi * v)
>   return $ x*sigma + mu

> logNormalVariable mean stddev = do
>   let var = stddev * stddev
>       mu = 2.0*(log mean) - (log (mean*mean + var))/2.0
>       sigma = sqrt (2*log (sqrt (mean*mean + var) / mean))
>   n <- normalVariable 0.0 1.0
>   return $ exp (mu + sigma*n)

> migrationTime = logNormalVariable 7.0 1.0

> r_1 time | time < 70 = 0.95
>          | otherwise = 0.0
> r_2 time | time < 70 = 0.0
>          | otherwise = 0.4

Now, just define the model.

> data ModelA = ModelA

> instance DivisionProcess ModelA where
>   data CellType ModelA = Progenitor Double | Differentiated Double deriving Show
>   data MeanFields ModelA = MF Double -- just the time
>   timeToLive (Progenitor ttl)     = ttl
>   timeToLive (Differentiated ttl) = ttl
>   progeny _ (MF time) (Progenitor _) = do
>       (fate :: Double) <- getRandomR (0.0, 1.0)
>       if fate < (r_1 time)
>         then pair cA cA
>         else if fate < (1.0 - (r_2 time))
>           then pair cA cB
>           else pair cB cB
>     where
>       cA = liftM Progenitor cycleTime
>       cB = liftM Differentiated migrationTime
>       pair c1 c2 = do
>         a <- c1
>         b <- c2
>         return [a, b]
>
>   progeny _ _ (Differentiated _) = return []
>
>   cellType (Progenitor _)     = 0
>   cellType (Differentiated _) = 1
>   evalMeanFields (time,_,_) = MF time

Let's try and reproduce current data. Focusing on the intermediate domain, which
starts dropping apparent mitotic index at t = 70 hr, we have the initial
condition that at t = 57.5 hr, there are roughly 34 cells in the progenitor
domain. Let us assume that they are in fact all progenitors.

> initial_condition :: (MonadRandom m) => m (Queue ModelA)
> initial_condition = do
>   ttls <- replicateM 340 cycleTime
>   let cells = map Progenitor ttls
>   return $ addCells emptyCulture 57.5 cells

> simulation = do
>   st <- initial_condition
>   put (57.5, ModelA, st)
>   (_,_,hist) <- runPopulationWithRecording 100 $ do
>     (time, _, cells) <- get
>     tell [(time, count 0 cells, count 1 cells)]
>   return $ hist

> main = do
>   hist <- evalRandStateIO simulation (0, ModelA, emptyCulture)
>   let (ts, ps', ds') = unzip3 hist
>       ps = map fromIntegral ps' :: [Double]
>       ds = map fromIntegral ds' :: [Double]
>       ns = zipWith (+) ps ds
>       ms = zipWith (/) ps ns
>       count_plot = layout1_plots ^= 
>                [Left (toPlot (plot_fillbetween_title ^= "progenitors"
>                             $ plot_fillbetween_style ^= solidFillStyle (sRGB 0.5 1 0.5 `withOpacity` 0.9)
>                             $ plot_fillbetween_values ^= zip ts (zip (repeat 0) ps)
>                             $ defaultPlotFillBetween)),
>                 Left (toPlot (plot_fillbetween_title ^= "differentiated"
>                             $ plot_fillbetween_style ^= solidFillStyle (sRGB 0.5 0.5 1 `withOpacity` 0.9)
>                             $ plot_fillbetween_values ^= zip ts (zip ps ns)
>                             $ defaultPlotFillBetween))]
>            $ layout1_bottom_axis ^: laxis_title ^= "time (hours)"
>            $ layout1_left_axis   ^: laxis_title ^= "number of cells (× 10)"
>            $ defaultLayout1
>   renderableToWindow (toRenderable count_plot) 640 480
>   renderableToPDFFile (toRenderable count_plot) (8*72) (6*72) "ModelA-cell-counts.pdf"
>   let mitotic_index_plot = 
>           layout1_plots ^=
>             [Left (toPlot (plot_lines_style ^= solidLine 1.0 (sRGB 0.5 1 0.5 `withOpacity` 0.9)
>                          $ plot_lines_values ^= [zip ts ms]
>                          $ defaultPlotLines))]
>         $ layout1_bottom_axis ^: laxis_title ^= "time (hours)"
>         $ layout1_left_axis   ^: laxis_title ^= "proportion of real progenitors"
>         $ defaultLayout1
>   renderableToWindow (toRenderable mitotic_index_plot) 640 480
>   renderableToPDFFile (toRenderable mitotic_index_plot) (8*72) (6*72) "ModelA-rho.pdf"


