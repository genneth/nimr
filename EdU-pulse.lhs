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

EdU pulse experiment --- single EdU injection labels cells going through S-phase
for about an hour. These cells are then almost perfectly synchronised. If 
division is not perfectly Markovian, then we should see some interesting time-
evolution.

We know that the mean stratification and division times are 9 days. Let's add a
little delay to the start, then assume that things are Markovian there after:

> divisionTime = exponentialVariable (1/6.0) >>= (return . (+3.0))
> stratificationTime = exponentialVariable (1/1.0) >>= (return . (+8.0))

> data EdU = EdU

> instance DivisionProcess EdU where
>   data CellType EdU = A Double | B Double deriving Show
>   data MeanFields EdU = EdUMF
>   timeToLive (A ttl) = ttl
>   timeToLive (B ttl) = ttl
>   progeny _ _ (A _) = do
>       (fate :: Double) <- getRandomR (0.0, 1.0)
>       if fate < 0.2
>         then pair cA cA
>         else if fate < (1.0 - 0.2)
>           then pair cA cB
>           else pair cB cB
>     where
>       cA = liftM A divisionTime
>       cB = liftM B stratificationTime
>       pair c1 c2 = do
>         a <- c1
>         b <- c2
>         return [a, b]
>   progeny _ _ (B _) = return []
>   cellType (A _)     = 0
>   cellType (B _) = 1
>   evalMeanFields _ = EdUMF

> main = do
>   (_,_,hist) <- evalRandStateIO 
>                   (runPopulationWithRecording 10 recorder) 
>                   (0, EdU, addCells emptyCulture 0.0 $ replicate 300 (A 0.0))
>   let (ts', ps', ds') = unzip3 hist
>       ts = map (*24.0) ts'
>       ps = map fromIntegral ps' :: [Double]
>       ds = map fromIntegral ds' :: [Double]
>       ns = zipWith (+) ps ds
>       count_plot = layout1_plots ^= 
>                [Left (toPlot (plot_fillbetween_title ^= "progenitors"
>                             $ plot_fillbetween_style ^= solidFillStyle (sRGB 0.5 1 0.5 `withOpacity` 0.9)
>                             $ plot_fillbetween_values ^= zip ts (zip (repeat 0) ps)
>                             $ defaultPlotFillBetween)),
>                 Left (toPlot (plot_fillbetween_title ^= "differentiated"
>                             $ plot_fillbetween_style ^= solidFillStyle (sRGB 0.5 0.5 1 `withOpacity` 0.9)
>                             $ plot_fillbetween_values ^= zip ts (zip ps ns)
>                             $ defaultPlotFillBetween))]
>            $ layout1_bottom_axis ^: (laxis_title ^= "time (hours)")
>                                   . (laxis_title_style ^= (font_name ^= "Segoe Print" $ defaultFontStyle))
>            $ layout1_left_axis   ^: laxis_title ^= "number of cells (× 10)"
>            $ defaultLayout1
>   renderableToWindow (toRenderable count_plot) 640 480
>   where
>     recorder = do
>       (time, _, cells) <- get
>       tell [(time, count 0 cells, count 1 cells)]

