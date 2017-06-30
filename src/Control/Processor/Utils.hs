module Control.Processor.Utils where

import Control.Processor
import Control.Arrow

marr :: Monad m => (a -> m b) -> Processor m a b
marr f = processor iter return f (const $ return ())
  where iter a _ = return a
  


pmap :: Processor m a b -> Processor m [a] [b]
pmap (Processor pf1 af1 cf1 rf1) = processor pf2 af2 cf2 rf2
  where pf2 as xs = mapM (uncurry pf1) $ zip as xs
        af2 as = mapM af1 as
        cf2 xs = mapM cf1 xs
        rf2 xs = mapM_ rf1 xs
