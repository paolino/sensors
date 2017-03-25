module MooreT where

import Control.Applicative

data MooreT m a b = MooreT  b (a -> m (MooreT m a b))

instance Functor m => Functor (MooreT m a) where
        f `fmap` (MooreT y g) = MooreT (f y) $ fmap (fmap f) . g 

instance (Functor m, Monad m) => Applicative (MooreT m a) where
        pure x = MooreT x  (const . return $ pure x)
        MooreT h f <*> MooreT y g = MooreT (h y) l where
                l x = do
                        m' <- f x
                        m'' <- g x
                        return $ m' <*> m''


