{-# LANGUAGE InstanceSigs #-}

module MealyT where

import Control.Applicative
import Control.Category
import Prelude hiding ((.),id)
import Control.Arrow
import Control.Monad.Identity

newtype MealyT m a b = MealyT {runMealyT :: a -> m (b,MealyT m a b)}
type Mealy a b = MealyT Identity a b

runMealy (MealyT f) = runIdentity . f

instance Monad m => Functor (MealyT m a) where
        f `fmap` MealyT g = MealyT $ \x -> do
                (y,m) <- g x
                return (f y,f `fmap` m)
                
instance Monad m => Applicative (MealyT m a) where
        pure y = MealyT $ \_ -> return (y,pure y)
        MealyT f <*> MealyT g =  MealyT $ \x -> do
                        (y,m') <- g x
                        (f,m'') <- f x
                        return (f y, m'' <*> m')

instance Monad m =>  Category (MealyT m) where
        id = let m = MealyT $ \x -> return (x,m) in m
        MealyT f . MealyT g = MealyT $ \x -> do
                        (y,m') <- g x
                        (z,m'') <- f y
                        return (z,m'' . m')

instance Monad m => Arrow (MealyT m) where
        arr f = MealyT (\x -> return (f x, arr f))
        first (MealyT f) = MealyT $ \(x,d) -> do
                        (y,m') <- f x
                        return ((y,d),first m')
              
instance Monad m => ArrowChoice (MealyT m) where
        left :: MealyT m a b -> MealyT m (Either a c) (Either b c) 
        left (MealyT f) = MealyT g where
                g (Left x) = do
                        (y,m') <- f x
                        return (Left y,left m')
                g (Right x) = return (Right x,left (MealyT f))

instance Monad m => ArrowApply (MealyT m) where
        app :: MealyT m (MealyT m b c, b) c 
        app = MealyT $ \(MealyT f ,x) -> do
                        (y,_) <- f x
                        return (y,app)
                

                
