
{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, TemplateHaskell #-}

module Filter where

--import Control.Lens
import Control.Lens.TH
import Control.Lens (view)
import MealyT
import Data.List (mapAccumL,nub)
import Control.Category ((.),id)
import Prelude hiding ((.),id)
import Data.Either (rights)
import Control.Arrow (second, (&&&), (***), arr)
import Control.Monad.Reader
import Control.Monad.ListM
import Data.List (sort, tails)
import Data.Sequence hiding (take, sort,tails,zip,zipWith) 
import qualified Data.Sequence as S 


type Filter s a b = MealyT (Reader s) a b

data ControlFilter r a b = ControlFilter 
        {       _filterParam :: r
        ,       _filter ::  Filter r a b 
        }

$(makeLenses ''ControlFilter)

stepControlFilter :: ControlFilter r a b -> a -> (b, ControlFilter r a b) 
stepControlFilter (ControlFilter p f) x = second (ControlFilter p) $ runReader (runMealyT f x) p

delay = let 
        step k del x 
                | S.length del < k = return (x,make k (del |>  x))
                | otherwise = do
                        let     d :< ds = viewl del
                                x' = (x + d)/2
                        return (x',make k $ ds |> x')
        make k del = MealyT (step k del)
        in make 60 empty
                        
repeater f g = let 
        step store actual x = do
                        recording <- asks f
                        reset <- asks g
                        return $ if reset && recording then
                                        (x,make actual (empty |> x))
                                else if reset then (x,make store store) else if
                                        recording then (x,make store (actual |> x))
                                                else case viewl actual of
                                                        EmptyL -> (x,make store actual)
                                                        x :< xs -> (x,make store $ xs |> x)
                                
        make k del = MealyT (step k del)
        in make empty empty


media n xs = sum xs / fromIntegral n

medium  :: Fractional a
        => (s -> Int) 
        -> Filter s a a

medium view =  let
        update xs x = do
                n <- asks view
                let xs' = take n (x:xs)
                return (media n xs', MealyT $ update xs')
        in MealyT $ update []

normalize     :: Filter a Float Float
normalize  = let
        update Nothing Nothing x = return (0, MealyT $ update (Just x) Nothing)
        update (Just mi) Nothing x
                | x < mi = return (0, MealyT $ update (Just x) Nothing)
                | otherwise = return (1, MealyT $ update (Just mi) (Just x))
        update (Just mi) (Just ma)  x 
                | x < mi = return (0 ,MealyT $ update (Just x) (Just ma))
                | x > ma = return (1 , MealyT $ update (Just mi) (Just x))
                | otherwise = return ((x - mi)/(ma - mi), MealyT $ update  (Just mi) (Just ma))
        update _ _ _ = error "who made this filter ?"
        in MealyT $ update Nothing Nothing
        
changing	:: Eq a =>  Filter s a (Maybe a)
changing = let 
	get (Just l) xs@(x0:x1:x2:[]) = if x0 /= x1 && x0 /= x2 && x0 /= l then (Just x0, make (Just x0) xs) else (Nothing, make (Just l) xs)
	get Nothing xs@(x0:x1:x2:[]) = if x0 /= x1 && x0 /= x2 then (Just x0, make (Just x0) xs) else (Nothing, make Nothing xs)
	get l xs = (Nothing, make l xs)
	make l xs =  MealyT $ \x ->  return (get l $ take 3 $ x:xs)
	in make Nothing []

data Environment = Environment {
        _reset :: Bool
        , _record :: Bool
        , _inertia :: Int
        }

$(makeLenses ''Environment)

type SensorFilter = ControlFilter Environment Float (Float,Maybe Int)
sensorFilter :: SensorFilter
sensorFilter  = ControlFilter 
        (Environment True False 10) $ 
        (id &&& changing  . arr (floor . (*128)))  . normalize   . medium  (view inertia). repeater (view record) (view reset)

{-
best :: Ord a => Filter s [a] (Int,a)
best = arr $ head . sort . zip [0..]

sequenceF :: [Filter s a b] -> Filter s [a] [b]
sequenceF xs = MealyT $ \ws -> do
                (rs,xs') <- unzip `fmap` sequence (zipWith runMealyT xs ws)
                return (rs,sequenceF xs')

corrs4 is = do
        (x:xs) <- it is
        (y:ys) <- it xs
        (z:zs) <- it ys
        w <- zs
        return [x,y,z,w]

it = init.tails
-}
