{-# LANGUAGE RankNTypes #-}

import Data.List
import Data.Traversable
import Data.Vector (Vector, toList, fromList, imap, (!))
import qualified Data.Vector as Vec
import Linear
import Debug.Trace
import Control.Lens

data Moon = Moon {
  position :: V3 Int,
  velocity :: V3 Int
} deriving (Eq, Show, Read)

applyGravity :: Vector Moon -> Vector Moon
applyGravity moons = fmap adjustMoon moons
  where
    adjustMoon :: Moon -> Moon
    adjustMoon me = Moon (position me) (velocity me + foldl' (+) zero (adjustments me))

    adjustments :: Moon -> [V3 Int]
    adjustments me = fmap (\other -> signum <$> position other - position me) . filter (/= me) . toList $ moons

applyVelocity :: Vector Moon -> Vector Moon
applyVelocity = fmap $ \(Moon pos vel) -> Moon (pos + vel) vel

energy = sum . fmap abs

totalEnergy = (*) <$> energy . position <*> energy . velocity

selectAxis :: Lens' (V3 Int) Int -> Vector Moon -> Vector (Int, Int)
selectAxis l = fmap $ \(Moon pos vel) -> (pos ^. l, vel ^. l)

period :: Lens' (V3 Int) Int -> [Vector Moon] -> Int
period axis moons = snd . head . filter (isRepetition . fst) $ zip (tail moons) [1..]
  where
    isRepetition v = selectAxis axis v == selectAxis axis (head moons)

main = do
  let initialState = fromList [ Moon (V3 12 0 (-15)) (V3 0 0 0)
                              , Moon (V3 (-8) (-5) (-10)) (V3 0 0 0)
                              , Moon (V3 7 (-17) 1) (V3 0 0 0)
                              , Moon (V3 2 (-11) (-6)) (V3 0 0 0)
                              ]
  let simulation = iterate (applyVelocity . applyGravity) $ initialState
  let first1001 = take 1001 simulation

  print . sum . fmap totalEnergy . last $ first1001


  let xPeriod = period _x simulation
  let yPeriod = period _y simulation
  let zPeriod = period _z simulation

  print . foldl' lcm 1 $ [xPeriod, yPeriod, zPeriod]
