import Codec.Picture
import Codec.Picture.Types

import Data.Vector as V

instance (Num a) => Num (Vector a) where
  negate v = V.map (\x -> -x) v
  (+) v w = V.zipWith (+) v w
  (*) v w = V.zipWith (*) v w
  (-) v w = V.zipWith (-) v w
  abs v = error "undefined"
  signum _ = error "undefined"
  fromInteger _ = error "undefined"




(*.>) :: (Num a) => a -> Vector a -> Vector a
k *.> v = V.map (\x -> k*x) v

linapp :: (Num a) => Vector (Vector a) -> Vector a -> Vector a
linapp m w = V.foldr (+) 0 $ V.zipWith (\v k -> k *.> v) m w

(*.*) :: (Num a) => Vector (Vector a) -> Vector (Vector a) -> Vector (Vector a)
(*.*) x y = V.map (linapp x) y

rotx :: Float -> Vector (Vector Float)
rotx a = fromList $ Prelude.map fromList [[1, 0, 0],[0,cos a, sin a],[0, -(sin a), cos a]]

roty :: Float -> Vector (Vector Float)
roty a = fromList $ Prelude.map fromList [[cos a, 0, -(sin a)],[0, 1, 0],[sin a, 0, cos a]]

rotz :: Float -> Vector (Vector Float)
rotz a = fromList $ Prelude.map fromList [[cos a, sin a, 0],[-(sin a), cos a, 0],[0, 0, 1]]

trans :: Vector (Vector Float) -> Vector (Vector Float)
trans m = generate (V.length (m ! 0)) $ \n -> V.map (\y -> y ! n) m
