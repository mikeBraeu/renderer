import Codec.Picture
import Codec.Picture.Types
import Data.Vector as V
import Debug.Trace

{-
General algebra stuff
-}

data Point = Point Float Float Float deriving (Eq, Show)

add :: Point -> Point -> Point
add (Point a b c) (Point d e f) = Point (a + d) (b + e) (c + f)

diff :: Point -> Point -> Point
diff (Point a b c) (Point d e f) = Point (a - d) (b - e) (c - f)

scale :: Float -> Point -> Point
scale k (Point a b c) = Point (k*a) (k*b) (k*c)

dot :: Point -> Point -> Float
dot (Point a b c) (Point d e f) = a*d + b*e + c*f

cross :: Point -> Point -> Point
cross (Point a b c) (Point d e f) = Point (b*f - c*e) (c*d - a*f) (a*e - d*b)

len :: Point -> Float
len p = sqrt (dot p p)

norm :: Point -> Point
norm p
  | len p < 10**(-9) = Point 0 0 0
  | otherwise = scale (1/(len p)) p

mkNormVect :: Point -> Point -> Point
mkNormVect p q = norm $ diff q p

-- coeffs to real roots
solvePoly :: [Float] -> [Float]
solvePoly []          = error "degree to low"
solvePoly [a]         = error "degree to low"
solvePoly [a,b]       = [-b/a]
solvePoly [a,b,c]     = let d = b*b - 4*a*c
                          in case (d<0,d==0,0<d) of
                              (True, False, False)  -> []
                              (False, True, False)   -> [-b/(2*a)]
                              (False, False, True)   -> [(-b - sqrt d)/(2*a), (-b + sqrt d)/(2*a)]

{-
Data structures and closely related functions
-}

data Object = Plane Point Float
              | Sphere Point Float deriving (Eq, Show)

type ColouredObject = (Object, Float)


data Ray = Ray Point Point deriving (Eq, Show)

mkRay :: Point -> Point -> Ray
mkRay p q = Ray p (mkNormVect p q)

data Scene = Scene {objects :: [ColouredObject], cam :: Camera, ambient :: Float}

data Camera = Camera Point Point Point


data Intersection = Intersection Float Ray ColouredObject deriving (Eq, Show)

{-
raytracer
-}

maxF :: Float -> Point -> Point
maxF f (Point x y z) = Point (max x f) (max y f) (max z f)

minF :: Float -> Point -> Point
minF f (Point x y z) = Point (min x f) (min y f) (min z f)

clip :: Point -> Point
clip = (maxF 0.0) . (minF 1.0)

intersectdists :: Ray -> Object -> [Float]
intersectdists (Ray start dir) (Plane norm d) =
  let part  = dot dir norm
      para  = abs(part) < 10**(-9)
      wha   = dot start norm
        in if para then [] else [- (d + wha) / part]
intersectdists (Ray start dir) (Sphere cen rad) =
  let (start', dir') = intersectdistsd (start,dir)
      d = diff start' cen
    in solvePoly [dot dir' dir', 2*(dot dir' d), (dot d d) - rad^2]

intersectdistsd :: (Point,Point) -> (Point,Point)
intersectdistsd x = x --trace ("intersectdistsd: " Prelude.++ (show x)) x


getNormal :: Point -> Object -> Point
getNormal _ (Plane n _) = norm n
getNormal p (Sphere cen rad) = norm $ scale (1/rad) $ diff p cen

type Resolution = (Int, Int)
type Dimension  = (Int, Int)
type ScreenPt   = (Int, Int)



project :: Resolution -> Dimension -> Camera -> ScreenPt -> Point
project (rx, ry) (w, h) (Camera campos cdir cup) (px, py) =
  let (rxD, ryD)  = (fromIntegral rx, fromIntegral ry)
      (pxD, pyD)  = (fromIntegral px, fromIntegral py)
      (wD, hD)    = (fromIntegral w, fromIntegral h)
      (x, y)      = (pxD-rxD/2, pyD-ryD/2)
      (x', y')    = (x*wD, y*hD)
      (x'', y'')  = (x'/rxD, y'/ryD)
        in add cdir $ add campos $ add (scale x'' (cross cdir cup)) (scale y'' cup)

intDist :: (Maybe Intersection) -> Float
intDist Nothing = 0
intDist (Just (Intersection d _ _)) = d

intCol :: (Maybe Intersection) -> Float
intCol Nothing = 0
intCol (Just (Intersection _ _ (_, c))) = c

normAt :: (Maybe Intersection) -> Point
normAt Nothing = Point 0 0 0
normAt i@(Just (Intersection _ _ (o, _))) = getNormal (intPt i) o

intPt :: (Maybe Intersection) -> Point
intPt Nothing = Point 0 0 0
intPt (Just (Intersection d (Ray start dir) _)) = add start $ scale d dir

fstPos :: [Float] -> Float
fstPos [] = 0.0
fstPos (l:ls) = if l > 10**(-6) then l else fstPos ls

closestInt :: Ray -> (Maybe Intersection) -> ColouredObject -> (Maybe Intersection)
closestInt r i (p, c) = if d > 10**(-6) && ((isNothing i) || d < (intDist i))
  then Just (Intersection d r (p, c))
  else i
    where
      d = fstPos $ intersectdists r p

intersect :: Ray -> [ColouredObject] -> Maybe Intersection
intersect r p = Prelude.foldl (closestInt r) Nothing p


colourPt :: Ray -> Float -> [ColouredObject] -> Float
colourPt r@(Ray _ dir) b objs =
  let   i             = intersect r objs
        c             = intCol i
          in if (isNothing i) then b else c

rayTracePt :: Scene -> Point -> Float
rayTracePt (Scene objs (Camera campos _ _) amb) p = colourPt (Ray p (mkNormVect campos p)) amb objs

isNothing :: Maybe a -> Bool
isNothing (Just x) = False
isNothing Nothing = True


--project :: Resolution -> Dimension -> Camera -> ScreenPt -> Point
rayTrace :: Resolution -> Dimension -> Scene -> (ScreenPt -> Float)
rayTrace r d s@(Scene _ cam _) = (rayTracePt s) . (project r d cam)


render :: Resolution -> Dimension -> Scene -> [[PixelF]]
render res@(xres, yres) dim sc = [[ rayTrace res dim sc (x, y)| x <- [0..xres]] | y <- [0..yres]]




{-
main program and test code
-}

debug = True

main = writeTiff "./out.tiff" image1

plane1 = Plane (Point 0             (sqrt(8)/3)   (-1/3)) 0
plane2 = Plane (Point (sqrt(2/3))   (-sqrt(8)/6)  (-1/3)) 0
plane3 = Plane (Point (-sqrt(2/3))  (-sqrt(8)/6)  (-1/3)) 0

sphere1 = Sphere (Point 0 0 (1)) 1

colouredobjs1 :: [(Object, Float)]
colouredobjs1 = [(plane1, 0.1), (plane2, 0.3), (plane3, 0.5), (sphere1, 0.9)]

colouredobjs2 :: [(Object, Float)]
colouredobjs2 = [(sphere1, 0.9)]

--pos dir up
camera1 = Camera (Point 0 0 15) (Point 0 0 (-1)) (Point 1 0 0)

scene1 = Scene colouredobjs1 camera1 0.5

xres = 200
yres = 200
res1 = (xres, yres)


image1 :: Image PixelF
image1 = generateImage (\x y -> render res1 (1,1) scene1 !! x !! y) xres yres
