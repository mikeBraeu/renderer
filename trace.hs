import Codec.Picture
import Codec.Picture.Types
import Data.Vector as V
import Debug.Trace

{-
General algebra stuff
-}

data Point = Point Double Double Double deriving (Eq, Show)

add :: Point -> Point -> Point
add (Point a b c) (Point d e f) = Point (a + d) (b + e) (c + f)

diff :: Point -> Point -> Point
diff (Point a b c) (Point d e f) = Point (a - d) (b - e) (c - f)

scale :: Double -> Point -> Point
scale k (Point a b c) = Point (k*a) (k*b) (k*c)

dot :: Point -> Point -> Double
dot (Point a b c) (Point d e f) = a*d + b*e + c*f

pointwise :: Point -> Point -> Point
pointwise (Point x y z) (Point a b c) = Point (x*a) (y*b) (z*c)

cross :: Point -> Point -> Point
cross (Point a b c) (Point d e f) = Point (b*f - c*e) (c*d - a*f) (a*e - d*b)

dist :: Point -> Point -> Double
dist a b = len $ diff a b

len :: Point -> Double
len p = sqrt (dot p p)

norm :: Point -> Point
norm p
  | len p < 10**(-9) = Point 0 0 0
  | otherwise = scale (1/(len p)) p

mkNormVect :: Point -> Point -> Point
mkNormVect p q = norm $ diff q p

-- coeffs to real roots
solvePoly :: [Double] -> [Double]
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

data Object = Plane Point Double
              | Sphere Point Double deriving (Eq, Show)

isSphere :: Object -> Bool
isSphere (Plane a b) = False
isSphere _ = True

data Light = PointL Point Colour

type Colour = Point

type ColouredObject = (Object, Colour)


data Ray = Ray Point Point deriving (Eq, Show)

mkRay :: Point -> Point -> Ray
mkRay p q = Ray p (mkNormVect p q)

data Scene = Scene {objects :: [ColouredObject], cam :: Camera, lights :: [Light], ambient :: Colour}

data Camera = Camera Point Point Point


data Intersection = Intersection Double Ray ColouredObject deriving (Eq, Show)

sphereint :: Intersection -> Bool
sphereint (Intersection d r (o, c)) = case o of
                                        Sphere a b -> True
                                        Plane a b -> False

{-
raytracer
-}

maxF :: Double -> Point -> Point
maxF f (Point x y z) = Point (max x f) (max y f) (max z f)

minF :: Double -> Point -> Point
minF f (Point x y z) = Point (min x f) (min y f) (min z f)

clip :: Point -> Point
clip = (maxF 0.0) . (minF 1.0)

intersectdists :: Ray -> Object -> [Double]
intersectdists (Ray start dir) (Plane norm d) =
  let part  = dot dir norm
      para  = abs(part) < 10**(-9)
      wha   = dot start norm
        in if para then [] else [- (d + wha) / part]
intersectdists (Ray start dir) (Sphere cen rad) =
  let d = diff start cen
    in solvePoly [dot dir dir, 2*(dot dir d), (dot d d) - rad^2]


getNormal :: Point -> Object -> Point
getNormal _ (Plane n _) = norm n
getNormal p (Sphere cen rad) = norm $ scale (1/rad) $ diff p cen



intDist :: (Maybe Intersection) -> Double
intDist Nothing = 0
intDist (Just (Intersection d _ _)) = d

intCol :: (Maybe Intersection) -> Colour
intCol Nothing = Point 0 0 0
intCol (Just (Intersection _ _ (o, c))) =  c

normAt :: (Maybe Intersection) -> Point
normAt Nothing = Point 0 0 0
normAt i@(Just (Intersection _ _ (o, _))) = getNormal (intPt i) o

intPt :: (Maybe Intersection) -> Point
intPt Nothing = Point 0 0 0
intPt (Just (Intersection d (Ray start dir) _)) = add start $ scale d dir

fstPos :: [Double] -> Double
fstPos [] = 0.0
fstPos (l:ls) = if l > 10**(-6) then l else fstPos ls

isPlane :: Object -> Bool
isPlane (Sphere a b) = False
isPlane _ = True

closestInt :: Ray -> (Maybe Intersection) -> ColouredObject -> (Maybe Intersection)
closestInt r i (p, c) = if d > 10**(-6) && ((isNothing i) || d < (intDist i))
  then Just (Intersection d r (p, c))
  else i
    where
      d = fstPos $ intersectdists r p

intersect :: Ray -> [ColouredObject] -> Maybe Intersection
intersect r p = case Prelude.foldl (closestInt r) Nothing p of
                    Nothing ->  Prelude.foldl (closestInt r) Nothing p
                    _ -> Prelude.foldl (closestInt r) Nothing p


-- diffuse shading only

diffuse :: (Maybe Intersection) -> Light -> Colour
diffuse i (PointL pos col) = pointwise (scale (dot (mkNormVect (intPt i) pos) (normAt i)) col) (intCol i)

shadePt :: Intersection -> Point -> [ColouredObject] -> Light -> Colour
shadePt i d o l@(PointL pos col)
  | s = Point 0 0 0
  | otherwise = diffuse (Just i) l
    where
    s = not(isNothing i_s)  && (intDist i_s) <= dist (intPt (Just i)) pos
    i_s = intersect (mkRay (intPt (Just i)) pos) o

colourPt :: Ray -> Colour -> [ColouredObject] -> [Light] -> Colour
colourPt r@(Ray _ dir) b objs lts =
  let   i             = intersect r objs
        c             = intCol i
        shadeColour   = Prelude.foldl add (Point 0.0 0.0 0.0) (Prelude.map (shadePt (fromJust i) dir objs) lts)
          in case i of
              Nothing -> b
              Just (Intersection d r (o, c)) -> case (o, shadeColour) of
                                                  (Sphere p rad, Point 0 0 0) -> trace ("Sphere dot: " Prelude.++ (show r)) $ clip $ shadeColour
                                                  x -> clip $ shadeColour

--if (isNothing i) then b else clip $ shadeColour

rayTracePt :: Scene -> Point -> Colour
rayTracePt (Scene objs (Camera campos _ _) lts amb) p = colourPt (Ray p (mkNormVect campos p)) amb objs lts

isNothing :: Maybe a -> Bool
isNothing (Just x) = False
isNothing Nothing = True

fromJust :: (Maybe a) -> a
fromJust (Just a) = a

rayTrace :: Resolution -> Dimension -> Scene -> (ScreenPt -> Colour)
rayTrace r d s@(Scene _ cam lts _) pt= rayTracePt s $ project r d cam pt


pixelmaker :: Colour ->  PixelRGB16
pixelmaker (Point r g b) = PixelRGB16 (round $ r*65530) (round $ g*65530) (round $ b*65530)


render :: Resolution -> Dimension -> Scene -> Vector (Vector PixelRGB16)
render res@(xres, yres) dim sc = generate yres (\y -> generate xres (\x -> pixelmaker $ rayTrace res dim sc (x, y)))


{-
main program and test code
-}

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


plane1 = Plane (norm (Point 0             (-sqrt(8)/3)   (1/3))) 0
plane2 = Plane (norm (Point (-sqrt(2/3))   (sqrt(8)/6)  (1/3))) 0
plane3 = Plane (norm (Point (sqrt(2/3))  (sqrt(8)/6)  (1/3))) 0

sphere1 = Sphere (Point 0 0 (3)) 1

colouredobjs1 :: [(Object, Colour)]
colouredobjs1 = [(plane1, (Point 0.9 0 0)), (plane2, (Point 0 0.9 0)), (plane3, (Point 0 0 0.9)), (sphere1, (Point 0.9 0.9 0.9))]


--pos dir up
camera1 = Camera (Point 0 0 10) (Point 0 0 (-1)) (Point 1 0 0)

light1 = PointL (Point 0 2 16) (Point 0.7 0.7 0.7)

scene1 = Scene colouredobjs1 camera1 [light1] (Point 0.4 0.4 0.4)

xres = 2000
yres = 2000
res1 = (xres, yres)

xdim = 1
ydim = 1
dim1 = (xdim, ydim)


image1 :: Image PixelRGB16
image1 = generateImage (\x y -> render res1 dim1 scene1 ! x ! y) xres yres

main = writeTiff "./out.tiff" image1
