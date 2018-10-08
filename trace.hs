import Codec.Picture
import Codec.Picture.Types
import Data.Vector as V
import Debug.Trace
import Control.Monad

{-
General algebra stuff
-}

type RealType = Double

data Point = Point RealType RealType RealType deriving (Eq, Show)

add :: Point -> Point -> Point
add (Point a b c) (Point d e f) = Point (a + d) (b + e) (c + f)

diff :: Point -> Point -> Point
diff (Point a b c) (Point d e f) = Point (a - d) (b - e) (c - f)

scale :: RealType -> Point -> Point
scale k (Point a b c) = Point (k*a) (k*b) (k*c)

dot :: Point -> Point -> RealType
dot (Point a b c) (Point d e f) = a*d + b*e + c*f

pointwise :: Point -> Point -> Point
pointwise (Point x y z) (Point a b c) = Point (x*a) (y*b) (z*c)

cross :: Point -> Point -> Point
cross (Point a b c) (Point d e f) = Point (b*f - c*e) (c*d - a*f) (a*e - d*b)

dist :: Point -> Point -> RealType
dist a b = len $ diff a b

len :: Point -> RealType
len p = sqrt (dot p p)

norm :: Point -> Point
norm p
  | len p < 10**(-9) = Point 0 0 0
  | otherwise = scale (1/(len p)) p

mkNormVect :: Point -> Point -> Point
mkNormVect p q = norm $ diff q p

-- coeffs to real roots
solvePoly :: [RealType] -> [RealType]
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

data Object = Plane Point RealType
              | Sphere Point RealType deriving (Eq, Show)

data Light = PointL Point Colour

type Colour = Point

type ColouredObject = (Object, Colour)


data Ray = Ray Point Point deriving (Eq, Show)

mkRay :: Point -> Point -> Ray
mkRay p q = Ray p (mkNormVect p q)

data Scene = Scene {objects :: [ColouredObject], cam :: Camera, lights :: [Light], ambient :: Colour}

data Camera = Camera Point Point Point

data Intersection = Intersection {intDist' :: RealType, intRay :: Ray, intObj:: ColouredObject} deriving (Eq, Show)


{-
raytracer
-}

maxF :: RealType -> Point -> Point
maxF f (Point x y z) = Point (max x f) (max y f) (max z f)

minF :: RealType -> Point -> Point
minF f (Point x y z) = Point (min x f) (min y f) (min z f)

clip :: Point -> Point
clip = (maxF 0.0) . (minF 1.0)

intersectdists :: Ray -> Object -> [RealType]
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



intCol' :: Intersection -> Colour
intCol' (Intersection _ _ (o, c)) = c


normAt' :: Intersection -> Point
normAt' i@(Intersection _ _ (o, _)) = getNormal (intPt' i) o


intPt' :: Intersection -> Point
intPt' (Intersection d (Ray start dir) _) = add start $ scale d dir

fstPos :: [RealType] -> RealType
fstPos [] = 0.0
fstPos (l:ls) = if l > 10**(-6) then l else fstPos ls


closestInt :: Ray -> (Maybe Intersection) -> ColouredObject -> (Maybe Intersection)
closestInt r i (p, c) = if d <= 10**(-6) || (not (isNothing i) && (intDist' $ fromJust i) <= d)
  then i
  else Just (Intersection d r (p, c))
    where
      d = fstPos $ intersectdists r p


intersect :: Ray -> [ColouredObject] -> Maybe Intersection
intersect r p = Prelude.foldl (closestInt r) Nothing p


-- diffuse shading only


diffuse :: Intersection -> Light -> Colour
diffuse i (PointL pos col) = pointwise (scale (dot (mkNormVect (intPt' i) pos) (normAt' i)) col) (intCol' i)



shadePt :: Intersection -> Point -> [ColouredObject] -> Light -> Colour
shadePt i d o l@(PointL pos col) =
  let pt  = intPt' i
      i_s = intersect (mkRay pt pos) o
      s = not (isNothing i_s) && (intDist' $ fromJust i_s) <= dist pt pos
        in if s then Point 0 0 0 else diffuse i l



colourPt :: Ray -> [ColouredObject] -> [Light] -> Maybe Colour
colourPt r@(Ray _ dir)  objs lts =
  do  i <- intersect r objs
      let shadeColour = Prelude.foldl add
                          (Point 0.0 0.0 0.0)
                          (Prelude.map (shadePt i dir objs) lts)
       in return $ clip $ shadeColour



rayTracePt :: Scene -> Point -> Colour
rayTracePt (Scene objs (Camera campos _ _) lts amb) p =
  let mcol = colourPt (Ray p (mkNormVect campos p)) objs lts
        in if (isNothing mcol) then amb else fromJust mcol


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

xres = 200
yres = 200
res1 = (xres, yres)

xdim = 1
ydim = 1
dim1 = (xdim, ydim)


image1 :: Image PixelRGB16
image1 = generateImage (\x y -> render res1 dim1 scene1 ! x ! y) xres yres

main = writeTiff "./out.tiff" image1
