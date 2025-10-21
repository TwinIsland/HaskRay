module Main where

import Control.Monad (replicateM)
import Data.List (minimumBy)
import System.IO
import System.Random (randomRIO)
import Text.Printf (printf)

-- vector utilities
data Vec3 = Vec3 {x :: Double, y :: Double, z :: Double}
  deriving (Show, Eq)

(^+^) :: Vec3 -> Vec3 -> Vec3
(Vec3 x1 y1 z1) ^+^ (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

(^-^) :: Vec3 -> Vec3 -> Vec3
(Vec3 x1 y1 z1) ^-^ (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

(*^) :: Double -> Vec3 -> Vec3
a *^ (Vec3 x_ y_ z_) = Vec3 {x = a * x_, y = a * y_, z = a * z_}

(^*^) :: Vec3 -> Vec3 -> Vec3
(Vec3 x1 y1 z1) ^*^ (Vec3 x2 y2 z2) = Vec3 {x = x1 * x2, y = y1 * y2, z = z1 * z2}

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

len :: Vec3 -> Double
len v = sqrt (dot v v)

norm :: Vec3 -> Vec3
norm v = (1 / len v) *^ v

writePPM :: FilePath -> Int -> Int -> [[Vec3]] -> IO ()
writePPM filename width height pixels = do
  withFile filename WriteMode $ \h -> do
    hPutStrLn h "P3"
    hPutStrLn h $ show width ++ " " ++ show height
    hPutStrLn h "255"
    mapM_ (writeRow h) pixels
  where
    writeRow h = mapM_ (writePixel h)
    writePixel h (Vec3 r g b) =
      let clamp a b_ v = max a (min b_ v)
          toInt :: Double -> Int
          toInt x_ = floor (255.999 * clamp 0 1 x_)
       in hPutStrLn h $ unwords (map show [toInt r, toInt g, toInt b])

-- ray
data Ray = Ray {origin :: Vec3, direction :: Vec3}
  deriving (Show, Eq)

at :: Ray -> Double -> Vec3
at (Ray o d) t = o ^+^ (t *^ d)

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v ^-^ ((2 * dot v n) *^ n)

-- world
newtype World = World [Sphere]

-- materials
data Material = Diffuse Vec3 | Metal Vec3 deriving (Show, Eq)

-- sphere
data Sphere = Sphere {center :: Vec3, radius :: Double, material :: Material}
  deriving (Show, Eq)

hitSphere :: Sphere -> Ray -> Maybe Double
hitSphere (Sphere c r _) (Ray o d) =
  let oc = o ^-^ c
      a = dot d d
      b = 2 * dot oc d
      c' = dot oc oc - r * r
      discriminant = b * b - 4 * a * c'
   in if discriminant < 0
        then Nothing
        else Just ((-b - sqrt discriminant) / (2 * a))

hit :: World -> Ray -> Maybe (Double, Material)
hit (World spheres) r =
  let results = [(t, material s) | s <- spheres, Just t <- [hitSphere s r], t > 0]
   in if null results
        then Nothing
        else Just (minimumBy (\(t1, _) (t2, _) -> compare t1 t2) results)

randomInUnitSphere :: IO Vec3
randomInUnitSphere = do
  x_ <- randomRIO (-1.0, 1.0)
  y_ <- randomRIO (-1.0, 1.0)
  z_ <- randomRIO (-1.0, 1.0)
  let p = Vec3 {x = x_, y = y_, z = z_}
  if len p >= 1.0 then randomInUnitSphere else return p

-- rendering
rayColor :: World -> Ray -> Int -> IO Vec3
rayColor world r depth
  | depth <= 0 = return (Vec3 0 0 0)
  | otherwise = case hit world r of
      Just (t, mat) -> do
        let (World spheres) = world
            p = at r t
            hitSphere' = head [s | s <- spheres, Just t' <- [hitSphere s r], abs (t' - t) < 1e-6]
            n = norm (p ^-^ center hitSphere')
        case mat of
          Diffuse albedo -> do
            rand <- randomInUnitSphere
            let target = p ^+^ n ^+^ rand
                r2 = Ray p (target ^-^ p)
            col <- rayColor world r2 (depth - 1)
            return (albedo ^*^ col)
          Metal albedo -> do
            let reflected = reflect (norm (direction r)) n
                r2 = Ray p reflected
            col <- rayColor world r2 (depth - 1)
            return (albedo ^*^ col)
      Nothing -> do
        let unitDir = norm (direction r)
            a = 0.5 * (y unitDir + 1.0)
        return $ ((1 - a) *^ Vec3 1 1 1) ^+^ (a *^ Vec3 0.5 0.7 1.0)

main :: IO ()
main = do
  let imageWidth = 1080
      aspectRatio = 16.0 / 9.0
      imageHeight = floor (fromIntegral imageWidth / aspectRatio)
      samplesPerPixel = 50
      depthLimitation = 20

      viewportHeight = 2.0
      viewportWidth = aspectRatio * viewportHeight
      focalLength = 1.0
      originCam = Vec3 0 0 0
      horizontal = Vec3 viewportWidth 0 0
      vertical = Vec3 0 viewportHeight 0
      lowerLeftCorner =
        originCam
          ^-^ (0.5 *^ horizontal)
          ^-^ (0.5 *^ vertical)
          ^-^ Vec3 0 0 focalLength

      world =
        World $
          [ Sphere (Vec3 (-1.2) 0.4 (-2.2)) 1.0 (Metal (Vec3 0.7 0.7 0.7)),
            Sphere (Vec3 1.2 0.2 (-2.0)) 0.8 (Diffuse (Vec3 0.7 0.3 0.3)),
            Sphere (Vec3 0 (-100.5) (-1)) 100 (Diffuse (Vec3 0.8 0.8 0.0))
          ]
            ++ [ Sphere (Vec3 (-0.2) (-0.3) (-1.7)) 0.2 (Metal (Vec3 0.8 0.8 0.8)),
                 Sphere (Vec3 (-0.8) (-0.3) (-2.5)) 0.2 (Metal (Vec3 0.8 0.8 0.8)),
                 Sphere (Vec3 0.6 (-0.3) (-1.1)) 0.2 (Metal (Vec3 0.8 0.8 0.8)),
                 Sphere (Vec3 0.0 (-0.2) (-2.0)) 0.3 (Diffuse (Vec3 0.2 0.4 0.8)),
                 Sphere (Vec3 1.0 (-0.2) (-0.8)) 0.3 (Metal (Vec3 0.2 0.4 0.3)),
                 Sphere (Vec3 (-0.7) (-0.4) (-0.6)) 0.1 (Diffuse (Vec3 0.4 0.4 0.3)),
                 Sphere (Vec3 (-2.0) (-0.2) (-1.2)) 0.3 (Diffuse (Vec3 0.1 0.3 0.5))
               ]

  pixels <-
    sequence
      [ do
          printf "\rRendering %d / %d" (imageHeight - y_) imageHeight
          hFlush stdout
          sequence
            [ do
                colors <- replicateM samplesPerPixel $ do
                  u <- randomRIO (0.0, 1.0)
                  v <- randomRIO (0.0, 1.0)
                  let u' = (fromIntegral x_ + u) / fromIntegral (imageWidth - 1)
                      v' = (fromIntegral y_ + v) / fromIntegral (imageHeight - 1)
                      dir = lowerLeftCorner ^+^ (u' *^ horizontal) ^+^ (v' *^ vertical)
                      r = Ray originCam dir
                  rayColor world r depthLimitation
                let avg = (1 / fromIntegral samplesPerPixel) *^ foldl1 (^+^) colors
                return avg
              | x_ <- [0 .. imageWidth - 1]
            ]
        | y_ <- reverse [0 .. imageHeight - 1]
      ]

  writePPM "output.ppm" imageWidth imageHeight pixels
  printf "\ndone!\n"
