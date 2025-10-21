module Main where

import Control.Concurrent (modifyMVar_, newMVar)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM)
import Data.List (minimumBy)
import GHC.Conc (getNumCapabilities)
import System.IO
-- import System.Random (randomRIO)

import System.Random (StdGen, mkStdGen, randomR)
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

-- image utils
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

randomInUnitSphere :: StdGen -> (Vec3, StdGen)
randomInUnitSphere gen =
  let (x_, g1) = randomR (-1.0, 1.0) gen
      (y_, g2) = randomR (-1.0, 1.0) g1
      (z_, g3) = randomR (-1.0, 1.0) g2
      p = Vec3 x_ y_ z_
   in if len p >= 1.0
        then randomInUnitSphere g3
        else (p, g3)

-- rendering
rayColor :: StdGen -> World -> Ray -> Int -> (Vec3, StdGen)
rayColor gen world r depth
  | depth <= 0 = (Vec3 0 0 0, gen)
  | otherwise =
      case hit world r of
        Just (t, mat) ->
          let (World spheres) = world
              p = at r t
              hitSphere' = head [s | s <- spheres, Just t' <- [hitSphere s r], abs (t' - t) < 1e-6]
              n = norm (p ^-^ center hitSphere')
           in case mat of
                Diffuse albedo ->
                  let (randVec, gen') = randomInUnitSphere gen
                      target = p ^+^ n ^+^ randVec
                      r2 = Ray p (target ^-^ p)
                      (col, gen'') = rayColor gen' world r2 (depth - 1)
                   in (albedo ^*^ col, gen'')
                Metal albedo ->
                  let reflected = reflect (norm (direction r)) n
                      r2 = Ray p reflected
                      (col, gen') = rayColor gen world r2 (depth - 1)
                   in (albedo ^*^ col, gen')
        Nothing ->
          let unitDir = norm (direction r)
              a = 0.5 * (y unitDir + 1.0)
           in (((1 - a) *^ Vec3 1 1 1) ^+^ (a *^ Vec3 0.5 0.7 1.0), gen)

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
        World
          [ Sphere (Vec3 (-1.2) 0.4 (-2.2)) 1.0 (Metal (Vec3 0.7 0.7 0.7)),
            Sphere (Vec3 1.2 0.2 (-2.0)) 0.8 (Diffuse (Vec3 0.7 0.3 0.3)),
            Sphere (Vec3 0 (-100.5) (-1)) 100 (Diffuse (Vec3 0.8 0.8 0.0)),
            Sphere (Vec3 (-0.2) (-0.3) (-1.7)) 0.2 (Metal (Vec3 0.8 0.8 0.8)),
            Sphere (Vec3 (-0.8) (-0.3) (-2.5)) 0.2 (Metal (Vec3 0.8 0.8 0.8)),
            Sphere (Vec3 0.6 (-0.3) (-1.1)) 0.2 (Metal (Vec3 0.8 0.8 0.8)),
            Sphere (Vec3 0.0 (-0.2) (-2.0)) 0.3 (Diffuse (Vec3 0.2 0.4 0.8)),
            Sphere (Vec3 1.0 (-0.2) (-0.8)) 0.3 (Metal (Vec3 0.2 0.4 0.3)),
            Sphere (Vec3 (-0.7) (-0.4) (-0.6)) 0.1 (Diffuse (Vec3 0.4 0.4 0.3)),
            Sphere (Vec3 (-2.0) (-0.2) (-1.2)) 0.3 (Diffuse (Vec3 0.1 0.3 0.5))
          ]

  n <- getNumCapabilities
  printf "[HaskRay] Using %d Haskell threads\n" n
  progress <- newMVar (1 :: Int)

  pixels <-
    mapConcurrently
      ( \y_ -> do
          row <-
            sequence
              [ do
                  colors <- forM [0 .. samplesPerPixel - 1] $ \s -> do
                    let seed = y_ * imageWidth * samplesPerPixel + x_ * samplesPerPixel + s
                        gen = mkStdGen (fromIntegral seed)
                        (u, g1) = randomR (0.0, 1.0) gen
                        (v, g2) = randomR (0.0, 1.0) g1

                        u' = (fromIntegral x_ + u) / fromIntegral (imageWidth - 1)
                        v' = (fromIntegral y_ + v) / fromIntegral (imageHeight - 1)
                        dir = lowerLeftCorner ^+^ (u' *^ horizontal) ^+^ (v' *^ vertical)
                        r = Ray originCam dir

                        (col, _) = rayColor g2 world r depthLimitation
                    return col

                  let avg = (1 / fromIntegral samplesPerPixel) *^ foldl1 (^+^) colors
                  return avg
                | x_ <- [0 .. imageWidth - 1]
              ]

          modifyMVar_ progress $ \curProgress -> do
            let n' = curProgress + 1
            printf "\r[HaskRay] %d / %d rows done" curProgress imageHeight
            hFlush stdout
            return n'

          return row
      )
      $ reverse [0 .. imageHeight - 1]

  printf "\n[HaskRay] dumping to 'output.ppm', wait...\n"
  writePPM "output.ppm" imageWidth imageHeight pixels
  printf "\n[HaskRay] done!\n"
