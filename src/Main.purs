module Main where

import Prelude
import Math as Math
import Control.Monad.Eff (Eff)
import Control.MonadPlus (guard)
import Data.Array ((..))
import Data.Int (toNumber)
import Data.List (List(..), fromFoldable)
import Data.Maybe (fromMaybe')
import Graphics.Canvas (CANVAS, Context2D, clearRect, getContext2D, getCanvasElementById, fill, closePath, lineTo, moveTo, beginPath, setFillStyle)
import Partial.Unsafe (unsafeCrashWith)

type Bounds = { min :: Number, max :: Number }

data R = XY { x :: Bounds, y :: Bounds, z :: Number, a :: Number }
       | YZ { x :: Number, y :: Bounds, z :: Bounds, a :: Number }
       | ZX { x :: Bounds, y :: Number, z :: Bounds, a :: Number }

data Split a = Front | Back | Split a a

instance functorSplit :: Functor Split where
  map _ Front = Front
  map _ Back  = Back
  map f (Split a b) = Split (f a) (f b)

backToFront :: forall eff. (R -> Eff eff Unit) -> Array R -> Eff eff Unit
backToFront f s = go $ fromFoldable s
  where
  go Nil = pure unit
  go (Cons split rest) = do
    go do
      r <- rest
      case r `splitOn` split of
        Back -> pure r
        Split _ r1 -> pure r1
        _ -> Nil
    f split
    go do 
      r <- rest
      case r `splitOn` split of
        Front -> pure r
        Split r1 _ -> pure r1
        _ -> Nil
  
  splitOn :: R -> R -> Split R
  splitOn (XY o1) (XY o2) | o1.z >= o2.z = Back
                          | otherwise    = Front
  splitOn (XY o1) (YZ o2) = XY <$> splitX o1 o2.x
  splitOn (XY o1) (ZX o2) = XY <$> splitY o1 o2.y
  splitOn (YZ o1) (XY o2) = YZ <$> splitZ o1 o2.z
  splitOn (YZ o1) (YZ o2) | o1.x >= o2.x = Front
                          | otherwise    = Back
  splitOn (YZ o1) (ZX o2) = YZ <$> splitY o1 o2.y
  splitOn (ZX o1) (XY o2) = ZX <$> splitZ o1 o2.z
  splitOn (ZX o1) (YZ o2) = ZX <$> splitX o1 o2.x
  splitOn (ZX o1) (ZX o2) | o1.y >= o2.y = Front
                          | otherwise    = Back

  splitX :: forall r. { x :: Bounds | r } -> Number -> Split { x :: Bounds | r }
  splitX o x | o.x.min >= x = Front
             | o.x.max <= x = Back
             | otherwise = Split (o { x = { max: o.x.max, min: x } }) 
                                 (o { x = { min: o.x.min, max: x } })

  splitY :: forall r. { y :: Bounds | r } -> Number -> Split { y :: Bounds | r }
  splitY o y | o.y.min >= y = Front
             | o.y.max <= y = Back
             | otherwise = Split (o { y = { max: o.y.max, min: y } })
                                 (o { y = { min: o.y.min, max: y } })

  splitZ :: forall r. { z :: Bounds | r } -> Number -> Split { z :: Bounds | r }
  splitZ o z | o.z.min >= z = Back
             | o.z.max <= z = Front
             | otherwise = Split (o { z = { min: o.z.min, max: z } })
                                 (o { z = { max: o.z.max, min: z } })

toPoints :: R -> Array { x :: Number, y :: Number, z :: Number }
toPoints (XY o) = [ { x: o.x.min, y: o.y.min, z: o.z }
                  , { x: o.x.min, y: o.y.max, z: o.z } 
                  , { x: o.x.max, y: o.y.max, z: o.z } 
                  , { x: o.x.max, y: o.y.min, z: o.z } 
                  ]
toPoints (YZ o) = [ { x: o.x, y: o.y.min, z: o.z.min }
                  , { x: o.x, y: o.y.min, z: o.z.max } 
                  , { x: o.x, y: o.y.max, z: o.z.max } 
                  , { x: o.x, y: o.y.max, z: o.z.min } 
                  ]
toPoints (ZX o) = [ { x: o.x.min, y: o.y, z: o.z.min }
                  , { x: o.x.min, y: o.y, z: o.z.max } 
                  , { x: o.x.max, y: o.y, z: o.z.max } 
                  , { x: o.x.max, y: o.y, z: o.z.min } 
                  ]

cube :: Number -> Number -> Number -> Number -> Number -> Number -> Number -> Array R
cube x0 x1 y0 y1 z0 z1 a =  
  [ XY { x: { min: x0, max: x1 }, y: { min: y0, max: y1 }, z: z0, a: a }
  , XY { x: { min: x0, max: x1 }, y: { min: y0, max: y1 }, z: z1, a: a }
  , YZ { x: x0, y: { min: y0, max: y1 }, z: { min: z0, max: z1 }, a: a }
  , YZ { x: x1, y: { min: y0, max: y1 }, z: { min: z0, max: z1 }, a: a }
  , ZX { x: { min: x0, max: x1 }, y: y0, z: { min: z0, max: z1 }, a: a }
  , ZX { x: { min: x0, max: x1 }, y: y1, z: { min: z0, max: z1 }, a: a }
  ]

scene :: Number -> Array R
scene t = do
  let s = Math.sin (0.25 * t) * 8.0 + 40.0
  x <- map toNumber $ (-3) .. 3
  y <- map toNumber $ (-3) .. 3
  z <- map toNumber $ (-3) .. 3
  let r = Math.abs x + Math.abs y + Math.abs z
  let alpha = Math.sin (x + t * 2.1) + 
              Math.sin (y + t * 2.2) + 
              Math.cos (z + t * 2.3) + 
              r / 7.5
  guard $ alpha > 0.5
  cube (x * s) (x * s + s)
       (y * s) (y * s + s) 
       (z * s) (z * s + s) 
       alpha

foreign import atFps :: forall e. Number -> (Number -> Eff e Unit) -> Eff e Unit

main :: forall e. Eff (canvas :: CANVAS | e) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = fromMaybe' (\_ -> unsafeCrashWith "no canvas element") mcanvas

  ctx <- getContext2D canvas

  atFps 30.0 \t -> do
    void $ clearRect ctx { x: 0.0, y: 0.0, w: 1000.0, h: 1000.0 }
    backToFront (render ctx t) (scene t)

  where
  render :: Context2D -> Number -> R -> Eff (canvas :: CANVAS | e) Unit
  render ctx t r = do
    void case r of
      XY o -> setFillStyle ("rgba(48, 196, 255, " <> show o.a <> ")") ctx
      YZ o -> setFillStyle ("rgba(24, 144, 200, " <> show o.a <> ")") ctx
      ZX o -> setFillStyle ("rgba(0 , 128, 196, " <> show o.a <> ")") ctx
    case toPoints r of
      [p1, p2, p3, p4] -> do
        void $ beginPath ctx
        l2 moveTo p1
        l2 lineTo p2
        l2 lineTo p3
        l2 lineTo p4
        void $ closePath ctx
        void $ fill ctx
      _  -> unsafeCrashWith "bad points"
    pure unit
    where
    l2 f p = let o = project p  in void $ f ctx o.x o.y

    theta :: Number
    theta = Math.pi / 4.0 + Math.pi / 12.0 * Math.sin (t * 0.11)

    c :: Number
    c = Math.cos theta

    s :: Number
    s = Math.sin theta 

    project :: { x :: Number, y :: Number, z :: Number } -> { x :: Number, y :: Number }
    project o = { x: 500.0 + o.x * c - o.y * s
                , y: 350.0 + o.x * s + o.y * c + o.z
                }

