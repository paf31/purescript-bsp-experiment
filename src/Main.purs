module Main where

import Debug.Trace

import Data.Maybe
import Data.Tuple
import Data.Foldable (foldl)

import Control.Monad.Eff

import Graphics.Canvas

type Bounds = { min :: Number, max :: Number }

data R = XY { x :: Bounds, y :: Bounds, z :: Number }
       | YZ { x :: Number, y :: Bounds, z :: Bounds }
       | ZX { x :: Bounds, y :: Number, z :: Bounds }

instance showR :: Show R where
  show (XY o) = "(XY [" ++ show o.x.min ++ ", " ++ show o.x.max ++ "]" ++
                  ", [" ++ show o.y.min ++ ", " ++ show o.y.max ++ "]" ++
                   ", " ++ show o.z ++ ")"
  show (YZ o) = "(YZ " ++ show o.x ++
                 ", [" ++ show o.y.min ++ ", " ++ show o.y.max ++ "]" ++
                  ", [" ++ show o.z.min ++ ", " ++ show o.z.max ++ "])"
  show (ZX o) = "(ZX [" ++ show o.x.min ++ ", " ++ show o.x.max ++ "]" ++
                   ", " ++ show o.y ++
                  ", [" ++ show o.z.min ++ ", " ++ show o.z.max ++ "])"

data BSP = Leaf | Branch BSP R BSP

data Split a = Front | Back | Split a a

instance functorSplit :: Functor Split where
  (<$>) _ Front = Front
  (<$>) _ Back  = Back
  (<$>) f (Split a b) = Split (f a) (f b)

buildTree :: [R] -> BSP
buildTree = foldl pushDown Leaf
  where
  pushDown :: BSP -> R -> BSP
  pushDown Leaf r = Branch Leaf r Leaf
  pushDown (Branch front split back) r = 
    case r `splitOn` split of
      Front -> Branch (pushDown front r) split back
      Back  -> Branch front split (pushDown back r)
      Split r1 r2 -> Branch (pushDown front r1) split (pushDown back r2)

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

view :: forall eff. (R -> Eff eff Unit) -> BSP -> Eff eff Unit
view render = go
  where
  go Leaf = return unit
  go (Branch front r@(XY o) back) = do 
    go back
    render r
    go front
  go (Branch front r@(YZ o) back) = do 
    go back
    render r
    go front
  go (Branch front r@(ZX o) back) = do
    go back
    render r
    go front

toPoints :: R -> [{ x :: Number, y :: Number, z :: Number }]
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

cube :: Number -> Number -> Number -> Number -> Number -> Number -> [R]
cube x0 x1 y0 y1 z0 z1 = 
  [ XY { x: { min: x0, max: x1 }, y: { min: y0, max: y1 }, z: z0 }
  , XY { x: { min: x0, max: x1 }, y: { min: y0, max: y1 }, z: z1 }
  , YZ { x: x0, y: { min: y0, max: y1 }, z: { min: z0, max: z1 } }
  , YZ { x: x1, y: { min: y0, max: y1 }, z: { min: z0, max: z1 } }
  , ZX { x: { min: x0, max: x1 }, y: y0, z: { min: z0, max: z1 } }
  , ZX { x: { min: x0, max: x1 }, y: y1, z: { min: z0, max: z1 } }
  ]

scene :: [R]
scene = cube   0 100   0 100   0 200
     ++ cube   0 100  50 200  50 200
     ++ cube  50 200 100 200 100 200
     ++ cube 100 200   0 150 150 200

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "rgba(48, 196, 255, 0.75)" ctx
  --setFillStyle "white" ctx
  setStrokeStyle "rgba(0, 0, 0, 0.2)" ctx

  let bsp = buildTree scene
  view (render ctx) bsp

  where
  render :: Context2D -> R -> Eff _ Unit
  render ctx r = do
    print r
    case toPoints r of
      [p1, p2, p3, p4] -> void do
        beginPath ctx
        l2 ctx moveTo p1
        l2 ctx lineTo p2
        l2 ctx lineTo p3
        l2 ctx lineTo p4
        closePath ctx
        fill ctx
        stroke ctx

  l2 ctx f p = case project p of
               o -> f ctx o.x o.y

  cos20 :: Number
  cos20 = Math.cos (Math.pi / 4)

  sin20 :: Number
  sin20 = Math.sin (Math.pi / 4)

  project :: { x :: Number, y :: Number, z :: Number } -> { x :: Number, y :: Number }
  project o = { x: 300.0 + o.x * cos20 - o.y * sin20
              , y: 100.0 + o.x * sin20 + o.y * cos20 + o.z
              }
