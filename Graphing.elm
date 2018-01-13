module Graphing where

import Functions as F
import List
import Color exposing (..)
import Window
import Time
import Text as T
import Signal exposing (Mailbox, mailbox)
import Graphics.Element as E
import Graphics.Input exposing (button, customButton)
import Graphics.Collage as C exposing (defaultLine)

graphStep = 0.01        -- graph step for drawing, subject to change

type alias Point = (Float, Float)
type alias Range = (Float, Float)

lightPink = Color.rgb 255 182 193

tickStrStyle = T.fromString >> T.height 8 >> T.italic >> T.color Color.black >> E.leftAligned

graphGridW = 720
graphGridH = 567

graphBoxTop = 0
graphBoxBottom = 567

graphLineStyle : C.LineStyle
graphLineStyle =
  { defaultLine         
      | width = 1.0   
      , color = black
      , cap   = C.Round   
      , join  = C.Smooth  
  }

drawLineSegments : (Float,Float) -> List Point -> C.Form
drawLineSegments (w,h) points =
  points |> C.path |> C.traced graphLineStyle


buildRange : Float -> Float -> Float -> List Float
buildRange min max stp =
    if min < max then
        min :: (buildRange (min + stp) max stp)
    else
        [max]

fApply : F.Function -> List Float -> List Point
fApply f r = 
  let func = \x -> (x, F.eval f x) in
    List.map func r

type alias GraphState = (List Point, Range)

type GraphEvent = GRAPH String Range | CLEAR | NONE

initGraphState : GraphState
initGraphState = ([], (-5.0,5.0))   -- initial plot range of -5 to 5

-- to be called by the Interface's upstate function
graphUpstate : GraphEvent -> GraphState -> GraphState
graphUpstate graphEvent currentState = case graphEvent of
                      CLEAR -> initGraphState
                      GRAPH funcString range -> (stringToGraph funcString range (graphGridW, graphGridH), range)
                      --RERANGE funcstring newRange -> (stringToGraph funcString newRange (graphGridW, graphGridH), newRange)
                      NONE -> currentState




stringToGraph : String -> (Float, Float) -> (Float, Float) -> List Point
stringToGraph s range (w,h) =
  let func = F.stringToFunction s in
  let r = buildRange (fst range) (snd range) graphStep in
  case func of
    Ok val -> fApply val r
    Err str -> []

numToTickX : (Float, Float) -> E.Element
numToTickX (x, label) =
  C.collage 1000 1000 [
    tickStrStyle (toString label) |> C.toForm |> C.move (x - 4.0,-5.0) ,
    ([(x,-4.0),(x,4.0)] |> C.path |> C.traced graphLineStyle) 
    ]

numToTickY : (Float, Float) -> E.Element
numToTickY (y, label) = 
  C.collage 1000 1000 [
    tickStrStyle (toString label) |> C.toForm |> C.move (-5.0,y+7.0) ,
    ([(-6.0,y),(6.0,y)] |> C.path |> C.traced graphLineStyle) 
    ]

graphStepCalc : Float -> Float
graphStepCalc size = 
  if size > 14.0 then 4.0 
  else if size > 8.0 then 2.0
  else 1.0

ySizeUp : Float -> Float
ySizeUp size = 
  if size > 10.0 then size + 11.0 
  else if size > 5.0 then size + 5.0
  else size + 2.0


drawAxes : (Float, Float) -> C.Form
drawAxes (left, right)  = 
  let maximize = \x -> ((fst x)*(40.0), (snd x)*(20.0)) in
  let yAxis = drawLineSegments (-50,50) (List.map maximize [(0,12.5),(0,-12.5)]) in
  let xAxis = drawLineSegments (-50,50) (List.map maximize [(-7.5,0),(7.5,0)]) in
  let xArrowLeft =  drawLineSegments (0,0) [(-296,4),(-300.0,0),(-296,-4)] in
  let xArrowRight =  drawLineSegments (0,0) [(296,4),(300.0,0),(296,-4)] in
  let yArrowTop =  drawLineSegments (0,0) [(4,246),(0,251),(-4,246)] in
  let yArrowBottom =  drawLineSegments (0,0) [(4,-246),(0,-251),(-4,-246)] in
  let sizedUpY = (ySizeUp right) in
  let stepX = graphStepCalc right in
  let stepY = graphStepCalc sizedUpY in
  let inBoxY = \x -> if abs (fst x) > 225 then False else True in
  let tickLocationsX = buildRange (left) (right) stepX in
  let tickLocationsY = buildRange (-1*sizedUpY) sizedUpY stepY in
  let ticksX = List.map C.toForm <| (List.map numToTickX (List.map (\x -> (x*(200.0/right),x)) tickLocationsX)) in
  let ticksY = List.map C.toForm <| (List.map numToTickY (List.filter inBoxY (List.map (\x -> (x*(200.0/sizedUpY),x)) tickLocationsY))) in
  C.group (List.concat [[xArrowLeft,xArrowRight, yArrowTop,yArrowBottom, xAxis, yAxis] , ticksX , ticksY])




graphPoints graphState = 
  let (pts, (left, right)) = graphState in
  let sizedUpY = (ySizeUp right) in
  let maximize = \x -> ((fst x)*(200.0/right), (snd x)*(200.0/sizedUpY)) in
  let inBoxY = \x -> if abs (snd x) > (225) then False else True in
  let boundedPts = List.filter inBoxY (List.map maximize pts) in
  let plot = drawLineSegments (-50,50) boundedPts in
  C.group [plot]
  








