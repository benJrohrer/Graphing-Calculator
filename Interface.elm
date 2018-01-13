module Interface where

import Parsing as P
import Graphing as G
import Random
import String
import Color exposing (..)
import Window
import Time
import Text as T
import Signal exposing (Mailbox, mailbox)
import Graphics.Element as E
import Graphics.Input exposing (button, customButton)
import Graphics.Collage as C exposing (defaultLine)
import Functions as F
import Graphics.Input.Field exposing (..)
import Graphics.Input exposing (..)



-- elm make Interface.elm --output=int.html && open -a Google\ Chrome int.html

type alias State  = (Bool, String, String, G.GraphState)

type CalcEvent =  ZOOMIN | ZOOMOUT |  CARAT | INV | SRT | XVAR | YEQ | ANS | DEL | PLU | MIN | DIV | TIM | PM | DEC | LPAR | RPAR |
                 Zero | One | Two | Three| Four | Five | Six | Seven | Eight | Nine | Compute | Clear


initInputState = (False, "", "", G.initGraphState)

upstate e (graphMode, stringToCompute , stringResult, graphState) = 
  case e of
    PLU  -> (graphMode, stringToCompute ++ " + ", stringResult, graphState)
    MIN  -> (graphMode, stringToCompute ++ " - ", stringResult, graphState)
    DIV  -> (graphMode, stringToCompute ++ " / ", stringResult, graphState)
    TIM  -> (graphMode, stringToCompute ++ " * ", stringResult, graphState)
    Clear ->  (False, "", "", G.graphUpstate G.CLEAR graphState)
    Zero -> (graphMode, stringToCompute ++ "0", stringResult, graphState)
    One   -> (graphMode, stringToCompute ++ "1", stringResult, graphState)
    Two   -> (graphMode, stringToCompute ++ "2", stringResult, graphState)
    Three   -> (graphMode, stringToCompute ++ "3", stringResult, graphState)
    Four   -> (graphMode, stringToCompute ++ "4", stringResult, graphState)
    Five   -> (graphMode, stringToCompute ++ "5", stringResult, graphState)
    Six   -> (graphMode, stringToCompute ++ "6", stringResult, graphState)
    Seven   -> (graphMode, stringToCompute ++ "7", stringResult, graphState)
    Eight   -> (graphMode, stringToCompute ++ "8", stringResult, graphState)
    Nine   -> (graphMode, stringToCompute ++ "9", stringResult, graphState)
    Compute -> case graphMode of
      True -> (graphMode, stringToCompute, "", G.graphUpstate (G.GRAPH stringToCompute (snd graphState)) graphState)
      False -> (graphMode, "", F.stringToResult stringToCompute, graphState) 
    LPAR -> (graphMode, stringToCompute ++ "( ", stringResult, graphState)
    RPAR -> (graphMode, stringToCompute ++ " ) ", stringResult, graphState)
    PM -> (graphMode, P.plusOrMin stringToCompute, stringResult, graphState)
    DEC -> (graphMode, stringToCompute ++ ".", stringResult, graphState)
    DEL -> (graphMode, P.backspace stringToCompute, stringResult, graphState)
    ANS -> (graphMode, stringToCompute ++ " " ++ stringResult, stringResult, graphState)
    YEQ -> ((not graphMode)||(P.stringContainsX stringToCompute), stringToCompute, stringResult, graphState)
    XVAR -> (True, stringToCompute ++ "x", stringResult, graphState)
    CARAT -> (graphMode, stringToCompute ++ " ^ ", stringResult, graphState)
    INV -> (graphMode, stringToCompute ++ " ^ ( -1 )", stringResult, graphState)
    SRT -> (graphMode, stringToCompute ++ " ^ ( 1 / 2 )", stringResult, graphState)
    ZOOMIN -> let (left, right) = snd graphState in
      if right < 2.0 then 
        (graphMode, stringToCompute, "", graphState) 
      else 
        let newRange = (left+2, right-2) in
        (graphMode, stringToCompute, "", G.graphUpstate (G.GRAPH stringToCompute newRange) graphState)
    ZOOMOUT -> let (left, right) = snd graphState in
      if right > 15.0 then 
        (graphMode, stringToCompute, "", graphState) 
      else 
        let newRange = (left-2, right+2) in
        (graphMode, stringToCompute, "", G.graphUpstate (G.GRAPH stringToCompute newRange) graphState)




strStyle : String -> E.Element
strStyle = T.fromString >> T.height 14 >> T.color Color.black >> E.centered
thickerLineStyle = { defaultLine | color = Color.black , width = 4 }
lineStyle = { defaultLine | color = Color.black , width = 3 }
spacerLineStyle = { defaultLine | color = lightPink , width = 3 }
buttonLineStyle = { defaultLine | color = Color.black , width = 2 }
captionStrStyle = T.fromString >> T.height 13 >> T.italic >> T.color Color.black >> E.leftAligned

graphGridW = G.graphGridW
graphGridH = G.graphGridH

captionW = 720
captionH = 47

btnW = 44
btnH = 44

arrowBtnW = 94
arrowBtnH = 47

lightPink = Color.rgb 255 182 193
darkPink = Color.rgb 255 50 147

type alias Point = { x:Float, y:Float }

type alias Input bool =
    { returnKey : bool
    , delta : Time.Time
    }

arrowButtonShape : Float -> Float -> C.Shape
arrowButtonShape size stretchFactor = 
  let sizeify = \(a,b) -> (size*a, size*b) in
  let stretchify = \(a,b) -> (stretchFactor*a, b) in
  let sizeUp = stretchify << sizeify in
  C.polygon (List.map sizeUp [(0.5,0.866), (1,0), (0.5,-0.866), (-0.5,-0.866), (-1,0), (-0.5, 0.866)])

myButton msg s =
  let drawButton c =
    C.collage btnW btnH
       [ C.filled c  (C.ngon 6 21)
       , C.outlined buttonLineStyle (C.ngon 6 21)
       , strStyle s |> C.toForm
    ]
  in
  customButton msg
    (drawButton lightPink)
    (drawButton darkPink)
    (drawButton Color.grey)

arrowButton msg s = 
  let drawButton c =
    C.collage arrowBtnW arrowBtnH
       [ C.filled c  (arrowButtonShape 20 2)
       , C.outlined lineStyle (arrowButtonShape 20 2)
       , strStyle s |> C.toForm
    ]
  in
  customButton msg
    (drawButton lightPink)
    (drawButton darkPink)
    (drawButton Color.grey)

rectButton msg s = 
  let drawButton c =
    C.collage 50 30
       [ C.filled c  (C.rect 50 30)
       , C.outlined thickerLineStyle (C.rect 50 30)
       , strStyle s |> C.toForm
    ]
  in
  customButton msg
    (drawButton lightPink)
    (drawButton darkPink)
    (drawButton Color.grey)
    
bigButton msg s = 
  let drawButton c =
    C.collage 100 60
       [ C.filled c  (C.rect 100 60)
       , C.outlined thickerLineStyle (C.rect 100 60)
       , strStyle s |> C.toForm
    ]
  in
  customButton msg
    (drawButton lightPink)
    (drawButton darkPink)
    (drawButton Color.grey)


buttonMailbox : Mailbox CalcEvent
buttonMailbox = mailbox Clear

plusButton   = myButton (Signal.message buttonMailbox.address PLU) "+"
minButton   = myButton (Signal.message buttonMailbox.address MIN) "-"
divButton   = myButton (Signal.message buttonMailbox.address DIV) "/"
timesButton   = myButton (Signal.message buttonMailbox.address TIM) "*"
zeroButton   = myButton (Signal.message buttonMailbox.address Zero) "0"
oneButton   = myButton (Signal.message buttonMailbox.address One) "1"
twoButton   = myButton (Signal.message buttonMailbox.address Two) "2"
threeButton   = myButton (Signal.message buttonMailbox.address Three) "3"
fourButton   = myButton (Signal.message buttonMailbox.address Four) "4"
fiveButton   = myButton (Signal.message buttonMailbox.address Five) "5"
sixButton   = myButton (Signal.message buttonMailbox.address Six) "6"
sevenButton   = myButton (Signal.message buttonMailbox.address Seven) "7"
eightButton   = myButton (Signal.message buttonMailbox.address Eight) "8"
nineButton   = myButton (Signal.message buttonMailbox.address Nine) "9"
clearButton = myButton (Signal.message buttonMailbox.address Clear) "ac"
computeButton = bigButton (Signal.message buttonMailbox.address Compute) "> compute <"
pmButton = myButton (Signal.message buttonMailbox.address PM) "+/-"
decimalButton = myButton (Signal.message buttonMailbox.address DEC) "."
lparButton = myButton (Signal.message buttonMailbox.address LPAR) "("
rparButton = myButton (Signal.message buttonMailbox.address RPAR) ")"
delButton = myButton (Signal.message buttonMailbox.address DEL) "del"
ansButton = rectButton (Signal.message buttonMailbox.address ANS) "ans"
yeqButton = rectButton (Signal.message buttonMailbox.address YEQ) "y = "
xButton = rectButton (Signal.message buttonMailbox.address XVAR) "x"
caratButton = myButton (Signal.message buttonMailbox.address CARAT) "^"
invButton = myButton (Signal.message buttonMailbox.address INV) "inv"
sqrtButton = myButton (Signal.message buttonMailbox.address SRT) "sqrt"
zoomInButton = rectButton (Signal.message buttonMailbox.address ZOOMIN) "+"
zoomOutButton = rectButton (Signal.message buttonMailbox.address ZOOMOUT) "-"

vspace = E.spacer 5 5

toStringMinusQuotes : String -> String
toStringMinusQuotes s =
  String.dropRight 1 (String.dropLeft 1 (toString s))

gmString : Bool -> String
gmString b = 
  if (not b) then 
    "INPUT:" ++ "  "
  else 
    "         INPUT:" ++ "  " ++ "    y ="

view inputState (w,h) =
  let (graphModeToggle, stringToCompute, stringResult, graphState) = inputState in
  let graphModeStr = (gmString graphModeToggle) in
  let squareSpacer = C.collage btnW btnH [C.outlined spacerLineStyle (C.rect 60 70)] in 
  let smallSpacer = C.collage 70 40 [C.outlined spacerLineStyle (C.rect 70 40)] in 
  let tinySpacer = C.collage 20 40 [C.outlined spacerLineStyle (C.rect 20 40)] in 
  let tallSquareSpacer = C.collage btnW btnH [C.outlined spacerLineStyle (C.rect 60 300)] in 
  let toCompute = C.collage captionW captionH [ 
    stringToCompute |> toStringMinusQuotes |> strStyle |> E.container captionW captionH E.middle |> C.toForm,
    C.outlined lineStyle (C.rect captionW captionH),
     captionStrStyle (graphModeStr) |> C.toForm |> C.move (-327,0)] in
  let resultBar = C.collage captionW captionH [
    stringResult |> toStringMinusQuotes |> strStyle |> E.container captionW captionH E.middle |> C.toForm,
    C.outlined lineStyle (C.rect captionW captionH),
    captionStrStyle "RESULT" |> C.toForm |> C.move (-327,0)] in
  
  let graph = G.graphPoints graphState in
  
  let graphPane = C.collage graphGridW graphGridH [
    graph |> C.move (20,-20),
    G.drawAxes (snd graphState) |> C.move (20,-20),
    C.outlined lineStyle (C.rect graphGridW graphGridH),
    captionStrStyle "GRAPH OUTPUT:" |> C.toForm |> C.move (-305,267)] in
  
  
  let spacerColumn = E.flow E.down <| List.intersperse vspace [squareSpacer, squareSpacer, squareSpacer, squareSpacer] in
  let topRow = E.flow E.right <| List.intersperse vspace [tinySpacer, tinySpacer, yeqButton, xButton, ansButton] in 
  let secondRow = E.flow E.right <| List.intersperse vspace [tinySpacer, clearButton, delButton,lparButton,rparButton ] in 
  
  let column1 = E.flow E.down <| List.intersperse vspace [pmButton, oneButton, fourButton] in
  let column2 = E.flow E.down <| List.intersperse vspace [invButton,  twoButton, fiveButton] in
  let column3 = E.flow E.down <| List.intersperse vspace [sqrtButton,threeButton, sixButton] in
  let column4 = E.flow E.down <| List.intersperse vspace [caratButton, plusButton, zeroButton] in
  let column5 = E.flow E.down <| List.intersperse vspace [ minButton, divButton, timesButton] in
  let columns = E.flow E.right <| List.intersperse vspace [column1, column2, column3, column4, column5] in
  
  let penultimateRow = E.flow E.right <| List.intersperse vspace [tinySpacer,  sevenButton,  eightButton, nineButton,decimalButton ] in 
  let bottomRow = E.flow E.right <| List.intersperse vspace [tinySpacer, tinySpacer, tinySpacer, computeButton] in 

  
  let rangeString = "Range: " ++ "(" ++ toString (fst (snd graphState)) ++ " , " ++ toString (snd (snd graphState)) ++ ")" in
  let zoomRow = E.flow E.right <| List.intersperse vspace [ zoomOutButton, tinySpacer, captionStrStyle rangeString, tinySpacer, zoomInButton] in
  
  let buttonGrid = C.collage 400 671 [ 
    C.outlined lineStyle (C.rect 400 671),
    C.toForm <| E.flow E.down <| List.intersperse vspace [topRow, secondRow, columns, penultimateRow, bottomRow, zoomRow]
    ] in
  let outputGrid = E.flow E.down <| List.intersperse vspace [toCompute , resultBar, graphPane] in
  let calcGrid = E.flow E.right <| List.intersperse vspace [buttonGrid, outputGrid] in
  let fullLayout = E.color lightPink <| E.container w h E.middle calcGrid in
  C.collage w h [(C.toForm fullLayout)]

stateOverTime : Signal State
stateOverTime = Signal.foldp upstate initInputState buttonMailbox.signal



main = 
  Signal.map2 view stateOverTime Window.dimensions





