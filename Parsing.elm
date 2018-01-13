module Parsing where


import Random
import String
import Color exposing (..)
import Window
import Time
import Text as T

isNumber : String -> Bool
isNumber s = 
  case (String.toInt s) of
    Ok _ -> True
    Err _ -> False

pickOutElem : List a -> a
pickOutElem l = 
  case l of
    [x] -> x
    x::y::rest -> x
    _ -> Debug.crash "pickOutElem failed"

plusOrMin : String -> String
plusOrMin s = 
  let fullList = (String.split " " s) in
  let n = String.length s in 
  let lastElem = List.drop ((List.length fullList) - 1) fullList in
  let lastElemStr = pickOutElem lastElem in 
  if lastElemStr == "~" then 
    String.slice 0 (n-1) s
  else String.join " " (List.append fullList ["~"])

negateString : String -> String
negateString s =
  let n = String.length s in
  if String.startsWith "~" s then "~" ++ s
  else String.slice 1 (n-1) s


backspace : String -> String
backspace s = 
  let n = String.length s in
  if n <= 0 then s
  else
    let fullList = (String.split "" s) in
    let lastElem = List.drop ((List.length fullList) - 1) fullList in
    let lastElemStr = pickOutElem lastElem in 
    if lastElemStr == " " then String.slice 0 (n-2) s else
    String.slice 0 (n-1) s

parseStringToCompute : String -> List String
parseStringToCompute s =
  let isNotSpace = \x -> ((x /= " ") && (x /= "")) in
  List.filter isNotSpace (String.split " " s)


stringContainsX : String -> Bool
stringContainsX s = 
  String.contains "x" s














