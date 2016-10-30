import Html exposing (Html)
import Html.Events exposing (onClick)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Array2D exposing (Array2D)
import Array exposing (Array)
import Random exposing (..)
import Set exposing (Set)
import Dict exposing (Dict)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


flattenArray2D : Array2D a -> List a
flattenArray2D array2d = Array.toList (Array.foldr Array.append Array.empty array2d.data)

-- MODEL


type alias Model =
  { time : Time
  , doCalculation : Bool
  , rows : Int
  , cols : Int
  , seed : Maybe Seed
  , numDistricts: Int
  , numParties : Int
  , districts : Array2D Int
  , bestSoFar : Array2D Int
  , seenSoFar : Set (List Int)
  , majorities : Array2D Int
  , allSwapValues : Array (Point, Point)
  }

riddlerExpressMajorities : Array2D Int
riddlerExpressMajorities = 
  Array2D.fromList 
    [ [0, 0, 1, 1, 1],
      [1, 0, 0, 1, 0],
      [0, 1, 1, 1, 1],
      [1, 1, 0, 0, 1],
      [1, 1, 1, 1, 0]
    ]

riddlerExpressDistricts : Array2D Int
riddlerExpressDistricts =
  Array2D.fromList
    [ [0, 0, 0, 0, 0],
      [1, 1, 1, 1, 1],
      [2, 2, 2, 2, 2],
      [3, 3, 3, 3, 3],
      [4, 4, 4, 4, 4]
    ]

riddlerClassicMajorities : Array2D Int
riddlerClassicMajorities =
  Array2D.fromList
    [ [1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1]
    , [1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1]
    , [1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1]
    , [1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1]
    , [1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1]
    , [1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    , [1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1]
    , [1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1]
    , [0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1]
    , [1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1]
    ]

riddlerClassicDistricts : Array2D Int
riddlerClassicDistricts =
  Array2D.fromList
    [ [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6]
    , [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6]
    , [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6]
    , [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6]
    , [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6]
    , [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6]
    , [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6]
    , [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6]
    , [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6]
    , [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6]
    ]

init : (Model, Cmd Msg)
init =
  ( { time = 0
    , doCalculation = False
    , numParties = 2
    , seed = Nothing
    , seenSoFar = Set.empty
    , allSwapValues = Array.empty
    , rows = 5
    , cols = 5
    , numDistricts = 5
    , majorities = riddlerExpressMajorities
    , districts = riddlerExpressDistricts
    , bestSoFar = riddlerExpressDistricts
    }, Cmd.none)

--    , rows = 10
--    , cols = 14
--    , numDistricts = 7
--    , majorities = riddlerClassicMajorities
--    , districts = riddlerClassicDistricts
--    , bestSoFar = riddlerClassicDistricts


-- UPDATE


type Msg
  = Tick Time | StartPressed

type alias Point = {x : Int, y: Int}

normalize : Array2D Int -> List Int
normalize arr2d =
  let 
    flattened = (flattenArray2D arr2d)
    normalizedDict = 
    List.foldr 
      (\v dict -> 
        if Dict.member v dict 
          then dict 
          else Dict.insert v (Dict.size dict) dict)
      Dict.empty
      flattened
  in 
    List.map 
      (\v ->
        case Dict.get v normalizedDict of
          Nothing -> -1
          Just a -> a
      )
      flattened

doSwap : (Point, Point) -> Array2D Int -> Array2D Int
doSwap points arr2d =
  let
    p1 = fst points
    p2 = snd points
    v1 = getVal p1.x p1.y arr2d
    v2 = getVal p2.x p2.y arr2d
  in
    Array2D.set p2.y p2.x v1 (Array2D.set p1.y p1.x v2 arr2d)

iterateModel : Int -> Model -> Float -> Model
iterateModel i model newTime =
  let 
    seed =
      case model.seed of
        Nothing -> Random.initialSeed (round newTime)
        Just a -> a
    allSwapValues = Array.fromList (allPossibleDistrictsToSwap model.numDistricts model.districts model.majorities model.seenSoFar)
    swapIndexAndSeed =
      Random.step
        (Random.int 0 ((Basics.max (Array.length allSwapValues) 1) - 1))
        seed
    point = Array.get (fst swapIndexAndSeed) allSwapValues
    swapTuple = 
      case point of
        Nothing -> ({x = 0, y = 0}, {x = 0, y = 0})
        Just a -> a
    newDistricts = doSwap swapTuple model.districts
    bestSoFarScore = scoreBoard model.numDistricts model.bestSoFar model.majorities
    newScore = scoreBoard model.numDistricts newDistricts model.majorities
    bestSoFar = if newScore > bestSoFarScore then newDistricts else model.bestSoFar
    newSeed = (snd swapIndexAndSeed)
    newModel =
      { model | time = newTime
        , seed = Just newSeed
        , districts = newDistricts
        , allSwapValues = allSwapValues
        , bestSoFar = bestSoFar 
        , seenSoFar = Set.insert (normalize newDistricts) model.seenSoFar }
  in
    if i <= 0 then newModel else (iterateModel (i - 1) newModel newTime)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartPressed -> ({ model | doCalculation = not model.doCalculation }, Cmd.none)
    Tick newTime -> if not model.doCalculation 
      then
        (model, Cmd.none)
      else 
        (iterateModel 1 model newTime, Cmd.none)

scoreBoard: Int -> Array2D Int -> Array2D Int -> Int
scoreBoard numDistricts districts majorities =
  let
    party = 0 -- hardcoded for now, but we'll assume we're looking out for party '0'
    districtList = flattenArray2D districts
    majorityList = flattenArray2D majorities
    zipped = List.map2 (,) districtList majorityList
    winList =
      List.foldr
        (\v wl ->
          let
            district = fst v
            majority = snd v
            wlVal = 
              case Array.get district wl of
                Nothing -> 0
                Just a -> a
          in
            Array.set district (wlVal + (if majority == party then 1 else -1)) wl
        )
        (Array.repeat numDistricts 0)
        zipped

    in Array.length (Array.filter (\x -> x >= 0) winList)

getVal: Int -> Int -> Array2D Int -> Int
getVal x y arr2d =
  case Array2D.get y x arr2d of
    Nothing -> -1
    Just a -> a

countSpaces: Int -> Int -> Int -> List (Int, Int) -> Array2D Int -> List (Int, Int)
countSpaces v x y visited arr2d =
  if List.member (x, y) visited || (getVal x y arr2d) /= v
    then visited
    else 
      let
        newVisited = (x, y) :: visited
        points = 
          List.filter 
            (\t -> 
              let 
                x1 = fst t
                y1 = snd t
              in
                x1 >= 0 && y1 >= 0 && x1 < arr2d.columns && y1 < arr2d.columns
            )
            [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
      in
        List.foldr (\p vis -> countSpaces v (fst p) (snd p) vis arr2d) newVisited points


isAttached: Int -> Int -> Int -> Int -> Array2D Int -> Bool
isAttached numDistricts v x y arr2d =
  (List.length (countSpaces v x y [] arr2d)) == ((arr2d.columns * (Array.length arr2d.data)) // numDistricts)

okayToSwap : Int -> Int -> Int -> Int -> Int -> Array2D Int -> Bool
okayToSwap numDistricts x1 y1 x2 y2 arr2d =
  let
    v1 = getVal x1 y1 arr2d
    v2 = getVal x2 y2 arr2d
    swapped = doSwap ({x = x1, y = y1}, {x = x2, y = y2}) arr2d
  in
    v1 /= -1 &&
    v2 /= -1 &&
    (isAttached numDistricts v1 x2 y2 swapped) &&
    (isAttached numDistricts v2 x1 y1 swapped)

-- enumerate all the possible values to swap
allPossibleDistrictsToSwap : Int -> Array2D Int -> Array2D Int -> Set (List Int) -> List (Point, Point)
allPossibleDistrictsToSwap numDistricts districts majorities seenSoFar =
  let
    allPossible =
      List.concat
        (flattenArray2D
          (Array2D.indexedMap
            (\y x v ->
                List.concat
                  (flattenArray2D 
                    (Array2D.indexedMap 
                      (\y2 x2 v2 ->
                          if v /= v2 && (okayToSwap numDistricts x y x2 y2 districts)
                            then [({x = x, y = y}, {x = x2, y = y2})]
                            else []
                      )
                      districts
                    )
                  )
            ) districts
          )
        )
    notSeenYet =
      List.filter (\p -> not (Set.member (normalize (doSwap p districts)) seenSoFar)) allPossible
    scoredDistricts =
      List.map (\p -> (scoreBoard numDistricts (doSwap p districts) majorities) ) notSeenYet
    maxValue = 
      case List.maximum scoredDistricts of
        Nothing -> -1
        Just a -> a
    filteredMax = 
      List.filterMap
        (\v -> if (fst v) == maxValue then Just (snd v) else Nothing)
        (List.map2 (,) scoredDistricts notSeenYet)
  in
    if List.isEmpty filteredMax then allPossible else filteredMax

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (Time.second / 1000) Tick



-- VIEW

cellSize : Int
cellSize = 50

cellSizeStr : String
cellSizeStr = toString cellSize

cellToView : Int -> Int -> Int -> Svg msg
cellToView vy vx i =
  let
    color = 
      if i == 1 then "#E4001B" else "#A5D1F3"
  in
    rect  [ x (toString (cellSize * vx)), 
            y (toString (cellSize * vy)),
            width cellSizeStr,
            height cellSizeStr,
            fill color] []

districtToView : Int -> Int -> Int -> Svg msg
districtToView vy vx i =
  text' [ x (toString (cellSize * vx) ), 
          y (toString (cellSize * vy + (cellSize // 4)  )),
          fontFamily "Verdana",
          fontSize (toString (cellSize // 4))] [text (toString i)]

modelToView : Array2D Int -> Array2D Int -> List (Svg msg)
modelToView majorities districts =
  let
    majorityViews = Array2D.indexedMap cellToView majorities
    districtViews = Array2D.indexedMap districtToView districts
  in
    List.append
      (flattenArray2D majorityViews)
      (flattenArray2D districtViews)

view : Model -> Html Msg
view model =
  let
    angle =
      turns (Time.inMinutes model.time)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)
  in
    Html.div []
  
      [ 
      svg [ viewBox "0 0 250 250", width "300px" ] (modelToView model.majorities model.districts)
      , Html.button
          [ onClick StartPressed ]
          [ text (if model.doCalculation then "Stop" else "Start") ]
      , svg [ viewBox "0 0 250 250", width "300px" ] (modelToView model.majorities model.bestSoFar)
      , text (toString (scoreBoard model.numDistricts model.districts model.majorities))
      , text (toString (scoreBoard model.numDistricts model.bestSoFar model.majorities))
      , text (toString model.allSwapValues)
      ]
