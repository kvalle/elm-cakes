import Window
import Mouse
import Random

-- DEFINITIONS

(gameWidth, gameHeight) = (800, 500)
maxCakes = 10

type Pos = (Int, Int)
type State = { cakes:[Pos] }

data Input pos = Click pos | Rand pos

-- SIGNALS

randomPos : Signal Pos
randomPos = 
    let tick = (every second)
        randX = (Random.range 20 gameWidth tick)
        randY = (Random.range 20 gameHeight tick)
    in lift2 (,) randX randY

input : Signal (Input (Int, Int))
input = 
    let clicks = (\pos -> Click pos) <~ sampleOn Mouse.clicks Mouse.position
        randoms = (\pos -> Rand pos) <~ randomPos
    in merge clicks randoms

isNotTooClose : Pos -> Pos -> Bool
isNotTooClose p1 p2 =
    let dist (x,y) (x',y') = sqrt <| (x-x')^2 + (y-y')^2
        distance = dist p1 p2
    in distance > 45

update : Input Pos -> State -> State
update input oldState = 
    case input of 
        Click pos -> { oldState | cakes <- filter (isNotTooClose pos) oldState.cakes }
        Rand pos -> if length oldState.cakes < maxCakes 
                    then { oldState | cakes <- pos::oldState.cakes }
                    else oldState
            
currentCakes : Signal State
currentCakes = foldp update {cakes=[]} input

-- DRAWING

makeCakeAt w h (x, y) = 
    image 500 573 "/cake.gif" 
        |> width 45 
        |> toForm 
        |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)

display (w,h) {cakes} = 
    let makeCake = makeCakeAt w h
    in  collage w h <| map makeCake cakes

-- MAIN

main =
    display <~ Window.dimensions ~ currentCakes
