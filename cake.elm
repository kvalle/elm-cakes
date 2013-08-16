import Window
import Mouse
import Random

-- DEFINITIONS

(gameWidth, gameHeight) = (800, 500)
maxCakes = 10

data Input = Click (Int,Int) | Random (Int,Int)

type State = { cakes:[(Int,Int)] }
type GameInput = { newCake:(Int,Int), click:(Int,Int)}

-- SIGNALS

randomPos : Signal (Int,Int)
randomPos = 
    let tick = (every second)
        randX = (Random.range 20 gameWidth tick)
        randY = (Random.range 20 gameHeight tick)
    in lift2 (,) randX randY

input : Signal GameInput
input =
    let currentClick = sampleOn Mouse.clicks Mouse.position
        mergeInputs p c = {newCake=p, click=c}
    in mergeInputs <~ randomPos ~ currentClick

input2 : Signal GameInput
input2 =
    let currentClick = sampleOn Mouse.clicks Mouse.position
        mergeInputs p c = {newCake=p, click=c}
    in mergeInputs <~ randomPos ~ currentClick

isNotTooClose : (Int,Int) -> (Int,Int) -> Bool
isNotTooClose p1 p2 =
    let dist (x,y) (x',y') = sqrt <| (x-x')^2 + (y-y')^2
        distance = dist p1 p2
    in distance > 45

update : GameInput -> State -> State
update input oldState = 
    let newCakes = if length oldState.cakes < maxCakes 
                    then input.newCake::oldState.cakes
                    else oldState.cakes
        filteredCakes = filter (isNotTooClose input.click) newCakes
    in {oldState | cakes <- filteredCakes}

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
