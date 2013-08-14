import Window
import Mouse
import Random

(gameWidth, gameHeight) = (800, 500)

dist (x,y) (x',y') = sqrt <| (x-x')^2 + (y-y')^2

type State = { cakes:[(Int,Int)] }
type Input = { newCake:(Int,Int), click:(Int,Int)}

randomPos : Signal (Int,Int)
randomPos = 
    let tick = (every second)
        randX = (Random.range 20 gameWidth tick)
        randY = (Random.range 20 gameHeight tick)
    in lift2 (,) randX randY

cakeCoords = foldp (::) [] randomPos

currentClick = sampleOn Mouse.clicks Mouse.position
clickedCoords = foldp (::) [] currentClick

together : (Int,Int) -> (Int,Int) -> Input
together p c = {newCake=p, click=c}

state = together <~ randomPos ~ currentClick

isNotTooClose : (Int,Int) -> (Int,Int) -> Bool
isNotTooClose p1 p2 =
    let distance = dist p1 p2
    in distance > 45

update : Input -> State -> State
update input oldState = 
    let filteredCakes = filter (isNotTooClose input.click) oldState.cakes
    in {oldState | cakes <- input.newCake::filteredCakes}

initialState : State
initialState = {cakes=[]}

allTheCakes = foldp update initialState state

makeCakeAt w h (x, y) = 
    image 500 573 "/cake.gif" 
        |> width 45 
        |> toForm 
        |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)

display (w,h) cakeCoords {cakes} = 
    let makeCake = makeCakeAt w h
    in  collage w h <| map makeCake cakes

-- MAIN

main =
    display <~ Window.dimensions ~ cakeCoords ~ allTheCakes
