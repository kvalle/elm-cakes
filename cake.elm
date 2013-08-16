import Window
import Mouse
import Random

-- DEFINITIONS

(gameWidth, gameHeight) = (800, 500)
maxCakes = 10
cakeSize = 45

type Pos = (Int, Int)
type State = { cakes:[Pos] }

data Input pos = Click pos | Rand pos

-- INPUTS

input : Signal (Input Pos)
input = 
    let tick = (every second)
        randX = (Random.range 20 gameWidth tick)
        randY = (Random.range 20 gameHeight tick)
        clicks = Click <~ sampleOn Mouse.clicks Mouse.position
        randoms = lift Rand <| (,) <~ randX ~ randY
    in merge clicks randoms

-- GAME UPDATE

initialState : State
initialState = {cakes=[]}

isNotTooClose : Pos -> Pos -> Bool
isNotTooClose p1 p2 =
    let dist (x,y) (x',y') = sqrt <| (x-x')^2 + (y-y')^2
        distance = dist p1 p2
    in distance > cakeSize

update : Input Pos -> State -> State
update input state = 
    case input of 
        Click pos -> { state | cakes <- filter (isNotTooClose pos) state.cakes }
        Rand pos -> if length state.cakes < maxCakes 
                    then { state | cakes <- pos::state.cakes }
                    else state
            
-- DISPLAY

display : Pos -> State -> Element
display (w,h) {cakes} = 
    let makeCake (x, y) = 
            image 500 573 "/cake.gif" 
                |> width cakeSize 
                |> toForm 
                |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
    in collage w h <| map makeCake cakes

-- MAIN

main =
    let state = foldp update initialState input
    in  display <~ Window.dimensions ~ state
