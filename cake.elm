import Window
import Mouse
import Random
import Color

-- SETTINGS

(gameWidth, gameHeight) = (800, 500)
gameMargin = 30
maxCakes = 10
cakeSize = 49

-- DEFINITIONS

type Pos = (Int, Int)
type State = { cakes:[Pos], num:Int }

data Input pos = Click pos | Rand pos

-- INPUTS

input : Signal (Input Pos)
input = 
    let tick = (every second)
        randX = (Random.range gameMargin (gameWidth - gameMargin) tick)
        randY = (Random.range gameMargin (gameHeight - gameMargin) tick)
        clicks = Click <~ sampleOn Mouse.clicks Mouse.position
        randoms = lift Rand <| (,) <~ randX ~ randY
    in merge clicks randoms

-- GAME UPDATE

initialState : State
initialState = { cakes=[], num=0 }

notTooClose : Pos -> Pos -> Bool
notTooClose p1 p2 =
    let dist (x,y) (x',y') = sqrt <| (x-x')^2 + (y-y')^2
        distance = dist p1 p2
    in distance > cakeSize

update : Input Pos -> State -> State
update input state = 
    case input of 
        Click pos -> let filteredCakes = filter (notTooClose pos) state.cakes
                         cakesClicked = (length state.cakes) - (length filteredCakes)
                     in { state | cakes <- filteredCakes
                                , num <- state.num + cakesClicked }
        Rand pos -> if length state.cakes < maxCakes 
                    then { state | cakes <- pos::state.cakes }
                    else state
            
-- DISPLAY

display : State -> Pos -> Element
display {cakes,num} mouse = 
    let moveTo x y = move (toFloat x - toFloat gameWidth / 2, toFloat gameHeight / 2 - toFloat y)
        makeCake (x, y) = 
            let img = if notTooClose mouse (x, y) 
                      then "/cake.png"
                      else "/cake-green.png"
            in image 1214 1214 img |> width cakeSize |> toForm |> moveTo x y
        line =  { defaultLine | width <- 10 }
        border = rect (toFloat gameWidth) (toFloat gameHeight) |> outlined line
        cakePics = map makeCake cakes
        gameScore = "# cakes: " ++ (show num) |> plainText |> toForm
    in collage gameWidth gameHeight <| gameScore::border::cakePics

-- MAIN

main =
    let state = foldp update initialState input
    in  display <~ state ~ Mouse.position
