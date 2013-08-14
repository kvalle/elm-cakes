import Window
import Mouse
import Random

(gameWidth, gameHeight) = (800, 500)

dist (x,y) (x',y') = sqrt <| (x-x')^2 + (y-y')^2

randomPos = 
    let tick = (every second)
        randX = (Random.range 20 gameWidth tick)
        randY = (Random.range 20 gameHeight tick)
    in lift2 (,) randX randY

currentClick = sampleOn Mouse.clicks Mouse.position
clickedCoords = foldp (::) [] currentClick

cakeCoords = foldp (::) [] randomPos

makeCakeAt w h (x, y) = 
    image 500 573 "/cake.gif" 
        |> width 45 
        |> toForm 
        |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)

display (w,h) cakeCoords click = 
    let makeCake = makeCakeAt w h
        filterCakes = filter (\pos -> (dist click pos) > 30) cakeCoords
    in  collage w h <| map makeCake filterCakes

main =
    display <~ Window.dimensions ~ cakeCoords ~ currentClick
