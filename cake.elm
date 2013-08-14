import Window
import Mouse
import Random

tick = (every second)
random1 = Random.range 0 1000 tick
random2 = Random.range 0 1000 tick
randomPos = lift2 (,) random1 random2

clickPos = sampleOn Mouse.clicks Mouse.position

cakeCoords = foldp (::) [] randomPos

makeCakeAt w h (x, y) = 
    image 500 573 "/cake.gif" 
        |> width 45 
        |> toForm 
        |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)

display (w,h) cakeCoords = 
    let makeCake = makeCakeAt w h
    in  collage w h <| map makeCake cakeCoords

main =
    display <~ Window.dimensions ~ cakeCoords
