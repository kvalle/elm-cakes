import Window
import Mouse

clickPos = sampleOn Mouse.clicks Mouse.position

cakeCoords = foldp (::) [] clickPos

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
