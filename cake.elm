import Window
import Mouse

clickPos =
    sampleOn Mouse.clicks Mouse.position

makeCake w h x y = 
    image 500 573 "/cake.gif" 
        |> width 45 
        |> toForm 
        |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)

display (w,h) (x,y) = 
    let cake = makeCake w h
    in  collage w h [cake x y]

main =
    display <~ Window.dimensions ~ clickPos
