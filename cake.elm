import Window
import Mouse

display (w,h) (x,y) = 
    let cake posx posy = 
            image 500 573 "/cake.gif" 
                |> width 45 
                |> toForm 
                |> move (toFloat posx - toFloat w / 2, toFloat h / 2 - toFloat posy)
        cakes = [ cake (w `div` 2) (h `div` 2)
                , cake (w `div` 3) (h `div` 4)
                , cake x y]
        drawCakes = collage w h cakes
    in drawCakes

main = 
    lift2 display Window.dimensions Mouse.position
