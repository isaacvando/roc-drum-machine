app "drum"
    packages {
        w4: "platform/main.roc",
    }
    imports [
        w4.Task.{ Task },
        w4.W4,
    ]
    provides [main, Model] to w4

Program : {
    init : Task Model [],
    update : Model -> Task Model [],
}

Model : {}

main : Program
main = { init, update }

init : Task Model []
init =
    {} <- W4.setPalette colors |> Task.await
    {} <- W4.setDrawColors drawColors |> Task.await

    Task.ok {}

update : Model -> Task Model []
update = \model ->
    {} <- drawBoard |> Task.await
    Task.ok model

drawBoard : Task {} []
drawBoard =
    {} <- drawRow 45 |> Task.await
    {} <- drawRow 65 |> Task.await
    {} <- drawRow 85 |> Task.await
    {} <- drawRow 105 |> Task.await
    Task.ok {}

drawRow : I32 -> Task {} []
drawRow = \y ->
    Task.loop 0 \n ->
        if
            n == 16
        then
            Task.ok (Done {})
        else
            {} <- drawCell (10 * n) y |> Task.await
            Task.ok (Step (n + 1))

drawCell : I32, I32 -> Task {} []
drawCell = \x, y ->
    W4.rect { x, y, width: 10, height: 10 }

colors = {
    color1: 0x000000,
    color2: 0x027ed6,
    color3: 0x000ff00,
    color4: 0x0000ff,
}

drawColors = {
    primary: Color1,
    secondary: Color2,
    tertiary: Color3,
    quaternary: Color4,
}

