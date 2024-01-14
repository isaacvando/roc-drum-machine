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

Model : {
    crashCymbal : Row,
    hihat : Row,
    snare : Row,
    kick : Row,
}

Row : List [On, Off]

main : Program
main = { init, update }

init : Task Model []
init =
    {} <- W4.setPalette colors |> Task.await
    {} <- W4.setDrawColors drawColors |> Task.await

    model = {
        crashCymbal: emptyRow,
        hihat: emptyRow,
        snare: emptyRow,
        kick: emptyRow,
    }

    Task.ok model

emptyRow : Row
emptyRow = List.repeat Off 16

update : Model -> Task Model []
update = \model ->
    {} <- drawBoard model |> Task.await
    Task.ok model

drawBoard : Model -> Task {} []
drawBoard = \model ->
    {} <- drawRow model.crashCymbal 45 |> Task.await
    {} <- drawRow model.hihat 65 |> Task.await
    {} <- drawRow model.snare 85 |> Task.await
    {} <- drawRow model.kick 105 |> Task.await
    Task.ok {}

drawRow : Row, I32 -> Task {} []
drawRow = \row, y ->
    Task.loop 0 \n ->
        if
            n == 16
        then
            Task.ok (Done {})
        else
            cellState = List.get row n |> unwrap
            x = 10 * n |> Num.toI32
            {} <- drawCell cellState x y |> Task.await
            Task.ok (Step (n + 1))

drawCell : [On, Off], I32, I32 -> Task {} []
drawCell = \state, x, y ->
    when state is
        On ->
            {} <- W4.setShapeColors { border: Color2, fill: Color2 } |> Task.await
            W4.rect { x, y, width: 10, height: 10 }

        Off -> W4.rect { x, y, width: 10, height: 10 }

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

unwrap = \x ->
    when x is
        Ok val -> val
        Err _ -> crash "bad unwrap"
