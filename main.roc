app "main"
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

Model : List Row

Row : List [On, Off]

main : Program
main = { init, update }

init : Task Model []
init =
    {} <- W4.setPalette colors |> Task.await
    {} <- W4.setDrawColors drawColors |> Task.await

    model = List.repeat emptyRow rows

    Task.ok model

emptyRow : Row
emptyRow = List.repeat Off 16

update : Model -> Task Model []
update = \model ->
    {} <- drawBoard model |> Task.await
    mouse <- W4.getMouse |> Task.await

    when getCellIndex mouse is
        Err _ -> Task.ok model
        Ok index ->
            {} <- W4.text (Inspect.toStr index) { x: 0, y: 0 } |> Task.await
            toggleCell model index |> Task.ok

getCellIndex = \mouse ->
    yIndex =
        List.range { start: At 0, end: At 3 }
        |> List.findFirstIndex \n ->
            start = offset + n * space
            mouse.y >= start && mouse.y < start + cellLength

    xIndex =
        List.range { start: At 0, end: At 15 }
        |> List.findFirstIndex \n ->
            start = n * 10
            mouse.x >= start && mouse.x < start + cellLength

    when (xIndex, yIndex) is
        (Ok x, Ok y) -> Ok (x, y)
        _ -> Err MouseNotInCell

toggleCell : Model, (Nat, Nat) -> Model
toggleCell = \model, (x, y) ->
    row <- List.update model y
    cell <- List.update row x
    when cell is
        On -> Off
        Off -> On

expect
    model = [[On, On], [On, On]]
    updated = toggleCell model (1, 1)
    updated == [[On, On], [On, Off]]

drawBoard : Model -> Task {} []
drawBoard = \model ->
    Task.loop 0 \n ->
        if
            n == rows
        then
            Done {} |> Task.ok
        else
            row = List.get model n |> unwrap
            y = offset + n * space |> Num.toI32
            {} <- drawRow row y |> Task.await
            Step (n + 1) |> Task.ok

offset = 45
space = 20
cellLength = 10
rows = 4

drawRow : Row, I32 -> Task {} []
drawRow = \row, y ->
    Task.loop 0 \n ->
        if
            n == 16
        then
            Done {} |> Task.ok
        else
            cellState = List.get row n |> unwrap
            x = 10 * n |> Num.toI32
            {} <- drawCell cellState x y |> Task.await
            Step (n + 1) |> Task.ok

drawCell : [On, Off], I32, I32 -> Task {} []
drawCell = \state, x, y ->
    when state is
        On ->
            {} <- W4.setShapeColors { border: Color2, fill: Color2 } |> Task.await
            {} <- W4.rect { x, y, width: 10, height: 10 } |> Task.await
            W4.setShapeColors { border: Color1, fill: Color2 }

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
