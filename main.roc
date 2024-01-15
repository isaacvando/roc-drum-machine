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

Model : { roll : Roll, frame : U64 }

# As in piano roll
Roll : List Row

Row : List Cell

Cell : { state : [On, Off], lastClicked : U64 }

main : Program
main = { init, update }

init : Task Model []
init =
    {} <- W4.setPalette colors |> Task.await
    {} <- W4.setDrawColors drawColors |> Task.await

    model = { roll: List.repeat emptyRow rows, frame: 0 }

    Task.ok model

emptyRow : Row
emptyRow = List.repeat { state: Off, lastClicked: 0 } 16

update : Model -> Task Model []
update = \model ->
    mouse <- W4.getMouse |> Task.await
    {} <- draw model |> Task.await
    {} <- W4.text (Inspect.toStr model.frame) { x: 0, y: 0 } |> Task.await

    roll =
        when getCellIndex mouse is
            Ok index if mouse.left ->
                toggleCell model.roll index

            _ -> model.roll

    { roll, frame: Num.addWrap model.frame 1 }
    |> Task.ok

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

toggleCell : Roll, (Nat, Nat) -> List Row
toggleCell = \roll, (x, y) ->
    row <- List.update roll y
    cell <- List.update row x
    when cell.state is
        On -> { cell & state: Off }
        Off -> { cell & state: On }

draw : Model -> Task {} []
draw = \model ->
    Task.loop 0 \n ->
        if
            n == rows
        then
            Done {} |> Task.ok
        else
            row = List.get model.roll n |> unwrap
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

drawCell : Cell, I32, I32 -> Task {} []
drawCell = \cell, x, y ->
    when cell.state is
        On ->
            {} <- W4.setShapeColors { border: Color2, fill: Color3 } |> Task.await
            {} <- W4.rect { x, y, width: 10, height: 10 } |> Task.await
            W4.setShapeColors { border: Color2, fill: Color1 }

        Off -> W4.rect { x, y, width: 10, height: 10 }

colors = {
    # black
    color1: 0x000000,
    # blue
    color2: 0x027ed6,
    # grey
    color3: 0x4a4a4a,
    # red
    color4: 0xe30505,
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
