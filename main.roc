app "main"
    packages {
        w4: "platform/main.roc",
    }
    imports [
        w4.Task.{ Task },
        w4.W4.{ Palette },
    ]
    provides [main, Model] to w4

Program : {
    init : Task Model [],
    update : Model -> Task Model [],
}

Model : {
    roll : Roll,
    frame : U64,
    mouseDown : Bool,
}

# As in piano roll
Roll : List Row

Row : List Cell

Cell : {
    enabled : Bool,
    focused : Bool,
    lastClicked : U64,
}

main : Program
main = { init, update }

init : Task Model []
init =
    {} <- W4.setPalette colors |> Task.await
    {} <- W4.setDrawColors drawColors |> Task.await

    emptyRow = List.repeat
        {
            enabled: Bool.false,
            focused: Bool.false,
            lastClicked: 0,
        }
        16

    model = {
        roll: List.repeat emptyRow rows,
        frame: 0,
        mouseDown: Bool.false,
    }

    Task.ok model

update : Model -> Task Model []
update = \model ->
    mouse <- W4.getMouse |> Task.await
    {} <- draw model |> Task.await

    # When the mouse is released we register a click and update the roll
    didClick = !mouse.left && model.mouseDown
    roll =
        when getCellIndex mouse is
            Ok index if didClick ->
                updateCell model.roll index \c ->
                    { c & enabled: !c.enabled }

            Ok index ->
                model.roll
                |> clearFocused
                |> updateCell index \c ->
                    { c & focused: !c.focused }

            _ -> model.roll

    {
        roll,
        frame: Num.addWrap model.frame 1,
        mouseDown: mouse.left,
    }
    |> Task.ok

# This is a temporary solution. Focused should be a single value on the model instead.
clearFocused : Roll -> Roll
clearFocused = \roll ->
    roll
    |> List.map \row ->
        row
        |> List.map \cell ->
            { cell & focused: Bool.false }

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

updateCell : Roll, (Nat, Nat), (Cell -> Cell) -> List Row
updateCell = \roll, (x, y), f ->
    row <- List.update roll y
    cell <- List.update row x
    f cell

# Parameters
offset = 45
space = 20
cellLength = 10
rows = 4

# Drawing
draw : Model -> Task {} []
draw = \model ->
    {} <- drawRoll model.roll |> Task.await
    drawIndicator model

drawIndicator : Model -> Task {} []
drawIndicator = \model ->
    # Assuming we maintain 60 FPS, this means 120 BPM
    index = (model.frame // 30) % 16
    x = index * 10 |> Num.toI32
    y = offset + rows * space - 5
    drawShape = W4.oval { height: 8, width: 8, x, y }
    drawShapeWithColors drawShape { border: Color4, fill: Color4 }

drawRoll : Roll -> Task {} []
drawRoll = \roll ->
    Task.loop 0 \n ->
        if n == rows then
            Done {} |> Task.ok
        else
            row = List.get roll n |> unwrap
            y = offset + n * space |> Num.toI32
            {} <- drawRow row y |> Task.await
            Step (n + 1) |> Task.ok

drawRow : Row, I32 -> Task {} []
drawRow = \row, y ->
    Task.loop 0 \n ->
        if n == 16 then
            Done {} |> Task.ok
        else
            cellState = List.get row n |> unwrap
            x = 10 * n |> Num.toI32
            {} <- drawCell cellState x y |> Task.await
            Step (n + 1) |> Task.ok

drawCell : Cell, I32, I32 -> Task {} []
drawCell = \cell, x, y ->
    if cell.enabled || cell.focused then
        drawShape = W4.rect { x, y, width: 10, height: 10 }
        drawShapeWithColors drawShape { border: Color2, fill: Color3 }
    else
        W4.rect { x, y, width: 10, height: 10 }

drawShapeWithColors : Task {} [], { border : Palette, fill : Palette } -> Task {} []
drawShapeWithColors = \drawShape, borderAndFill ->
    {} <- W4.setShapeColors borderAndFill |> Task.await
    {} <- drawShape |> Task.await
    W4.setShapeColors { border: Color2, fill: Color1 }

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

# Utils
unwrap = \x ->
    when x is
        Ok val -> val
        Err _ -> crash "bad unwrap"
