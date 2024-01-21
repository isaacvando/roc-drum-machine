app "main"
    packages {
        w4: "platform/main.roc",
    }
    imports [
        w4.Task.{ Task },
        w4.W4.{ Palette },
        # w4.Sprite.{ Sprite },
    ]
    provides [main, Model] to w4

Program : {
    init : Task Model [],
    update : Model -> Task Model [],
}

Model : {
    roll : Roll,
    frame : U64,
    sinceLastBeat: U64,
    currentBeat : I32,
    mouseDown : Bool,
    leftDown : Bool,
    rightDown : Bool,
    interval : U64,
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
        sinceLastBeat: 0,
        currentBeat: 0,
        mouseDown: Bool.false,
        leftDown: Bool.false,
        rightDown: Bool.false,
        interval: 8,
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

    
    gamepad <- W4.getGamepad Player1 |> Task.await

    # The screen is updated at 60hz. We use the frame count and the fact that it is 60hz to determine the tempo.
    # Because we only have 60 calls to update per second, we are limited in the number of different tempos possible.
    # 60hz = 60bps = 3600bpm
    # For each beat (quarter note) we have 4 sixteenth notes, each of which require a distinct update. 
    # The interval is the number of updates between each sixteenth note.
    # 3600 / 4 = 900, so to compute the current bpm, we do the following:
    bpm = Num.toF32 (900.0 / (Num.toF32 model.interval))

    {} <- drawText"$(floatToStr bpm) BPM" |> Task.await

    interval =
            if model.leftDown && model.rightDown then
                model.interval
            else if !gamepad.left && model.leftDown then           
                model.interval
                |> Num.addSaturated 1
            else if !gamepad.right && model.rightDown then
                model.interval
                |> Num.sub 1
                |> Num.max 1
            else
                model.interval

    sinceLastBeat = 
        model.sinceLastBeat 
        |> Num.add 1
        |> Num.rem interval

    currentBeat = 
        if model.sinceLastBeat == 0 then
            model.currentBeat
            |> Num.add 1
            |> Num.rem 16
        else 
            model.currentBeat

    {} <- playSounds model currentBeat |> Task.await

    {
        roll,
        frame: Num.addWrap model.frame 1,
        interval,
        sinceLastBeat,
        currentBeat,
        mouseDown: mouse.left,
        leftDown: gamepad.left,
        rightDown: gamepad.right
    }
    |> Task.ok

floatToStr : F32 -> Str
floatToStr = \f -> 
    intPart = Num.floor f
    decPart = Num.floor (f * 100) - (intPart * 100)
    decStr = Num.toStr decPart
    if List.len (Str.toUtf8 decStr) == 1 then 
        "$(Num.toStr intPart).$(decStr)0"
    else
        "$(Num.toStr intPart).$(decStr)"

drawText : Str -> Task {} []
drawText = \str ->
    {} <- W4.setTextColors { fg: Color2, bg: Color1 } |> Task.await
    {} <- W4.text str { x: 45, y: 10 } |> Task.await
    W4.setTextColors { fg: Color1, bg: Color2 }

getCurrentColumn : Model, I32 -> List Cell
getCurrentColumn = \model, index ->
    model.roll
    |> List.map \row ->
        List.get row (Num.toNat index) |> unwrap

playSounds : Model, I32 -> Task {} []
playSounds = \model, currentBeat ->
    if currentBeat != model.currentBeat then
        getCurrentColumn model currentBeat
        |> playColumn
    else
        Task.ok {}

playColumn : List Cell -> Task {} []
playColumn = \column ->
    List.map2 sounds column \x, y ->
        (x, y)
    |> Task.loop \state ->
        when state is
            [(sound, cell), .. as rest] ->
                task = if cell.enabled then sound else Task.ok {}
                {} <- task |> Task.await
                Task.ok (Step rest)

            [] -> Task.ok (Done {})

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
        List.range { start: At 0, end: At (rows - 1) }
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

# Drawing
draw : Model -> Task {} []
draw = \model ->
    {} <- drawRoll model.roll |> Task.await
    {} <- drawBarMarkers |> Task.await
    drawIndicator model

drawBarMarkers =
    Task.loop 0 \bar ->
        if bar == 4 then
            Done {} |> Task.ok
        else
            x = bar * cellLength * 4
            shape = W4.vline { x, y: offset - 2, len: 2 }
            {} <- drawShapeWithColors shape { border: Color4, fill: Color4 } |> Task.await
            Step (bar + 1) |> Task.ok

drawIndicator : Model -> Task {} []
drawIndicator = \model ->
    # Assuming we maintain 60 FPS, this means 120 BPM
    x = model.currentBeat * cellLength
    len = 4
    palette = { border: Color4, fill: Color4 }
    drawShapeWithColors (W4.vline { x, y: offset - len, len }) palette

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
            x = cellLength * n |> Num.toI32
            {} <- drawCell cellState x y |> Task.await
            Step (n + 1) |> Task.ok

drawCell : Cell, I32, I32 -> Task {} []
drawCell = \cell, x, y ->
    if cell.enabled || cell.focused then
        drawShape = W4.rect { x, y, width: cellLength, height: cellLength }
        drawShapeWithColors drawShape { border: Color2, fill: Color3 }
    else
        W4.rect { x, y, width: cellLength, height: cellLength }

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

# Sounds
sounds = [highTom, lowTom, hihat, snare, kick]

kick : Task {} []
kick = W4.tone {
    startFreq: 190,
    endFreq: 40,
    attackTime: 0,
    decayTime: 4,
    sustainTime: 4,
    releaseTime: 4,
    peakVolume: 100,
    volume: 100,
    channel: Triangle,
}

snare : Task {} []
snare = W4.tone {
    startFreq: 400,
    endFreq: 0,
    attackTime: 0,
    decayTime: 6,
    sustainTime: 8,
    releaseTime: 0,
    peakVolume: 45,
    volume: 45,
    channel: Noise,
}

hihat : Task {} []
hihat = W4.tone {
    startFreq: 680,
    endFreq: 675,
    attackTime: 0,
    decayTime: 6,
    sustainTime: 1,
    releaseTime: 0,
    peakVolume: 60,
    volume: 60,
    channel: Pulse1 Eighth,
}

lowTom : Task {} []
lowTom = W4.tone {
    startFreq: 400,
    endFreq: 200,
    attackTime: 2,
    decayTime: 22,
    sustainTime: 14,
    releaseTime: 8,
    peakVolume: 68,
    volume: 58,
    channel: Pulse2 Eighth,
}

highTom : Task {} []
highTom = W4.tone {
    startFreq: 600,
    endFreq: 400,
    attackTime: 2,
    decayTime: 22,
    sustainTime: 14,
    releaseTime: 8,
    peakVolume: 68,
    volume: 58,
    channel: Pulse2 Eighth,
}

# Parameters
offset = 55
space = 10
cellLength = 10
rows = 5

# Utils
unwrap = \x ->
    when x is
        Ok val -> val
        Err _ -> crash "bad unwrap"
