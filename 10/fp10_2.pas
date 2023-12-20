{$mode ObjFPC}
{$H+}
program fp10_2;

uses
    Classes, Crt, Generics.Collections, SysUtils;

type
    TMazeDirection = (mdNorth, mdSouth,
                      mdWest, mdEast,
                      mdStart);
    TMazeDirectionList = specialize TList<TMazeDirection>;
    TMazePipe = (mpVertical, mpHorizontal,
                 mpNorthEast, mpNorthWest,
                 mpSouthEast, mpSouthWest,
                 mpGround, mpStart, mpEnd,
                 mpVisited, mpInside,
                 mpOutside, mpError);
    TMaze = Array of Array of TMazePipe;
    TLocation = record
        x: Integer;
        y: Integer;
    end;
    TLocationList = specialize TList<TLocation>;

var
    total: Integer;

procedure ShowMaze(maze: TMaze);
var
    i, n: Integer;

begin
    for i := Low(maze) to High(maze) do
    begin
        for n := Low(maze[i]) to High(maze[i]) do
        begin
            case maze[i][n] of
                {
                mpVertical: Write('║');
                mpHorizontal: Write('═');
                mpNorthEast: Write('╚');
                mpNorthWest: Write('╝');
                mpSouthEast: Write('╔');
                mpSouthWest: Write('╗');
                mpGround: Write('░');
                mpStart: Write('*');
                mpVisited: Write('█');
                }
                mpVertical: Write('|');
                mpHorizontal: Write('-');
                mpNorthEast: Write('L');
                mpNorthWest: Write('J');
                mpSouthEast: Write('F');
                mpSouthWest: Write('7');
                mpGround: Write('.');
                mpStart: Write('*');
                mpVisited: Write(':');
                mpInside: Write('I');
                mpOutside: Write('O');
                mpError: Write('?');
            end;
        end;
        WriteLn;
    end;
end;


function CoordinatesInRange(
    const maze: Tmaze;
    const x, y: Integer): Boolean;

begin
    if ((y < Low(maze)) or (y > High(maze)) or
        (x < Low(maze[0])) or (x > High(maze[0]))) then
        Result := False
    else
        Result := True;
end;


function ExpandStart(const slInput: TStringList; const x, y: Integer; dir: TMazeDirection): AnsiChar;
var
    connector: AnsiChar;
    newx, newy: Integer;
    pipes: set of Char;

begin
    connector := #0;
    newx := x;
    newy := y;
    case dir of
        mdNorth:
            begin
                Dec(newy);
                connector := '|';
                pipes := ['|', '7', 'F'];
            end;
        mdSouth:
            begin
                Inc(newy);
                connector := '|';
                pipes := ['|', 'J', 'L'];
            end;
        mdEast:
            begin
                Inc(newx);
                connector := '-';
                pipes := ['-', '7', 'J'];
            end;
        mdWest:
            begin
                Dec(newx);
                connector := '-';
                pipes := ['-', 'F', 'L'];
            end;
    end;
    Result := '.';
    if ((newy >= 0) and (newy < slInput.Count) and
        (newx >= 1) and (newx <= Length(slInput[newy])) and
        (slInput[newy][newx] in pipes)) then
    begin
        Result := connector
    end;
end;


function FillExpanded(const slInput: TStringList): TStringList;
var
    x, y: Integer;
    slMaze: TStringList;
    line_a, line_b, line_c: String;

begin
    slMaze := TStringList.Create;
    for y := 0 to slInput.Count - 1 do
    begin
        line_a := '';
        line_b := '';
        line_c := '';
        for x := 1 to Length(slInput[y]) do
        begin
            case slInput[y][x] of
                '.':
                    begin
                        line_a := line_a + '...';
                        line_b := line_b + '...';
                        line_c := line_c + '...';
                    end;
                '7':
                    begin
                        line_a := line_a + '...';
                        line_b := line_b + '-7.';
                        line_c := line_c + '.|.';
                    end;
                'J':
                    begin
                        line_a := line_a + '.|.';
                        line_b := line_b + '-J.';
                        line_c := line_c + '...';
                    end;
                'L':
                    begin
                        line_a := line_a + '.|.';
                        line_b := line_b + '.L-';
                        line_c := line_c + '...';
                    end;
                'F':
                    begin
                        line_a := line_a + '...';
                        line_b := line_b + '.F-';
                        line_c := line_c + '.|.';
                    end;
                '-':
                    begin
                        line_a := line_a + '...';
                        line_b := line_b + '---';
                        line_c := line_c + '...';
                    end;
                'S':
                    begin
                        line_a := line_a + '.' + ExpandStart(slInput, x, y, mdNorth) + '.';
                        line_b := line_b + ExpandStart(slInput, x, y, mdWest) + 'S' + ExpandStart(slInput, x, y, mdEast);
                        line_c := line_c + '.' + ExpandStart(slInput, x, y, mdSouth) + '.';
                    end;
                '|':
                    begin
                        line_a := line_a + '.|.';
                        line_b := line_b + '.|.';
                        line_c := line_c + '.|.';
                    end;
                #13:
                      begin
                      end;
                else
                    WriteLn('Error: Character not supported: ', slInput[y][x]);
            end;
        end;
        slMaze.Add(line_a);
        slMaze.Add(line_b);
        slMaze.Add(line_c);
    end;
    Result := slMaze;
end;


function CompressMaze(expMaze: TMaze): TMaze;
var
    maze: TMaze;
    x, y: Integer;

begin
    SetLength(maze, High(expMaze) div 3 + 1, High(expMaze[0]) div 3 + 1);
    for y := Low(maze) to High(maze) do
        for x := Low(maze[0]) to High(maze[0]) do
            maze[y][x] := expMaze[y * 3 + 1][x * 3 + 1];
    Result := maze;
end;


function StringListToMaze(slInput: TStringList): TMaze;
var
    i, n, starts: Integer;
    maze: TMaze;
begin
    SetLength(maze, slInput.Count, Length(slInput[0]));
    starts := 0;
    for i := 0 to slInput.Count - 1 do
    begin
        // Fill row
        for n := 0 to Length(slInput[i]) - 1 do
        begin
            case slInput[i][n+1] of
                '|': maze[i][n] := mpVertical;
                '-': maze[i][n] := mpHorizontal;
                'L': maze[i][n] := mpNorthEast;
                'J': maze[i][n] := mpNorthWest;
                'F': maze[i][n] := mpSouthEast;
                '7': maze[i][n] := mpSouthWest;
                '.': maze[i][n] := mpGround;
                'S':
                    begin
                        maze[i][n] := mpStart;
                        Inc(starts);
                    end;
                else
                    begin
                        maze[i][n] := mpError;
                        WriteLn('Warning: Unknown character in line ', i,
                                ': ', slInput[i][n+1], ' - ', slInput[i]);
                    end;
            end;
        end;
    end;
    if (starts <> 1) then
        WriteLn('Warning: Incorrect number of starts: ', starts);
    Result := maze;
end;


function PipesConnected(
    const newx, newy: Integer;
    const dir: TMazeDirection;
    const maze: TMaze): Boolean;

var
    connected: Boolean;
    pipe: TMazePipe;

begin
    // x and y are already sanitized by NewCoordinates
    connected := False;
    pipe := maze[newy][newx];
    case dir of
        mdNorth:
            if (pipe in [mpVertical, mpSouthEast, mpSouthWest, mpStart]) then
                connected := True;
        mdSouth:
            if (pipe in [mpVertical, mpNorthEast, mpNorthWest, mpStart]) then
                connected := True;
        mdEast:
            if (pipe in [mpHorizontal, mpNorthWest, mpSouthWest, mpStart]) then
                connected := True;
        mdWest:
            if (pipe in [mpHorizontal, mpNorthEast, mpSouthEast, mpStart]) then
                connected := True;
    end;
    Result := connected;
end;


function NewCoordinates(
    var x, y: Integer;
    const dir: TMazeDirection;
    const maze, mazeVisits: TMaze): Boolean;
var
    newx, newy: Integer;
    connected: Boolean;

begin
    newx := x;
    newy := y;
    connected := False;
    case dir of
        mdStart:
            begin
                x := newx;
                y := newy;
                Exit(True);
            end;
        mdNorth:
            newy := newy - 1;
        mdSouth:
            newy := newy + 1;
        mdEast:
            newx := newx + 1;
        mdWest:
            newx := newx - 1;
    end;
    if ((CoordinatesInRange(maze, newx, newy) = False) or
        (mazeVisits[newy][newx] = mpVisited) or
        (not (maze[newy][newx] in [mpStart, mpVertical, mpHorizontal,
                             mpNorthEast, mpNorthWest,
                             mpSouthEast, mpSouthWest]))) then
        Exit(False);
    connected := PipesConnected(newx, newy, dir, maze);
    if (connected = True) then
    begin
        x := newx;
        y := newy;
    end;
    Result := connected;
end;


function StartPipeWall(
    const startx, starty: Integer;
    const maze: TMaze): TLocationList;

var
    x, y: Integer;
    pipe: TMazePipe;
    newDir: TMazeDirection;
    mazeDirs: TMazeDirectionList;
    mazeVisits: TMaze;
    loc: TLocation;
    wall: TLocationList;
    step: Integer;
    finished, connections: Boolean;

const
    LOOPLIMIT = 10000000;

begin
    wall := TLocationList.Create;
    if (maze[starty][startx] <> mpStart) then
        Exit(wall);
    SetLength(mazeVisits, High(maze) + 1, High(maze[0]) + 1);
    for y := Low(maze) to High(maze) do
        for x := Low(maze[0]) to High(maze[0]) do
            mazeVisits[y][x] := maze[y][x];
    x := startx;
    y := starty;
    loc.x := x;
    loc.y := y;
    mazeVisits[y][x] := mpVisited;
    wall.Add(loc);
    step := 0;
    finished := False;
    repeat
        mazeDirs := TMazeDirectionList.Create;
        case maze[y][x] of
                mpStart:
                    for newDir in [mdNorth, mdSouth, mdEast, mdWest] do
                        mazeDirs.Add(newDir);
                mpHorizontal:
                    for newDir in [mdEast, mdWest] do
                        mazeDirs.Add(newDir);
                mpVertical:
                    for newDir in [mdNorth, mdSouth] do
                        mazeDirs.Add(newDir);
                mpNorthEast:
                    for newDir in [mdNorth, mdEast] do
                        mazeDirs.Add(newDir);
                mpNorthWest:
                    for newDir in [mdNorth, mdWest] do
                        mazeDirs.Add(newDir);
                mpSouthEast:
                    for newDir in [mdSouth, mdEast] do
                        mazeDirs.Add(newDir);
                mpSouthWest:
                    for newDir in [mdSouth, mdWest] do
                        mazeDirs.Add(newDir);
            end;
        connections := False;
        for newDir in mazeDirs do
            if (NewCoordinates(x, y, newDir, maze, mazeVisits) = True) then
            begin
                loc.x := x;
                loc.y := y;
                mazeVisits[y][x] := mpVisited;
                wall.Add(loc);
                connections := True;
                break;
            end;
        if (connections = False) then
            finished := True;
        Inc(step);
        FreeAndNil(mazeDirs);
    until ((finished = True) or (step >= LOOPLIMIT));
    if (step >= LOOPLIMIT) then
    begin
        FreeAndNil(wall); // Free used memory
        wall := TLocationList.Create;
    end;
    Result := wall;
end;


function LoopMazeXY(const startx, starty: Integer;
                  const maze: TMaze;
                  var mazeVisits: TMaze): Boolean;
var
    x, y: Integer;
    loc, newloc: TLocation;
    locQueue, newVisits: TLocationList;
    pipe: TMazePipe;
    inside: Boolean;

begin
    loc.x := startx;
    loc.y := starty;
    inside := True;
    locQueue := TLocationList.Create;
    newVisits := TLocationList.Create;
    locQueue.Add(loc);
    while (locQueue.Count > 0) do
    begin
        loc := locQueue.Last;
        locQueue.Delete(locQueue.Count - 1);
        if (maze[loc.y][loc.x] = mpGround) then
        begin
            mazeVisits[loc.y][loc.x] := mpVisited;
            newVisits.Add(loc);
            if ((loc.x = Low(maze[0])) or (loc.x = High(maze[0])) or
                (loc.y = Low(maze)) or (loc.y = High(maze))) then
            begin
                inside := False;
            end;
        end;
        for y := -1 to 1 do
            for x := -1 to 1 do
            begin
                newloc.x := loc.x + x;
                newloc.y := loc.y + y;
                if ((CoordinatesInRange(maze, newloc.x, newloc.y) = True) and
                    (maze[newloc.y][newloc.x] = mpGround) and
                    (CoordinatesInRange(mazeVisits, newloc.x, newloc.y) = True) and
                    not (mazeVisits[newloc.y][newloc.x] in [mpVisited, mpInside, mpOutside])) then
                begin
                    locQueue.Add(newloc);
                end;
            end;
    end;
    // Translate new mpVisited to mpInside or mpOutside
    if (inside = True) then
        pipe := mpInside
    else
        pipe := mpOutside;
    for loc in newVisits do
        mazeVisits[loc.y][loc.x] := pipe;
    FreeAndNil(newVisits);
    FreeAndNil(locQueue);
    Result := inside;
end;


function SolveMaze(maze: TMaze): Integer;
var
    mazeVisits: TMaze;
    mazeClean: TMaze;
    newVisits: TLocationList;
    wall: TLocationList;
    x, y, i, total: Integer;
    startx, starty: Integer;
    loc: TLocation;
    inside: Boolean;
    isWall: Boolean;

begin
    total := 0;
    SetLength(mazeVisits, High(maze)+1, High(maze[0])+1);
    SetLength(mazeClean, High(maze)+1, High(maze[0])+1);
    // Find start
    startx := -1;
    starty := -1;
    for y := Low(maze) to High(maze) do
        for x := Low(maze[0]) to High(maze[0]) do
        begin
            mazeClean[y][x] := maze[y][x];
            mazeVisits[y][x] := maze[y][x];
            if (maze[y][x] = mpStart) then
            begin
                startx := x;
                starty := y;
            end;
        end;
    if (startx = -1) then
    begin
        WriteLn('Didnt find a start');
        Exit(-1);
    end;
    // Remove non-wall pipes
    wall := StartPipeWall(startx, starty, maze);
    if (wall.Count > 0) then
        for y := Low(mazeClean) to High(mazeClean) do
            for x := Low(mazeClean[0]) to High(mazeClean[0]) do
            begin
                isWall := False;
                i := 0;
                repeat
                    loc := wall[i];
                    if ((loc.x = x) and (loc.y = y)) then
                        isWall := True;
                    Inc(i);
                until ((i >= wall.Count) or (isWall = True));
                if (isWall = False) then
                begin
                    mazeClean[y][x] := mpGround;
                end;
            end;
    // Solve maze
    ShowMaze(maze);
    for y := -1 to 1 do
        for x := -1 to 1 do
            if ((x <> 0) and (y <> 0)) then
                inside := LoopMazeXY(startx + x, starty + y, mazeClean, mazeVisits);
    WriteLn;
    ShowMaze(mazeVisits);
    mazeVisits := CompressMaze(mazeVisits);
    for y := Low(mazeVisits) to High(mazeVisits) do
        for x := Low(mazeVisits[0]) to High(mazeVisits[0]) do
            if (mazeVisits[y][x] = mpInside) then
                Inc(total);
    ShowMaze(mazeVisits);
    FreeAndNil(wall);
    Result := total;
end;


function ProcessFile(const FileName: String): Integer;
var
    slInput, slExpanded: TStringList;
    maze, expMaze: TMaze;
    i, n, starts, total: Integer;

begin
    slInput := TStringList.Create;
    total := 0;
    starts := 0;
    slInput.LoadFromFile(FileName);

    // Fill maze
    maze := StringListToMaze(slInput);
    ShowMaze(maze); WriteLn;
    // Fill and solve expanded maze
    expMaze := StringListToMaze(FillExpanded(slInput));
    total := SolveMaze(expMaze);

    Result := total;
    FreeAndNil(slInput);
end;

begin
    ClrScr;

    // Test
    total := ProcessFile('test-4.txt');
    WriteLn('Test total: ', total);
    Assert(total = 8);

    // Test
    total := ProcessFile('test-5.txt');
    WriteLn('Test total: ', total);
    Assert(total = 10);

    // Exercise
    total := ProcessFile('input-1.txt');
    WriteLn('Input total: ', total);
end.
