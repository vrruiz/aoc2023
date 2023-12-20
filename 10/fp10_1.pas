{$mode ObjFPC}
{$H+}
program fp10_1;

uses
    Classes, Crt, Generics.Collections, Types, SysUtils, StrUtils;

type
    TMazeDirection = (mdNorth, mdSouth,
                      mdWest, mdEast,
                      mdStart);
    TMazeDirectionList = specialize TList<TMazeDirection>;
    TMazePipe = (mpVertical, mpHorizontal,
                 mpNorthEast, mpNorthWest,
                 mpSouthEast, mpSouthWest,
                 mpGround, mpStart,
                 mpEnd, mpVisited,
                 mpError);
    TMaze = Array of Array of TMazePipe;
    TSteps = Array of Array of Integer;

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
                mpVertical: Write('║');
                mpHorizontal: Write('═');
                mpNorthEast: Write('╚');
                mpNorthWest: Write('╝');
                mpSouthEast: Write('╔');
                mpSouthWest: Write('╗');
                mpGround: Write('░');
                mpStart: Write('*');
                mpVisited: Write('█');
            end;
        end;
        WriteLn;
    end;
end;


function CoordinatesInRange(
    const x, y: Integer;
    const maze: Tmaze): Boolean;

begin
    if ((y < Low(maze)) or (y > High(maze)) or
        (x < Low(maze[0])) or (x > High(maze[0]))) then
        Result := False
    else
        Result := True;
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
            if (pipe in [mpVertical, mpSouthEast, mpSouthWest]) then
                connected := True;
        mdSouth:
            if (pipe in [mpVertical, mpNorthEast, mpNorthWest]) then
                connected := True;
        mdEast:
            if (pipe in [mpHorizontal, mpNorthWest, mpSouthWest]) then
                connected := True;
        mdWest:
            if (pipe in [mpHorizontal, mpNorthEast, mpSouthEast]) then
                connected := True;
    end;
    Result := connected;
end;

function NewCoordinates(
    out x, y: Integer;
    const prevx, prevy, steps: Integer;
    const dir: TMazeDirection;
    const maze, mazeVisits: TMaze;
    const mazeSteps: TSteps): Boolean;
var
    newx, newy: Integer;
    connected: Boolean;

begin
    newx := prevx;
    newy := prevy;
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
    if ((CoordinatesInRange(newx, newy, maze) = False) or
        ((mazeVisits[newy][newx] = mpVisited) and (mazeSteps[newy][newx] < steps)) or
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

procedure SearchMaze(
    const prevx, prevy, steps: Integer;
    const dir: TMazeDirection;
    const maze: TMaze;
    mazeVisits: TMaze;
    mazeSteps: TSteps);

var
    x, y: Integer;
    pipe: TMazePipe;
    newDir: TMazeDirection;
    mazeDirs: TMazeDirectionList;

begin
    if ((maze[prevy][prevx] = mpStart) and (steps = 0)) then
    begin
        x := prevx;
        y := prevy;
    end
    else if ((maze[prevy][prevx] = mpStart) and (dir = mdStart)) then
        Exit
    else if (NewCoordinates(x, y, prevx, prevy, steps, dir, maze, mazeVisits, mazeSteps) = False) then
        Exit;
    mazeVisits[y][x] := mpVisited;
    mazeSteps[y][x] := steps;
    {WriteLn('prevx:', prevx, ' prevy:', prevy, ' x:', x, ' y:', y, ' steps:', steps, ' dir:', dir);
    ShowMaze(mazeVisits);
    WriteLn;}
    // Continue traversing the maze loop
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
    for newDir in mazeDirs do
        SearchMaze(x, y, steps+1, newDir, maze, mazeVisits, mazeSteps);
    FreeAndNil(mazeDirs);
end;

function TraverseMaze(const maze: TMaze): Integer;
var
    mazeVisits: TMaze;
    mazeSteps: TSteps;
    row, col: Integer;
    startx, starty: Integer;
    maxSteps: Integer;

begin
    if (Length(maze) < 1) then
    begin
        WriteLn('Error: TraverseMaze: Incorrect maze dimensions.');
        Exit(-1);
    end;
    // WriteLn('Maze y:', High(maze)+1, ' x:', High(maze[0])+1);
    SetLength(mazeVisits, High(maze)+1, High(maze[0])+1);
    SetLength(mazeSteps, High(maze)+1, High(maze[0])+1);
    startx := -1;
    starty := -1;
    // Search start coordinates
    for row := Low(mazeVisits) to High(mazeVisits) do
        for col := Low(mazeVisits[row]) to High(mazeVisits[row]) do
        begin
            mazeVisits[row][col] := mpGround;
            mazeSteps[row][col] := 0;
            if (maze[row][col] = mpStart) then
            begin
                startx := col;
                starty := row;
            end;
        end;
    // Don't start
    if ((startx < 0) or (starty < 0)) then
    begin
        WriteLn('Error: No starting point found.');
        Exit(-1);
    end;
    // Search maze
    SearchMaze(startx, starty, 0, mdStart, maze, mazeVisits, mazeSteps);
    // Return max steps
    maxSteps := -1;
    for row := Low(mazeSteps) to High(mazeSteps) do
        for col := Low(mazeSteps[row]) to High(mazeSteps[row]) do
            if (mazeSteps[row][col] > maxSteps) then
                maxSteps := mazeSteps[row][col];
    WriteLn;
    ShowMaze(mazeVisits);
    Result := maxSteps;
end;

function ProcessFile(const FileName: String): Integer;
var
    slInput: TStringList;
    maze: TMaze;
    i, n, starts, total: Integer;

begin
    slInput := TStringList.Create;
    total := 0;
    starts := 0;
    slInput.LoadFromFile(FileName);
    // Fill maze
    for i := 0 to slInput.Count - 1 do
    begin
        // Resize maze array
        if (i = 0) then
            SetLength(maze, slInput.Count, Length(slInput[i]))
        else if (Length(slInput[i]) <> Length(maze[0])) then
        begin
            WriteLn('Incorrect maze dimensions in row ', i, ' ', slInput[i]);
            Exit(-1);
        end;
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
                    WriteLn('Error: Unknown character in line ', i,
                            ': ', slInput[i][n], ' - ', slInput[i]);
                    Exit(-1);
                end;
            end;
        end;
    end;
    if (starts <> 1) then
    begin
        WriteLn('Error: Incorrect number of starts: ', starts);
        Exit(-1);
    end;
    ShowMaze(maze);
    FreeAndNil(slInput);
    Result := TraverseMaze(maze);
end;

begin
    ClrScr;

    // Test
    total := ProcessFile('test-1.txt');
    WriteLn('Test total: ', total);
    Assert(total = 4);

    // Test
    total := ProcessFile('test-2.txt');
    WriteLn('Test total: ', total);
    Assert(total = 8);

    // Test
    total := ProcessFile('test-3.txt');
    WriteLn('Test total: ', total);
    Assert(total = 4);

    // Exercise
    total := ProcessFile('input-1.txt');
    WriteLn('Input total: ', total);
end.
