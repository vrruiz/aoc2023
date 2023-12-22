{$mode ObjFPC}
{$H+}
program fp11_1;

uses
    Classes, Crt, Generics.Collections, Types, SysUtils, StrUtils, Math;

type
    TLocation = record
        x: Integer;
        y: Integer;
    end;
    TLocationList = specialize TList<TLocation>;

    TSpace = class(TObject)
    private
        spaceOrig: TStringList;
        usedCols: Array of Boolean;
        usedRows: Array of Boolean;
    public
        space: TStringList;
        galaxies: TLocationList;
        distances: Array of Array of Integer;
        totalDist: Integer;
        constructor Create(const FileName: String);
        destructor Destroy; override;
        procedure ShowSpace(const slist: TStringList);
    end;

var
    space: TSpace;

constructor TSpace.Create(const FileName: String);
var
    loc, locA, locB: TLocation;
    i, n, dx, dy, dis: Integer;
    newCols, newRows: Integer;
    total: Integer;
    line: String;

begin
    inherited Create;
    galaxies := TLocationList.Create;
    space := TStringList.Create;
    spaceOrig := TStringList.Create;
    spaceOrig.LoadFromFile(FileName);
    space.AddStrings(spaceOrig);
    SetLength(usedCols, Length(space[0]));
    SetLength(usedRows, space.Count);
    // Locate galaxies
    for i := 0 to space.Count - 1 do
        for n := 1 to Length(space[i]) do
            if (space[i][n] = '#') then
            begin
                usedCols[n-1] := True;
                usedRows[i] := True;
                loc.x := n;
                loc.y := i;
                galaxies.Add(loc);
            end;
    SetLength(distances, galaxies.Count, galaxies.Count);
    // Expand space
    line := '';
    for i := 1 to Length(spaceOrig[0]) do
        line := line + '.';
    for i := spaceOrig.Count - 1 downto 0 do
        if (usedRows[i] = False) then
        begin
            WriteLn('Expand row ', i);
            space.Insert(i, line);
        end;
    for i := Length(spaceOrig[0]) downto 1 do
        if (usedCols[i-1] = False) then
        begin
            WriteLn('Expand col ', i);
            for n := 0 to space.Count - 1 do
            begin
                line := space[n];
                space[n] := Copy(line, 1, i) + '.' + Copy(line, i+1, Length(line));
            end;
        end;
    ShowSpace(space);
    // Recalculate galaxy positions
    galaxies.Clear;
    for i := 0 to space.Count - 1 do
        for n := 1 to Length(space[i]) do
            if (space[i][n] = '#') then
            begin
                loc.x := n;
                loc.y := i;
                galaxies.Add(loc);
            end;
    // Calculate distances
    WriteLn('Galaxies: ', galaxies.Count);
    total := 0;
    for i := 0 to galaxies.Count - 1 do
    begin
        locA := galaxies[i];
        for n := i to galaxies.Count - 1 do
        begin
            if (i = n) then continue; // Distance 0 to itself
            locB := galaxies[n];
            dx := Abs(locA.x - locB.x);
            dy := Abs(locA.y - locB.y);
            dis := Max(dx, dy) + Min(dx, dy);
            distances[i][n] := dis;
            distances[n][i] := dis;
            total := total + dis;
            WriteLn(i+1, '->', n+1, ' (', locA.x, ',', locA.y, ') (', locB.x, ',', locB.y, ') = ',
                    dis, ' ', dx, '-', dy);
        end;
    end;
    totalDist := total;
end;

destructor TSpace.Destroy;
begin
    FreeAndNil(galaxies);
    FreeAndNil(spaceOrig);
    FreeAndNil(space);
    inherited;
end;

procedure TSpace.ShowSpace(const slist: TStringList);
var
    line: String;
begin
    for line in slist do
        WriteLn(line);
end;

begin
    ClrScr;

    // Test
    space := TSpace.Create('test-1.txt');
    WriteLn('Test total: ', space.totalDist);
    space.Destroy;
    Assert(space.totalDist = 374);

    // Exercise
    space := TSpace.Create('input-1.txt');
    WriteLn('Input total: ', space.totalDist);
    space.Destroy;
    Assert(space.totalDist = 374);
end.