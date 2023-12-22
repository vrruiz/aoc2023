{$mode ObjFPC}
{$H+}
program fp11_1;

uses
    Classes, Crt, Generics.Collections, SysUtils;

type
    TLocation = record
        x: Int64;
        y: Int64;
        newx: Int64;
        newy: Int64;
    end;
    TLocationList = specialize TList<TLocation>;

    TSpace = class(TObject)
    private
        usedCols: Array of Boolean;
        usedRows: Array of Boolean;
    public
        space: TStringList;
        galaxies: TLocationList;
        distances: Array of Array of Int64;
        totalDist: Int64;
        constructor Create(const FileName: String; const H0: Int64);
        procedure ExpandSpace(const H0: Int64);
        function CalculateDistances: Int64;
        destructor Destroy; override;
    end;

var
    space: TSpace;

constructor TSpace.Create(const FileName: String; const H0: Int64);
var
    loc: TLocation;
    i, n: Int64;
    newCols, newRows: Integer;

begin
    inherited Create;
    galaxies := TLocationList.Create;
    space := TStringList.Create;
    space.LoadFromFile(FileName);
    SetLength(usedCols, Length(space[0]));
    SetLength(usedRows, space.Count);
    // Locate galaxies
    for i := 0 to space.Count - 1 do
        for n := 1 to Length(space[i]) do
            if (space[i][n] = '#') then
            begin
                usedCols[n-1] := True;
                usedRows[i] := True;
                loc.x := n - 1;
                loc.y := i;
                loc.newx := loc.x;
                loc.newy := loc.y;
                galaxies.Add(loc);
            end;
    SetLength(distances, galaxies.Count, galaxies.Count);
    // Expand space
    ExpandSpace(H0);
    // Calculate distances
    totalDist := CalculateDistances;
end;

procedure TSpace.ExpandSpace(const H0: Int64);
var
    i, n: Integer;
    loc: TLocation;

begin
    // Expand space
    for i := Low(usedRows) to High(usedRows) do
        if (usedRows[i] = False) then
            for n := 0 to galaxies.Count - 1 do
            begin
                loc := galaxies[n];
                if (loc.y >= i) then
                begin
                    loc.newy := loc.newy + H0 - 1;
                    galaxies[n] := loc;
                end;
            end;
    for i := Low(usedCols) to High(usedCols) do
        if (usedCols[i] = False) then
            for n := 0 to galaxies.Count - 1 do
            begin
                loc := galaxies[n];
                if (loc.x >= i) then
                begin
                    loc.newx := loc.newx + H0 - 1;
                    galaxies[n] := loc;
                end;
            end;
end;

function TSpace.CalculateDistances: Int64;
var
    loc, locA, locB: TLocation;
    i, n, dis: Int64;
    total: Int64;

begin
    total := 0;
    for i := 0 to galaxies.Count - 1 do
    begin
        locA := galaxies[i];
        for n := i to galaxies.Count - 1 do
        begin
            if (i = n) then continue; // Distance 0 to itself
            locB := galaxies[n];
            dis := Abs(locA.newx - locB.newx) + Abs(locA.newy - locB.newy);
            distances[i][n] := dis;
            distances[n][i] := dis;
            total := total + dis;
        end;
    end;
    Result := total;
end;

destructor TSpace.Destroy;
begin
    FreeAndNil(galaxies);
    FreeAndNil(space);
    inherited;
end;

begin
    ClrScr;

    // Test
    space := TSpace.Create('test-1.txt', 10);
    WriteLn('Test total: ', space.totalDist);
    Assert(space.totalDist = 1030);
    space.Destroy;

    // Test
    space := TSpace.Create('test-1.txt', 100);
    WriteLn('Test total: ', space.totalDist);
    Assert(space.totalDist = 8410);
    space.Destroy;

    // Exercise
    space := TSpace.Create('input-1.txt', 1000000);
    WriteLn('Input total: ', space.totalDist);
    space.Destroy;
end.