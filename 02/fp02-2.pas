{$mode ObjFPC}
program fp02_2;

uses
  Classes, Crt, Generics.Collections, Types, SysUtils, StrUtils;

type
  TGameCount = class
    red: Integer;
    green: Integer;
    blue: Integer;
  end;
  TGameCountList = specialize TObjectList<TGameCount>;

var
  total: LongInt;

function CountCubes(sGame: String): TGameCount;
var
  gcCubes: TGameCount;

begin
  gcCubes.red := 0;
  gcCubes.green := 0;
  gcCubes.blue := 0;
  Result := gcCubes;
end;

function ParseString(sInput: AnsiString): TGameCountList;
var
  saGame, saSet, saColors, saNumbers: TStringDynArray;
  gclGameList: TGameCountList;
  gcGame: TGameCount;
  s, c, n, t: Integer;

begin
  // Parses a line
  gclGameList := TGameCountList.Create;
  // Splits "Game n: <rest>"
  saGame := SplitString(sInput, ': ');
  if (Length(saGame) = 2) then
  begin
    // Splits "<set 1>; <set 2>; ...; <set n>"
    saSet := SplitString(saGame[1], '; ');
    for s := 0 to Length(saSet) - 1 do
    begin
      // Splits "x color1, y color2, ..., z colorn"
      saColors := SplitString(saSet[s], ', ');
      gcGame := TGameCount.Create;
      for c := 0 to Length(saColors) - 1 do
      begin
        // Splits "x color"
        saNumbers := SplitString(saColors[c], ' ');
        t := StrToInt(saNumbers[0]);
        case saNumbers[1] of
          'red'  : gcGame.red := t;
          'green': gcGame.green := t;
          'blue' : gcGame.blue := t;
        else
          WriteLn('Incorrect input: ', saGame[1]);
        end;
      end;
      gclGameList.Add(gcGame);
    end;
  end;
  Result := gclGameList;
end;

function CountFile(FileName: AnsiString): LongInt;
var
  slInput: TStringList;
  i, c: Integer;
  restricted: Boolean;
  total, localTotal: LongInt;
  gclCountList: TGameCountList;
  gcGameMax: TGameCount;

begin
  slInput := TStringList.Create;
  total := 0;
  try
    // Reads line
    slInput.LoadFromFile(FileName);
    for i := 0 to slInput.Count - 1 do
    begin
      gcGameMax := TGameCount.Create;
      gcGameMax.red := 0;
      gcGameMax.green := 0;
      gcGameMax.blue := 0;
      WriteLn(slInput[i]);
      // Parses line and counts colors
      gclCountList := ParseString(slInput[i]);
      for c := 0 to gclCountList.Count - 1 do
      begin
        WriteLn(' -', gclCountList[c].red, ' ', gclCountList[c].green, ' ', gclCountList[c].blue);
        if gcGameMax.red < gclCountList[c].red then gcGameMax.red := gclCountList[c].red;
        if gcGameMax.green < gclCountList[c].green then gcGameMax.green := gclCountList[c].green;
        if gcGameMax.blue < gclCountList[c].blue then gcGameMax.blue := gclCountList[c].blue;
      end;
      WriteLn(' =', gcGameMax.red, ' ', gcGameMax.green, ' ', gcGameMax.blue);
      total := total + (gcGameMax.red * gcGameMax.green * gcGameMax.blue);
      FreeAndNil(gcGameMax);
    end;
  except
    on E: Exception do
      WriteLn('Error reading file: ', E.Message);
  end;

  FreeAndNil(gclCountList);
  FreeAndNil(slInput);
  Result := total;
end;

begin
  ClrScr;

  // Test
  total := CountFile('test-1.txt');
  WriteLn('Test total: ', total);
  Assert(total = 2286);

  // Exercise
  total := CountFile('input-1.txt');
  WriteLn('Input total: ', total);
end.
