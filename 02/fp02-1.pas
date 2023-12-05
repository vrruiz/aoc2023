{$mode ObjFPC}
program fp02_1;

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
  gcRestriction: TGameCount;

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

function CountFile(FileName: AnsiString; gcRestriction: TGameCount): LongInt;
var
  slInput: TStringList;
  i, c: Integer;
  restricted: Boolean;
  total, localTotal: LongInt;
  gclCountList: TGameCountList;

begin
  slInput := TStringList.Create;
  total := 0;
  try
    // Reads line
    slInput.LoadFromFile(FileName);
    for i := 0 to slInput.Count - 1 do
    begin
      WriteLn(slInput[i]);
      // Parses line and counts colors
      gclCountList := ParseString(slInput[i]);
      restricted := False;
      for c := 0 to gclCountList.Count - 1 do
      begin
        WriteLn(' -', gclCountList[c].red, ' ', gclCountList[c].green, ' ', gclCountList[c].blue);
        // Check restrictions for this set
        if ((gclCountList[c].red > gcRestriction.red) or
            (gclCountList[c].green > gcRestriction.green) or
            (gclCountList[c].blue > gcRestriction.blue)) then
          restricted := True; // This set is above limits
      end;
      if restricted = False then
      begin
        total := total + (i+1);
        WriteLn(' :OK');
      end;
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
  gcRestriction := TGameCount.Create;
  gcRestriction.red := 12;
  gcRestriction.green := 13;
  gcRestriction.blue := 14;

  // Test
  total := CountFile('test-1.txt', gcRestriction);
  WriteLn('Test total: ', total);
  Assert(total = 8);

  // Exercise
  total := CountFile('input-1.txt', gcRestriction);
  WriteLn('Input total: ', total);

  FreeAndNil(gcRestriction);
end.
