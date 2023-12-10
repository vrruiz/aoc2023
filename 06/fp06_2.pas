{$mode ObjFPC}
program fp6_1;

uses
    Classes, Crt, Generics.Collections, Types, SysUtils, StrUtils;

type
    TNumber = UInt64;
    TListNumber = specialize TList<TNumber>;
    TListNumbers = specialize TList<TListNumber>;

var
    total: TNumber;

function ParseString(sInput: AnsiString): TListNumber;
var
    lNumber: TListNumber;
    number: String;
    ch: Char;

begin
    // Parses a line
    number := '';
    lNumber := TListNumber.Create;
    for ch in sInput do
    case ch of
        '0'..'9':
            number := number + ch;
    end;
    if (Length(number) > 0) then lNumber.Add(StrToInt64(number));
    Result := lNumber;
end;

function CheckWins(lNumbers: TListNumbers): TNumber;
var
    i, v, wins, time, dist, mult: TNumber;
    s: TNumber;

begin
    assert(lNumbers.Count = 2);
    assert(lNumbers[0].Count = lNumbers[1].Count);
    wins := 0;
    mult := 0;
    for i := 0 to lNumbers[0].Count - 1 do
    begin
        time := lNumbers[0][i];
        dist := lNumbers[1][i];
        wins := 0;
        // Hold the button from 1 to time and caculate
        for v := 1 to time - 1 do
        begin
            s := v * (time - v);
            if (s > dist) then
                Inc(wins);
        end;
        if (mult = 0) then mult := wins
        else mult := mult * wins;
    end;
    Result := mult;
end;

function ProcessFile(FileName: AnsiString): TNumber;
var
    slInput: TStringList;
    lNumber: TListNumber;
    lNumbers: TListNumbers;
    line: String;
    i, total: TNumber;

begin
    slInput := TStringList.Create;
    lNumbers := TListNumbers.Create;
    slInput.LoadFromFile(FileName);
    // Process line
    for line in slInput do lNumbers.Add(ParseString(line));
    // Check wins
    total := CheckWins(lNumbers);
    FreeAndNil(lNumbers);
    FreeAndNil(slInput);
    Result := total;
end;

begin
    ClrScr;

    // Test
    total := ProcessFile('test-1.txt');
    WriteLn('Test total: ', total);
    Assert(total = 71503);

    // Part 2
    total := ProcessFile('input-1.txt');
    WriteLn('Input total: ', total);
end.
