{$mode ObjFPC}
program fp6_1;

uses
    Classes, Crt, Generics.Collections, Types, SysUtils, StrUtils;

type
    TNumber = Integer;
    TListNumber = specialize TList<TNumber>;
    TListNumbers = specialize TList<TListNumber>;

var
    total: LongInt;

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
        ' ':
            if (Length(number) > 0) then
            begin
                lNumber.Add(StrToInt(number));
                number := '';
            end;
        '0'..'9':
            number := number + ch;
    end;
    if (Length(number) > 0) then lNumber.Add(StrToInt(number));
    Result := lNumber;
end;

function CheckWins(lNumbers: TListNumbers): LongInt;
var
    i, v, wins: Integer;
    s, time, dist: TNumber;
    mult: LongInt;

begin
    assert(lNumbers.Count = 2);
    assert(lNumbers[0].Count = lNumbers[1].Count);
    wins := 0;
    mult := 1;
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
            begin
                Inc(wins);
                Write('*');
            end;
            WriteLn('time:', time, ' dist:', dist, ' hold:', v, ' remaining:', time - v, ' reach:', s);
        end;
        mult := mult * wins;
        WriteLn('=', wins, ' m:', mult);
    end;
    Result := mult;
end;

function ProcessFile(FileName: AnsiString): Integer;
var
    slInput: TStringList;
    lNumber: TListNumber;
    lNumbers: TListNumbers;
    line: String;
    i, total: Integer;

begin
    slInput := TStringList.Create;
    lNumbers := TListNumbers.Create;
    slInput.LoadFromFile(FileName);
    // Process line
    for line in slInput do
    begin
        WriteLn(line);
        lNumber := ParseString(line);
        lNumbers.Add(lNumber);
    end;
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
    Assert(total = 288);

    // Part 1
    total := ProcessFile('input-1.txt');
    WriteLn('Input total: ', total);
end.
