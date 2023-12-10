{$mode ObjFPC}
program fp5_2;

uses
  Classes, Crt, Generics.Collections, SysUtils;

type
  AlmanacMode = (amSeeds, amSoil, amFertilizer,
                 amWater, amLight, amTemperature,
                 amHumidity, amLocation);
  TNumber = UInt64;
  TListNumber = specialize TList<TNumber>;
  TListMap = specialize TList<TListNumber>;
  TListMaps = specialize TList<TListMap>;

var
  total: TNumber;

function ParseString(sInput: AnsiString): TListNumber;
var
  lNumbers: TListNumber;
  number: String;
  ch: Char;
  i: Integer;

begin
    // Parses a line
    lNumbers := TListNumber.Create;
    number := '';
    for i := 1 to Length(sInput) do
    begin
        ch := sInput[i];
        if (i = Length(sInput)) then
        begin
            number := number + ch;
            lNumbers.Add(StrToInt64(number));
        end
        else
        case ch of
            ' ':
                begin
                    if (number <> '') then lNumbers.Add(StrToInt64(number));
                    number := '';
                end;
            '0'..'9':
                number := number + ch;
        end;
    end;
    Result := lNumbers;
end;

procedure ListMaps(lMaps: TListMaps);
var
  lNumbers: TListNumber;
  lMap: TListMap;
  number: TNumber;

begin
    for lMap in lMaps do
    begin
        if lMap = lMaps[0] then continue;
        for lNumbers in lMap do
        begin
            for number in lNumbers do
                Write(number, ' ');
            WriteLn();
        end;
        WriteLn();
    end;
end;

function ProcessMaps(lMaps: TListMaps): Integer;
var
  lNumbers: TListNumber;
  lMap: TListMap;
  i, n: Integer;
  loc, lowest, t: TNumber;

begin
    lowest := 0;
    n := 0;
    while (n < lMaps[0][0].Count) do
    begin
        WriteLn(lMaps[0][0][n], ' times ', lMaps[0][0][n+1], '...');
        for t := 0 to lMaps[0][0][n+1] do
        begin
            loc := lMaps[0][0][n] + t;
            for i := 1 to lMaps.Count - 1 do
            begin
                for lNumbers in lMaps[i] do
                begin
                    // WriteLn(n, ' ', lNumbers[1], ' ', lNumbers[1] + lNumbers[2]);
                    if ((loc >= lNumbers[1]) and (loc < lNumbers[1] + lNumbers[2])) then
                    begin
                        loc := lNumbers[0] + loc - lNumbers[1];
                        break;
                    end;
                end;
            end;
            // WriteLn('Seed: ', lMaps[0][0][n], ' Soil: ', loc);
            if ((lowest = 0) or (loc < lowest)) then
                lowest := loc;
        end;
        n := n + 2;
    end;
    WriteLn('Lowest: ', lowest);
    Result := lowest;
end;

function ProcessFile(FileName: AnsiString): TNumber;
var
  slInput: TStringList;
  lNumbers, lSeeds: TListNumber;
  lMap: TListMap;
  lMaps: TListMaps;
  line: String;
  i, n: Integer;
  total, number, times: TNumber;
  mode: AlmanacMode;

begin
    slInput := TStringList.Create;
    lMaps := TListMaps.Create;
    total := 0;
    mode := amSeeds;
    lMaps.Add(TListMap.Create); // Seed array
    try
        // Reads line
        slInput.LoadFromFile(FileName);
        for line in slInput do
        begin
            if (Length(line) = 0) then continue;
            if (Pos('seeds: ', line) > 0) then
                lMaps[lMaps.Count - 1].Add(ParseString(line))
            else
            case line[1] of
                '0'..'9':
                    lMaps[lMaps.Count - 1].Add(ParseString(line));
                else
                    lMaps.Add(TListMap.Create); // Add new map
            end;
        end;
    except
        on E: Exception do
            WriteLn('Error reading file: ', E.Message);
    end;
    // Process maps
    ListMaps(lMaps);
    Result := ProcessMaps(lMaps);

    FreeAndNil(lMaps);
    FreeAndNil(slInput);
end;

begin
    ClrScr;

    // Test
    total := ProcessFile('test-1.txt');
    WriteLn('Test total: ', total);
    Assert(total = 46);

    // Exercise
    total := ProcessFile('input-1.txt');
    WriteLn('Input total: ', total);
end.
