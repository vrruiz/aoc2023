{$mode ObjFPC}
program fp5_1;

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
  loc, lowest: TNumber;

begin
    lowest := 0;
    for n := 0 to lMaps[0][0].Count - 1 do
    begin
        loc := lMaps[0][0][n];
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
    // WriteLn('Lowest: ', lowest);
    Result := lowest;
end;

function ProcessFile(FileName: AnsiString): TNumber;
var
  slInput: TStringList;
  lNumbers: TListNumber;
  lMap: TListMap;
  lMaps: TListMaps;
  i, n: Integer;
  total: TNumber;
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
        for i := 0 to slInput.Count - 1 do
        begin
            if (Length(slInput[i]) = 0) then continue;
            if (slInput[i].IndexOf('seeds: ') >= 0) then
                lMaps[lMaps.Count - 1].Add(ParseString(slInput[i]))
            else
            case slInput[i][1] of
                '0'..'9':
                    lMaps[lMaps.Count - 1].Add(ParseString(slInput[i]));
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
    Assert(total = 35);

    // Exercise
    total := ProcessFile('input-1.txt');
    WriteLn('Input total: ', total);
end.
