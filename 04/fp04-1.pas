{$mode ObjFPC}
program fp4_1;

uses
  Classes, Crt, Generics.Collections, Types, SysUtils, StrUtils;

type
  TIntegerList = specialize TList<Integer>;
  TIntegerLists = specialize TList<TIntegerList>;
  CardMode = (cmCard, cmWinning, cmOwned);

var
  total: LongInt;

function ParseLine(sInput: AnsiString): TIntegerLists;
var
  i, list: Integer;
  number: String;
  ch: Char;
  ilCard: TIntegerLists;
  mode: CardMode;

begin
    // Parses a line
    ilCard := TIntegerLists.Create;
    ilCard.Add(TIntegerList.Create); // Winning
    ilCard.Add(TIntegerList.Create); // Owned
    mode := cmCard;
    list := 0;
    number := '';
    for i := 1 to Length(sInput) do
    begin
        ch := sInput[i];
        case ch of
            ':':
                mode := cmWinning;
            '|':
                begin
                    mode := cmOwned;
                    list := 1;
                end;
            ' ':
                begin;
                    if ((mode <> cmCard) and (Length(number) > 0)) then
                    begin
                       ilCard[list].Add(StrToInt(number));
                       number := '';
                    end;
                end;
            '0'..'9':
                begin
                    if (mode <> cmCard) then number := number + ch;
                    if (i = Length(sInput)) then ilCard[list].Add(StrToInt(number));
                end;
        end; // case ch
    end; // for i
    Result := ilCard;
end;

function CountFile(FileName: AnsiString): LongInt;
var
  slInput: TStringList;
  i, l, n, points, total: Integer;
  ilCard: TIntegerLists;

begin
  slInput := TStringList.Create;
  total := 0;
  try
      // Read lines
      slInput.LoadFromFile(FileName);
      // Process lines
      for i := 0 to slInput.Count - 1 do
      begin
          WriteLn(slInput[i]);
          ilCard := ParseLine(slInput[i]);
          // Print card
          {
          for l := 0 to 1 do
              for n := 0 to ilCard[l].Count - 1 do
                  WriteLn('  ', l, '[', n, '] = ', ilCard[l][n]);
          }
          // Compare winning and owned cards
          points := 0;
          for n := 0 to ilCard[0].Count - 1 do
          begin
              if (ilCard[1].IndexOf(ilCard[0][n]) <> -1) then
              begin
                  // WriteLn(ilCard[0][n], ' owned');
                  if (points = 0) then points := 1
                  else points := points * 2;
              end;
          end;
          total := total + points;
          FreeAndNil(ilCard);
      end;
  except
      on E: Exception do
          WriteLn('Error reading file: ', E.Message);
  end;

  FreeAndNil(slInput);
  Result := total;
end;

begin
  ClrScr;

  // Test
  total := CountFile('test-1.txt');
  WriteLn('Test total: ', total);
  Assert(total = 13);

  // Exercise
  total := CountFile('input-1.txt');
  WriteLn('Input total: ', total);
end.
