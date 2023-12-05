{$mode ObjFPC}
program fp01_2;

uses
  Classes, FGL, Crt, SysUtils;

var
  slInput: TStringList;
  i, c: Integer;
  total: LongInt;

function ReplaceNumbers(s: String): String;
var
  sReplace: String;

begin
  s := StringReplace(s, 'oneight', '18', [rfReplaceAll]);
  s := StringReplace(s, 'twone', '21', [rfReplaceAll]);
  s := StringReplace(s, 'threeight', '38', [rfReplaceAll]);
  s := StringReplace(s, 'fiveight', '58', [rfReplaceAll]);
  s := StringReplace(s, 'sevenine', '79', [rfReplaceAll]);
  s := StringReplace(s, 'eightwo', '82', [rfReplaceAll]);
  s := StringReplace(s, 'eighthree', '83', [rfReplaceAll]);
  s := StringReplace(s, 'nineight', '98', [rfReplaceAll]);
  s := StringReplace(s, 'one', '1', [rfReplaceAll]);
  s := StringReplace(s, 'two', '2', [rfReplaceAll]);
  s := StringReplace(s, 'three', '3', [rfReplaceAll]);
  s := StringReplace(s, 'four', '4', [rfReplaceAll]);
  s := StringReplace(s, 'five', '5', [rfReplaceAll]);
  s := StringReplace(s, 'six', '6', [rfReplaceAll]);
  s := StringReplace(s, 'seven', '7', [rfReplaceAll]);
  s := StringReplace(s, 'eight', '8', [rfReplaceAll]);
  s := StringReplace(s, 'nine', '9', [rfReplaceAll]);
  Result := s;
end;

function CountDigits(s: String): Integer;
var
  i, c: Integer;
  first, last: Integer;

begin
  c := 0;
  first := -1;
  last := -1;
  for i := 1 to Length(s) do
  begin
    try
      c := StrToInt(s[i]);
      if first = -1 then
      begin
        first := c;
        last := c;
      end
      else
        last := c;
    except on E: EConvertError do

    end;
  end;
  if first = -1 then
    Result := -1
  else
    Result := first * 10 + last;
end;

function CountFile(FileName: String): LongInt;
var
  i, c: Integer;
  total: LongInt;

begin
  slInput := TStringList.Create;
  total := 0;

  try
    slInput.LoadFromFile(FileName);
    for i := 0 to slInput.Count - 1 do
    begin
      c := CountDigits(ReplaceNumbers(slInput[i]));
      total := total + c;
      // WriteLn(slInput[i], ' ', c);
    end;

  except
    on E: Exception do
      WriteLn('Error reading file: ', E.Message);
  end;

  slInput.Free;
  Result := total;
end;

begin
  ClrScr;

  total := CountFile('test-2.txt');
  WriteLn('Test total: ', total);

  total := CountFile('input-1.txt');
  WriteLn('Final total: ', total);
end.