{$mode ObjFPC}
program fp01_1;

uses
  Classes, FGL, Crt, SysUtils;

const
  C_FNAME = 'input-1.txt';

var
  slInput: TStringList;
  i, c, total: Integer;


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


begin
  ClrScr;
  // Read file
  slInput := TStringList.Create;

  try
    slInput.LoadFromFile(C_FNAME);
    total := 0;
    for i := 0 to slInput.Count - 1 do
    begin
      c := CountDigits(slInput[i]);
      total := total + c;
      WriteLn(slInput[i], ' ', c);
    end;
    WriteLn('Total: ', total);

  except on E: EInOutError do
    WriteLn('Error reading file: ', E.Message);
  end;

  slInput.Free;
end.
