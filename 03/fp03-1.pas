{$mode ObjFPC}
program fp3_1;

uses
  Classes, Crt, FGL, Types, SysUtils, StrUtils;

type
  TNumberList = specialize TFPGList<Integer>;

var
  total: LongInt;

function ProcessFile(FileName: AnsiString): LongInt;
var
  slInput: TStringList;
  i, n: Integer;
  x, y: Integer;
  isNumber: Boolean;
  isSymbol: Boolean;
  ch: AnsiChar;
  iStart, iEnd: Integer;
  numbers: TNumberList;

begin
  slInput := TStringList.Create;
  numbers := TNumberList.Create;
  total := 0;
  try
    // Reads lines
    slInput.LoadFromFile(FileName);
    // Search numbers
    for i := 0 to slInput.Count - 1 do
    begin
      Write('+');
      isSymbol := False;
      isNumber := False;
      iStart := 0;
      iEnd := 0;
      for n := 1 to Length(slInput[i]) do
      begin
        ch := slInput[i][n];
        case ch of
          '0'..'9':
            begin
              // Is a number
              Write(ch);
              // Search symbol in the boundaries
              if (isSymbol = False) then
              begin
                for y := (i - 1) to (i + 1) do
                begin
                  for x := (n - 1) to (n + 1) do
                  begin
                    if ((y >= 0) and (y < slInput.Count)) and
                        (x > 0) and (x < Length(slInput[i])) then
                    begin
                      case slInput[y][x] of
                        '0'..'9':
                          begin
                          end; // 0..9
                        '.':
                          begin
                          end; // ..
                        else
                          begin
                            isSymbol := True;
                          end; // case else
                      end; // case
                    end; // if boundaries
                  end; // for x
                end; // for y
              end; // if symbol
              if (isNumber = False) then
              begin
                isNumber := True;
                iStart := n;
              end;
              // Is this the last character?
              if ((n = Length(slInput[i])) and (isSymbol = True)) then
              begin
                iEnd := n + 1;
                numbers.Add(StrToInt(Copy(slInput[i], iStart, iEnd - iStart)));
              end;
            end; // case 0..9
          else
          begin
            Write(ch);
            // Is not a number
            if (isNumber = True) then
            begin
              iEnd := n;
              if (isSymbol = True) then numbers.Add(StrToInt(Copy(slInput[i], iStart, iEnd - iStart)));
              isNumber := False;
              isSymbol := False;
              iStart := 0;
              iEnd := 0;
            end;
          end; // case else
        end; // case ch
      end; // for n
      WriteLn('+');
    end; // for i

    for n in numbers do
      WriteLn(n);

    // Sum numbers
    for i in numbers do
    begin
      total := total + i;
    end;

  except
    on E: Exception do
      WriteLn('Error reading file: ', E.Message);
  end;

  FreeAndNil(numbers);
  FreeAndNil(slInput);
  Result := total;
end;

begin
  ClrScr;

  // Test
  total := ProcessFile('test-1.txt');
  WriteLn('Test total: ', total);
  Assert(total = 4361);

  // Exercise
  total := ProcessFile('input-1.txt');
  WriteLn('Input total: ', total);
end.
