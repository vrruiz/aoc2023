{$mode ObjFPC}
{$H+}
program fp9_1;

uses
    Classes, Crt, Generics.Collections, SysUtils;

type
    THistory = specialize TList<Int64>;
    TReport = specialize TList<THistory>;

const
    MAXSTEPS = 100000000;

var
    total: Int64;

function ParseString(sInput: String): THistory;
var
    history: THistory;
    ch: Char;
    i: Integer;
    number: String;

begin
    // Parses a line
    history := THistory.Create;
    number := '';
    for i := 1 to Length(sInput) do
    begin
        case sInput[i] of
            '-', '0'..'9':
                begin
                    number := number + sInput[i];
                    if (i = Length(sInput)) then
                    begin
                        history.Add(StrToInt64(number));
                        number := '';
                    end;
                end;
            else
                if (number <> '') then
                begin
                    history.Add(StrToInt64(number));
                    number := '';
                end;
        end;
    end;
    Result := history;
end;

procedure ShowReport(report: TReport);
var
    history: THistory;
    i, n, value: Int64;

begin
    for i := 0 to report.Count - 1 do
    begin
        history := report[i];
        if (i > 0) then
            for n := 1 to i+1 do Write(' ');
        for value in history do
            Write(value:02, ' ');
        WriteLn;
    end;
end;

function NextValues(report: TReport): Int64;
var
    newReport: TReport;
    history, newHistory, prevHistory: THistory;
    i, n, dif, total, subtotal: Int64;
    step: Integer;
    zero: Boolean;

begin
    total := 0;
    for history in report do
    begin
        newReport := TReport.Create;
        newHistory := THistory.Create;
        for i in history do
            newHistory.Add(i);
        newReport.Add(newHistory);

        // Calculate piramid
        prevHistory := newHistory;
        step := 0;
        repeat
            newHistory := THistory.Create;
            newReport.Add(newHistory);
            zero := True;
            if (prevHistory.Count > 1) then
            begin
                for i := 1 to prevHistory.Count - 1 do
                begin
                    dif := prevHistory[i] - prevHistory[i-1];
                    newHistory.Add(dif);
                    if (dif <> 0) then zero := False;
                end;
            end
            else
            begin
                WriteLn('Error: Only one value');
                ShowReport(newReport);
                Exit(-1);
            end;
            prevHistory := newHistory;
            Inc(step);
        until ((zero = True) or (step >= MAXSTEPS));
        if (step >= MAXSTEPS) then
        begin
            WriteLn('Error: MAXSTEPS');
            exit(-1);
        end;
        // WriteLn('Original');
        // ShowReport(newReport);

        // Calculate new value
        dif := 0;
        subtotal := 0;
        for i := newReport.Count - 1 downto 0 do
        begin
            newHistory := newReport[i];
            if (newHistory = newReport.Last) then
                newHistory.Add(0)
            else if (i = newReport.Count - 2) then
            begin
                dif := newHistory[0];
                newHistory.Add(dif);
            end
            else
            begin
                dif := newHistory[newHistory.Count - 1] + dif;
                newHistory.Add(dif);
                subtotal := dif;
                dif := newHistory[newHistory.Count - 1];
            end;
        end;
        // WriteLn('New');
        // ShowReport(newReport);

        total := total + subtotal;
        FreeAndNil(newReport);
    end;
    Result := total;
end;

function ProcessFile(FileName: String): Int64;
var
    slInput: TStringList;
    report: TReport;
    history: THistory;
    line: String;
    i, total: Int64;

begin
    slInput := TStringList.Create;
    report := TReport.Create;
    slInput.LoadFromFile(FileName);
    for line in slInput do
        report.Add(ParseString(line));
    total := NextValues(report);
    FreeAndNil(report);
    FreeAndNil(slInput);
    Result := total;
end;

begin
    ClrScr;

    // Test
    total := ProcessFile('test-1.txt');
    WriteLn('Test total: ', total);
    Assert(total = 114);

    // Exercise
    total := ProcessFile('input-1.txt');
    WriteLn('Input total: ', total);
end.
