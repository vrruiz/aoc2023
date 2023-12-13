{$mode ObjFPC}
{$H+}
program fp8_1;

uses
    Classes, Crt, Generics.Collections, Types;

type
    TNode = class
        name: String;
        left: String;
        right: String;
    end;
    TNodes = specialize THashMap<String, TNode>;
    TReadMode = (rmName, rmLeft, rmRight);

var
    total: Integer;
    nodes: TNodes;

function ParseString(sInput: AnsiString): TNode;
var
    node: TNode;
    mode: TReadMode;
    i: Integer;
    ch: Char;
    value: String;

begin
    // Parses a line
    node := TNode.Create;
    mode := rmName;
    value := '';
    for i := 1 to Length(sInput) do
    begin
        ch := sInput[i];
        case ch of
            '(':
                begin
                    mode := rmLeft;
                    node.name := value;
                    value := '';
                end;
            ',':
                begin
                    mode := rmRight;
                    node.left := value;
                    value := ''
                end;
            ')':
                node.right := value;
            '=', ' ':
                begin
                end;
            else
            begin
                value := value + ch;
            end;
        end;
    end;
    Result := node;
end;

function Navigate(nodes: TNodes; paths: String; first: String; last: String; maxSteps: Integer): Integer;
var
    steps, currentPath: Integer;
    element: String;
    node: TNode;

begin
    steps := 0;
    currentPath := 1;
    element := first;
    node := nodes[element];
    WriteLn(node.name, ' ', Length(paths));
    repeat
        case paths[currentPath] of
            'L':
                element := node.left;
            'R':
                element := node.right;
        end;
        if (element <> last) then node := nodes[element];
        Inc(steps);
        Inc(currentPath);
        if currentPath > Length(paths) then currentPath := 1;
    until ((element = last) or (steps >= maxSteps));
    Result := steps;
end;

function ProcessFile(FileName: String): Integer;
var
    i: Integer;
    total, steps: Integer;
    slInput: TStringList;
    paths: String;
    line: String;
    node: TNode;
    nodes: TNodes;

begin
    slInput := TStringList.Create;
    nodes := TNodes.Create;
    total := 0;
    steps := 0;
    slInput.LoadFromFile(FileName);
    paths := slInput[0];
    for i := 2 to slInput.Count - 1 do
    begin
        node := ParseString(slInput[i]);
        nodes.Add(node.name, node);
        if ((node.name = node.left) or (node.name = node.right)) then
            WriteLn('Warning: Self reference in ', node.name);
    end;
    steps := Navigate(nodes, paths, 'AAA', 'ZZZ', nodes.Count * nodes.Count);
    FreeAndNil(nodes);
    FreeAndNil(slInput);
    Result := steps;
end;

begin
    ClrScr;

    // Test
    total := ProcessFile('test-1.txt');
    WriteLn('Test total: ', total);
    Assert(total = 2);

    // Exercise
    total := ProcessFile('input-1.txt');
    WriteLn('Input total: ', total);
end.
