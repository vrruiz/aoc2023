{$mode ObjFPC}
{$H+}
program fp8_2;

uses
    Classes, Crt, Generics.Collections, SysUtils;

type
    TNode = class
        name: String;
        left: String;
        right: String;
    end;
    TNodes = specialize THashMap<String, TNode>;
    TInt64List = specialize TList<Int64>;
    TReadMode = (rmName, rmLeft, rmRight);

var
    total: Int64;

function GreatestCommonDivisor(a, b: Int64): Int64;
var
    temp: Int64;
begin
    while b <> 0 do
    begin
        temp := b;
        b := a mod b;
        a := temp
    end;
    Result := a;
end;

function LeastCommonMultiple(a, b: Int64): Int64;
begin
    Result := b * (a div GreatestCommonDivisor(a, b));
end;

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

function Navigate(nodes: TNodes; paths: String; first: String; maxSteps: Integer): Integer;
var
    steps, currentPath: Integer;
    next: String;
    node: TNode;

begin
    steps := 0;
    currentPath := 1;
    next := first;
    repeat
        node := nodes[next];
        case paths[currentPath] of
            'L':
                next := node.left;
            'R':
                next := node.right;
        end;
        Inc(steps);
        Inc(currentPath);
        if currentPath > Length(paths) then currentPath := 1;
    until ((next[3] = 'Z') or (steps >= maxSteps));
    Result := steps;
end;

function ProcessFile(FileName: String): Int64;
var
    i: Integer;
    steps: TInt64List;
    lcm: Int64;
    slInput, startNodes: TStringList;
    paths: String;
    line: String;
    nodeName: String;
    node: TNode;
    nodes: TNodes;

begin
    slInput := TStringList.Create;
    startNodes := TStringList.Create;
    nodes := TNodes.Create;
    steps := TInt64List.Create;
    lcm := 0;
    slInput.LoadFromFile(FileName);
    paths := slInput[0];
    for i := 2 to slInput.Count - 1 do
    begin
        node := ParseString(slInput[i]);
        nodes.Add(node.name, node);
        if (node.name[3] = 'A') then
            startNodes.Add(node.name);
        if ((node.name = node.left) or (node.name = node.right)) then
            WriteLn('Warning: Self reference in ', node.name);
    end;
    WriteLn(startNodes.Count);
    for nodeName in startNodes do
    begin
        steps.Add(Navigate(nodes, paths, nodeName, nodes.Count * nodes.Count));
        WriteLn(nodeName, ' ', steps[steps.Count - 1]);
    end;
    if (steps.Count > 0) then
    begin
        lcm := steps[0];
        for i := 1 to steps.Count - 1 do
            lcm := leastCommonMultiple(lcm, steps[i]);
        Result := lcm;
    end
    else
        Result := 0;
    FreeAndNil(steps);
    FreeAndNil(nodes);
    FreeAndNil(startNodes);
    FreeAndNil(slInput);
end;

begin
    ClrScr;

    // Test
    total := ProcessFile('test-2.txt');
    WriteLn('Test total: ', total);
    Assert(total = 6);

    // Exercise
    total := ProcessFile('input-1.txt');
    WriteLn('Input total: ', total);
end.
