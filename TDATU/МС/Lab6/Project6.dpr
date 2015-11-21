program Project6;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils;

var
  row, col: integer;

  dInput: array [1 .. 3, 1 .. 4] of integer = //
    ( //
    (1, 0, 1, 0), //
    (1, 2, 0, 0), //
    (0, 1, 0, 1) //
    );
  dOutput: array [1 .. 3, 1 .. 4] of integer = //
    ( //
    (0, 1, 0, 1), //
    (0, 0, 1, 2), //
    (1, 0, 1, 0) //
    );
  m: array [1 .. 3] of integer;

Begin
  try
    Writeln('Входы: ');
    for row := Low(dInput) to High(dInput) do
    Begin
      for col := Low(dInput[row]) to High(dInput[row]) do
      begin
        Write(dInput[row, col], ' ');
      end;
      Writeln;
    End;
    Writeln('Выходы: ');
    for row := Low(dOutput) to High(dOutput) do
    Begin
      for col := Low(dOutput[row]) to High(dOutput[row]) do
      begin
        Write(dOutput[row, col], ' ');
      end;
      Writeln;
    End;
    Writeln('Маркировка: ');
    for col := Low(m) to High(m) do
    begin
      Write('m[', col, '] = ');
      Readln(m[col]);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
