program lab5;

{$APPTYPE CONSOLE}
{$R *.res}
{ /$DEFINE AUTO }

uses
  System.SysUtils,
  QueueingTheory in 'QueueingTheory.pas';

var
  Core: TQueueingTheoryMultiWait;
{$IFNDEF AUTO} Buffer: Single; {$ENDIF}
  i: Integer;
  Suma: Single = 0;

begin
  Core := TQueueingTheoryMultiWait.Create;
  try
    { TODO -oUser -cConsole Main : Insert code here }
{$IFDEF AUTO}
    Core.N := 2;
    Core.m := 4;
    Core.λ := 2;
    Core.μ := 2 / 3;
{$ELSE} Writeln('-----Ввод информации----');
    Writeln('Количество каналов: ');
    Readln(Buffer);
    Core.N := Buffer.ToString.ToInteger;

    Write('Число мест в очереди: ');
    Readln(Buffer);
    Core.m := Buffer.ToString.ToInteger;
    Write('Интенсивность потокоа заявок за единицу времени: ');
    Readln(Buffer);
    Core.λ := Buffer;
    Writeln('Интенсивность обслуживания заявок за единицу времени: ');
    Readln(Buffer);
    Core.μ := Buffer;

{$ENDIF}
    {
      -----Ввод информации----
      Число мест в очереди: 2                                  m
      Интенсивность потокоа заявок за единицу времени: 1      λ
      Интенсивность обслуживания заявок за единицу времени:    μ
      0.2
      Количество каналов:     N
      3
    }
    Writeln('-----Параметры СМО----');
    Writeln('1. Интенсивность нагрузки = ', Core.ρ:3:5);
    Writeln('2. Время обслуживания = ', Core.Tобс:3:5);
    // Writeln('Время обслуживания = ', Core.ρ:3:5);
    for i := 0 to Core.N + Core.m do
    Begin
      Suma := Suma + Core.P(i);
      Writeln('p[', i, '] = ', Core.P(i):3:5);
    End;
    // Writeln(Suma:3:10);
    Writeln('4. Вероятность отказа = ', Core.Pотказа:3:5);
    Writeln('5. Вероятность обслуживания поступающих заявок = ', Core.Pобс:3:5);
    Writeln('6. Среднее число каналов, занятых обслуживанием = ', Core.Nз:3:5);
    Writeln('6. Среднее число простаивающих каналов = ', Core.Nпр:3:5);
    Writeln('7. Коэффициент занятости каналов обслуживанием = ', Core.Кз:3:5);
    Writeln('8. Абсолютная пропускная способность = ', Core.A:3:5);
    Writeln('9. Среднее время простоя СМО. = ', Core.Tпр:3:5);
    Writeln('Вероятность образования очереди. = ', Core.Poh:3:5);
    Writeln('Вероятность отсутствия очереди. = ', Core.Pот:3:5);
    Writeln('10. Среднее число заявок, находящихся в очереди = ', Core.Loh:3:5);
    Writeln('11. Среднее время простоя СМО = ', Core.Toh:3:5);
    Writeln('12. Среднее число обслуживаемых заявок. = ', Core.Lобс:3:5);
    Writeln('13. Среднее число заявок в системе. = ', Core.Lсмо:3:5);
    Writeln('14. Среднее время пребывания заявки в СМО. = ', Core.Tсмо:3:5);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Core.free;
  Readln;

end.
