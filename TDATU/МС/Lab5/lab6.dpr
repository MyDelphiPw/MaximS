program lab6;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  QueueingTheory in 'QueueingTheory.pas';

var
  Core: TQueueingTheoryMultiWait;
  Buffer: Single;
  i: Integer;

begin
  Core := TQueueingTheoryMultiWait.Create;
  try
    { TODO -oUser -cConsole Main : Insert code here }
    {
      Writeln('-----Ввод информации----');
      Write('Число мест в очереди: ');
      Readln(Buffer);
      Core.m := Buffer;
      Write('Интенсивность потокоа заявок за единицу времени: ');
      Readln(Buffer);
      Core.λ := Buffer;
      Writeln('Интенсивность обслуживания заявок за единицу времени: ');
      Readln(Buffer);
      Core.μ := Buffer;
      Writeln('Количество каналов: ');
      Readln(Buffer);
      Core.N := Buffer.ToString.ToInteger; }
    Core.N := 2;
    Core.m := 3;
    Core.λ := 2;
    Core.μ := 0.5;

    Writeln('-----Параметры СМО----');
    Writeln('Интенсивность нагрузки = ', Core.ρ:3:5);
    // Writeln('Время обслуживания = ', Core.ρ:3:5);
    for i := 0 to Core.N do
      Writeln('p[', i, '] = ', Core.P(i):3:5);
    Writeln('Вероятность отказа = ', Core.Pотказа:3:5);
    Writeln('Относительная пропускная способность = ', Core.Q:3:5);
    Writeln('Абсолютная пропускная способность = ', Core.A:3:5);
    Writeln('Коэффициент занятости каналов обслуживанием != ',
      Core.Kзанятости:3:5);
    Writeln('Среднее число заявок, находящихся в очереди = ',
      Core.Lочереди:3:5);
    Writeln('Среднее время простоя СМО = ', Core.Tочереди:3:5);
    Writeln('Среднее число заявок в системе = ', Core.Lsys:3:5);
    Writeln('Среднее время пребывания заявки в СМО = ', Core.Tsmo:3:5);
    Writeln('Вероятность образования очереди = ', Core.Pоч:3:5);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Core.free;
  Readln;

end.
