unit Philosopher;

interface

uses
  Classes,

  FMX.Objects,
  System.SyncObjs;

type
  Tmode = (phThinking, phHungry, phEating); // Состояния философа

  TPhilosophers = class(TThread) // Объект философа, наследник от "потока"
  private
    procedure goThink; // Метод "думать"
    procedure goEat; // Метод "кушать"
    procedure takeStick; // Взять вилку
    procedure putStick; // Положить вилку
    procedure testOponents(i: integer); // Проверить, можно ли взять вилку

    function FindNumber: integer; // по ид потока определям место на столе
  protected
    procedure Execute; override; // рабочий метод
  end;

implementation

uses
  System.SysUtils,
  Unit3,
  System.UITypes;

var
  CriticalSection: TCriticalSection; // критическая секция

procedure TPhilosophers.Execute;
begin
  while (not Terminated) do
  begin
    takeStick;
    goEat;
    putStick;
    goThink;
  end;
end;

procedure TPhilosophers.goThink;
begin
  sleep(Random(10 * 1000));
end;

procedure TPhilosophers.takeStick;
var
  i: integer;
  Pie: TPie;
begin
  i := FindNumber;
  Pie := TPie(Form3.FindComponent('Pie' + i.toString));
  CriticalSection.Acquire;
  try
    state[i] := phHungry;
    Pie.Fill.Color := TAlphaColorRec.Blueviolet;
    testOponents(i);
  finally
    CriticalSection.Release;
  end;
end;

procedure TPhilosophers.goEat;
begin
  sleep(Random(10 * 1000));
end;

procedure TPhilosophers.putStick;
var
  i, left, right: integer;
  Pie: TPie;
begin
  i := FindNumber;
  Pie := TPie(Form3.FindComponent('Pie' + i.toString));
  CriticalSection.Acquire;
  try
    state[i] := phThinking;
    Pie.Fill.Color := TAlphaColorRec.Sienna;
    left := i + 1;
    if left = 6 then
      left := 1;
    right := i - 1;
    if right = 0 then
      right := 5;
    testOponents(left);
    testOponents(right);
  finally
    CriticalSection.Release;
  end;
end;

// процедура проверки доступности палочек для еды
procedure TPhilosophers.testOponents(i: integer);
var
  left, right: integer;
  Pie: TPie;
begin
  left := i + 1;
  if left = 6 then
    left := 1;
  right := i - 1;
  if right = 0 then
    right := 5;
  if (state[i] = phHungry) and (state[left] <> phEating) and
    (state[right] <> phEating) then
  begin
    state[i] := phEating;
    Pie := TPie(Form3.FindComponent('Pie' + i.toString));
    Pie.Fill.Color := TAlphaColorRec.Blue;
  end;
end;

// дополнительная функция, позволяющая определить номер философа по
// идентификатору потока.
function TPhilosophers.FindNumber: integer;
var
  i: integer;
begin
  for i := 1 to 5 do
    if PHID[i] = ThreadID then
    begin
      FindNumber := i;
      break;
    end;
end;

initialization

CriticalSection := TCriticalSection.Create;

finalization

CriticalSection.Free;

end.
