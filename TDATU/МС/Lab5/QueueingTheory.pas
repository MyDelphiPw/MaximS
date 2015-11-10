unit QueueingTheory;

interface

uses System.Math;

Type
{$M+}
  TQueueingTheory = Class(TObject)
  private
    Fλ: Single;
    Fμ: Single;
  protected
    /// <summary>Факториал числа</summary>
    Class Function Factorial(Const Value: Integer): Integer;
  public
    /// <summary>Интенсивность нагрузки</summary>
    function ρ: Single;
  published
    /// <summary>Интенсивность потокоа заявок за единицу времени</summary>
    property λ: Single read Fλ write Fλ;
    /// <summary>Интенсивность обслуживания заявок за единицу времени</summary>
    property μ: Single read Fμ write Fμ;
  End;

  /// <summary>Многоканаьльная СМО с отказами</summary>
  TQueueingTheoryMultiFailure = Class(TQueueingTheory) // главній
  private
    Fn: Integer;
  public
    /// <summary>Вероятность занятости канала
    /// <param name="0">Вероятность, что канал свободен (доля времени простоя каналов). </param>
    /// <param name="1..N">Вероятность того, что обслуживанием занят K канал(ов)</param>
    /// </summary>
    Function P(Const I: Integer): Single; virtual;
    /// <summary> Доля заявок, получивших отказ </summary>
    Function Pотказа: Single; virtual;
    /// <summary>Относительная пропускная способность</summary>
    Function Q: Single; virtual;
    /// <summary>Абсолютная пропускная способность.</summary>
    Function A: Single;
    constructor Create;
  published
    /// <summary>Количество каналов</summary>
    property N: Integer read Fn write Fn default 1;

  End;

  TQueueingTheoryMultiWait = Class(TQueueingTheoryMultiFailure) // наследник
  private
    Fm: Integer;
  public
    /// <summary> Вероятность, что канал свободен</summary>
    Function P(Const I: Integer): Single; reintroduce;
    /// <summary> Вероятность отказа (Доля заявок, получивших отказ).</summary>
    function Pотказа: Single; override;
    /// <summary> Вероятность обслуживания поступающих заявок (вероятность того, что клиент будет обслужен).</summary>
    function Pобс: Single;
    /// <summary> Среднее число каналов, занятых обслуживанием (Среднее число занятых каналов).</summary>
    function Nз: Single;

    /// <summary> Среднее число простаивающих каналов.</summary>
    function Nпр: Single;
    /// <summary> Коэффициент занятости каналов обслуживанием.</summary>
    function Кз: Single;
    /// <summary> Абсолютная пропускная способность (Интенсивность выходящего потока обслуженных заявок).</summary>
    function A: Single;
    /// <summary> Время обслуживания.</summary>
    function Tобс: Single;
    /// <summary> Среднее время простоя СМО.</summary>
    function Tпр: Single;
    /// <summary> Вероятность образования очереди.</summary>
    function Poh: Single;
    /// <summary> Вероятность отсутствия очереди.</summary>
    function Pот: Single;

    /// <summary> Среднее число заявок, находящихся в очереди.</summary>
    function Loh: Single;

    /// <summary> Среднее время простоя СМО (среднее время ожидания обслуживания заявки в очереди).</summary>
    Function Toh: Single;
    /// <summary> Среднее число обслуживаемых заявок.</summary>
    Function Lобс: Single;
    /// <summary> Среднее число заявок в системе.</summary>
    Function Lсмо: Single;
    /// <summary> Среднее время пребывания заявки в СМО.</summary>
    Function Tсмо: Single;
  published
    /// <summary>Число мест в очереди</summary>
    property m: Integer read Fm write Fm;
  End;

implementation

{ TQueueingTheoryMultiFailure }

function TQueueingTheoryMultiFailure.A: Single;
begin
  Result := λ * Q;
end;

constructor TQueueingTheoryMultiFailure.Create;
begin
  N := 1;
  λ := 16;
  μ := 12;
end;

function TQueueingTheoryMultiFailure.P(const I: Integer): Single;
var
  j: Integer;
Begin
  Result := 1;
  if I = 0 then
  Begin
    for j := 1 to N do
      Result := Result + Power(ρ, j) / Factorial(j);
    Result := 1 / Result;
  End
  else if I >= Self.N then
  Begin

  End
  Else
    Result := Power(ρ, I) / Factorial(I) * P(0);
end;

function TQueueingTheoryMultiFailure.Pотказа: Single;
begin
  Result := Power(ρ, N) / Factorial(N) * P(0);
end;

function TQueueingTheoryMultiFailure.Q: Single; // класс наследник
begin
  Result := 1 - Pотказа;
end;

{ TQueueingTheoryMultiWait }

function TQueueingTheoryMultiWait.A: Single;
begin
  Result := Pобс * λ;
end;

function TQueueingTheoryMultiWait.Loh: Single;
begin

  Result := (Power(ρ, (N + 1)) / (Factorial(N) * N)) *
    ((1 - Power((ρ / N), m) * (m + 1 - m * ρ / N)) / Power(1 - (ρ / N), 2)) * P(0);
end;

function TQueueingTheoryMultiWait.Lобс: Single;
begin
  Result := Q * ρ;
end;

function TQueueingTheoryMultiWait.Lсмо: Single;
begin
  Result := Loh + Lобс;
end;

function TQueueingTheoryMultiWait.Nз: Single;
begin
  Result := ρ * Pобс;
end;

function TQueueingTheoryMultiWait.Nпр: Single;
begin
  Result := N - Nз;
end;

function TQueueingTheoryMultiWait.P(const I: Integer): Single;
var
  l: Integer;
Begin
  if I = 0 then

  begin
    Result := 1;
    for l := 1 to N do
      Result := Result + (Power(ρ, l) / Factorial(l));
    Result := Result + (Power(ρ, N) / Factorial(N)) * ((ρ / N - Power(ρ / N, m + 1)) / (1 - ρ / N));
    Result := Power(Result, -1);
  end
  else if (I > 0) and (I <= N) then
  Begin
    Result := (Power(ρ, I) / Factorial(I)) * P(0);
  End
  else
  begin
    Result := P(0) * (Power(ρ, I)) / (Factorial(N) * Power(N, I - N));
  end;
end;

function TQueueingTheoryMultiWait.Poh: Single;
begin
  Result := Power(ρ, N) / Factorial(N);
  Result := Result * ((1 - Power(ρ / N, m)) / (1 - ρ / N));
  Result := Result * P(0);
end;

function TQueueingTheoryMultiWait.Pобс: Single;
begin
  Result := 1 - Pотказа;
end;

function TQueueingTheoryMultiWait.Pот: Single;
begin
  Result := 1 - Poh;
end;

function TQueueingTheoryMultiWait.Pотказа: Single;
begin
  Result := Power(ρ, N + m) / (Power(N, m) * Factorial(N)) * P(0);
end;

function TQueueingTheoryMultiWait.Toh: Single;
begin
  Result := Loh / (A);
end;

function TQueueingTheoryMultiWait.Tобс: Single;
begin
  Result := 1 / μ;
end;

function TQueueingTheoryMultiWait.Tпр: Single;
begin
  Result := Pотказа * Tобс
end;

function TQueueingTheoryMultiWait.Tсмо: Single;
begin
  Result := Lсмо / A;
end;

function TQueueingTheoryMultiWait.Кз: Single;
begin
  Result := Nз / N;
end;

{ TQueueingTheory }

class function TQueueingTheory.Factorial(const Value: Integer): Integer;
begin
  Result := 1;
  if Value > 1 then
    Result := Value * Factorial(Value - 1);
end;

function TQueueingTheory.ρ: Single;
begin
  Result := λ / μ;
end;

end.
