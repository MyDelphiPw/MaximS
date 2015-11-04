unit QueueingTheory;

interface

uses
  System.Math;

Type
{$M+}
  TQueueingTheory = Class(TObject)
  private
    Fn: Integer;
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
    /// <summary> </summary>
    Function P(Const I: Integer): Single; reintroduce;
    /// <summary> Вероятность отказа</summary>
    Function Pотказа: Single;
    /// <summary>Относительная пропускная способность </summary>
    Function Q: Single;
    /// <summary> Абсолютная пропускная способность</summary>
    Function A: Single;
    /// <summary>Коэффициент занятости каналов обслуживанием. </summary>
    function Kзанятости: Single;
    /// <summary>  Среднее число заявок, находящихся в очереди</summary>
    Function Lочереди: Single;
    /// <summary> Среднее время простоя СМО (среднее время ожидания обслуживания заявки в очереди).</summary>
    Function Tочереди: Single;
    /// <summary> Среднее число заявок в системе.</summary>
    function Lsys: Single;
    /// <summary>Среднее время пребывания заявки в СМО. </summary>
    function Tsmo: Single;
    /// <summary>Вероятность образования очереди. </summary>
    function Pоч: Single;
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
  else
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
function TQueueingTheoryMultiWait.Pотказа: Single;
begin
  Result := (Power(ρ, N + m) / (Power(N, m) * Factorial(N))) * P(0);

end;

function TQueueingTheoryMultiWait.Pоч: Single;
begin
  Result := Power(ρ, N) / Factorial(N);
  Result := Result * ((1 - Power(ρ / N, m)) / (1 - ρ / N));
  Result := Result * P(0);
end;

function TQueueingTheoryMultiWait.Q: Single;
begin
  Result := 1 - Pотказа;
end;

function TQueueingTheoryMultiWait.Tsmo: Single;
begin
  Result := Lsys / A;
end;

function TQueueingTheoryMultiWait.Tочереди: Single;
begin
  Result := Lочереди / (A);
end;

function TQueueingTheoryMultiWait.A: Single;
begin
  Result := λ * Q;
end;

function TQueueingTheoryMultiWait.Kзанятости: Single;
begin
  Result := A / μ / N; // a/m/n
end;

function TQueueingTheoryMultiWait.Lsys: Single;
begin
  Result := Lочереди + A / μ;
end;

function TQueueingTheoryMultiWait.Lочереди: Single;
var
  mer: Single;
begin
  mer := 1 - (ρ / N);
  Result := (Power(ρ, (N + 1)) / (Factorial(N) * N)) *
    ((1 - Power((ρ / N), m) * (m + 1 - m * ρ / N)) / (mer * mer)) * P(0);
end;

function TQueueingTheoryMultiWait.P(const I: Integer): Single;
var
  k: Integer;
  tmpA, tmpB: Single;
Begin
  tmpA := 1;
  tmpB := 0;
  // P0 = 0.0158
  if I = 0 then
  Begin
    for k := 1 to N do
      tmpA := tmpA + (Power(ρ, k) / Factorial(k));
    for k := 1 to m do
      tmpB := tmpB + Power(ρ / N, k);
    Result := 1 / (tmpA + (Power(ρ, N) / Factorial(N)) * tmpB);
  End
  else
    Result := Power(ρ, I) / Factorial(I) * P(0);
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
