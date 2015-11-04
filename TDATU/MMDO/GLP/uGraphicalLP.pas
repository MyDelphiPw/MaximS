unit uGraphicalLP;

interface

{$DEFINE UsingFMX}

// http://www.cyberforum.ru/delphi-beginners/thread405643.html
uses
  System.Generics.Collections,
  System.UITypes,
  System.Types,
  System.Classes,
  System.SysUtils,

  FMX.Graphics,
  FMX.Types,
  FMX.Controls;

type
  TCanvasHelper = class helper for TCanvas
  public
    procedure TextOut(X, Y: Single; Text: String);
  end;

  TInequality = Record { коэффициенты неравенств а1х+а2у<=b }
  private
    fx: Single; { a1 }
    fy: Single; { a2 }
    fb: Single; { b }
    procedure Minimization;
  public
    constructor Create(aX, aY, aB: Single);
    Function PointA: TPointF;
    Function PointB: TPointF;
    property X: Single read fx;
    property Y: Single read fy;
    property B: Single read fb;

  end;

  TFunctionLP = Record
    X, Y: Single;
    IsMaximal: Boolean;
    constructor Create(aX, aY: Single; IsMax: Boolean);
  End;

  TGraphicLP = Class(TStyledControl)
  private
    FTargerFunction: TFunctionLP;
    FInequality: TList<TInequality>;
    FCenterXY: TPointF;
    FScale: Single;
    FColorDSC: TAlphaColor;
    FColorVectors: TAlphaColor;
    procedure SetCenterXY(const Value: TPointF);
    procedure SetScale(const Value: Single);
  protected
    procedure Paint; override;
    Procedure PaintXY;
    Procedure PaintTInequality;
    Procedure PaintVectorC;
    Procedure PaintFunction;
    Procedure PerpendikularPaint(P1, P2: TPointF; h: Single);
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); override;
  public
    procedure Add(Inequality: TInequality);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TargerFunction: TFunctionLP read FTargerFunction
      write FTargerFunction;
    property CenterXY: TPointF read FCenterXY write SetCenterXY;
    property Scale: Single read FScale write SetScale;
    property ColorDSC: TAlphaColor read FColorDSC write FColorDSC;
    property ColorVectors: TAlphaColor read FColorVectors write FColorVectors;
  End;

implementation

{ TInequality }

constructor TInequality.Create(aX, aY, aB: Single);
begin
  Self.fx := aX;
  Self.fy := aY;
  Self.fb := aB;
  Minimization;
end;

procedure TInequality.Minimization;
var
  j, k: Single;
begin
  j := fx;
  k := fy;
  { далее идет сокращение коэф-тов если это возможно }
  repeat
    if (Frac(fb / j) = 0) then
      if (Frac(fx / j) = 0) then
        Break;
    j := j - 1;
  until (j <= 0);
  if j >= 0 then
    repeat
      if (Frac(fb / k) = 0) then
      begin
        if (Frac(fy / k) = 0) then
          if (j = k) then
          begin
            fb := fb / k;
            fx := fx / k;
            fy := fy / k;
            Break;
          end
      end;
      k := k - 1;
    until (k <= 0);

end;

function TInequality.PointA: TPointF;
begin
  Result := TPointF.Create(0, fb / fy);

end;

function TInequality.PointB: TPointF;
begin
  Result := TPointF.Create(fb / fx, 0);
end;

{ TCanvasHelper }

procedure TCanvasHelper.TextOut(X, Y: Single; Text: String);
begin
  FillText(TRectF.Create(X, Y, X + TextWidth(Text), Y + TextHeight(Text)), Text,
    False, 100, [], TTextAlign.Leading, TTextAlign.Center);
end;

{ TFunction }

constructor TFunctionLP.Create(aX, aY: Single; IsMax: Boolean);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.IsMaximal := IsMax;

end;

{ TGraphicLP }

procedure TGraphicLP.Add(Inequality: TInequality);
begin
  FInequality.Add(Inequality)
end;

constructor TGraphicLP.Create(AOwner: TComponent);
begin
  inherited;
  Width := 200;
  Height := 200;
  Scale := 10;
  FCenterXY := GetBoundsRect.CenterPoint;
  FInequality := TList<TInequality>.Create;
  ColorDSC := TAlphaColorRec.Blue;

  ColorVectors := (TAlphaColorRec.Green);
end;

destructor TGraphicLP.Destroy;
begin
  FInequality.Free;
  inherited;
end;

procedure TGraphicLP.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  if ssMiddle in Shift then
    CenterXY := TPointF.Create(X, Y)
end;

procedure TGraphicLP.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
var
  Koef: Single;
begin
  inherited;
  Koef := 10;
  if ssShift in Shift then
    Koef := 50;
  if ssCtrl in Shift then
    Koef := 5;
  Scale := Scale + Koef / WheelDelta;
  Handled := True;
end;

procedure TGraphicLP.Paint;
begin
  inherited;
  PaintXY;
  PaintTInequality;
  PaintVectorC;
  PaintFunction;
end;

procedure TGraphicLP.PaintFunction;
begin

end;

procedure TGraphicLP.PaintTInequality;
var
  I: Integer;
  X, Y: TPointF;
begin
  Canvas.Stroke.Color := ColorVectors;
  for I := 0 to FInequality.Count - 1 do
  Begin
    X := FInequality[I].PointA;
    Y := FInequality[I].PointB;
    X.Y := -X.Y;
    Canvas.DrawLine(X * Scale + CenterXY, Y * Scale + CenterXY, 1);
  End;
end;

procedure TGraphicLP.PaintVectorC;
begin
  Canvas.Stroke.Color := TAlphaColorRec.Chocolate;
  Canvas.Stroke.Dash := TStrokeDash.Dash;
  Canvas.DrawLine(TPointF.Create(0, -FTargerFunction.Y) * Scale + CenterXY,
    TPointF.Create(FTargerFunction.X, -FTargerFunction.Y) * Scale +
    CenterXY, 1);
  Canvas.DrawLine(TPointF.Create(FTargerFunction.X, -FTargerFunction.Y) * Scale
    + CenterXY, TPointF.Create(FTargerFunction.X, 0) * Scale + CenterXY, 1);
  Canvas.Stroke.Dash := TStrokeDash.Solid;
  Canvas.DrawLine(TPointF.Zero + CenterXY, TPointF.Create(FTargerFunction.X,
    -FTargerFunction.Y) * Scale + CenterXY, 1);
  Canvas.Stroke.Color := TAlphaColorRec.Black;
  Canvas.DrawLine(TPointF.Create(0, -FTargerFunction.Y) * Scale + CenterXY,
    TPointF.Create(FTargerFunction.X, 0) * Scale + CenterXY, 1);

  // PerpendikularPaint(TPointF.Zero, TPointF.Create(FTargerFunction.X,    -FTargerFunction.Y), 1);
end;

procedure TGraphicLP.PaintXY;
var
  I: Single;
  j: Integer;
  Function NeedPrintText: Boolean;
  Begin
    Result := False;
    if Scale >= 0 then
      Result := j mod 20 = 0;
    if Scale >= 5 then
      Result := (j mod 5 = 0)
    else if Scale >= 20 then
      Result := True;
  End;

begin
  Canvas.Stroke.Color := ColorDSC;
  Canvas.Fill.Color := TAlphaColorRec.Red;
  { Линия абсцис }
  Canvas.DrawLine(TPointF.Create(0, FCenterXY.Y),
    TPointF.Create(Width, FCenterXY.Y), 1);
  // стрелка
  Canvas.DrawLine(TPointF.Create(Width, CenterXY.Y),
    TPointF.Create(Width - 7, CenterXY.Y + 3), 1);
  Canvas.DrawLine(TPointF.Create(Width, CenterXY.Y),
    TPointF.Create(Width - 7, CenterXY.Y - 3), 1);
  { Линия Ординат }
  Canvas.DrawLine(TPointF.Create(CenterXY.X, 0), TPointF.Create(CenterXY.X,
    Height), 1);
  // стрелка
  Canvas.DrawLine(TPointF.Create(CenterXY.X, 0),
    TPointF.Create(CenterXY.X - 3, 7), 1);
  Canvas.DrawLine(TPointF.Create(CenterXY.X, 0),
    TPointF.Create(CenterXY.X + 3, 7), 1);

  I := FCenterXY.X;
  j := 0;

  while I + 2 * Scale < Self.Width do
  Begin
    // Черточки вправо
    I := I + Scale;
    j := j + 1;

    if NeedPrintText then
    Begin
      Canvas.DrawLine(TPointF.Create(I, FCenterXY.Y - 5),
        TPointF.Create(I, FCenterXY.Y + 5), 1);
      Canvas.TextOut(I - 3, CenterXY.Y + 6, j.ToString);
    End
    else
      Canvas.DrawLine(TPointF.Create(I, FCenterXY.Y - 3),
        TPointF.Create(I, FCenterXY.Y + 3), 1);
  End;
  I := FCenterXY.X;
  j := 0;
  while I > Scale do
  Begin
    // Черточки влево
    I := I - Scale;

    if NeedPrintText then
    Begin
      Canvas.DrawLine(TPointF.Create(I, FCenterXY.Y - 5),
        TPointF.Create(I, FCenterXY.Y + 6), 1);
      Canvas.TextOut(I - 3, CenterXY.Y + 5, (-j).ToString);
    End
    else
      Canvas.DrawLine(TPointF.Create(I, FCenterXY.Y - 3),
        TPointF.Create(I, FCenterXY.Y + 3), 1);
    j := j + 1;
  End;
  //
  I := FCenterXY.Y;
  j := 0;
  while I < Self.Height do
  Begin
    // Черточки Вниз
    I := I + Scale;
    j := j + 1;

    if NeedPrintText then
    Begin
      Canvas.DrawLine(TPointF.Create(FCenterXY.X - 5, I),
        TPointF.Create(FCenterXY.X + 5, I), 1);
      Canvas.TextOut(CenterXY.X - 25, I - 5, (-j).ToString);
    End
    else
      Canvas.DrawLine(TPointF.Create(FCenterXY.X - 3, I),
        TPointF.Create(FCenterXY.X + 3, I), 1);
  End;
  I := FCenterXY.Y;
  j := 0;
  while I > Scale do
  Begin
    // Черточки Вверх
    I := I - Scale;
    j := j + 1;

    if NeedPrintText then
    Begin
      Canvas.DrawLine(TPointF.Create(FCenterXY.X - 5, I),
        TPointF.Create(FCenterXY.X + 5, I), 1);
      Canvas.TextOut(CenterXY.X - 25, I - 5, (j).ToString);
    End
    else
      Canvas.DrawLine(TPointF.Create(FCenterXY.X - 3, I),
        TPointF.Create(FCenterXY.X + 3, I), 1);
  End;
end;

procedure TGraphicLP.PerpendikularPaint(P1, P2: TPointF; h: Single);
var
  tmpPoint: TPointF;
  CoefK, CoefB: Single;
begin
  // Getting middle point
  tmpPoint := TPointF.Create((P1.X + P2.X) / 2, (P1.Y + P2.Y) / 2);
  // Calculating coefficients for line equation (y=kx+b)
  try
    CoefK := (P2.Y - P1.Y) / (P1.X - P1.X);
    CoefK := -1 / CoefK;
  except

    CoefK := 0;
  end;
  CoefB := tmpPoint.Y - CoefK * tmpPoint.X;
  // Drawing perpendicular line

  Canvas.DrawLine(TPointF.Create(0, CoefK * 0 + CoefB) * Scale + CenterXY,
    TPointF.Create(Width, CoefK * Width + CoefB) * Scale + CenterXY, 1);
end;

procedure TGraphicLP.SetCenterXY(const Value: TPointF);
begin
  FCenterXY := Value;
  Repaint;
end;

procedure TGraphicLP.SetScale(const Value: Single);
begin
  if Value < 0.5 then
    exit;

  FScale := Value;
  Repaint;
end;

end.
