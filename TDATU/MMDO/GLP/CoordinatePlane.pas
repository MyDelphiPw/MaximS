unit CoordinatePlane;

interface

uses

  System.Diagnostics,
  msFPS,
  System.UITypes,
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  FMX.Graphics,
  FMX.Types,
  FMX.Controls;

Type {$M+}
  TmsShape = class(TObject)
  private
    fScale: Single;
    FCenterXY: TPointF;
    procedure DrawTo(aTarget: TStyledControl);
    procedure DrawShape(aTarget: TStyledControl); virtual; abstract;
    procedure SetupCorrectiv(aScale: Single; aCenterXY: TPointF);
    Function ToLocalPoint(aPoint: TPointF): TPointF;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Single); virtual; abstract;
  Public
    Constructor Create;
  published
  end;

  TmsLine = Class(TmsShape)
  private
    fA, fB: TPointF;
    FColor: TAlphaColor;
    procedure DrawShape(aTarget: TStyledControl); override;

  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(aA, aB: TPointF); overload;
    constructor Create(X1, Y1, X2, Y2: Single); overload;
  published
    property Color: TAlphaColor read FColor write FColor;
  End;

  TmsFunction = Class(TmsShape)
  private
    fA, fB: Single;
    FColor: TAlphaColor;
    procedure DrawShape(aTarget: TStyledControl); override;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(A, B: Single; IsMax: Boolean);
  published
    property Color: TAlphaColor read FColor write FColor;
  End;

  TInequality = (Better, Equally, Less);

  TmsLimitation = Class(TmsShape)
  public

  private
    fx: Single; { a1 }
    fy: Single; { a2 }
    fB: Single;
    FColor: TAlphaColor;
    FInequality: TInequality;
    procedure DrawShape(aTarget: TStyledControl); override;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(aX, aY: Single; aInequality: TInequality; aB: Single);
    Function PointA: TPointF;
    Function PointB: TPointF;
  published
    property Color: TAlphaColor read FColor write FColor;
    property X: Single read fx write fx;
    property Y: Single read fy write fy;
    property B: Single read fB write fB;
    property Inequality: TInequality read FInequality write FInequality;
  End;

  TcpCells = Class(TmsShape)
  private
    FColor: TAlphaColor;
    FLength: Single;
    procedure DrawShape(aTarget: TStyledControl); override;
    procedure SetColor(const Value: TAlphaColor);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(Const aLength: Single);
  published
    property Color: TAlphaColor read FColor write SetColor;
    property Length: Single read FLength write FLength;
  End;

  TCoordinatePlane = Class(TStyledControl)
  private
    fMousePosition: TPointF;
    fScale: Single;
    FCenterXY: TPointF;
    FShapeList: TObjectList<TmsShape>;
    FCells: TcpCells;
    fFPS: TmsFPS;
    procedure OnFPS(Sender: TObject; Value: Single);
    procedure SetScale(const Value: Single);
    function GetShapeList: TObjectList<TmsShape>;
    procedure SetCenterXY(const Value: TPointF);
    procedure ListChanged(Sender: TObject; const Item: TmsShape; Action: TCollectionNotification);
  protected
    // override block \\
    procedure Paint; override;
    Procedure PaintAllShapes;
    procedure PaintDebugInfo;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    // my draw method \\

    Procedure PaintXY;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Cells: TcpCells read FCells write FCells;
    property Scale: Single read fScale write SetScale;
    property ShapeList: TObjectList<TmsShape> read GetShapeList;
    property CenterXY: TPointF read FCenterXY write SetCenterXY;
  End;

implementation

type
  TCanvasHelper = class helper for TCanvas
  public
    procedure TextOut(X, Y: Single; Text: String);
  end;
  { TCoordinatePlane }

constructor TCoordinatePlane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShapeList := TObjectList<TmsShape>.Create;
  FShapeList.OnNotify := ListChanged;
  Scale := 10;
  FCenterXY := GetBoundsRect.CenterPoint;
  Cells := TcpCells.Create(Scale);
  ClipChildren := True;
  fFPS := TmsFPS.Create(nil);
  fFPS.Control := Self;
  fFPS.OnFPS := OnFPS;
end;

destructor TCoordinatePlane.Destroy;
begin
  FShapeList.Free;
  FCells.Free;
  fFPS.Free;
  inherited Destroy;
end;

function TCoordinatePlane.GetShapeList: TObjectList<TmsShape>;
begin
  Result := FShapeList;
end;

procedure TCoordinatePlane.ListChanged(Sender: TObject; const Item: TmsShape;
  Action: TCollectionNotification);
begin
  Item.SetupCorrectiv(Scale, CenterXY);
end;

procedure TCoordinatePlane.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;

end;

procedure TCoordinatePlane.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  fMousePosition := TPointF.Create(X, Y) - CenterXY;
end;

procedure TCoordinatePlane.MouseMove(Shift: TShiftState; X, Y: Single);
var
  I: Integer;
begin
  inherited;
  if (ssMiddle in Shift) or (ssRight in Shift) then
  Begin
    CenterXY := TPointF.Create(X, Y) - fMousePosition;
  End;
  for I := 0 to ShapeList.Count - 1 do
    ShapeList[I].MouseMove(Shift, X, X);
end;

procedure TCoordinatePlane.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;

end;

procedure TCoordinatePlane.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
var
  Koef: Single;
begin

  inherited MouseWheel(Shift, WheelDelta, Handled);
  Koef := 10;
  if ssShift in Shift then
    Koef := 50;
  if ssCtrl in Shift then
    Koef := 5;
  Scale := Scale + Koef / WheelDelta;
  Handled := True;
end;

procedure TCoordinatePlane.OnFPS(Sender: TObject; Value: Single);
begin
  Canvas.TextOut(0, 20, Value.ToString);
end;

procedure TCoordinatePlane.Paint;

begin

  inherited Paint;
  FCells.DrawShape(Self);
  PaintXY;
  PaintAllShapes;
  PaintDebugInfo;
end;

procedure TCoordinatePlane.PaintAllShapes;
var
  LShape: TmsShape;
begin
  Canvas.BeginScene;
  try
    for LShape in FShapeList do
      LShape.DrawTo(Self);
  finally
    Canvas.EndScene;
  end; // try..finally
end;

procedure TCoordinatePlane.PaintDebugInfo;
var
  debug: String;
begin
  case Canvas.Quality of
    TCanvasQuality.SystemDefault:
      debug := 'SystemDefault';
    TCanvasQuality.HighPerformance:
      debug := 'HighPerformance';
    TCanvasQuality.HighQuality:
      debug := 'HighQuality';
  end;
  debug := Canvas.ClassName + ' ' + debug;
  Canvas.TextOut(0, 0, debug);
  Canvas.TextOut(0, 20, 'FPS: ' + fFPS.Value.ToString);
end;

procedure TCoordinatePlane.PaintXY;
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
  // Canvas.Stroke.Color := ColorDSC;
  Canvas.Fill.Color := TAlphaColorRec.Red;
  { Линия абсцис }
  Canvas.DrawLine(TPointF.Create(0, FCenterXY.Y), TPointF.Create(Width, FCenterXY.Y), 1);
  // стрелка
  Canvas.DrawLine(TPointF.Create(Width, CenterXY.Y), TPointF.Create(Width - 7, CenterXY.Y + 3), 1);
  Canvas.DrawLine(TPointF.Create(Width, CenterXY.Y), TPointF.Create(Width - 7, CenterXY.Y - 3), 1);
  { Линия Ординат }
  Canvas.DrawLine(TPointF.Create(CenterXY.X, 0), TPointF.Create(CenterXY.X, Height), 1);
  // стрелка
  Canvas.DrawLine(TPointF.Create(CenterXY.X, 0), TPointF.Create(CenterXY.X - 3, 7), 1);
  Canvas.DrawLine(TPointF.Create(CenterXY.X, 0), TPointF.Create(CenterXY.X + 3, 7), 1);

  I := FCenterXY.X;
  j := 0;

  while I + 2 * Scale < Self.Width do
  Begin
    // Черточки вправо
    I := I + Scale;
    j := j + 1;

    if NeedPrintText then
    Begin
      Canvas.DrawLine(TPointF.Create(I, FCenterXY.Y - 5), TPointF.Create(I, FCenterXY.Y + 5), 1);
      Canvas.TextOut(I - 3, CenterXY.Y + 6, j.ToString);
    End
    else
      Canvas.DrawLine(TPointF.Create(I, FCenterXY.Y - 3), TPointF.Create(I, FCenterXY.Y + 3), 1);
  End;
  I := FCenterXY.X;
  j := 0;
  while I > Scale do
  Begin
    // Черточки влево
    I := I - Scale;

    if NeedPrintText then
    Begin
      Canvas.DrawLine(TPointF.Create(I, FCenterXY.Y - 5), TPointF.Create(I, FCenterXY.Y + 6), 1);
      Canvas.TextOut(I - 3, CenterXY.Y + 5, (-j).ToString);
    End
    else
      Canvas.DrawLine(TPointF.Create(I, FCenterXY.Y - 3), TPointF.Create(I, FCenterXY.Y + 3), 1);
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
      Canvas.DrawLine(TPointF.Create(FCenterXY.X - 5, I), TPointF.Create(FCenterXY.X + 5, I), 1);
      Canvas.TextOut(CenterXY.X - 25, I - 5, (-j).ToString);
    End
    else
      Canvas.DrawLine(TPointF.Create(FCenterXY.X - 3, I), TPointF.Create(FCenterXY.X + 3, I), 1);
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
      Canvas.DrawLine(TPointF.Create(FCenterXY.X - 5, I), TPointF.Create(FCenterXY.X + 5, I), 1);
      Canvas.TextOut(CenterXY.X - 25, I - 5, (j).ToString);
    End
    else
      Canvas.DrawLine(TPointF.Create(FCenterXY.X - 3, I), TPointF.Create(FCenterXY.X + 3, I), 1);
  End;
end;

procedure TCoordinatePlane.SetCenterXY(const Value: TPointF);
var
  I: Integer;
begin
  FCenterXY := Value;
  for I := 0 to FShapeList.Count - 1 do
    FShapeList[I].SetupCorrectiv(Scale, CenterXY);
  Repaint;
end;

procedure TCoordinatePlane.SetScale(const Value: Single);
var
  I: Integer;
begin
  fScale := Value;
  for I := 0 to FShapeList.Count - 1 do
    FShapeList[I].SetupCorrectiv(Scale, CenterXY);
  Repaint;
end;

{ TMyShape }

function TmsShape.ToLocalPoint(aPoint: TPointF): TPointF;
begin
  Result := aPoint * fScale + FCenterXY;

end;

constructor TmsShape.Create;
begin

end;

procedure TmsShape.DrawTo(aTarget: TStyledControl);
begin
  aTarget.Canvas.BeginScene();
  DrawShape(aTarget);
  aTarget.Canvas.EndScene;
end;

{ TLine }

constructor TmsLine.Create(aA, aB: TPointF);
begin
  fA := aA;
  fB := aB;
  fA.Y := -fA.Y;
  fB.Y := -fB.Y;
  Color := TAlphaColorRec.Brown;
end;

constructor TmsLine.Create(X1, Y1, X2, Y2: Single);
begin
  Self.Create(TPointF.Create(X1, Y1), TPointF.Create(X2, Y2));
end;

procedure TmsLine.DrawShape(aTarget: TStyledControl);
begin
  aTarget.Canvas.Stroke.Color := Color;
  aTarget.Canvas.DrawLine(ToLocalPoint(fA), ToLocalPoint(fB), 1);
end;

procedure TmsLine.MouseMove(Shift: TShiftState; X, Y: Single);
begin

end;

procedure TmsShape.SetupCorrectiv(aScale: Single; aCenterXY: TPointF);
begin

  fScale := aScale;
  FCenterXY := aCenterXY;
end;

{ TCanvasHelper }

procedure TCanvasHelper.TextOut(X, Y: Single; Text: String);
begin
  FillText(TRectF.Create(X, Y, X + TextWidth(Text), Y + TextHeight(Text)), Text, False, 100, [],
    TTextAlign.Leading, TTextAlign.Center);
end;

{ TcpCells }

constructor TcpCells.Create(const aLength: Single);
begin
  FLength := aLength;
end;

procedure TcpCells.DrawShape(aTarget: TStyledControl);
var
  Step: Single;
begin

  Step := 0;
  while Step < aTarget.Width do
  Begin
    Step := Step + Length;
    aTarget.Canvas.DrawLine(TPointF.Create(Step, 0), TPointF.Create(Step, aTarget.Height), 0.5);
  End;
  Step := 0;
  while Step < aTarget.Height do
  Begin
    Step := Step + Length;
    aTarget.Canvas.DrawLine(TPointF.Create(0, Step), TPointF.Create(aTarget.Width, Step), 0.5);
  End;

end;

procedure TcpCells.MouseMove(Shift: TShiftState; X, Y: Single);
begin

end;

procedure TcpCells.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
end;

{ TmsFunction }

constructor TmsFunction.Create(A, B: Single; IsMax: Boolean);
begin
  fA := A;
  fB := B;
end;

procedure TmsFunction.DrawShape(aTarget: TStyledControl);
begin
  With aTarget.Canvas do
  Begin
    Stroke.Color := TAlphaColorRec.Chocolate;
    Stroke.Dash := TStrokeDash.Dash;
    DrawLine(ToLocalPoint(TPointF.Create(0, -Self.fB)),
      ToLocalPoint(TPointF.Create(Self.fA, -Self.fB)), 1);
    DrawLine(ToLocalPoint(TPointF.Create(Self.fA, -Self.fB)),
      ToLocalPoint(TPointF.Create(Self.fA, 0)), 1);
    Stroke.Dash := TStrokeDash.Solid;
    DrawLine(ToLocalPoint(TPointF.Zero), ToLocalPoint(TPointF.Create(Self.fA, -Self.fB)), 1);
    Stroke.Color := TAlphaColorRec.Black;
    DrawLine(ToLocalPoint(TPointF.Create(0, -Self.fA)),
      ToLocalPoint(TPointF.Create(Self.fB, 0)), 1);
  end;
end;

procedure TmsFunction.MouseMove(Shift: TShiftState; X, Y: Single);
begin

end;

constructor TmsLimitation.Create(aX, aY: Single; aInequality: TInequality; aB: Single);
begin
  X := aX;
  Y := aY;
  Inequality := aInequality;
  B := aB;
  Color := TAlphaColorRec.Blue;
end;

procedure TmsLimitation.DrawShape(aTarget: TStyledControl);
var
  X, Y: TPointF;

begin
  aTarget.Canvas.BeginScene;
  try
    aTarget.Canvas.Stroke.Color := Color;
    X := PointA;
    Y := PointB;
    X.Y := -X.Y;
    X := ToLocalPoint(X);
    Y := ToLocalPoint(Y);
    aTarget.Canvas.DrawLine((X), (Y), 1);
    aTarget.Canvas.Fill.Kind := TBrushKind.Solid;
    aTarget.Canvas.DrawPolygon([Point(30, 30), Point(100, 30), Point(200, 250), Point(30, 250)], 1);
  finally
    aTarget.Canvas.EndScene;
  end;
end;

procedure TmsLimitation.MouseMove(Shift: TShiftState; X, Y: Single);
begin

end;

function TmsLimitation.PointA: TPointF;
begin
  Result := TPointF.Create(0, B / Y);
end;

function TmsLimitation.PointB: TPointF;
begin
  Result := TPointF.Create(B / X, 0);
end;

end.
