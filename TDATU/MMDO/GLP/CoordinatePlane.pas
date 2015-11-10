unit CoordinatePlane;

interface

uses

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
    procedure DrawTo(aCanvas: TCanvas);
    procedure DrawShape(aCanvas: TCanvas); virtual; abstract;
    procedure SetupCorrectiv(aScale: Single; aCenterXY: TPointF);
    Function CorrectToLocalPoint(aPoint: TPointF): TPointF;
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
    procedure DrawShape(aCanvas: TCanvas); override;
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
    procedure DrawShape(aCanvas: TCanvas); override;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(A, B: Single; IsMax: Boolean);
  published
    property Color: TAlphaColor read FColor write FColor;
  End;

  TmsInequality = Class(TmsShape)
  private
    fx: Single; { a1 }
    fy: Single; { a2 }
    fB: Single;
    FColor: TAlphaColor;
    procedure DrawShape(aCanvas: TCanvas); override;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(aX, aY, aB: Single);
    Function PointA: TPointF;
    Function PointB: TPointF;
  published
    property Color: TAlphaColor read FColor write FColor;
    property X: Single read fx write fx;
    property Y: Single read fy write fy;
    property B: Single read fB write fB;
  End;

  TcpCells = Class(TmsShape)
  private
    FColor: TAlphaColor;
    FLength: Single;
    procedure DrawShape(aCanvas: TCanvas); override;
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
    fCellLength: Single;
    fMousePosition: TPointF;
    fScale: Single;
    FCenterXY: TPointF;
    FShapeList: TObjectList<TmsShape>;
    FCells: TcpCells;
    procedure setCellLength(const Value: Single);
    procedure SetScale(const Value: Single);
    function GetShapeList: TObjectList<TmsShape>;
    procedure SetCenterXY(const Value: TPointF);
    procedure ListChanged(Sender: TObject; const Item: TmsShape; Action: TCollectionNotification);
  protected
    // override block \\
    procedure Paint; override;
    Procedure PaintAllShapes;
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
    property CellLength: Single read fCellLength write setCellLength;
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
  CellLength := 15;
  Scale := 10;
  FCenterXY := GetBoundsRect.CenterPoint;
  FCells := TcpCells.Create(5);

end;

destructor TCoordinatePlane.Destroy;
begin
  FShapeList.Free;
  FCells.Free;
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
  if ssMiddle in Shift then
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

procedure TCoordinatePlane.Paint;
begin
  inherited Paint;
  FCells.DrawShape(Canvas);
  PaintXY;
  PaintAllShapes;
end;

procedure TCoordinatePlane.PaintAllShapes;
var
  LShape: TmsShape;
begin
  Canvas.BeginScene;
  try
    for LShape in FShapeList do
      LShape.DrawTo(Canvas);
  finally
    Canvas.EndScene;
  end; // try..finally
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

procedure TCoordinatePlane.setCellLength(const Value: Single);
begin
  fCellLength := Value;
  Repaint;
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

function TmsShape.CorrectToLocalPoint(aPoint: TPointF): TPointF;
begin
  Result := aPoint * fScale + FCenterXY;
end;

constructor TmsShape.Create;
begin

end;

procedure TmsShape.DrawTo(aCanvas: TCanvas);
begin
  aCanvas.BeginScene();
  DrawShape(aCanvas);
  aCanvas.EndScene;
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

procedure TmsLine.DrawShape(aCanvas: TCanvas);
begin
  aCanvas.Stroke.Color := Color;
  aCanvas.DrawLine(CorrectToLocalPoint(fA), CorrectToLocalPoint(fB), 1);
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

procedure TcpCells.DrawShape(aCanvas: TCanvas);
var
  Step: Single;
begin
  Step := 0;
  while Step < aCanvas.Width do
  Begin
    Step := Step + Length;
    aCanvas.DrawLine(TPointF.Create(Step, 0), TPointF.Create(Step, aCanvas.Height), 0.5);
  End;
  Step := 0;
  while Step < aCanvas.Height do
  Begin
    Step := Step + Length;
    aCanvas.DrawLine(TPointF.Create(0, Step), TPointF.Create(aCanvas.Width, Step), 0.5);
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

procedure TmsFunction.DrawShape(aCanvas: TCanvas);
begin
  With aCanvas do
  Begin
    Stroke.Color := TAlphaColorRec.Chocolate;
    Stroke.Dash := TStrokeDash.Dash;
    DrawLine(CorrectToLocalPoint(TPointF.Create(0, -Self.fB)),
      CorrectToLocalPoint(TPointF.Create(Self.fA, -Self.fB)), 1);
    DrawLine(CorrectToLocalPoint(TPointF.Create(Self.fA, -Self.fB)),
      CorrectToLocalPoint(TPointF.Create(Self.fA, 0)), 1);
    Stroke.Dash := TStrokeDash.Solid;
    DrawLine(CorrectToLocalPoint(TPointF.Zero),
      CorrectToLocalPoint(TPointF.Create(Self.fA, -Self.fB)), 1);
    Stroke.Color := TAlphaColorRec.Black;
    DrawLine(CorrectToLocalPoint(TPointF.Create(0, -Self.fA)),
      CorrectToLocalPoint(TPointF.Create(Self.fB, 0)), 1);
  end;
end;

procedure TmsFunction.MouseMove(Shift: TShiftState; X, Y: Single);
begin

end;

{ TmsInequality }

constructor TmsInequality.Create(aX, aY, aB: Single);
begin
  X := aX;
  Y := aY;
  B := aB;
  Color := TAlphaColorRec.Blue;
end;

procedure TmsInequality.DrawShape(aCanvas: TCanvas);
var
  X, Y: TPointF;
begin
  aCanvas.BeginScene;
  try
    aCanvas.Stroke.Color := Color;
    X := PointA;
    Y := PointB;
    X.Y := -X.Y;
    aCanvas.DrawLine(CorrectToLocalPoint(X), CorrectToLocalPoint(Y), 1);
  finally
    aCanvas.EndScene;
  end;
end;

procedure TmsInequality.MouseMove(Shift: TShiftState; X, Y: Single);
begin

end;

function TmsInequality.PointA: TPointF;
begin
  Result := TPointF.Create(0, B / Y);
end;

function TmsInequality.PointB: TPointF;
begin
  Result := TPointF.Create(B / X, 0);
end;

end.
