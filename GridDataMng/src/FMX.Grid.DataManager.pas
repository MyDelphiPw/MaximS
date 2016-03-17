unit FMX.Grid.DataManager;

interface

uses
  System.Classes,
  System.Rtti,
  FMX.Grid;

type
  TGridDataManager = Class(System.Classes.TComponent)
  private
    FData: array of array of TValue; // Col and Row
    FMyGrid: TCustomGrid; //
  protected
    Procedure SetColLength(Const Value: Integer); Virtual;
    Procedure SetRowLength(Const Value: Integer); Virtual;
    procedure SetColRowLength(Const Col, Row: Integer); Virtual;
  public
    /// <summary>Получить значение</summary>
    Function Read(Const Col, Row: Integer): TValue;
    /// <summary>Записать значение</summary>
    Procedure Write(Const Col, Row: Integer; Const Value: TValue);
    //
    procedure Clear; overload;
    procedure Clear(Const Col, Row: Integer); overload;

    function ColumnCount: Integer;
    function RowCount: Integer;
  published
    property MyGrid: TCustomGrid read FMyGrid write FMyGrid;
  End;

procedure Register;

implementation

procedure Register;
Begin
  RegisterComponents('Grids', [TGridDataManager]);
End;

{ TGridDataManager }

procedure TGridDataManager.Clear(const Col, Row: Integer);
begin
  Write(Col, Row, TValue.Empty);
end;

procedure TGridDataManager.Clear;
var
  I: Integer;
  J: Integer;
begin
  for I := Low(FData) to High(FData) do
    for J := Low(FData[I]) to High(FData[I]) do
      Clear(I, J);
end;

function TGridDataManager.ColumnCount: Integer;
begin
  Result := Length(FData);
end;

function TGridDataManager.Read(const Col, Row: Integer): TValue;
begin
  if (NOT Assigned(MyGrid)) or (Col > FMyGrid.ColumnCount) or
    (Row > FMyGrid.RowCount) then
    Exit;
  SetColRowLength(FMyGrid.ColumnCount, FMyGrid.RowCount);
  Result := FData[Col][Row];
end;

function TGridDataManager.RowCount: Integer;
begin
  Result := 0;
  if ColumnCount > 0 then
    Result := Length(FData[0]);
end;

procedure TGridDataManager.SetColLength(const Value: Integer);
begin
  if FMyGrid.ColumnCount = self.ColumnCount then
    Exit;
  SetLength(FData, Value);
end;

procedure TGridDataManager.SetColRowLength(const Col, Row: Integer);
begin
  SetColLength(Col);
  SetRowLength(Row);
end;

procedure TGridDataManager.SetRowLength(const Value: Integer);
var
  I: Integer;
begin
  if FMyGrid.RowCount = self.RowCount then
    Exit;
  for I := Low(FData) to High(FData) do
    SetLength(FData[I], Value);
end;

procedure TGridDataManager.Write(const Col, Row: Integer; const Value: TValue);
begin
  if (NOT Assigned(MyGrid)) or (Col > FMyGrid.ColumnCount) or
    (Row > FMyGrid.RowCount) then
    Exit;
  SetColRowLength(FMyGrid.ColumnCount, FMyGrid.RowCount);
  FData[Col][Row] := Value;
  MyGrid.Repaint;
end;

end.
