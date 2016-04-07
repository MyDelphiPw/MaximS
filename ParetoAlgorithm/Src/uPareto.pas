unit uPareto;

interface

uses
  System.Math,
  System.Generics.Collections,
  Generics.Defaults;

type
{$M+}
  TBasePareItem<T> = Class
  private
    FA: T;
    FB: T;
    FTag: T;
  public
    Constructor Create(Const A, B: T); Virtual;
  published
    property A: T read FA write FA;
    property B: T read FB write FB;
    property Tag: T read FTag write FTag;
  End;

  TWebItems = TArray<TBasePareItem<Integer>>;

  TParetoItem = Class(TBasePareItem<Single>)
  private
    FName: String;
  public
    Constructor Create(Const A, B: Single; Const Name: String);
  published
    property Name: String read FName write FName;
  End;

  TPareto = CLass(TObjectList<TParetoItem>)
  private
    FMaxA: Boolean;
    FMaxB: Boolean;
  protected
    Class Function FillMapAB(Const Start, Finish: Integer): TWebItems;
  public
    Class Function WebElement(Const Count: Integer): TWebItems; overload;

    Class Function WebElement(Const Values: TArray<Integer>)
      : TWebItems; overload;

    Function Compare(Const Index1, Index2: Integer): TValueSign;
    Function Compress: Integer;
    Constructor Create;
    destructor Destroy; override;
  public
    property MaxA: Boolean read FMaxA write FMaxA default true;
    property MaxB: Boolean read FMaxB write FMaxB default true;
  End;

implementation

{ TMyClass }

function TPareto.Compare(const Index1, Index2: Integer): TValueSign;
var
  Coeff1, Coeff2: SmallInt;
begin
  Coeff1 := CompareValue(Items[Index1].A, Items[Index2].A);
  Coeff2 := CompareValue(Items[Index1].B, Items[Index2].B);
  if NOT MaxA then
    Coeff1 := Coeff1 * -1;
  if NOT MaxB then
    Coeff2 := Coeff2 * -1;
  Result := (Coeff1 + Coeff2);
  if Result > 1 then
    Result := 1;
  if Result < -1 then
    Result := -1;
end;

function TPareto.Compress: Integer;
Var
  MAP_COMPRESS: TWebItems;

  mapForDelete: TList<Integer>;
  I: Integer;
begin
  MAP_COMPRESS := WebElement(Self.Count);
  mapForDelete := TList<Integer>.Create;
  try
    for I := Low(MAP_COMPRESS) to High(MAP_COMPRESS) do
      if Compare(MAP_COMPRESS[I].A, MAP_COMPRESS[I].B) > 0 then
      Begin
        if NOT mapForDelete.Contains(MAP_COMPRESS[I].B) then
          mapForDelete.Add(MAP_COMPRESS[I].B);
      End
      else if Compare(MAP_COMPRESS[I].A, MAP_COMPRESS[I].B) < 0 then
      Begin
        if NOT mapForDelete.Contains(MAP_COMPRESS[I].A) then
          mapForDelete.Add(MAP_COMPRESS[I].A);
      End;

    mapForDelete.Sort;
    for I := mapForDelete.Count - 1 downTo 0 do
      Self.Delete(mapForDelete[I]);
  finally
    for I := Low(MAP_COMPRESS) to High(MAP_COMPRESS) do
      MAP_COMPRESS[I].Free;
    mapForDelete.Free;

  end;
  Result := Count;
end;

constructor TPareto.Create;
begin
  inherited Create;
  MaxA := true;
  MaxB := true;
end;

destructor TPareto.Destroy;
begin
  inherited Destroy;
end;

Class function TPareto.FillMapAB(const Start, Finish: Integer): TWebItems;
var
  I, j: Integer;
begin
  SetLength(Result, (Finish - Start));
  j := low(Result);
  for I := Start + 1 to Finish do
  Begin
    Result[j] := TBasePareItem<Integer>.Create(Start, I);
    Inc(j);
  End;
end;

class function TPareto.WebElement(const Values: TArray<Integer>): TWebItems;
Var
  IndexPath: TArray<TBasePareItem<Integer>>;
  I: Integer;
begin
  try
    IndexPath := WebElement(Length(Values));
    SetLength(Result, Length(IndexPath));
    for I := Low(Result) to High(Result) do
      Result[I] := TBasePareItem<Integer>.Create(Values[IndexPath[I].A],
        Values[IndexPath[I].B]);
  finally
    for I := Low(IndexPath) to High(IndexPath) do
      IndexPath[I].Free;
  end;
end;

Class function TPareto.WebElement(const Count: Integer): TWebItems;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to Count - 1 do
    Result := Result + FillMapAB(I, Count - 1);
end;

{ TBasePareItem<T> }

constructor TBasePareItem<T>.Create(const A, B: T);
begin
  Self.A := A;
  Self.B := B;

end;

{ TParetoItem<T> }

constructor TParetoItem.Create(const A, B: Single; const Name: String);
begin
  Self.A := A;
  Self.B := B;
  Self.Name := Name;
end;

end.
