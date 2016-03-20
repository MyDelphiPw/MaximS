unit uPareto;

interface

uses
  System.Generics.Collections;

type
  TParetoItem = Class
  private
    FA: Single;
    FB: Single;
    FName: String;
  public
    Constructor Create(Const A, B: Single; Const Name: String);
  published
    property A: Single read FA write FA;
    property B: Single read FB write FB;
    property Name: String read FName write FName;
  End;

  TParetoResult = (RESULT_BIGER = 1, RESULT_INCOMPARABLY = 0,
    RESULT_SMALLER = -1);

  TPareto = CLass(TObjectList<TParetoItem>)
  private
    FMaxA: Boolean;
    FMaxB: Boolean;
  protected
    Function MapCompress(Const Count: Integer): TArray<TArray<Integer>>;
  public
    Function Compare(Const Index1, Index2: Integer): TParetoResult;
    Function Compress: Integer;
    Constructor Create;
    destructor Destroy; override;
  public
    property MaxA: Boolean read FMaxA write FMaxA default true;
    property MaxB: Boolean read FMaxB write FMaxB default true;
  End;

implementation

Uses
  System.Math;
{ TMyClass }

function TPareto.Compare(const Index1, Index2: Integer): TParetoResult;
var
  IndexA: Integer;
  IndexB: Integer;
begin
  //
  if (Self[Index1].A = Self[Index2].A) and (Self[Index1].B = Self[Index2].B)
  then
    Exit(TParetoResult.RESULT_INCOMPARABLY);
  if MaxA then
  Begin
    if Self[Index1].A > Self[Index2].A then
      IndexA := Index1
    else
      IndexA := Index2;
  End
  else if NOT MaxA then
  Begin
    if Self[Index1].A < Self[Index2].A then
      IndexA := Index1
    else
      IndexA := Index2;
  End;
  //
  if MaxB then
  Begin
    if Self[Index1].B > Self[Index2].B then
      IndexB := Index1
    else
      IndexB := Index2;
  End
  else if NOT MaxB then
  Begin
    if Self[Index1].B < Self[Index2].B then
      IndexB := Index1
    else
      IndexB := Index2;
  End;
  //
  if IndexA <> IndexB then
  Begin
    Exit(TParetoResult.RESULT_INCOMPARABLY);
  End
  else if IndexA = IndexB then
  Begin
    if IndexA = Index1 then
      Exit(TParetoResult.RESULT_BIGER)
    else
      Exit(TParetoResult.RESULT_SMALLER)
  End;
end;

function TPareto.Compress: Integer;
Var
  MAP_COMPRESS: TArray<TArray<Integer>>;
begin
  MAP_COMPRESS := MapCompress(Self.Count);
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

function TPareto.MapCompress(const Count: Integer): TArray<TArray<Integer>>;
var
  I: Integer;
  LLength: Integer;
begin
  LLength := 0;
  for I := 1 to Count - 1 do
    inc(LLength, I);
  SetLength(Result, LLength);
  for I := 0 to Count - 2 do // Последний нам не нужен
  Begin

  End;
end;

{ TParetoItem }

constructor TParetoItem.Create(const A, B: Single; const Name: String);
begin
  Self.A := A;
  Self.B := B;
  Self.Name := Name;
end;

end.
