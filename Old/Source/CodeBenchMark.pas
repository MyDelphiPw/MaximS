unit CodeBenchMark;

interface

(* TCodeSpeed - подсчет времени выполнения кода *)
(* 2015, Maxim Sysoev, www.codmasters.ru *)

uses
  System.Generics.Collections;

Type

  TCodeBenchMarkCore = Class(TInterfacedObject)
  Strict private
    fQPF, fTimeStart, fTimeStop: Int64;
  public
    Constructor Create;
    Procedure Start(); stdcall;
    Function Stop(): Single; stdcall;
    destructor Destroy; override;

  End;

  TCodeBenchMark = Class
  Public type
    TCodeBenchMarkItem = Class
    private
      FName: String;
      fBenchMark: TCodeBenchMarkCore;
      FDuration: Single;
    published
      property Name: String read FName;
      property Duration: Single read FDuration;
    End;
  private
    fItem: TList<TCodeBenchMark.TCodeBenchMarkItem>;
    function getItem(Index: Integer): TCodeBenchMark.TCodeBenchMarkItem;
  public
    Function Count: Integer;
    procedure Clear;
    Constructor Create;
    Destructor Destroy; override;
    Procedure Add(ID: String);
    Function Stop(ID: String): Single;
    property Items[Index: Integer]: TCodeBenchMark.TCodeBenchMarkItem read getItem; default;
  End;

implementation

{$IF defined(MSWINDOWS)}

uses Winapi.Windows;
{$ELSEIF defined(MACOS)}

uses Macapi.Mach;
{$ELSEIF defined(POSIX)}

uses Posix.Time;
{$ENDIF}
{ TCodeSpeed }

constructor TCodeBenchMarkCore.Create;
begin
  Start;
end;

destructor TCodeBenchMarkCore.Destroy;
begin

  inherited;
end;

procedure TCodeBenchMarkCore.Start;
begin
  QueryPerformanceFrequency(fQPF);
  QueryPerformanceCounter(fTimeStart);
end;

function TCodeBenchMarkCore.Stop: Single;
begin
  QueryPerformanceCounter(fTimeStop);
  Result := (fTimeStop - fTimeStart) / fQPF;
end;

{ TCodeBenchMark }

procedure TCodeBenchMark.Add(ID: String);
var
  Item: TCodeBenchMarkItem;
begin
  Item := TCodeBenchMarkItem.Create;
  Item.FName := ID;
  Item.fBenchMark := TCodeBenchMarkCore.Create;
  Item.fBenchMark.Create;
  fItem.Add(Item);
end;

procedure TCodeBenchMark.Clear;
begin
  fItem.Clear;
end;

function TCodeBenchMark.Count: Integer;
begin
  Result := fItem.Count;
end;

constructor TCodeBenchMark.Create;
begin
  fItem := TList<TCodeBenchMarkItem>.Create;
end;

destructor TCodeBenchMark.Destroy;
begin
  fItem.Free;
  inherited;
end;

function TCodeBenchMark.getItem(Index: Integer): TCodeBenchMark.TCodeBenchMarkItem;
begin
  Result := fItem[Index];
end;

function TCodeBenchMark.Stop(ID: String): Single;
var
  I: Integer;
begin
  for I := fItem.Count - 1 Downto 0 do
    if fItem[I].Name = ID then
      fItem[I].FDuration := fItem[I].fBenchMark.Stop;
end;

end.
