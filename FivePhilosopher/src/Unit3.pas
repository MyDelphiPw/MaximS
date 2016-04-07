unit Unit3;

interface

uses
  Philosopher,
  System.Generics.Collections,
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, FMX.Edit, FMX.EditBox,
  FMX.SpinBox, FMX.Objects;

type
  TForm3 = class(TForm)
    Layout1: TLayout;
    Switch1: TSwitch;
    Label1: TLabel;
    Layout2: TLayout;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Switch1Switch(Sender: TObject);

  private
    { Private declarations }
    FPieList: TObjectList<TPie>;
  public
    { Public declarations }

  end;

var
  Form3: TForm3;
  Ph: array [1 .. 5] of TPhilosophers; // массив философов
  PHID: array [1 .. 5] of Cardinal; // идентификаторы потоков
  state: array [1 .. 5] of Tmode; // состояния философов

implementation

{$R *.fmx}
{ TForm3 }

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FPieList.Free;
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  I: Integer;
  PieAngle: Single;
begin
  Randomize;
  (*
    Создаем философов. Создаем "Стол" философов.
  *)
  FPieList := TObjectList<TPie>.Create;
  for I := low(Ph) to High(Ph) do
  begin
    Ph[I] := TPhilosophers.Create(true);
    PHID[I] := Ph[I].ThreadID;
    state[I] := phThinking;
    FPieList.Add(TPie.Create(Self));
    FPieList.Last.Parent := Self;
    FPieList.Last.Align := TAlignLayout.Client;
    FPieList.Last.Name := 'Pie' + I.ToString;
  end;

  PieAngle := 360 / FPieList.Count;
  for I := 0 to FPieList.Count - 1 do
  Begin
    FPieList[I].StartAngle := I * PieAngle;
    FPieList[I].EndAngle := FPieList[I].StartAngle + PieAngle;
  End;
end;

procedure TForm3.Switch1Switch(Sender: TObject);
var
  MyElem: TPhilosophers;
begin
  (*
    Запускаем или останавливаем философов
  *)
  for MyElem in Ph do
    if Switch1.IsChecked then
      MyElem.Resume
    else
      MyElem.Suspend;
end;

end.
