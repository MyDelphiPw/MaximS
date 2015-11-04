unit main;

interface

uses
  uGraphicalLP,
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TForm3 = class(TForm)
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Core: TGraphicLP;
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.Button1Click(Sender: TObject);
begin
  Caption := Core.CenterXY.y.ToString;
  Core.Scale := Core.Scale + 0.2;
end;

procedure TForm3.FormCreate(Sender: TObject);

begin
  ReportMemoryLeaksOnShutdown := True;
  Core := TGraphicLP.Create(Panel1);

  Core.Parent := Panel1;
  Core.Align := TAlignLayout.Client;
  {
    6x1 + 4x2 ≤ 24
    x1 + 2x2 ≤ 6
    x2 - x1 ≤  1
  }
  Core.CenterXY := TPointF.Create(60, Panel1.Height - 60);
  Core.Add(TInequality.Create(12, 4, 300));
  Core.Add(TInequality.Create(4, 4, 120));
  Core.Add(TInequality.Create(3, 12, 252));
  Core.TargerFunction := TFunctionLP.Create(30, 40, True);
  Caption := Core.CenterXY.X.ToString;
  Core.Scale := 1;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  Core.Free;
end;

end.
