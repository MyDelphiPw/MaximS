unit main;

interface

uses

  CoordinatePlane,
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Edit, FMX.ComboEdit,
  FMX.ComboTrackBar;

{$DEFINE NewMS}

type
  TForm3 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Timer1: TTimer;
    StyleBook1: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Core: {$IFDEF NewMS}TCoordinatePlane{$ELSE}TGraphicLP{$ENDIF};

  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
{$IFDEF NewMS}
  Core := TCoordinatePlane.Create(Self);
  Core.Parent := Panel1;
  Core.Align := TAlignLayout.Client;
  Core.CenterXY := Core.BoundsRect.CenterPoint;
  // Core.Canvas.Quality := TCanvasQuality.HighQuality;
  Core.ShapeList.Add(TmsFunction.Create(30, 40, True));
  Core.ShapeList.Add(TmsLimitation.Create(3, 12, TInequality.Better, 252));
  Core.ShapeList.Add(TmsLimitation.Create(12, 4, TInequality.Better, 300));
  Core.ShapeList.Add(TmsLimitation.Create(4, 4, TInequality.Better, 120));

{$ELSE}
{$ENDIF}
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin

  Core.Free;
  // new.Free;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  Core.Repaint;
end;

end.
