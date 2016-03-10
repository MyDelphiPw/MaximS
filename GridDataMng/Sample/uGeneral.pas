unit uGeneral;

interface

uses
  FMX.Grid.DataManager,
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid, FMX.Layouts, FMX.StdCtrls, FMX.Edit, FMX.EditBox, FMX.NumberBox,
  FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Grid1: TGrid;
    Column1: TColumn;
    CheckColumn1: TCheckColumn;
    StringColumn1: TStringColumn;
    ProgressColumn1: TProgressColumn;
    PopupColumn1: TPopupColumn;
    ImageColumn1: TImageColumn;
    DateColumn1: TDateColumn;
    TimeColumn1: TTimeColumn;
    Layout1: TLayout;
    Label1: TLabel;
    NumberBox1: TNumberBox;
    NumberBox2: TNumberBox;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Grid1SetValue(Sender: TObject; const Col, Row: Integer;
      const Value: TValue);
    procedure Grid1GetValue(Sender: TObject; const Col, Row: Integer;
      var Value: TValue);
  private
    { Private declarations }
    GDM: TGridDataManager;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  GDM.Write(NumberBox1.Text.ToInteger, NumberBox2.Text.ToInteger, Edit1.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GDM := TGridDataManager.Create(Self);
  GDM.MyGrid := Grid1;
  GDM.Write(2, 2, true);
end;

procedure TForm1.Grid1GetValue(Sender: TObject; const Col, Row: Integer;
  var Value: TValue);
begin
  Value := GDM.Read(Col, Row);
end;

procedure TForm1.Grid1SetValue(Sender: TObject; const Col, Row: Integer;
  const Value: TValue);
begin
  GDM.Write(Col, Row, Value);
end;

end.
