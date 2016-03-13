unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  msFPS, msBaseComponent, FMX.ListBox, FMX.Layouts, System.Math.Vectors,
  FMX.Controls3D, FMX.Objects3D, FMX.Viewport3D, FMX.Ani, FMX.Objects;

type
  TForm2 = class(TForm)
    msFPS1: TmsFPS;
    AniIndicator1: TAniIndicator;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    msFPS2: TmsFPS;
    ListBoxItem2: TListBoxItem;
    Pie1: TPie;
    FloatAnimation1: TFloatAnimation;
    msFPS3: TmsFPS;
    ListBoxItem3: TListBoxItem;
    procedure msFPS1FPS(Sender: TObject; Value: Single);
    procedure msFPS2FPS(Sender: TObject; Value: Single);
    procedure msFPS3FPS(Sender: TObject; Value: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.msFPS1FPS(Sender: TObject; Value: Single);
begin
  ListBox1.Items.Strings[0] := 'AniIndicator1 = ' + Value.ToString;
end;

procedure TForm2.msFPS2FPS(Sender: TObject; Value: Single);
begin
  ListBox1.Items.Strings[1] := 'ListBox1 = ' + Value.ToString;
end;

procedure TForm2.msFPS3FPS(Sender: TObject; Value: Single);
begin
  ListBox1.Items.Strings[2] := 'Pie1 = ' + Value.ToString;
end;

end.
