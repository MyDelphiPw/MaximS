unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure OnFindProxy(Sender: TObject; Const IpPort: String);

    procedure _Begin(Sender: TObject);
    procedure _END(Sender: TObject);
    procedure _SiteProgress(Sender: TObject; Const Site: String;
      Index: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses msProxyFind;

var
  ProxyFinder: TmsProxyFind;

procedure TForm1.Button1Click(Sender: TObject);
var
  x: Integer;
begin
  for x := 0 to Memo1.Lines.Count - 1 do
    ProxyFinder.AddSite(Memo1.Lines[x]);
  ProgressBar1.Max := Memo1.Lines.Count;
  ProxyFinder.FindOnSites;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ProxyFinder.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ProxyFinder := TmsProxyFind.Create;
  ProxyFinder.OnFindProxy := OnFindProxy;
  ProxyFinder.OnBeforFindOnSite := _Begin;
  ProxyFinder.OnAfterFindOnSite := _END;
  ProxyFinder.OnSiteFindProgress := _SiteProgress;
  ProxyFinder.SaveProxy := False;
end;

procedure TForm1.OnFindProxy(Sender: TObject; const IpPort: String);
begin
  Memo2.Lines.Append(IpPort);
  GroupBox2.Caption := 'Point prox: ' + IntToStr(Memo2.Lines.Count);
end;

procedure TForm1._Begin(Sender: TObject);
begin
  Caption := 'Proxy Finder: TmsProxyFind Example [Start]';
end;

procedure TForm1._END(Sender: TObject);
begin
  Caption := 'Proxy Finder: TmsProxyFind Example [Finish]';
end;

procedure TForm1._SiteProgress(Sender: TObject; const Site: String;
  Index: Integer);
begin
  ProgressBar1.Position := Index + 1;
end;

end.
