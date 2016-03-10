program GridDataManagerSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  uGeneral in 'uGeneral.pas' {Form1},
  FMX.Grid.DataManager in '..\src\FMX.Grid.DataManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
