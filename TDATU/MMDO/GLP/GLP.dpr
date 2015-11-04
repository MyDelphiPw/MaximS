program GLP;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {Form3},
  uGraphicalLP in 'uGraphicalLP.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
