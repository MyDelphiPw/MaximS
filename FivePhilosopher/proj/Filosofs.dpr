program Filosofs;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit3 in '..\src\Unit3.pas' {Form3},
  Philosopher in '..\src\Philosopher.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
