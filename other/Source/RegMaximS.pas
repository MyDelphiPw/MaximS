unit RegMaximS;

interface

{$I JEDI.INC}

uses
  Classes
{$IFDEF DELPHIXE_UP}
    , msProxyFind
{$ENDIF}
    ;

procedure Register;

implementation

procedure Register;
const
  AutorCell = 'MaximS';
begin
{$IFDEF DELPHIXE_UP}
  RegisterComponents(AutorCell, [TmsProxyFind]);
{$ENDIF}
end;

end.
