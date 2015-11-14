unit RegMaximS;

interface

{$I JEDI.INC}

uses
  Classes
{$IFDEF DELPHIXE_UP}
    , msProxyFind, msFPS
{$ENDIF}
    ;

procedure Register;

implementation

procedure Register;
const
  AutorCell = 'MaximS';
begin
{$IFDEF DELPHIXE_UP}
  RegisterComponents(AutorCell, [
                                  TmsProxyFind,
                                  TmsFPS
                                ]
                    );
{$ENDIF}
end;

end.
