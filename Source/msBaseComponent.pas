unit msBaseComponent;

interface

uses
  Classes;

type
  TmsBase = class(TComponent)
  Private Const
    CAutor = '©Maxim Sysoev, mydelphi.pw, 2015';
  protected
    fAutor: String;
    fVersion: String;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); overload; override;
    Constructor Create; overload; Virtual;
  published
    { Published declarations }
    Property Autor: String read fAutor;
    Property Version: String read fVersion;
  end;

implementation

{ TmsBase }

constructor TmsBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Create;
end;

constructor TmsBase.Create;
begin
  fAutor := CAutor;
  fVersion := '0.1';
end;

end.
