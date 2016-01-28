unit msFPS;

interface

uses
  msBaseComponent,
  FMX.Types,
  FMX.Controls,
  FMX.Graphics,
  System.Types, System.Classes;

Type
  TOnFPS = procedure(Sender: TObject; Value: Single) of object;

  TmsFPS = Class(TmsBase)
  private
    fValue, fFPSCounter: Single;
    FOnFPS: TOnFPS;
    FControl: TControl;
    FTimer: TTimer;
    FEnabled: Boolean;
    procedure OnTimer(Sender: TObject);
    procedure OnPaint(Sender: TObject; Canvas: TCanvas; const [Ref] ARect: TRectF);
    procedure SetControl(const Value: TControl);
  public

    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled default true;
    property Value: Single read fValue;
    property Control: TControl read FControl write SetControl;
    property OnFPS: TOnFPS read FOnFPS write FOnFPS;
  End;

implementation

constructor TmsFPS.Create(AOwner: TComponent);
begin
  inherited;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := OnTimer;
  Enabled := true;
  fValue := 0;
  fFPSCounter := 0;
end;

destructor TmsFPS.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TmsFPS.OnPaint(Sender: TObject; Canvas: TCanvas; const [Ref] ARect: TRectF);
begin
  fFPSCounter := fFPSCounter + 1;
end;

procedure TmsFPS.OnTimer(Sender: TObject);
begin
  Control.Repaint;
  fValue := fFPSCounter;
  if Assigned(OnFPS) then
    OnFPS(Self, Value);
  fFPSCounter := 0;
end;

procedure TmsFPS.SetControl(const Value: TControl);
begin
  FTimer.Enabled := Enabled and Assigned(Value);
  if not FTimer.Enabled then
    exit;
  FControl := Value;
  FControl.OnPaint := OnPaint;
end;

end.
