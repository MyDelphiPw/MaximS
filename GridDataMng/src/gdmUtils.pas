unit gdmUtils;

interface

uses
  System.Rtti;

type
  TgdmUtils = Class
    function ValueAs<T>(const Value: TValue): T;
  End;

implementation

uses
  System.SysUtils,
  System.TypInfo;

{ TgdmUtils }

function TgdmUtils.ValueAs<T>(const Value: TValue): T;
begin
  case PTypeInfo(System.TypeInfo(T)).Kind of
    TTypeKind.tkInteger:
      Result := Value.AsString.ToInteger;
    TTypeKind.tkFloat:
      Result := Value.AsString.ToSingle;
  end;
end;

end.
