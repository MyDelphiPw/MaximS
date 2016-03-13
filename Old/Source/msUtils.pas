unit msUtils;

interface

uses Classes;

type
  TmsUParsing = Class(TObject)
    Class Function GiveTag(Value, Start, Stop: String): String;
    Class Function GiveTagS(Value, Start, Stop: string): TStringList;
  End;

  TmsApplication = Class(TObject)
    Class Function Path: String;
  End;

  TmsEdDeCoder = Class(TObject)

    Class Function EncodeURIComponent(const ASrc: string): UTF8String;
  End;

implementation

uses
  System.SysUtils;
{ TmsUtilsParsing }

class function TmsUParsing.GiveTag(Value, Start, Stop: string): string;
begin
  Start := Copy(Value, pos(Start, Value) + Length(Start), Length(Value));
  Result := Copy(Start, 1, pos(Stop, Start) - 1);
end;

class function TmsUParsing.GiveTagS(Value, Start, Stop: string): TStringList;
var
  start_, fin: Integer;
begin
  Result := TStringList.Create;
  repeat
    start_ := pos(Start, Value);
    if start_ > 0 then
    begin
      Value := Copy(Value, start_ + Length(Start), Length(Value) - 1);
      start_ := 1;
      fin := pos(Stop, Value);
      Result.Add(Copy(Value, start_, fin - start_));
      Value := Copy(Value, fin + Length(Stop), Length(Value) - 1);
    end;
  until start_ <= 0;
end;

{ TmsApplication }

class function TmsApplication.Path: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

{ TmsEdDeCoder }

class function TmsEdDeCoder.EncodeURIComponent(const ASrc: string): UTF8String;
const
  HexMap: UTF8String = '0123456789ABCDEF';
  function IsSafeChar(ch: Integer): Boolean;
  begin
    if (ch >= 48) and (ch <= 57) then
      Result := True // 0-9
    else if (ch >= 65) and (ch <= 90) then
      Result := True // A-Z
    else if (ch >= 97) and (ch <= 122) then
      Result := True // a-z
    else if (ch = 33) then
      Result := True // !
    else if (ch >= 39) and (ch <= 42) then
      Result := True // '()*
    else if (ch >= 45) and (ch <= 46) then
      Result := True // -.
    else if (ch = 95) then
      Result := True // _
    else if (ch = 126) then
      Result := True // ~
    else
      Result := False;
  end;

var
  I, J: Integer;
  ASrcUTF8: UTF8String;
begin
  Result := ''; { Do not Localize }
  ASrcUTF8 := UTF8Encode(ASrc);
  // UTF8Encode call not strictly necessary but
  // prevents implicit conversion warning
  I := 1;
  J := 1;
  SetLength(Result, Length(ASrcUTF8) * 3); // space to %xx encode every byte
  while I <= Length(ASrcUTF8) do
  begin
    if IsSafeChar(Ord(ASrcUTF8[I])) then
    begin
      Result[J] := ASrcUTF8[I];
      Inc(J);
    end
    else if ASrcUTF8[I] = ' ' then
    begin
      Result[J] := '+';
      Inc(J);
    end
    else
    begin
      Result[J] := '%';
      Result[J + 1] := HexMap[(Ord(ASrcUTF8[I]) shr 4) + 1];
      Result[J + 2] := HexMap[(Ord(ASrcUTF8[I]) and 15) + 1];
      Inc(J, 3);
    end;
    Inc(I);
  end;
  SetLength(Result, J - 1);
end;

end.
