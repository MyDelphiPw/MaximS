{ ******************************************************* }
{ }
{ MaximS Component Library }
{ }
{ Copyright(c) 2012-2015 MyDelphi.Pw }
{ }
{ Original Author: Sysoev Maxim }
{ }
{ ******************************************************* }

unit msProxyFind;

interface

uses
  RegularExpressions,
  SysUtils,
  Classes,
  msBaseComponent,
  idHTTP;

{$I ../Install/JEDI.INC}

Type
  TOnFindProxy = Procedure(Sender: TObject; Const IpPort: String) Of Object;
  TOnSiteFindProgress = Procedure(Sender: TObject; Const Site: String;
    Index: Integer) Of Object;
  DynString = array of string;

  TmsProxyFind = Class(TmsBase)
  Private Const
    PATTERN = //
      '\b(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.' +
    { } '(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.' +
    { } '(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.' +
    { } '(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\:' +
    { } '\d{1,5}\b';
  private
    { Private declarations }
    fHTTP: TIdHTTP;
    fSites, fProxy: TStringList;
    fOnFindProxy: TOnFindProxy;
    fSaveProxy: boolean;
    fOnAfterFind: TNotifyEvent;
    fOnBeforFind: TNotifyEvent;
    fOSFP: TOnSiteFindProgress;
    fEnabled: boolean;
    function GetItems(Index: Integer): string;
    procedure SetSaveProxy(const Value: boolean);
    procedure SetEnabled(const Value: boolean);
  protected
    { Protected declarations }
    Function Core(Text: String): DynString;
  public
    { Public declarations }
    /// <summary> Получаем из строки IP и Port
    /// </summary>
    /// <param name="IpPort">Строка, в которой находятся IP и Port
    /// </param>
    /// <param name="Ip">В эту переменную запишется IP
    /// </param>
    /// <param name="Port">В эту переменную запишется Port
    /// </param>
    /// <remarks>
    /// В качестве разделителя должен быть символ ":"
    /// </remarks>
    Class Procedure SplitIpPort(IpPort: String; Var Ip, Port: String);
    /// <summary> Добавление сайтов, где можно спарсить прокси
    /// </summary>
    /// <param name="Site">Адресс сайта, на котором находятся прокси сервера.
    /// </param>
    /// <remarks>
    /// Формат сайта: http://*****.**
    /// </remarks>
    Procedure AddSite(Const Site: String); overload;
    /// <summary> Добавление сайтов, где можно спарсить прокси
    /// </summary>
    /// <param name="Site">Массив с адрессами сайтов, на которых находятся прокси сервера.
    /// </param>
    /// <remarks>
    /// Формат сайта: http://*****.**
    /// </remarks>
    Procedure AddSite(Const Site: DynString); overload;
    /// <summary> Добавление сайтов, где можно спарсить прокси
    /// </summary>
    /// <param name="Site">Список с адрессами сайтов, на которых находятся прокси сервера.
    /// </param>
    /// <remarks>
    /// Формат сайта: http://*****.**
    /// </remarks>
    Procedure AddSite(Const Site: TStrings); overload;
    /// <summary> Парсим прокси с введенного текста
    /// </summary>
    /// <remarks>
    /// Обертка над Function Core
    /// </remarks>
    /// <param name="Text">Текст, в котором находятся IP:Port
    /// </param>
    /// <returns>Результатом функции будет массив IP:Port Array of String
    /// </returns>

    Class Function FindProxyInText(Text: String): DynString; overload;
    /// <summary> Парсим прокси с введенного текста
    /// </summary>
    /// <remarks>
    /// Обертка над Function Core
    /// </remarks>
    /// <param name="Text">Текст, в котором находятся IP:Port
    /// </param>
    /// <param name="OutPut">Добавляется массив IP:Port Array of String
    /// </param>
    Class Procedure FindProxyInText(Text: String; OutPut: DynString); overload;
    /// <summary> Парсим прокси с введенного текста
    /// </summary>
    /// <remarks>
    /// Обертка над Function Core
    /// </remarks>
    /// <param name="Text">Текст, в котором находятся IP:Port
    /// </param>
    /// <param name="OutPut">Добавляется Список IP:Port TStrings
    /// </param>
    Class Procedure FindProxyInText(Text: String; OutPut: TStrings); overload;
    /// <summary> Парсим прокси с введенного текста
    /// </summary>
    /// <remarks>
    /// Обертка над Function Core
    /// </remarks>
    /// <param name="Text">Текст, в котором находятся IP:Port
    /// </param>
    /// <param name="OutPut">Добавляется Список IP:Port TStrings
    /// </param>
    Class Procedure FindProxyInText(Text: String; OutPut: TStringList);
      overload;

    /// <summary>Загрузка и парсинг прокси с сайтов
    /// </summary>
    /// <remarks>
    /// Сразу Необходимо добавить сайты (AddSite).
    /// </remarks>
    /// <param name="Text">Текст, в котором находятся IP:Port
    /// </param>
    /// <param name="OutPut">Добавляется Список IP:Port TStrings
    /// </param>
    Procedure FindOnSites;
    (* Применить пользовательские настройки idHTTP *)
    Procedure ApplyHTTP(const idHTTP: TIdHTTP);
    (* Доступ к списку прокси *)
    property Items[Index: Integer]: string read GetItems; default;
    Function Count: Integer;
    Constructor Create; override;
    Destructor Destroy; override;

  published
    { Published declarations }
    property Enabled: boolean read fEnabled write SetEnabled default true;
    property SaveProxy: boolean read fSaveProxy write SetSaveProxy default true;
    Property OnFindProxy: TOnFindProxy read fOnFindProxy write fOnFindProxy;
    Property OnBeforFindOnSite: TNotifyEvent read fOnBeforFind
      write fOnBeforFind;
    Property OnAfterFindOnSite: TNotifyEvent read fOnAfterFind
      write fOnAfterFind;
    Property OnSiteFindProgress: TOnSiteFindProgress read fOSFP write fOSFP;
  end;

implementation

type
{$M+}
  TDownLoadThread = class(TThread)
  private
    msProxyFind: TmsProxyFind;
    fOnFindProxy: TOnFindProxy;
    fOSFP: TOnSiteFindProgress;
  protected
    procedure Execute; override;
  public
    constructor Create(Var AmsProxyFind: TmsProxyFind);
  published
    Property OnFindProxy: TOnFindProxy read fOnFindProxy write fOnFindProxy;
    Property OnSiteFindProgress: TOnSiteFindProgress read fOSFP write fOSFP;
  end;

var
  DownLoadThread: TDownLoadThread;
  { TmsProxyFind }

procedure TmsProxyFind.AddSite(const Site: String);
begin
  fSites.Append(Site);
end;

procedure TmsProxyFind.AddSite(const Site: DynString);
var
  i, j: Integer;
begin
  j := High(Site);
  for i := 0 to j do
    AddSite(Site[i]);
end;

procedure TmsProxyFind.AddSite(const Site: TStrings);
var
  x: Integer;
begin
  for x := 0 to Site.Count - 1 do
    AddSite(Site[x]);
end;

procedure TmsProxyFind.ApplyHTTP(const idHTTP: TIdHTTP);
begin
  fHTTP := idHTTP;
end;

function TmsProxyFind.Core(Text: String): DynString;
var
  vRegEx: TRegEx;
  vMatchCollection: TMatchCollection;
  i, c: Integer;
  WhoAdd: String;
begin
  vRegEx.Create(Self.PATTERN);
  vMatchCollection := vRegEx.Matches(Text);
  c := vMatchCollection.Count;
  SetLength(Result, 0);
  for i := 0 to c - 1 do
  begin
    SetLength(Result, Length(Result) + 1);
    WhoAdd := vMatchCollection[i].Value;
    Result[High(Result)] := WhoAdd;
    if Assigned(fProxy) then
      if SaveProxy then
        fProxy.Append(WhoAdd);
    if Assigned(OnFindProxy) then
      OnFindProxy(Self, WhoAdd);
  end;
end;

function TmsProxyFind.Count: Integer;
begin
  Result := fProxy.Count;
end;

constructor TmsProxyFind.Create;
begin
  inherited;
  fVersion := '0.2';
  fHTTP := TIdHTTP.Create;
  fSites := TStringList.Create;
  fProxy := TStringList.Create;
  SaveProxy := true;
  Enabled := true;
end;

destructor TmsProxyFind.Destroy;
begin
  fHTTP.Free;
  fSites.Free;
  fProxy.Free;
  inherited Destroy;
end;

Class procedure TmsProxyFind.FindProxyInText(Text: String; OutPut: TStrings);
var
  i, j: Integer;
  Temp: DynString;
begin
  Temp := FindProxyInText(Text);
  j := High(Temp);
  for i := 0 to j do
    OutPut.Add(Temp[i]);
end;

procedure TmsProxyFind.FindOnSites;

begin
  DownLoadThread := TDownLoadThread.Create(Self);
  // raise Exception.Create(Self.fSites.Text);
  DownLoadThread.OnFindProxy := Self.OnFindProxy;
  if Assigned(OnBeforFindOnSite) then
    OnBeforFindOnSite(Self);
  DownLoadThread.OnTerminate := OnAfterFindOnSite;
  DownLoadThread.OnSiteFindProgress := OnSiteFindProgress;
  DownLoadThread.Start;
end;

Class procedure TmsProxyFind.FindProxyInText(Text: String; OutPut: TStringList);
begin
  FindProxyInText(Text, OutPut);
end;

function TmsProxyFind.GetItems(Index: Integer): string;
begin
  Result := fProxy[Index];
end;

procedure TmsProxyFind.SetEnabled(const Value: boolean);
begin
  fEnabled := Value;
  if not Value then
    if Assigned(DownLoadThread) then
      DownLoadThread.Terminate;
end;

procedure TmsProxyFind.SetSaveProxy(const Value: boolean);
begin
  fSaveProxy := Value;
  if not fSaveProxy then
    fProxy.Clear;
end;

class procedure TmsProxyFind.SplitIpPort(IpPort: String; var Ip, Port: String);
begin
  Ip := Copy(IpPort, 1, pos(':', IpPort) - 1);
  Port := Copy(IpPort, pos(':', IpPort) + 1, Length(IpPort));
end;

Class procedure TmsProxyFind.FindProxyInText(Text: String; OutPut: DynString);
begin
  OutPut := FindProxyInText(Text);
end;

Class function TmsProxyFind.FindProxyInText(Text: String): DynString;
var
  MyClass: TmsProxyFind;
begin
  MyClass := TmsProxyFind.Create;
  try
    Result := MyClass.Core(Text);
  finally
    MyClass.Free;
  end;
end;

{ TDownLoadThread }

constructor TDownLoadThread.Create(Var AmsProxyFind: TmsProxyFind);
begin
  inherited Create(true);
  msProxyFind := AmsProxyFind;
  FreeOnTerminate := true;
  // Self.Start;
end;

procedure TDownLoadThread.Execute;
var
  x, z: Integer;
  Value: String;
  ProxyArray: DynString;
begin
  if Terminated then
    exit;
  for x := 0 to msProxyFind.fSites.Count - 1 do
  begin
    if Terminated then
      exit;
    Value := msProxyFind.fHTTP.Get(msProxyFind.fSites[x]);
    ProxyArray := msProxyFind.FindProxyInText(Value);;
    if Assigned(Self.OnFindProxy) then
      for z := Low(ProxyArray) to High(ProxyArray) do
        Synchronize(
          procedure
          begin
            Self.OnFindProxy(Self, ProxyArray[z]);
          end);
    if Assigned(OnSiteFindProgress) then
      Synchronize(
        procedure
        begin
          OnSiteFindProgress(Self, msProxyFind.fSites[x], x);
        end);
  end;
  msProxyFind.Free;
end;

end.
