unit common;

// YTuner: Common constants, variables and procedures.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IdStack, IdGlobal, StrUtils, crc,
{$IFDEF UNIX}
  dl,
{$ENDIF}
  SQLDBLib, SQLDB, SQLite3Conn, FileUtil,
  fphttpclient;

type
  TLogType = (ltNone, ltInfo, ltWarning, ltError, ltDebug);
  TResponseContentType = (ctNone, ctXML, ctPNG, ctJPG, ctGIF, ctTIFF, ctJSON);
  TCacheType = (catNone, catFile, catMemStr, catDB, catMemDB, catPermMemDB);

  TLocalHttpClient = class(TFPHTTPClient)
  private
    FMaxDataToReceive: integer;
    procedure HandleDataReceived(Sender : TObject; Const ContentLength, CurrentPos : Int64);
  public
    constructor Create(AMaxDataToReceive: integer = 0); overload;
  end;

const
  APP_NAME = 'YTuner';
  APP_VERSION = '1.2.6';
  APP_COPYRIGHT = 'Copyright (c) 2024 Greg P. (https://github.com/coffeegreg)';
  INI_VERSION = '1.2.2';

  YTUNER_USER_AGENT = 'YTuner';
  YTUNER_HOST = 'ytunerhost';

  LOG_TYPE_MSG : array[TLogType] of string = ('','Inf','Wrn','Err','Dbg');

  MSG_FILE_LOAD_ERROR = ' file load error';
  MSG_FILE_SAVE_ERROR = ' file save error';
  MSG_FILE_DELETE_ERROR = ' file delete error';
  MSG_FILE_CREATE_ERROR = ' file create error';
  MSG_LOADING = 'loading';
  MSG_REMOVED = 'removed';
  MSG_GETTING = 'getting';
  MSG_CHANGING = 'changing';
  MSG_ERROR = 'error';
  MSG_LOADED = 'loaded';
  MSG_SAVE = 'save';
  MSG_SAVED = 'saved';
  MSG_EMPTY = 'empty';
  MSG_CACHE = 'cache';
  MSG_FILE = 'file';
  MSG_DIRECTORY = 'directory';
  MSG_VERSION = 'version';
  MSG_OBSOLETE = 'obsolete';
  MSG_REQUIRED = 'required';
  MSG_STREAM = 'stream';
  MSG_OBJECTS = 'objects';
  MSG_BOOKMARK = 'bookmark';
  MSG_SUCCESSFULLY_LOADED = 'successfully loaded ';
  MSG_SUCCESSFULLY_SAVED = 'successfully saved ';
  MSG_SUCCESSFULLY_DOWNLOADED = 'successfully downloaded ';
  MSG_ERROR_LOAD = 'load error of';
  MSG_NOT_LOADED = 'not loaded';
  MSG_FOUND = 'found';
  MSG_NOT_FOUND = 'not found';
  MSG_STATIONS = 'stations';
  MSG_GOOGLE_TRANSLATE = 'Google Translate:';
  MSG_TRANSLATOR = 'Translator';

  MSG_RBUUID_CACHE_FILE = 'RB UUIDs cache file';

  MSG_INI_WARNING1 = 'You are running out of INI file! Some features may not work properly!';
  MSG_INI_WARNING2 = 'Your ytuner.ini file is outdated! Some features may not work properly!';
  MSG_INI_WARNING3 = 'YTuner will try to fill in the missing options, but check their description and usage on https://github.com/coffeegreg/YTuner/tree/master/cfg.';
  MSG_VTUNER_ERROR_LINK1 = 'Obslolete vTuner radio station link - trying redirect to ';
  MSG_VTUNER_ERROR_LINK2 = 'Obslolete vTuner radio station link can not be resolved - trying redirect to your first radio station of stations.ini/yaml file.';
  MSG_FIRST_STATION_NEEDED = 'Be sure you have at least one category and one (first one) radio station DIRECT link in stations.ini/yaml file and option "Enable=1" is placed in "[MyStations]" section of ytuner.ini file .';
  MSG_REDIRECT_SUPPORT_NEEDED = 'Your AVR must support HTTP redirects! See "RedirectHTTPCode" option.';

  INI_CONFIGURATION = 'Configuration';
  INI_INI_VERSION = 'INIVersion';
  INI_IP_ADDRESS = 'IPAddress';
  INI_ACT_AS_HOST = 'ActAsHost';
  INI_USE_SSL = 'UseSSL';
  INI_REDIRECT_HTTP_CODE = 'RedirectHTTPCode';
  INI_MESSAGE_INFO_LEVEL = 'MessageInfoLevel';
  INI_ICON_SIZE = 'IconSize';
  INI_ICON_CACHE = 'IconCache';
  INI_ICON_EXTENSION = 'IconEndPointExtension';
  INI_MY_TOKEN = 'MyToken';
  INI_COMMON_AVR_INI = 'CommonAVRini';
  INI_CACHE_FOLDER_LOCATION = 'CacheFolderLocation';
  INI_CONFIG_FOLDER_LOCATION = 'ConfigFolderLocation';
  INI_DB_FOLDER_LOCATION = 'DBFolderLocation';
  INI_DB_LIB_FILE = 'DBLibFile';
  INI_ENABLE = 'Enable';
  INI_MYSTATIONS = 'MyStations';
  INI_MY_STATIONS_FILE = 'MyStationsFile';
  INI_MY_STATIONS_AUTO_REFRESH_PERIOD = 'MyStationsAutoRefreshPeriod';
  INI_RADIOBROWSER = 'RadioBrowser';
  INI_RB_API_URL = 'RBAPIURL';
  INI_RB_POPULAR_AND_SEARCH_STATIONS_LIMIT = 'RBPopularAndSearchStationsLimit';
  INI_RB_MIN_STATIONS_PER_CATEGORY = 'RBMinStationsPerCategory';
  INI_RB_UUIDS_CACHE_TTL = 'RBUUIDsCacheTTL';
  INI_RB_UUIDS_CACHE_AUTO_REFRESH = 'RBUUIDsCacheAutoRefresh';
  INI_RB_CACHE_TYPE = 'RBCacheType';
  INI_RB_CACHE_TTL = 'RBCacheTTL';
  INI_BOOKMARK = 'Bookmark';
  INI_COMMON_BOOKMARK = 'CommonBookmark';
  INI_BOOKMARK_STATIONS_LIMIT = 'BookmarkStationsLimit';
  INI_WEBSERVER = 'WebServer';
  INI_WEBSERVER_IPADDRESS = 'WebServerIPAddress';
  INI_WEBSERVER_PORT = 'WebServerPort';
  INI_DNSSERVER = 'DNSServer';
  INI_DNSSERVER_IPADDRESS = 'DNSServerIPAddress';
  INI_DNSSERVER_PORT = 'DNSServerPort';
  INI_INTERCEPT_DNS = 'InterceptDNs';
  INI_DNSSERVERS = 'DNSServers';
  INI_MAINTENANCESERVER = 'MaintenanceServer';
  INI_MAINTENANCESERVER_IPADDRESS = 'MaintenanceServerIPAddress';
  INI_MAINTENANCESERVER_PORT = 'MaintenanceServerPort';

  HTTP_HEADER_ACCEPT = 'Accept';
  HTTP_HEADER_USER_AGENT = 'User-Agent';
  HTTP_HEADER_LOCATION = 'Location';
  HTTP_HEADER_SERVER = 'Server';
  HTTP_HEADER_ACCEPT_ENCODING = 'Accept-Encoding';
  HTTP_HEADER_ACCEPT_LANGUAGE = 'Accept-Language';
  HTTP_HEADER_CACHE_CONTROL = 'Cache-Control';
  HTTP_HEADER_CONNECTION = 'Connection';
  HTTP_HEADER_PRAGMA = 'Pragma';
  HTTP_HEADER_UPGRADE_INSECURE_REQUESTS = 'Upgrade-Insecure-Requests';

  WEBBROWSER_HTTP_HEADER_USER_AGENT = 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:109.0) Gecko/20100101 Firefox/115.0';
  WEBBROWSER_HTTP_HEADER_ACCEPT ='text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9';
  WEBBROWSER_HTTP_HEADER_ACCEPT_ENCODING = 'gzip, deflate';
  WEBBROWSER_HTTP_HEADER_ACCEPT_LANGUAGE = 'en-US,en;q=0.9';
  WEBBROWSER_HTTP_HEADER_CACHE_CONTROL = 'no-cache';
  WEBBROWSER_HTTP_HEADER_CONNECTION = 'keep-alive';
  WEBBROWSER_HTTP_HEADER_PRAGMA = 'no-cache';

  HTTP_RESPONSE_CONTENT_TYPE : array[TResponseContentType] of string = ('text/html; charset=utf-8','application/xml','image/png','image/jpeg','image/gif','image/tiff','application/json');

  MY_STATIONS_PREFIX = 'MS';
  RADIOBROWSER_PREFIX = 'RB';
  UNKNOWN_PREFIX = 'UN';
  PATH_MY_STATIONS = 'mystations';

  PATH_PARAM_ID = 'id';
  PATH_PARAM_MAC = 'mac';
  PATH_PARAM_FAV = 'fav';
  PATH_PARAM_SEARCH = 'search';
  PATH_PARAM_SSEARCH_TYPE = 'sSearchtype';
  PATH_PARAM_TOKEN = 'token';

  HTTP_CODE_OK = 200;
  HTTP_CODE_REDIRECT = 302;
  HTTP_CODE_NOT_FOUND = 404;
  HTTP_CODE_UNAVAILABLE = 503;

  DEFAULT_STRING = 'default';
  ESC_CHARS : Array Of AnsiString = ('\t','\n','\r','\b','\f',';');

  PATH_CACHE = 'cache';
  PATH_CONFIG = 'config';
  PATH_DB = 'db';

  CACHE_EXT = '.cache';

  ICON_SIZE = 200;
  ICON_CACHE = True;
  COMMON_AVR_INI = True;
  RB_POPULAR_AND_SEARCH_STATIONS_LIMIT = 100;
  RB_MIN_STATIONS_PER_CATEGORY = 3;
  RB_UUIDS_CACHE_TTL = 0;
  RB_CACHE_TTL = 0;

  SQLITE_VER_X_MIN = 3;
  SQLITE_VER_Y_MIN = 33;
  SQLITE_VER_Z_MIN = 0;

var
  MyIPAddress: string = DEFAULT_STRING;
  URLHost: string = '';
  LogType: TLogType = ltError;
  MyAppPath: string;
  UseSSL: boolean = True;
  CachePath: string = DEFAULT_STRING;
  ConfigPath: string = DEFAULT_STRING;
  DBPath: string = DEFAULT_STRING;
  DBLibFile: string = DEFAULT_STRING;

{$IFDEF WINDOWS}
  DBLib: array[1..1] of string = ('sqlite3.dll');
{$ELSE}
  {$IFDEF DARWIN}
    DBLib: array[1..2] of string = ('libsqlite3.0.dylib','libsqlite3.dylib');
  {$ELSE}
    DBLib: array[1..2] of string = ('libsqlite3.so.0','libsqlite3.so');
  {$ENDIF}
{$ENDIF}

{$IFDEF WINDOWS}
  DBLibSearchPath: array[1..1] of string = ('');
{$ELSE}
  {$IFDEF DARWIN}
    DBLibSearchPath: array[1..3] of string = ('/opt/local/lib','/usr/lib','/usr/local/lib');
  {$ELSE}
    {$IFDEF SOLARIS}
      {$IFDEF CPU64}
        DBLibSearchPath: array[1..1] of string = ('/usr/lib/64');
      {$ELSE CPU32}
        DBLibSearchPath: array[1..1] of string = ('/usr/lib');
      {$ENDIF}
    {$ELSE}
      {$IFDEF BSD}
        DBLibSearchPath: array[1..2] of string = ('/usr/local/lib','/usr/lib');
      {$ELSE}
        {$IFDEF LINUX}
          {$IFDEF CPU64}
            DBLibSearchPath: array[1..4] of string = ('/usr/lib64','/usr/lib/x86_64-linux-gnu','/usr/lib/aarch64-linux-gnu','/usr/lib');
          {$ELSE CPU32}
            DBLibSearchPath: array[1..4] of string = ('/usr/lib','/usr/lib/i386-linux-gnu','/usr/lib/arm-linux-gnueabi','/usr/lib/arm-linux-gnueabihf');
          {$ENDIF}
        {$ELSE}
          DBLibSearchPath: array[1..3] of string = ('/usr/lib','/usr/local/lib','/opt/local/lib');
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

procedure Logging(ALogType: TLogType; ALogMessage: string);
function GetLocalIP(ADefaultIP: string): string;
function CalcFileCRC32(AFileName: string): Cardinal;
function RemoveEscChars(LInputStr: RawByteString): RawByteString;
function HaveCommonElements(AStr: string; AStrArray: array of string): boolean;
function ContainsIn(AStr: string; AStrArray: array of string): boolean;
function StripChars(AInputString: string; const AChars: array of AnsiString):string;
function URLEncode(const AStr: String): AnsiString;
function TryToFindSQLite3Lib(ALibFile: string): string;
function LoadSQLite3Lib: boolean;
function CheckSQLite3LibVer: boolean;
function GetMyAppPath: string;

implementation
uses radiobrowserdb;

constructor TLocalHttpClient.Create(AMaxDataToReceive: integer = 0);
begin
  inherited Create(nil);
  FMaxDataToReceive:=AMaxDataToReceive;
  if FMaxDataToReceive>0 then
    OnDataReceived := @HandleDataReceived;
end;

{$WARN 5024 OFF}
procedure TLocalHttpClient.HandleDataReceived(Sender : TObject; Const ContentLength, CurrentPos : Int64);
begin
  if (FMaxDataToReceive>0) and (CurrentPos>FMaxDataToReceive) then
    Self.Terminate;
end;
{$WARN 5024 ON}

procedure Logging(ALogType: TLogType; ALogMessage: string);
begin
  if ALogType<=LogType then
    begin
      ALogMessage[1]:=UpCase(ALogMessage[1]);
      Writeln(DateTimeToStr(Now)+' : '+LOG_TYPE_MSG[ALogType]+' : '+ALogMessage+'.');
    end;
end;

function GetLocalIP(ADefaultIP: string): string;
var
  IPList: TIdStackLocalAddressList;
  i: integer = 0;
  LIPLoopback: string = '';

  function IsIP_v4(AIP_v4: string): Boolean;
  var
    i: LongInt;

    function TryStrToByte(const s: String; out i: LongInt): Boolean;
    begin
      Result:=((TryStrToInt(s,i)) and (i>=0) and (i<=255));
    end;

  begin
    Result:=((Length(AIP_v4.Split(['.']))=4)
             and (TryStrToByte(AIP_v4.Split(['.'])[0],i))
             and (TryStrToByte(AIP_v4.Split(['.'])[1],i))
             and (TryStrToByte(AIP_v4.Split(['.'])[2],i))
             and (TryStrToByte(AIP_v4.Split(['.'])[3],i)));
  end;
begin
//  Due to the filtering out of loopback interface IP addresses in the GetLocalAddressList procedure of the Indy library, we add it manually at this moment.
//  For more information look at: https://github.com/IndySockets/Indy/issues/494
//  Be careful! As you can see below, the given loopback IP address is not verified with the list of available IP addresses.
  if IsIP_v4(ADefaultIP) and (ADefaultIP.Split(['.'])[0]='127') then
    Result:=ADefaultIP
  else
    begin
      Result:='';
      try
        IPList := TIdStackLocalAddressList.Create;
        try
          TIdStack.IncUsage;
          try
            GStack.GetLocalAddressList(IPList);
          finally
            TIdStack.DecUsage;
          end;

            if IPList.Count > 0 then
              begin
                while (i<=IPList.Count-1) and (Result<>ADefaultIP) do
                  begin
                    if IPList[i].IPVersion = Id_IPv4 then
                      begin
                        if IPList[i].IPAddress.Split(['.'])[0]='127' then
                          LIPLoopback:=IPList[i].IPAddress
                        else
                          if Result='' then
                            Result:=IPList[i].IPAddress;
                        if IPList[i].IPAddress = ADefaultIP then
                          Result:=ADefaultIP;
                      end;
                    i:=i+1;
                  end;
                if (Result='') and (LIPLoopback<>'') then
                  Result:=LIPLoopback;
              end
            else
              Logging(ltError, 'No entries on IP List');
        finally
          IPList.Free;
        end;
      except
        On E: Exception do
          Logging(ltError, 'IP error: ' + E.message);
      end;
    end;
end;

function CalcFileCRC32(AFileName: string): Cardinal;
var
  Buffer : TBytes;
begin
  Result:=CRC32(0,nil,0);
  with TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone) do
    try
      SetLength(Buffer, Size);
      Read(Buffer,Size);
      Result:=CRC32(Result,@Buffer[0],Size);
    finally
      Free;
    end;
end;

function RemoveEscChars(LInputStr: RawByteString): RawByteString;
var
  i: integer;
begin
  Result := LInputStr;
  for i:=Low(ESC_CHARS) to High(ESC_CHARS) do
    Result:=AnsiReplaceStr(LInputStr,ESC_CHARS[i],'');
end;

function HaveCommonElements(AStr: string; AStrArray: array of string): boolean;
var
  LStrEnum,LStrArrayEnum: string;
begin
  Result:=False;
  for LStrEnum in AStr.Split([',','/',';']) do
    for LStrArrayEnum in AStrArray do
      if IsWild(Trim(LStrEnum),LStrArrayEnum,True) then
      begin
        Result:=True;
        Exit;
      end;
end;

function ContainsIn(AStr: string; AStrArray: array of string): boolean;
var
  LStrArrayEnum: string;
begin
  Result:=False;
  for LStrArrayEnum in AStrArray do
    if ContainsText(AStr,LStrArrayEnum) then
      begin
        Result:=True;
        Exit;
      end;
end;

function StripChars(AInputString: string; const AChars: array of AnsiString):string;
begin
  Result:=DelSpace1(StringsReplace(AInputString,AChars,(DupeString(' ,',Length(AChars)-1)+' ').Split([',']),[rfReplaceAll]).Trim);
end;

function URLEncode(const AStr: String): AnsiString;
var
  LAnsiChar: AnsiChar;
begin
  Result:='';
  for LAnsiChar in AStr do
    begin
      if ((Ord(LAnsiChar)<65) or (Ord(LAnsiChar)>90))
         and ((Ord(LAnsiChar)<97) or (Ord(LAnsiChar)>122)) then
        Result:=Result+'%'+IntToHex(Ord(LAnsiChar),2)
      else
        Result:=Result+LAnsiChar;
    end;
end;

function TryToFindSQLite3Lib(ALibFile: string): string;
var
  LFound: boolean = False;
  LLibFile: string = '';
  LDBLibIdx, LDBLibSearchPathIdx: integer;
{$IFDEF UNIX}
  LLibHandle: Pointer;
  LPdlinfo: Pdl_info;
  LPtrLibFile: Pointer;
{$ENDIF}
begin
  ALibFile:=ALibFile.Trim;
  if (ALibFile<>DEFAULT_STRING) and (not ALibFile.IsEmpty) and (FileExists(ALibFile)) then
    Result:=ALibFile
  else
    begin
      Result:='';
      LFound:=False;
      LDBLibIdx:=1;
      while (LDBLibIdx<=Length(DBLib)) and (not LFound) do
        begin
          LDBLibSearchPathIdx:=1;
          while (LDBLibSearchPathIdx<=Length(DBLibSearchPath)) and (not LFound) do
            begin
              LLibFile:=IfThen(DBLibSearchPath[LDBLibSearchPathIdx]='',MyAppPath,DBLibSearchPath[LDBLibSearchPathIdx]);
              if not LLibFile.EndsWith(DirectorySeparator) then
                LLibFile:=LLibFile+DirectorySeparator;
              LLibFile:=LLibFile+DBLib[LDBLibIdx];
              LFound:=FileExists(LLibFile);
              LDBLibSearchPathIdx:=LDBLibSearchPathIdx+1;
            end;
          LDBLibIdx:=LDBLibIdx+1;
        end;
{$IFDEF UNIX}
      if not LFound then
        begin
          LDBLibIdx:=1;
          while (LDBLibIdx<=Length(DBLib)) and (not LFound) do
            begin
              LLibHandle:=dlopen(PAnsiChar(DBLib[LDBLibIdx]), RTLD_LAZY);
              if LLibHandle<>nil then
                begin
                  LPdlinfo:=LLibHandle;
                  LPtrLibFile:=LPdlinfo^.dli_fbase;
                  LLibFile:=String(LPtrLibFile);
{$IFDEF DARWIN}
                  if LLibFile='' then
                    LLibFile:=DBLib[LDBLibIdx];
{$ENDIF}
                  LPtrLibFile:=nil;
                  dlclose(LLibHandle);
                  LFound:=True;
                end;
              LDBLibIdx:=LDBLibIdx+1;
            end;
        end;
{$ENDIF}
      if LFound then
        Result:=LLibFile;
    end;
end;

function LoadSQLite3Lib: boolean;
begin
  Result:=False;
  with TSQLDBLibraryLoader.Create(nil) do
    try
      try
        ConnectionType:='SQLite3';
        LibraryName:=DBLibFile;
        Enabled:=True;
        LoadLibrary;
        Result:=Enabled;
      except
        on E: Exception do
          begin
            Logging(ltError, string.Join(' ',[MSG_RBDB_DB, MSG_RBDB_LIBRARY, MSG_RBDB_ERROR,' ('+E.Message+')']));
            Free;
          end;
      end;
    finally
      Free;
    end;
end;

function CheckSQLite3LibVer: boolean;
var
  LSQLite3Connection: TSQLite3Connection;
  LSQLTransaction: TSQLTransaction;
begin
  Result:=False;
  LSQLite3Connection:=TSQLite3Connection.Create(nil);
  with LSQLite3Connection do
    try
      LogEvents:=[];
      OpenFlags:=[sofReadOnly,sofMemory];
      DatabaseName:=RBDB_FIELD_VER;
      Connected:=True;
      with TSQLQuery.Create(nil) do
        try
          ReadOnly:=True;
          DataBase:=LSQLite3Connection;
          LSQLTransaction:=TSQLTransaction.Create(nil);
          LSQLTransaction.Options:=[stoUseImplicit];
          LSQLTransaction.DataBase:=LSQLite3Connection;
          Transaction:=LSQLTransaction;
          SQL.Text:=DBRBGetResourceSQLScript(RBDB_SQL_VERSION);
          try
            Open;
            if not EOF then
              begin
                Logging(ltDebug, string.Join(' ',[MSG_RBDB_DB,MSG_RBDB_LIBRARY,':',FieldByName(RBDB_FIELD_VER).AsString]));
                Result:=(FieldByName(RBDB_FIELD_VER).AsString.Split(['.'])[0].ToInteger*1000000+
                         FieldByName(RBDB_FIELD_VER).AsString.Split(['.'])[1].ToInteger*1000+
                         FieldByName(RBDB_FIELD_VER).AsString.Split(['.'])[2].ToInteger >= SQLITE_VER_X_MIN*1000000+SQLITE_VER_Y_MIN*1000+SQLITE_VER_Z_MIN);
                if not Result then
                  Logging(ltError, string.Join(' ',[MSG_OBSOLETE,MSG_RBDB_DB,MSG_RBDB_LIBRARY,MSG_VERSION,':',FieldByName(RBDB_FIELD_VER).AsString,MSG_REQUIRED,'>=',SQLITE_VER_X_MIN.ToString+'.'+SQLITE_VER_Y_MIN.ToString+'.'+SQLITE_VER_Z_MIN.ToString]));
              end;
          except
            on E: Exception do
              begin
                Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_DB,MSG_RBDB_LIBRARY,MSG_RBDB_CHECKING,MSG_RBDB_ERROR,' ('+E.Message+')']));
                Exit;
              end;
          end;
        finally
          Close;
          Free;
          LSQLTransaction.Free;
        end;
    finally
      LSQLite3Connection.Connected:=False;
      LSQLite3Connection.Free;
    end;
end;

function GetMyAppPath: string;
begin
{$IFDEF UNIX}
// At this moment I have no better idea how to detect Alpine Linux Busybox ?
// ! In this case, enter the YTuner directory first and then run it with ./ytuner !
  if ParamStr(0).Contains('ld-musl-') then
    Result:='./'
  else
{$ENDIF}
    Result:=ProgramDirectory;
end;

end.


