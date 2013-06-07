unit httputil;

interface

uses Windows, SysUtils, Classes, StrUtils, Wininet;

function ExtractHeader(AHeaders: TStringList; AHeader: String): String;
function ParseURL(AURL: String; var ASchema, AHostname, AUsername,
	APassword, APath, AExtra: String): Boolean;
function ParseURLPath(AURL: String): String;
function ParseURLFilename(AURL: String): String;

implementation

function ParseURL(AURL: String; var ASchema, AHostname, AUsername,
	APassword, APath, AExtra: String): Boolean;
var
  aURLC: TURLComponents;
begin
  FillChar(aURLC, SizeOf(TURLComponents), 0);
  with aURLC do
  begin
    lpszScheme := nil;
    dwSchemeLength := INTERNET_MAX_SCHEME_LENGTH;
    lpszHostName := nil;
    dwHostNameLength := INTERNET_MAX_HOST_NAME_LENGTH;
    lpszUserName := nil;
    dwUserNameLength := INTERNET_MAX_USER_NAME_LENGTH;
    lpszPassword := nil;
    dwPasswordLength := INTERNET_MAX_PASSWORD_LENGTH;
    lpszUrlPath := nil;
    dwUrlPathLength := INTERNET_MAX_PATH_LENGTH;
    lpszExtraInfo := nil;
    dwExtraInfoLength := INTERNET_MAX_PATH_LENGTH;
    dwStructSize := SizeOf(aURLC);
  end;
  Result := InternetCrackUrl(PChar(AURL), Length(AURL), 0, aURLC);
  if Result then
  begin
    ASchema := aURLC.lpszScheme;
    AHostname := aURLC.lpszHostName;
    AUsername := aURLC.lpszUserName;
    APassword := aURLC.lpszPassword;
    APath := aURLC.lpszUrlPath;
    AExtra := aURLC.lpszExtraInfo;
  end;
end;

function ParseURLPath(AURL: String): String;
var
	s: String;
begin
	s := '';
	ParseURL(AURL, s, s, s, s, Result, s);
end;

function ParseURLFilename(AURL: String): String;
var
	s, Path, Frag: String;
begin
	s := '';
	ParseURL(AURL, s, s, s, s, Path, Frag);
  Delete(Path, Length(Path)-Length(Frag)+1, Length(Frag));
  Path := StringReplace(Path, '/', '\', [rfReplaceAll]);
  Result := ExtractFilename(Path);
end;


function ExtractHeader(AHeaders: TStringList; AHeader: String): String;
var i: integer;
begin
	Result := '';
  for i:= 0 to AHeaders.Count-1 do
  begin
    if (Pos(AHeader + ':', AHeaders.Strings[i]) = 1) then
    begin
    	Result := Trim(Copy(AHeaders[i], Length(AHeader + ': ')+1, Length(AHeaders[i])));
    end;
  end;
end;

end.
