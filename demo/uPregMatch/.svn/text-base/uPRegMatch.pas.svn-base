{

uPregMatch 是一个TPerlRegEx的正则表达式控件的封装，让Delphi用正则时更加精简。

使用例子：
uses
  SysUtils,
  uPRegMatch in 'uPRegMatch.pas';

const
	SRegEx = '^https?\:\/\/.+?\.sdo\.com\/.+\bticket\=(?<ticket>[\w\-]+)';
  SSDOTicket = 'http://test.123.sdo.com/login_check.php?blank=1&ticket=ST-400c3e98-3d01-4077-9b92-a6d3e542a091&g=3';

var
	matches: IPRegMatchResult;
begin
  try

    // Return true
    assert(preg_match(SRegEx, SSDOTicket));

    // Indexed Matches, matches
    assert(preg_match(SRegEx, SSDOTicket, matches));
    assert(matches[1]='ST-400c3e98-3d01-4077-9b92-a6d3e542a091');
      
		assert(matches['ticket']='ST-400c3e98-3d01-4077-9b92-a6d3e542a091');

  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  readln;
end.



WilliiamYZYang 杨延喆


Changelog:

2011.06.02
支持最新版本PerlRegEx，部分属性名更变


2011.02.01
第一个版本

}
unit uPRegMatch;

interface

uses SysUtils, Classes, PerlRegEx;

type

	IPRegMatchResult = interface

    function GetMatches(AName: Variant): String;
    function GetCount: Integer;
    function GetRegExpr: String;
    function GetSubject: String;
    function GetMatched: Boolean;
    function MatchAgain: Boolean;

    
  	property RegExpr: String read GetRegExpr;
    property Subject: String read GetSubject;
    property Matched: Boolean read GetMatched;
    property Count: Integer read GetCount;
    property Matches[AName: Variant]: String read GetMatches; default;

  end;

  TPRegMatchResult = class(TInterfacedObject, IPRegMatchResult)
  private
  	FRegEx: TPerlRegEx;
    //FRegExpr: String;
    //FCount: Integer;
    //FSubject: String;
    //FMatched: Boolean;
    function GetMatches(AName: Variant): String;
    function GetCount: Integer;
    function GetRegExpr: String;
    function GetSubject: String;
    function GetMatched: Boolean;
  public
  	constructor Create(const ARegExpr: String;
		  const ASubject:String;
		  const AFlags: TPerlRegExOptions = [preCaseLess, preSingleLine, preExtended]);
    destructor Destroy; override;

    function MatchAgain: Boolean;

  	property RegExpr: String read GetRegExpr;
    property Subject: String read GetSubject;
    property Matched: Boolean read GetMatched;
    property Count: Integer read GetCount;
    property Matches[AName: Variant]: String read GetMatches; default;
  end;

function preg_match(
	const ARegExpr: String;
  const ASubject:String;
  const AFlags: TPerlRegExOptions = [ preCaseLess, preSingleLine, preExtended]): Boolean; overload;

function preg_match(
	const ARegExpr: String;
  const ASubject:String;
  out APRegResult: IPRegMatchResult;
  const AFlags: TPerlRegExOptions = [preCaseLess, preSingleLine, preExtended]): Boolean; overload;


function preg_quote(ASubject: String): String;

implementation

uses Variants, TypInfo;

function preg_quote(ASubject: String): String;
begin
  Result := TPerlRegEx.EscapeRegExChars(ASubject);
end;

function preg_match(
	const ARegExpr: String;
  const ASubject:String;
  const AFlags: TPerlRegExOptions): Boolean; overload;
begin
  with TPRegMatchResult.Create(ARegExpr, ASubject, AFlags) do
  begin
    try
	    Result := Matched;    
    finally
    	Free;
    end;
  end;
end;

function preg_match(
	const ARegExpr: String;
  const ASubject:String;
  out APRegResult: IPRegMatchResult;
  const AFlags: TPerlRegExOptions): Boolean; overload;
begin
	APRegResult := TPRegMatchResult.Create(ARegExpr, ASubject, AFlags);
  Result := APRegResult.Matched;
end;

  
{ TPRegMatchResult }

constructor TPRegMatchResult.Create(const ARegExpr, ASubject: String;
  const AFlags: TPerlRegExOptions);
begin
	inherited Create;
  FRegEx := TPerlRegEx.Create;
  FRegEx.RegEx := ARegExpr;
  FRegEx.Subject := ASubject;
  FRegEx.Options := AFlags;
end;

destructor TPRegMatchResult.Destroy;
begin

	FreeAndNil( FRegEx );
  
  inherited;
end;

function TPRegMatchResult.GetCount: Integer;
begin
  Result := FRegEx.GroupCount;
end;

function TPRegMatchResult.GetMatched: Boolean;
begin
  Result := FRegEx.Match;
end;

function TPRegMatchResult.GetMatches(AName: Variant): String;
begin
  if VarIsOrdinal(AName) then
  	  Result := FRegEx.Groups[Integer(AName)]
  else if VarIsStr(AName) then
		  Result := FRegEx.Groups[FRegEx.NamedGroup(String(AName))]
  ;
end;


function TPRegMatchResult.GetRegExpr: String;
begin
	Result := FRegEx.RegEx;
end;

function TPRegMatchResult.GetSubject: String;
begin
	Result := FRegEx.Subject;
end;

function TPRegMatchResult.MatchAgain: Boolean;
begin
	Result := FRegEx.MatchAgain;
end;

end.
