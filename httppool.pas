{

http://code.google.com/p/delphi-httppool/
HTTPPool - Multithreaded HTTP request management library written with
	OmniThreadLib and Synapse HTTPSend components.
  JSON support using SuperObject.
  
Author: WILLIAM YZ YANG
Email: flyingdragontooth at gmail.com
Date: 2011.06.02

Features
* Does http requests asynchronously
* Handles all completed requests in your main thread (or which ever)
* supports common usage through: ContentAsString, ContentAsJSON and ContentAsImage
* Counters on queued, completed requests.
* Allows http request to be queued with custom tag object

Todo:
* Support requests per second limit

References
  OmniThreadLib Homepage:  http://code.google.com/p/omnithreadlibrary/
  Synapse Homepage: http://synapse.ararat.cz/doku.php/start
  SuperObject: http://www.progdigy.com/?page_id=6

}
unit httppool;

interface

uses Classes, SysUtils, OtlComm, OtlTask, OtlTaskControl, OtlEventMonitor,
  HTTPSend, Contnrs, Graphics, ThreadObjectList, SuperObject
  ;

const
  HTTPWORKER_JOB = 100;
  HTTPWORKER_JOB_BEFOREREQUEST = 101;
  HTTPWORKER_JOB_AFTERREQUEST = 102;
  HTTPWORKER_JOB_SUCCESS = 103;
  HTTPWORKER_JOB_ERROR = 104;

  MAX_HTTPWORKERS = 4;

type
  THTTPJob = class;
  THTTPJobEvent = procedure(ASender: TObject; AJob: THTTPJob) of object;
  THTTPJobRequestEvent = procedure(ASender: TObject; AJob: THTTPJob) of object;

  THTTPJob = class(TObject)
  private
    FHeaders: TStringList;
    FMethod: String;
    FCookies: TStringList;
    FURL: String;
    FOnError: THTTPJobRequestEvent;
    FOnSuccess: THTTPJobRequestEvent;
    FOnBeforeRequest: THTTPJobRequestEvent;
    FData: TStream;
    FContentType: String;
    FTagObject: TObject;
    FTag: Integer;
    FResultCode: Integer;
    
    procedure SetCookies(const Value: TStringList);
    procedure SetHeaders(const Value: TStringList);
    procedure SetMethod(const Value: String);
    procedure SetOnBeforeRequest(const Value: THTTPJobRequestEvent);
    procedure SetOnError(const Value: THTTPJobRequestEvent);
    procedure SetOnSuccess(const Value: THTTPJobRequestEvent);
    procedure SetURL(const Value: String);

    procedure DoSuccess(ASender: TObject); virtual;
    procedure DoError(ASender: TObject); virtual;
    procedure DoBeforeRequest(ASender: TObject); virtual;
    procedure SetData(const Value: TStream);
    procedure SetContentType(const Value: String);
    function getDataLength: Cardinal;
    function getDataPtr: Pointer;
    procedure SetTag(const Value: Integer);
    procedure SetTagObject(const Value: TObject);
    procedure SetResultCode(const Value: Integer);

  public
    constructor Create(AURL: String;
      const AMethod: String = 'GET';
      const AHeaders: TStringList = nil;
      const ACookies: TStringList = nil);

    destructor Destroy; override;

    {$IFDEF HTTPPOOL_IMAGESUPPORT}
    function ContentAsImage: TGraphic;
    {$ENDIF}
    function ContentAsString: String;
    function ContentAsJSON: ISuperObject;

    property URL: String read FURL write SetURL;
    property Method: String read FMethod write SetMethod;
    property Headers: TStringList read FHeaders write SetHeaders;
    property ResultCode: Integer read FResultCode write SetResultCode;
    property Cookies: TStringList read FCookies write SetCookies;
    property OnSuccess: THTTPJobRequestEvent read FOnSuccess write SetOnSuccess;
    property OnError: THTTPJobRequestEvent read FOnError write SetOnError;
    property OnBeforeRequest: THTTPJobRequestEvent read FOnBeforeRequest write SetOnBeforeRequest;
    property Data: TStream read FData write SetData;
    property DataPtr: Pointer read getDataPtr;
    property DataLength: Cardinal read getDataLength;
    property ContentType: String read FContentType write SetContentType;
    property TagObject: TObject read FTagObject write SetTagObject;
    property Tag: Integer read FTag write SetTag;
  end;

  THTTPJobQueue = class(TObjectQueue)
  public
    function Push(AObject: THTTPJob): THTTPJob;
    function Pop: THTTPJob;
    function Peek: THTTPJob;
  end;


  THTTPWorker = class(TOmniWorker)
  private
    FHTTPSend: THTTPSend;
    FActiveJob: THTTPJob;
    FName: String;
    procedure SetActiveJob(const Value: THTTPJob);
    procedure SetName(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Work(const ATask: IOmniTask);
    property HTTPSend: THTTPSend read FHTTPSend;
    property ActiveJob: THTTPJob read FActiveJob write SetActiveJob;
    property Name: String read FName write SetName;
  end;

  THTTPWorkerCount = 1..MAX_HTTPWORKERS;

  THTTPWorkPool = class(TObject)
  private
    fJobList: TThreadObjectList;
    fQueue: THTTPJobQueue;
    FWorkerCount: THTTPWorkerCount;
    fHTTPWorkMon: TOmniEventMonitor;
    FSelfTask: IOmniTaskControl;
    FOnSuccess: THTTPJobRequestEvent;
    FOnBeforeRequest: THTTPJobRequestEvent;
    FOnError: THTTPJobRequestEvent;
    FRequestsPerSecond: Cardinal;
  	fCompletedRequests: Integer;
    procedure SetWorkerCount(const Value: THTTPWorkerCount);
    procedure DoWorkTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure SetOnBeforeRequest(const Value: THTTPJobRequestEvent);
    procedure SetOnError(const Value: THTTPJobRequestEvent);
    procedure SetOnSuccess(const Value: THTTPJobRequestEvent);
    procedure SetRequestsPerSecond(const Value: Cardinal);
    
  protected
    procedure DoBeforeRequest(AJob: THTTPJob); virtual;
    procedure DoSuccess(AJob: THTTPJob); virtual;
    procedure DoError(AJob: THTTPJob); virtual;
    function getCompletedRequests: Cardinal; virtual;
    function getQueuedRequests: Cardinal; virtual;


  public
    constructor Create;
    destructor Destroy; override;

    function Job(AURL: String;
      const AMethod: String = 'GET';
      const AParam: String = '';
      const AHeaders: TStringList = nil;
      const ACookies: TStringList = nil): THTTPJob;

    function JobTagged(AURL: String;
      const AMethod: String = 'GET';
      const AParam: String = '';
      const ATagObject: TObject = nil;
      const ATag: Integer = 0;
      const AHeaders: TStringList = nil;
      const ACookies: TStringList = nil): THTTPJob;


    procedure Pooling(const ATask: IOmniTask);
    property WorkerCount: THTTPWorkerCount read FWorkerCount write SetWorkerCount;

    property OnSuccess: THTTPJobRequestEvent read FOnSuccess write SetOnSuccess;
    property OnError: THTTPJobRequestEvent read FOnError write SetOnError;
    property OnBeforeRequest: THTTPJobRequestEvent read FOnBeforeRequest write SetOnBeforeRequest;

    property RequestsPerSecond: Cardinal read FRequestsPerSecond write SetRequestsPerSecond;
    property CompletedRequests: Cardinal read getCompletedRequests;
    property QueuedRequests: Cardinal read getQueuedRequests;

  end;




implementation

uses Windows, HTTPUtil
{$IFDEF USE_CODESITE}
,CodeSiteLogging
{$ENDIF}
{$IFDEF HTTPPOOL_IMAGESUPPORT}
, GifImg, PngImage, Jpeg
{$ENDIF}
;
{ THTTPWorker }

constructor THTTPWorker.Create;
begin
  inherited Create;
  fHTTPSend := THTTPSend.Create;
  fActiveJob := nil;
end;

destructor THTTPWorker.Destroy;
begin
  fHTTPSend.Free;
end;

procedure THTTPWorker.SetActiveJob(const Value: THTTPJob);
begin
  FActiveJob := Value;
end;

procedure THTTPWorker.SetName(const Value: String);
begin
  FName := Value;
end;

procedure THTTPWorker.Work(const ATask: IOmniTask);

  procedure ProcessJob(AJob: THTTPJob);
  begin

  	{$IFDEF USE_CODESITE}
    CodeSite.Send(FName + ': Start to download ' + AJob.URL);
    {$ENDIF}

    FHTTPSend.Clear;
    //ATask.Comm.Send(HTTPWORKER_JOB_BEFOREQUEST, [Self, FHTTPSend]);
    FHTTPSend.Headers.Assign(AJob.Headers);
    FHTTPSend.Cookies.Assign(AJob.Cookies);
    if FHTTPSend.HTTPMethod(AJob.Method, AJob.URL) then
    begin

      {$IFDEF USE_CODESITE}
      CodeSite.Send(FName + ': Download Success ' + AJob.URL);
      {$ENDIF}
    
      AJob.ContentType := ExtractHeader(FHTTPSend.Headers, 'Content-Type');

      AJob.Data.CopyFrom(FHTTPSend.Document, FHTTPSend.Document.Size);
      AJob.Headers.Assign(FHTTPSend.Headers);
      AJob.Cookies.Assign(FHTTPSend.Cookies);
      AJob.ResultCode := FHTTPSend.ResultCode;

      {$IFDEF USE_CODESITE}
      CodeSite.Send(FName + ': Content-Type= ' + AJob.ContentType );
      CodeSite.Send(FName + ': Content-Length= ' + IntToStr(FHTTPSend.Document.Size));
      CodeSite.Send(FName + ': Content', Utf8Decode(AJob.ContentAsString));
      {$ENDIF}


      ATask.Comm.Send(HTTPWORKER_JOB_SUCCESS, AJob)
    end
    else
      ATask.Comm.Send(HTTPWORKER_JOB_ERROR, AJob);


    //ATask.Comm.Send(HTTPWORKER_JOB_AFTERQUEST, [Self, FHTTPSend]);
  end;

var
  msg: TOmniMessage;
begin
//	ActiveJob := nil;
  while not ATask.Terminated do
  begin
    if ATask.Comm.ReceiveWait(msg, 100) then
    begin
      if msg.MsgID = HTTPWORKER_JOB then
      begin
        try
          ProcessJob(ActiveJob);
        finally
          ActiveJob := nil;
        end;
      end;
    end;
    Sleep(100);
  end;
end;

{ THTTPJob }
{$IFDEF HTTPPOOL_IMAGESUPPORT}
function THTTPJob.ContentAsImage: TGraphic;
var
	fname: String;
begin
	Result := nil;
  if ContentType = 'image/gif' then
  	Result := TGifImage.Create
  else if ContentType = 'image/jpeg' then
  	Result := TJpegImage.Create
  else if ContentType = 'image/png' then
  	Result := TPngObject.Create;

  if Result <> nil then
	begin
    TMemoryStream(Data).SaveToFile('tmp');
    Data.Seek(0, 0);
  	Result.LoadFromStream(Data);
  end;
end;
{$ENDIF}
function THTTPJob.ContentAsJSON: ISuperObject;
begin
  Result := SO(UTF8Decode(ContentAsString));
end;

function THTTPJob.ContentAsString: String;
begin
	with TStringStream.Create('') do
  begin
  	try
    	Data.Seek(0, 0);
		  CopyFrom(Data, Data.Size);
      Result := DataString;
    finally
	    Free;
    end;
  end;
end;

constructor THTTPJob.Create(AURL: String; const AMethod: String; const AHeaders,
  ACookies: TStringList);
begin
  inherited Create;
  fURL := AURL;
  fMethod := AMethod;
  
  FHeaders := TStringList.Create;
  if AHeaders <> nil then
	  FHeaders.Assign(AHeaders);

  FCookies := TStringList.Create;
  if ACookies <> nil then
	  FCookies.Assign(ACookies);

  FData := TMemoryStream.Create;
end;

destructor THTTPJob.Destroy;
begin
	FData.Free;
  FHeaders.Free;
  FCookies.Free;
  inherited;
end;

procedure THTTPJob.DoBeforeRequest(ASender: TObject);
begin
  if Assigned(FOnBeforeRequest) then
    FOnBeforeRequest(ASender, Self);
end;

procedure THTTPJob.DoError(ASender: TObject);
begin
  if Assigned(FOnError) then
    FOnError(ASender, Self);
end;

procedure THTTPJob.DoSuccess(ASender: TObject);
begin
  if Assigned(FOnSuccess) then
    FOnSuccess(ASender, Self);
end;

function THTTPJob.getDataLength: Cardinal;
begin
	Result := fData.Size;
end;

function THTTPJob.getDataPtr: Pointer;
begin
  Result := TMemoryStream(fData).Memory;
end;

procedure THTTPJob.SetContentType(const Value: String);
begin
  FContentType := Value;
end;

procedure THTTPJob.SetCookies(const Value: TStringList);
begin
  FCookies.Assign(Value);
end;

procedure THTTPJob.SetData(const Value: TStream);
begin
  FData := Value;
end;

procedure THTTPJob.SetHeaders(const Value: TStringList);
begin
  FHeaders.Assign( Value );
end;

procedure THTTPJob.SetMethod(const Value: String);
begin
  FMethod := Value;
end;

procedure THTTPJob.SetOnBeforeRequest(const Value: THTTPJobRequestEvent);
begin
  FOnBeforeRequest := Value;
end;

procedure THTTPJob.SetOnError(const Value: THTTPJobRequestEvent);
begin
  FOnError := Value;
end;

procedure THTTPJob.SetOnSuccess(const Value: THTTPJobRequestEvent);
begin
  FOnSuccess := Value;
end;

procedure THTTPJob.SetResultCode(const Value: Integer);
begin
  FResultCode := Value;
end;

procedure THTTPJob.SetTag(const Value: Integer);
begin
  FTag := Value;
end;

procedure THTTPJob.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

procedure THTTPJob.SetURL(const Value: String);
begin
  FURL := Value;
end;

{ THTTPJobQueue }

function THTTPJobQueue.Peek: THTTPJob;
begin
  Result := THTTPJob(inherited Peek);
end;

function THTTPJobQueue.Pop: THTTPJob;
begin
  Result := THTTPJob(inherited Pop);
end;

function THTTPJobQueue.Push(AObject: THTTPJob): THTTPJob;
begin
  Result := THTTPJob(inherited Push(AObject));
end;

{ THTTPWorkPool }

constructor THTTPWorkPool.Create;
begin
  inherited Create;
  fJobList := TThreadObjectList.Create;
  fQueue := THTTPJobQueue.Create;
  fJobList.OwnsObjects := True;
  fHTTPWorkMon := TOmniEventMonitor.Create(nil);
  fHTTPWorkMon.OnTaskMessage := DoWorkTaskMessage;
  FRequestsPerSecond := 0;
  FSelfTask := CreateTask(Self.Pooling).Run();
end;

destructor THTTPWorkPool.Destroy;
begin
  fHTTPWorkMon.Free;
  fJobList.Free;
  fQueue.Free;
  inherited;
end;

procedure THTTPWorkPool.DoBeforeRequest(AJob: THTTPJob);
begin
	AJob.DoBeforeRequest(Self);
  if Assigned(FOnBeforeRequest) then FOnBeforeRequest(Self, AJob);
end;

procedure THTTPWorkPool.DoError(AJob: THTTPJob);
begin
	AJOb.DoError(Self);
  if Assigned(FOnError) then FOnError(Self, AJob);  
end;

procedure THTTPWorkPool.DoSuccess(AJob: THTTPJob);
begin
	Inc(fCompletedRequests);
	AJOb.DoSuccess(Self);
  if Assigned(FOnSuccess) then FOnSuccess(Self, AJob);
end;

procedure THTTPWorkPool.DoWorkTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
var
//  msg: TOmniMessage;
  AJob: THTTPJob;
begin

    AJob := THTTPJob(Msg.MsgData.AsObject);

    case msg.MsgID of
    HTTPWORKER_JOB_BEFOREREQUEST:
      DoBeforeRequest(AJob);
//    HTTPWORKER_JOB_AFTERREQUEST:
//      AJob.DoAfterRequest(Self, AHTTPSend);
    HTTPWORKER_JOB_SUCCESS:
      DoSuccess(AJob);
    HTTPWORKER_JOB_ERROR:
      DoError(AJob);
    end;

end;

function THTTPWorkPool.getCompletedRequests: Cardinal;
begin
	Result := fCompletedRequests;
end;

function THTTPWorkPool.getQueuedRequests: Cardinal;
begin
  Result := fQueue.Count;
end;

function THTTPWorkPool.Job(AURL: String;
      const AMethod: String = 'GET';
      const AParam: String = '';
      const AHeaders: TStringList = nil;
      const ACookies: TStringList = nil): THTTPJob;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Job' );{$ENDIF}
  fJobList.LockList;
  try
    Result := THTTPJob.Create(AURL, AMethod, AHeaders, ACookies);
    fQueue.Push(Result);
    fJobList.Add(Result);
  finally
    fJobList.UnlockList;
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Job' );{$ENDIF}
end;

function THTTPWorkPool.JobTagged(AURL: String; const AMethod, AParam: String;
  const ATagObject: TObject; const ATag: Integer; const AHeaders,
  ACookies: TStringList): THTTPJob;
var
	URL: String;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'JobTagged' );{$ENDIF}
  fJobList.LockList;
  try
  	if Length(AParam) = 0 then URL := AURL else URL := AURL + '?' + AParam;

    Result := THTTPJob.Create(URL, AMethod, AHeaders, ACookies);
    Result.TagObject := ATagObject;
    Result.Tag := ATag;
    fQueue.Push(Result);
    fJobList.Add(Result);
  finally
    fJobList.UnlockList;
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'JobTagged' );{$ENDIF}
end;

procedure THTTPWorkPool.Pooling(const ATask: IOmniTask);

var
  fWorkers: array[1..MAX_HTTPWORKERS] of THTTPWorker;
  fTaskCtrls: array[1..MAX_HTTPWORKERS] of IOmniTaskControl;

  function GetWorkerTask(AIndex: THTTPWorkerCount): IOmniTaskControl;
  begin
    if not Assigned(fTaskCtrls[AIndex]) then
    begin

      fTaskCtrls[AIndex] := CreateTask(fWorkers[AIndex].Work);
      fTaskCtrls[AIndex].MonitorWith(fHTTPWorkMon);
//      fTaskCtrls[AIndex].OnMessage(DoWorkTaskMessage);
        fTaskCtrls[AIndex].Run;
    end;
    Result := fTaskCtrls[AIndex];
  end;

  function GiveJobToNextWorker(AJob: THTTPJob): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for I := 1 to MAX_HTTPWORKERS do
    begin
      if FWorkers[i].ActiveJob = nil then
      begin
      	FWorkers[i].ActiveJob := AJob;
        GetWorkerTask(i).Comm.Send(HTTPWORKER_JOB);
        Result := True;
        Break;
      end;
    end;
  end;


var
  fCurrMPS: Integer;
  fStart, fDelay: Int64;
  Job: THTTPJob;
  i: Integer;
begin

  for I := 1 to MAX_HTTPWORKERS do
  begin
    fWorkers[i] := THTTPWorker.Create;
    fWorkers[i].Name := 'Worker #' + IntToStr(i);
  end;

  try

    while not ATask.Terminated do
    begin

      fStart := GetTickCount;
      fCurrMPS := 0;

      // 队列中至少有一个 或
    	while fQueue.AtLeast(1)do
      begin

        // 需要检测 最大HTTP请求频率
        if (FRequestsPerSecond > 0) and
        	(GetTickCount-fStart < 1000) and
          (fCurrMPS < fRequestsPerSecond) then
        begin

          {$IFDEF USE_CODESITE}
          CodeSite.Send('Controlling RPS: Current RPS = %d, RequestsPerSecond = %d',
          	[fCurrMPS, fRequestsPerSecond]);
          {$ENDIF}
        	Break;

        end;

        Job := fQueue.Pop;
        while not GiveJobToNextWorker(Job) do
        begin
        	Sleep(0);
        	// Next
//          ControlRPS;
        end;
        Inc(fCurrMPS);
        Sleep(0);

      end;

      fDelay := (1000 - (GetTickCount-fStart));
      if (fDelay > 0) then
        Sleep(fDelay);

    end;
  finally

    for I := 1 to MAX_HTTPWORKERS do
    begin
      if fWorkers[i] <> nil then fWorkers[i].Free;
      
    end;
  end;
end;

procedure THTTPWorkPool.SetOnBeforeRequest(const Value: THTTPJobRequestEvent);
begin
  FOnBeforeRequest := Value;
end;

procedure THTTPWorkPool.SetOnError(const Value: THTTPJobRequestEvent);
begin
  FOnError := Value;
end;

procedure THTTPWorkPool.SetOnSuccess(const Value: THTTPJobRequestEvent);
begin
  FOnSuccess := Value;
end;

procedure THTTPWorkPool.SetRequestsPerSecond(const Value: Cardinal);
begin
  FRequestsPerSecond := Value;
end;

procedure THTTPWorkPool.SetWorkerCount(const Value: THTTPWorkerCount);
begin
  FWorkerCount := Value;
end;

end.


