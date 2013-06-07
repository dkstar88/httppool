{

This is a demo app for THTTPWorkPool
http://code.google.com/p/delphi-httppool/

It searches "Delphi" images on google image, and when the search completes
	it extracts all images and display them on TListView.


}
unit httppooldemoform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, httppool, ComCtrls, ImgList;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    ImageList1: TImageList;
    ListView1: TListView;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fWorkers: THTTPWorkPool;
    procedure JobOnSuccess(ASender: TObject; AJob: THTTPJob);
    procedure AddImageJob(AURL: String);
  public
    { Public declarations }
    procedure AddJob(AURL: String);
    procedure AddImage(AImage: TGraphic);
  end;

const
	search_url = 'http://www.google.com.hk/search?q=delphi&hl=en&newwindow=1&safe=active&sa=G&gbv=2&as_st=y&tbs=isz:m&biw=1440&bih=431&tbm=isch&ijn=ls&page=0&start=0';
  

var
  Form2: TForm2;

implementation

{$R *.dfm}
uses PerlRegEx, uPregMatch;

procedure TForm2.AddImage(AImage: TGraphic);
var
	bmp: TBitmap;
begin

	if AImage.Empty then Exit;


	bmp := Tbitmap.Create;
  try

  	with Listview1.Items.add do
    begin
      Caption := '';
      bmp.Assign(AImage);
      ImageIndex := ImageList1.Add(bmp, nil);
    end;

  finally
    bmp.Free;
  end;
end;

procedure TForm2.AddJob(AURL: String);
begin
  Memo1.Lines.Add('>> ' + AURL);
  fWorkers.Job(AURL);
end;


procedure TForm2.AddImageJob(AURL: String);
var
	URL: String;
begin
	if Pos('gstatic.com', AURL) > 0 then
  	URL := AURL
	else
  	URL := 'http://t0.gstatic.com' + AURL;
    
  AddJob(URL);
end;


procedure TForm2.Button1Click(Sender: TObject);
begin
	Button1.Caption := 'Searching ...';
	Button1.Enabled := False;
 	AddJob(search_url);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
	fWorkers := THTTPWorkPool.Create;
  fWorkers.OnSuccess := JobOnSuccess;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
	fWorkers.Free;
end;

procedure TForm2.JobOnSuccess(ASender: TObject; AJob: THTTPJob);
const
	S_EXTRACT_URLS = 'src="((/[-A-Z0-9+&@#/%=~_|!:,.;]*)?(\?[A-Z0-9+&@#-/%=~_|!:,.;]*)?)';
var
	RegExRes: IPRegMatchResult;
  Str: String;
  img: TGraphic;
begin

	// Our search has returned
	if AJob.URL = search_url then
  begin

    Button1.Caption := 'Searching';
    Button1.Enabled := True;
    // Try extract all images from the html
    Str := AJob.ContentAsString;
    // Memo1.Lines.Text := Str;
    if preg_match(S_EXTRACT_URLS, Str, RegExRes, [preCaseless, preExtended]) then
    begin
//    	Memo1.Lines.Add(RegExRes.Matches[0]);
    	AddImageJob(RegExRes.Matches[1]);
      while RegExRes.MatchAgain do
	    	AddImageJob(RegExRes.Matches[1]);
    end;

    if preg_match('src="(http://([-A-Z0-9.]+)(/[-A-Z0-9+&@#/%=~_|!:,.;]*)?(\?[A-Z0-9+&@#-/%=~_|!:,.;]*)?)', 
    	Str, RegExRes, [preCaseless, preExtended]) then
    begin
//    	Memo1.Lines.Add(RegExRes.Matches[0]);
    	AddImageJob(RegExRes.Matches[1]);
      while RegExRes.MatchAgain do
	    	AddImageJob(RegExRes.Matches[1]);
    end;
    
  end else
  begin
    // These must be our images
    try
    	img := AJob.ContentAsImage;
    	AddImage(img);
      img.Free;
    except
      if img <> nil then img.Free;
      
    end;

  end;

  Memo1.Lines.Add('<< ' + AJob.URL);

  // Note that JOB is always freed after this call by THTTPWorkPool
end;

end.
