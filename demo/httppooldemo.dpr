program httppooldemo;

uses
  Forms,
  httppooldemoform in 'httppooldemoform.pas' {Form2},
  httputil in '..\httputil.pas',
  httppool in '..\httppool.pas',
  uPRegMatch in 'uPregMatch\uPRegMatch.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
