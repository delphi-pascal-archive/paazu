program Paazu;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  PCSApi in 'PCSApi\PCSApi.pas',
  PathScan in 'PathScanner\PathScan.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
