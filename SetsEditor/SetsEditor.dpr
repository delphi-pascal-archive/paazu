program SetsEditor;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  PCSApi in '..\PCSApi\PCSApi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
