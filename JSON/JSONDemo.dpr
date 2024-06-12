program JSONDemo;

uses
  Vcl.Forms,
  JSONDemo.Main in 'JSONDemo.Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
