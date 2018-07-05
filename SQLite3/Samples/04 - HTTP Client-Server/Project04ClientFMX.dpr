program Project04ClientFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitFMX in 'UnitFMX.pas' {Form1},
  SampleData in '..\01 - In Memory ORM\SampleData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
