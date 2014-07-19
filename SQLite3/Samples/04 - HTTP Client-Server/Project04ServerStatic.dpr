{
   Synopse mORMot framework

   Sample 04 - HTTP Client-Server
     purpose of this sample is to show how to serve files in
     addition to RESTful Client/Server of a SQLite3 database

   This sample will serve as REST the data as defined in SampleData,
   and serve 'www' sub-folder content within localhost:8080/static

   See also http://synopse.info/forum/viewtopic.php?id=1896

   Version 1.18
  - added Project04ServerStatic.dpr program

}

program Project04ServerStatic;

uses
  Forms,
  Unit2Static in 'Unit2Static.pas' {Form1},
  SampleData in '..\01 - In Memory ORM\SampleData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
