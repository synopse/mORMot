program RESTclient;

{$APPTYPE CONSOLE}

uses
  SynCommons,
  mORMot,
  mORMotHttpClient,
  RESTModel;

var aModel: TSQLModel;
    aClient: TSQLHttpClient;
    aPerson: TPerson;
    aID: integer;
begin
  aModel := DataModel;
  try
    aClient := TSQLHttpClientWinHTTP.Create('localhost',SERVER_PORT,aModel);
    try
      writeln('Add a new TPerson');
      aPerson := TPerson.Create;
      try
        Randomize;
        aPerson.Name := 'Name'+Int32ToUtf8(Random(10000));
        aID := aClient.Add(aPerson,true);
      finally
        aPerson.Free;
      end;
      aPerson := TPerson.Create(aClient,aID);
      try
        writeln('Name read from DB = ',aPerson.Name);
      finally
        aPerson.Free;
      end;
    finally
      aClient.Free;
    end;
    readln;
  finally
    aModel.Free;
  end;
end.
