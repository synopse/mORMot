{{ in order to be able to use http.sys server for TestSQL3.exe under
   Vista or Seven, call first this program with Administrator rights
  - you can unregister it later with command line parameter /delete }
program TestSQL3Register;

{$APPTYPE CONSOLE}

uses
  SynCommons,
  SynCrtSock,
  SysUtils;

const
  REGSTR: array[boolean] of string = (
    'Registration', 'Deletion');

{$R VistaAdm.res} // force elevation to Administrator under Vista/Seven

var delete: boolean;
    i: integer;
procedure Call(const Root: RawByteString);
begin
  writeln(REGSTR[delete],' of /',root,':888/+ for http.sys');
  writeln(THttpApiServer.AddUrlAuthorize(root,'888',false,'+',delete));
end;
begin
  // perform url registration for http.sys
  // (e.g. to be run as administrator under Windows Vista/Seven)
  delete := (ParamCount=1) and SameText(ParamStr(1),'/DELETE');
  // parameters below must match class function
  //  TTestClientServerAccess.RegisterAddUrl in mORMotHttpServer.pas:
  Call('root');
  for i := 0 to 2 do
    Call('root'+UInt32ToUtf8(i));
  // we're done
  WriteLn('Done - Press ENTER to Exit');
  ReadLn;
end.
 