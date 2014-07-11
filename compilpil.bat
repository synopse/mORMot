@echo off

if not "%DelphiVersion%"=="" goto AlreadySet
rem ** Default compiler is Delphi 7
set DCC=c:\progs\delphi7\bin\dcc32.exe
set DelphiVersion=Delphi 7 %LVCL%
if exist \dev\lib\RTL7\Classes.pas goto EnhancedRTL
set Switches=-B -Q -DLVCL;INCLUDE_FTS3 -GD -U\dev\lib\LVCL;\dev\lib\RTL7;\dev\lib;\dev\lib\sqlite3;\dev\lib\syndbdataset -I\dev\lib\LVCL\;\dev\lib -R\dev\lib  -O\dev\lib\sqlite3 -E\dev\lib\tempbuild\exe -N\dev\lib\tempbuild\dcu
if not "%LVCL%"=="" goto AllSet
set Switches=-B -Q -DINCLUDE_FTS3 -GD -U\dev\lib\RTL7;\dev\lib;\dev\lib\sqlite3;\dev\lib\syndbdataset -I\dev\lib -R\dev\lib  -O\dev\lib\sqlite3 -E\dev\lib\tempbuild\exe -N\dev\lib\tempbuild\dcu
goto AllSet
:EnhancedRTL
set Switches=-B -Q -DENHANCEDRTL;INCLUDE_FTS3 -GD -U\dev\lib\RTL7;\dev\lib;\dev\lib\sqlite3;\dev\lib\syndbdataset -I\dev\lib -R\dev\lib  -O\dev\lib\sqlite3 -E\dev\lib\tempbuild\exe -N\dev\lib\tempbuild\dcu
goto AllSet
:AlreadySet
set Switches=-B -Q -DINCLUDE_FTS3 -GD -Uc:\progs\delphi5\lib;\dev\lib;\dev\lib\sqlite3;\dev\lib\syndbdataset -I\dev\lib -R\dev\lib  -O\dev\lib\sqlite3 -E\dev\lib\tempbuild\exe -N\dev\lib\tempbuild\dcu
if "%DelphiVersion%"=="Delphi 5" goto AllSet
set Switches=-B -Q -DINCLUDE_FTS3 -GD -Uc:\progs\delphi6\lib;\dev\lib;\dev\lib\sqlite3;\dev\lib\syndbdataset -I\dev\lib -R\dev\lib  -O\dev\lib\sqlite3 -E\dev\lib\tempbuild\exe -N\dev\lib\tempbuild\dcu
if "%DelphiVersion%"=="Delphi 6" goto AllSet
set Switches=-B -Q -DINCLUDE_FTS3 -GD -U\dev\lib;\dev\lib\sqlite3;\dev\lib\syndbdataset -I\dev\lib -R\dev\lib -O\dev\lib\sqlite3 -E\dev\lib\tempbuild\exe -N\dev\lib\tempbuild\dcu -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;Vcl;System.Win
:AllSet

if not exist %DCC% goto NoDCCCompiler

echo.
echo ********** mORMot integration using %DelphiVersion% *********
echo.

echo Switches=%Switches%
echo.

cd  \dev\lib
mkdir tempbuild
mkdir tempbuild\exe
del tempbuild\exe\*.exe
del tempbuild\exe\*.drc
del tempbuild\exe\*.map
del tempbuild\exe\*.db3
del tempbuild\exe\*.ini
del tempbuild\exe\*.data
del tempbuild\exe\*.mdb
del tempbuild\exe\TestSQL3.*
mkdir tempbuild\dcu
del tempbuild\dcu\*.dcu


cd \dev\lib\sqlite3

%DCC% TestSQL3.dpr %Switches%
@if errorlevel 1 pause

if "%DelphiVersion%"=="Delphi 5" goto SKIPSYNPROJECT

%DCC% TestSQL3Register.dpr %Switches%
@if errorlevel 1 pause
%DCC% ServiceTestSQL3.dpr %Switches%
@if errorlevel 1 pause

if "%LVCL%"=="LVCL" goto NoDCCCompiler

%DCC% TestOleDB.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\01 - In Memory ORM"
%DCC% Project01.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\02 - Embedded SQLite3 ORM"
%DCC% Project02.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\03 - NamedPipe Client-Server"
%DCC% Project03Client.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project03Server.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\04 - HTTP Client-Server"
%DCC% Project04Client.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project04Server.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\05 - Report created from code"
%DCC% TestSQLite3Pages.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\06 - Remote JSON REST Service"
%DCC% Project06Client.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project06Server.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\07 - SynTest"
%DCC% SynTest.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\08 - TaskDialog"
%DCC% TaskDialogTest.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\09 - HttpApi web server"
%DCC% HttpApiServer.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\10 - Background Http service"
%DCC% httpservice.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\11 - Exception logging"
%DCC% LibraryTest.dpr %Switches%
@if errorlevel 1 pause
%DCC% LoggingTest.dpr %Switches%
@if errorlevel 1 pause
%DCC% LogView.dpr %Switches%
@if errorlevel 1 pause
%DCC% Map2Mab.dpr %Switches%
@if errorlevel 1 pause
%DCC% MyLibrary.dpr %Switches%
@if errorlevel 1 pause
%DCC% UnSynLz.dpr %Switches%
@if errorlevel 1 pause

cd "\Dev\Lib\SQLite3\Samples\12 - SynDB Explorer"
%DCC% SynDBExplorer.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\13 - StandAlone JSON SQL server"
%DCC% JSONSQLClient.dpr %Switches%
@if errorlevel 1 pause
%DCC% JSONSQLServer.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\14 - Interface based services"
%DCC% Project14Client.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project14Server.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project14ServerExternal.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project14ServerHttp.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project14ServerHttpWeak.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project14ServerInMemory.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\15 - External DB performance"
%DCC% PerfTest.dpr %Switches% -DNONET;NOSTATIC -I\dev\UniDac\Source;\dev\zeos\src -U\dev\UniDac\Source;D:\Dev\UniDAC\Source\UniProviders\Oracle;D:\Dev\UniDAC\Source\UniProviders\InterBase;D:\Dev\UniDAC\Source\UniProviders\SQLite;D:\Dev\UniDAC\Source\UniProviders\SQLServer;\Dev\Zeos\packages\delphi7\build;\Dev\Zeos\src;\Dev\Zeos\src\core;\Dev\Zeos\src\dbc;\Dev\Zeos\src\parsesql;\Dev\Zeos\src\plain
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\16 - Execute SQL via services"
%DCC% Project16Client.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project16ServerHttp.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\17 - TClientDataset use"
%DCC% mORMotVCLTest.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\18 - AJAX ExtJS Grid"
%DCC% Project18Server.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\19 - AJAX ExtJS FishFacts"
%DCC% Project19Server.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\20 - DTO interface based service"
%DCC% Project20Client.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project20ServerInMemory.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\21 - HTTP Client-Server performance"
%DCC% Project21HttpClient.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project21HttpServer.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\22 - JavaScript HTTPApi web server"
%DCC% JSHttpApiServer.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\23 - JavaScript Tests"
%DCC% TestSynSM.dpr %Switches%
@if errorlevel 1 pause
%DCC% TestMustache.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\24 - MongoDB"
%DCC% MongoDBTests.dpr %Switches%
@if errorlevel 1 pause

cd "\dev\lib\sqlite3\Samples\MainDemo"
call FileMainRes.bat
%DCC% SynFile.dpr %Switches%
@if errorlevel 1 pause

goto SKIPSYNPROJECT

cd "\dev\lib\SynProject"
brcc32 FileMain.rc
%DCC% SynProject.dpr %Switches%
@if errorlevel 1 pause

:SKIPSYNPROJECT

cd \dev\lib\tempbuild\exe
Map2Mab.exe *.exe

echo Running automated tests for mORMot
TestSQL3 "%DelphiVersion% "

:NoDCCCompiler
cd \dev\lib\tempbuild\exe
del *.map
del *.drc

cd \dev\lib
set DCC=
set DelphiVersion=
rem pause
