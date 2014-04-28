/// Main unit testing program of the Synopse mORMot framework
// - this program is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
program TestSQL3;

(*
    This file is part of Synopse mORMot database framework.

    Synopse mORMot framework. Copyright (C) 2014 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2014
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****


  Version 1.13
  - code modifications to compile with Delphi 5 compiler (no ORM code compiles
    yet: so only low-level units like SynCommons / SynCrypto / SynPdf are tested)
  - note: in order to be able to use http.sys server under Vista or Seven,
    first compile then execute TestSQL3Register.dpr (need Administrator rights)

  Version 1.15
  - all tests passed with Delphi XE2 (32 Bit)
  - SynSQLite3 logic extracted from SQLite3 unit
  - enhanced tests about external database handling
  - tests renamed to match the new "mORMot" framework name

  Version 1.16
  - all tests are now implemented in a separated SQLite3SelfTests unit -
    this is requested by Delphi XE2 background compiler issues 
	
  Version 1.18
  - renamed SQLite3*.pas units to mORMot*.pas
  - included Windows 64 bit regression tests
  - all tests passed with Delphi XE3 and XE4 for Win32 and Win64 platforms


  this application has EnableMemoryLeakReporting conditional defined in its
  Project/Options -> we can therefore ensure that our mORMot Client/Server
  framework classes have no memory leak
  - Search Path and Debug Path: [\Dev\Lib\LVCL]  (if you plan to use LVCL)
  - Conditional defines: EnableMemoryLeakReporting;USEFTS[;ENHANCEDRTL][;LVCL]
  - if you don't have installed our Enhanced Run Time Library, please delete
    ENHANCEDRTL global conditional
  - if you do not plan to use LVCL, do not refers to these libraries
    (which works only for Delphi 7)
  - first line of uses clause must be  {$I SynDprUses.inc}  to enable FastMM4
    on older versions of Delphi *)


{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

uses
  {$I SynDprUses.inc}
  mORMotSelfTests in 'mORMotSelfTests.pas',
  SynLZ in '..\SynLZ.pas',
  SynLZO in '..\SynLZO.pas',
  SynCrypto in '..\SynCrypto.pas',
  SynCrtSock in '..\SynCrtSock.pas',
  SynCommons in '..\SynCommons.pas',
{$ifndef DELPHI5OROLDER}
  {$ifndef LVCL}
  SynMongoDB in '..\SynMongoDB.pas',
  {$ifndef CPU64}
  SynSMAPI in '..\SynSMAPI.pas',
  SynSM in '..\SynSM.pas',
  {$endif}
  {$endif}
  SynBigTable in '..\SynBigTable.pas',
  SynSQLite3 in '..\SynSQLite3.pas',
  SynSQLite3Static in '..\SynSQLite3Static.pas',
{$ifndef FPC}
  mORMot in 'mORMot.pas',
  mORMotSQLite3 in 'mORMotSQLite3.pas',
  mORMotFastCgiServer in 'mORMotFastCgiServer.pas',
  mORMotHttpClient in 'mORMotHttpClient.pas',
  mORMotHttpServer in 'mORMotHttpServer.pas',
  mORMotService in 'mORMotService.pas',
  //mORMotBigTable,
{$endif}
{$endif}
{$ifndef FPC}
{$ifndef LVCL}
  SynPdf in '..\SynPdf.pas',
  SynGdiPlus in '..\SynGdiPlus.pas',
  SynDB in '..\SynDB.pas',
  SynOleDB in '..\SynOleDB.pas',
  SynDBOracle in '..\SynDBOracle.pas',
  SynDBODBC in '..\SynDBODBC.pas',
  SynDBSQLite3 in '..\SynDBSQLite3.pas',
{$ifndef FPC}
{$ifndef DELPHI5OROLDER}
  mORMotDB in 'mORMotDB.pas',
  SynZipFiles in '..\SynZipFiles.pas',
{$endif DELPHI5OROLDER}
{$endif FPC}
{$endif LVCL}
{$endif FPC}
  SynMustache in '..\SynMustache.pas',
  SynZip in '..\SynZip.pas',
  SynSelfTests in '..\SynSelfTests.pas';

begin
  SQLite3ConsoleTests;
end.
