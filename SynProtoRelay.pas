/// implements asynchronous safe TCP tunnelling, using SynEcc cryptography
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynProtoRelay;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2020 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

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

  Portions created by the Initial Developer are Copyright (C) 2020
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


  Secured TCP tunneling
  ---------------------

 It will encapsulate any TCP/IP duplex stream over a public server,
 allowing to connect a client to connect to a local server behind a firewall,
 using a public server (e.g. a simple Linux box) as relay.

 For best security, it uses asymetric encryption using public and private keys,
 as supplied by our SynEcc/SynCrypto units.

 Actual communication is done using WebSockets, in binary mode.

 See Sample "38 - TCP relay" for some numbers on actual hardware.

}


{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  SynWinSock,
  {$else}
  {$ifdef KYLIX3}
  LibC,
  {$endif}
  SynFPCSock, // shared with Kylix
  {$endif}
  SysUtils,
  SynCommons,
  SynCrtSock,
  SynBidirSock,
  SynLog,
  SynCrypto,
  SynEcc,
  mORMot;

type
  TProtoRelayConnectionID = type Int64;

  IProtoRelayCallback = interface(IInvokable)
    procedure RemoteCallJSON(const Method, InHead: RawUTF8; const InBody: RawJSON;
      out OutHead: RawUTF8; out OutBody: RawJSON; out OutStatus, OutInternalState: integer);
    procedure RemoteCallBinary(const Method, InHead: RawUTF8; const InBody: RawByteString;
      out OutHead: RawUTF8; out OutBody: RawByteString; out OutStatus, OutInternalState: integer);
  end;

  IProtoRelayClientCallback = interface(IProtoRelayCallback)
    ['{075D6627-8611-4182-A245-72DF1DD545E2}']
  end;
  IProtoRelayServerCallback = interface(IProtoRelayCallback)
    ['{BF6403E6-8FBA-452C-AF77-B4E4B86C5A17}']
  end;

  IProtoRelay = interface(IServiceWithCallbackReleased)
    ['{B42D9449-0C66-4A02-9F53-7B584B6A3C78}']
    function ClientSubscribe(id: TProtoRelayConnectionID;
      const callback: IProtoRelayClientCallback): boolean;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IProtoRelay),TypeInfo(IProtoRelayClientCallback),
    TypeInfo(IProtoRelayServerCallback)]);
end.

