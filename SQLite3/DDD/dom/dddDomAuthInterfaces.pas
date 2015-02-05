/// mORMot shared DDD Domain: Authentication types and interfaces
unit dddDomAuthInterfaces;

{
    This file is part of Synopse mORmot framework.

    Synopse mORMot framework. Copyright (C) 2015 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2015
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

  Version 1.18
  - first public release, corresponding to Synopse mORMot Framework 1.18

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SynCommons,
  SysUtils,
  Classes,
  mORMot,
  mORMotDDD;
  
type
  /// the data type which will be returned during a password challenge
  // - in practice, will be e.g. Base-64 encoded SHA-256 binary hash
  TAuthQueryNonce = RawUTF8;

  TAuthInfoName = RawUTF8;

  /// DDD object used to store authentication information
  TAuthInfo = class(TPersistent)
  protected
    fLogonName: TAuthInfoName;
  published
    /// the textual identifier by which the user would recognize himself
    property LogonName: TAuthInfoName read fLogonName write fLogonName;
  end;

  /// repository service to authenticate credentials via a dual pass challenge
  IAuthQuery = interface(ICQRSQuery)
    ['{5FB1E4A6-B432-413F-8958-1FA1857D1195}']
    /// initiate the first phase of a dual pass challenge authentication
    function ChallengeSelectFirst(const aLogonName: RawUTF8): TAuthQueryNonce;
    /// validate the first phase of a dual pass challenge authentication
    function ChallengeSelectFinal(const aChallengedPassword: TAuthQueryNonce): TCQRSResult;
    /// set the credential for Get() or further IAuthCommand.Update/Delete
    // - this method execution will be disabled for most clients
    function SelectByName(const aLogonName: RawUTF8): TCQRSResult;
    /// retrieve some information about the current selected credential
    function Get(out aAggregate: TAuthInfo): TCQRSResult;
  end;

  /// repository service to update or register new authentication credentials
  IAuthCommand = interface(IAuthQuery)
    ['{8252727B-336B-4105-80FD-C8DFDBD4801E}']
    /// register a new credential, from its LogonName/HashedPassword values
    // - aHashedPassword should match the algorithm expected by the actual
    // implementation class
    // - on success, the newly created credential will be the currently selected
    function Add(const aLogonName: RawUTF8; aHashedPassword: TAuthQueryNonce): TCQRSResult;
    /// update the current selected credential password
    // - aHashedPassword should match the algorithm expected by the actual
    // implementation class
    // - will be allowed only for the current challenged user
    function UpdatePassword(const aHashedPassword: TAuthQueryNonce): TCQRSResult;
    /// delete the current selected credential
    // - this method execution will be disabled for most clients
    function Delete: TCQRSResult;
    /// write all pending changes prepared by Add/UpdatePassword/Delete methods
    function Commit: TCQRSResult;
  end;

  
implementation

initialization
  TInterfaceFactory.RegisterInterfaces(
    [TypeInfo(IAuthQuery),TypeInfo(IAuthCommand)]);
end.