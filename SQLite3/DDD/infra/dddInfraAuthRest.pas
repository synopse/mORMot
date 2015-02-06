/// mORMot shared DDD Infrastructure: Authentication implementation
unit dddInfraAuthRest;

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

  TODO:
   - manage Authentication expiration

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  SynCrypto,
  mORMot,
  mORMotDDD,
  dddDomAuthInterfaces;

  
{ ----- Authentication Implementation using SHA-256 dual step challenge }

type
  /// ORM object to persist authentication information
  TSQLRecordAuthInfo = class(TSQLRecord)
  protected
    fLogon: RawUTF8;
    fHashedPassword: RawUTF8;
    class procedure InternalRegisterCustomProperties(Props: TSQLRecordProperties); override;
  published
    property Logon: RawUTF8 read fLogon write fLogon stored AS_UNIQUE;
    property HashedPassword: RawUTF8 read fHashedPassword write fHashedPassword;
  end;

  /// generic class for implementing authentication
  // - do not instantiate this abstract class, but e.g. TDDDAuthenticationSHA256
  // or TDDDAuthenticationMD5
  TDDDAuthenticationAbstract = class(TDDDRepositoryRestCommand,IAuthCommand)
  protected
    fChallengeLogonName: RawUTF8;
    fChallengeNonce: TAuthQueryNonce;
    class function ComputeHashPassword(const aLogonName,aPassword: RawUTF8): TAuthQueryNonce;
    /// overriden classes should override this method with the proper algorithm
    class function DoHash(const aValue: TAuthQueryNonce): TAuthQueryNonce; virtual; abstract;
  public
    /// initiate the first phase of a dual pass challenge authentication
    function ChallengeSelectFirst(const aLogonName: RawUTF8): TAuthQueryNonce;
    /// validate the first phase of a dual pass challenge authentication
    function ChallengeSelectFinal(const aChallengedPassword: TAuthQueryNonce): TCQRSResult;
    /// set the credential for Get() or further IAuthCommand.Update/Delete
    // - this method execution will be disabled for most clients
    function SelectByName(const aLogonName: RawUTF8): TCQRSResult;
    /// retrieve some information about the current selected credential
    function Get(out aAggregate: TAuthInfo): TCQRSResult;
    /// register a new credential, from its LogonName/HashedPassword values
    // - on success, the newly created credential will be the currently selected
    function Add(const aLogonName: RawUTF8; aHashedPassword: TAuthQueryNonce): TCQRSResult;
    /// update the current selected credential password
    function UpdatePassword(const aHashedPassword: TAuthQueryNonce): TCQRSResult;
    /// class method to be used on the client side to compute the password
    // - is basically
    // !   result := DoHash(aLogonName+':'+aChallengeFromServer+':'+
    // !   ComputeHashPassword(aLogonName,aPlainPassword));
    class function ClientComputeChallengedPassword(
      const aLogonName,aPlainPassword: RawUTF8;
      const aChallengeFromServer: TAuthQueryNonce): TAuthQueryNonce; virtual;
  end;

  /// allows to specify which actual hashing algorithm would be used
  // - i.e. either TDDDAuthenticationSHA256 or TDDDAuthenticationMD5
  TDDDAuthenticationClass = class of TDDDAuthenticationAbstract;

  /// implements authentication using SHA-256 hashing
  // - more secure than TDDDAuthenticationMD5
  TDDDAuthenticationSHA256 = class(TDDDAuthenticationAbstract)
  protected
    /// will use SHA-256 algorithm for hashing, and the class name as salt
    class function DoHash(const aValue: TAuthQueryNonce): TAuthQueryNonce; override;
  end;

  /// implements authentication using MD5 hashing
  // - less secure than TDDDAuthenticationSHA256
  TDDDAuthenticationMD5 = class(TDDDAuthenticationAbstract)
  protected
    /// will use MD5 algorithm for hashing, and the class name as salt
    class function DoHash(const aValue: TAuthQueryNonce): TAuthQueryNonce; override;
  end;

  /// abstract factory of IAuthCommand repository instances using REST
  TDDDAuthenticationRestFactoryAbstract = class(TDDDRepositoryRestFactory)
  protected
  public
    /// initialize a factory with the supplied implementation algorithm
    constructor Create(aRest: TSQLRest; aImplementationClass: TDDDAuthenticationClass;
      aOwner: TDDDRepositoryRestManager); reintroduce;
  end;
  
  /// factory of IAuthCommand repository instances using a RESTful ORM access
  // and SHA-256 hashing algorithm
  TDDDAuthenticationRestFactorySHA256 = class(TDDDAuthenticationRestFactoryAbstract)
  protected
  public
    /// initialize a factory with the SHA-256 implementation algorithm
    constructor Create(aRest: TSQLRest; aOwner: TDDDRepositoryRestManager=nil); reintroduce;
  end;

  /// factory of IAuthCommand repository instances using a RESTful ORM access
  // and SHA-256 hashing algorithm
  TDDDAuthenticationRestFactoryMD5 = class(TDDDAuthenticationRestFactoryAbstract)
  protected
  public
    /// initialize a factory with the SHA-256 implementation algorithm
    constructor Create(aRest: TSQLRest; aOwner: TDDDRepositoryRestManager=nil); reintroduce;
  end;

  

implementation

{ TDDDAuthenticationAbstract }

function TDDDAuthenticationAbstract.ChallengeSelectFirst(
  const aLogonName: RawUTF8): TAuthQueryNonce;
begin
  fChallengeLogonName := Trim(aLogonName);
  fChallengeNonce := DoHash(aLogonName+NowToString);
end;

function TDDDAuthenticationAbstract.ChallengeSelectFinal(
  const aChallengedPassword: TAuthQueryNonce): TCQRSResult;
begin
  if (fChallengeLogonName='') or (fChallengeNonce='') then
    result := ORMError(cqrsBadRequest) else
    result := SelectByName(fChallengeLogonName);
  if result<>cqrsSuccess then
    exit;
  ORMBegin(qaSelect,result);
  if DoHash(fChallengeLogonName+':'+fChallengeNonce+':'+
     (ORM as TSQLRecordAuthInfo).HashedPassword)=aChallengedPassword then
    ORMResult(cqrsSuccess) else
    ORMResultMsg(cqrsBadRequest,'Wrong Password for "%"',[fChallengeLogonName]);
  fChallengeNonce := '';
  fChallengeLogonName := '';
end;

class function TDDDAuthenticationAbstract.ComputeHashPassword(
  const aLogonName, aPassword: RawUTF8): TAuthQueryNonce;
begin
  result := DoHash(aLogonName+':'+aPassword);
end;

class function TDDDAuthenticationAbstract.ClientComputeChallengedPassword(
  const aLogonName,aPlainPassword: RawUTF8; const aChallengeFromServer: TAuthQueryNonce): TAuthQueryNonce;
begin // see TDDDAuthenticationAbstract.ChallengeSelectFinal
  result := DoHash(aLogonName+':'+aChallengeFromServer+':'+
    ComputeHashPassword(aLogonName,aPlainPassword));
end;

function TDDDAuthenticationAbstract.SelectByName(
  const aLogonName: RawUTF8): TCQRSResult;
begin
  result := ORMSelectOne('Logon=?',[aLogonName],(aLogonName=''));
end;

function TDDDAuthenticationAbstract.Get(
  out aAggregate: TAuthInfo): TCQRSResult;
begin
  result := ORMGetAggregate(aAggregate);
end;

function TDDDAuthenticationAbstract.Add(const aLogonName: RawUTF8;
  aHashedPassword: TAuthQueryNonce): TCQRSResult;
begin
  if not ORMBegin(qaCommandDirect,result) then
    exit;
  with ORM as TSQLRecordAuthInfo do begin
    Logon := aLogonName;
    HashedPassword := aHashedPassword;
  end;
  ORMPrepareForCommit(soInsert);
end;

function TDDDAuthenticationAbstract.UpdatePassword(
  const aHashedPassword: TAuthQueryNonce): TCQRSResult;
begin
  if not ORMBegin(qaCommandOnSelect,result) then
    exit;
  (ORM as TSQLRecordAuthInfo).HashedPassword := aHashedPassword;
  ORMPrepareForCommit(soUpdate);
end;


{ TDDDAuthenticationSHA256 }

class function TDDDAuthenticationSHA256.DoHash(
  const aValue: TAuthQueryNonce): TAuthQueryNonce;
begin
  result := SHA256(RawUTF8(ClassName)+aValue);
end;

{ TDDDAuthenticationMD5 }

class function TDDDAuthenticationMD5.DoHash(
  const aValue: TAuthQueryNonce): TAuthQueryNonce;
begin
  result := MD5(RawUTF8(ClassName)+aValue);
end;


{ TDDDAuthenticationRestFactoryAbstract }

constructor TDDDAuthenticationRestFactoryAbstract.Create(aRest: TSQLRest;
  aImplementationClass: TDDDAuthenticationClass;
  aOwner: TDDDRepositoryRestManager);
begin
  inherited Create(aOwner,
    IAuthCommand,aImplementationClass,TAuthInfo,aRest,TSQLRecordAuthInfo,
    ['Logon','LogonName']);
end;

{ TDDDAuthenticationRestFactorySHA256 }

constructor TDDDAuthenticationRestFactorySHA256.Create(aRest: TSQLRest;
  aOwner: TDDDRepositoryRestManager);
begin
  inherited Create(aRest,TDDDAuthenticationSHA256,aOwner);
end;

{ TDDDAuthenticationRestFactoryMD5 }

constructor TDDDAuthenticationRestFactoryMD5.Create(aRest: TSQLRest;
  aOwner: TDDDRepositoryRestManager);
begin
  inherited Create(aRest,TDDDAuthenticationMD5,aOwner);
end;


{ TSQLRecordAuthInfo }

class procedure TSQLRecordAuthInfo.InternalRegisterCustomProperties(
  Props: TSQLRecordProperties);
begin
  AddFilterNotVoidText(['HashedPassword']);
end;

end.
