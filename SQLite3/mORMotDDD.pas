/// DDD tollbox types for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotDDD;

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
  SysUtils,
  Classes,
  Contnrs,
  SynCommons,
  SynCrypto,
  mORMot;

{ some mORMot conventions about DDD implementation:

 * most services methods should return an enumerate:
  - Exceptions should be raised only in case of a real failure (e.g. unexpected
    execution context, service shutdown...) but most of the time an enumerate
    will be used to manage errors
  - no textual error message should be sent by the application layer: it is up
    to the end-user application to react to a given unexpected result
  - within the DDD services, CQRS methods will use a TCQRSResult enumerate
    and advanced error process, with an optional error text for debugging
  - first value should mean success, so that TInterfaceStub would let the
    test pass by returning the default 0 ordinal

 * persistence ignorance is mostly implemented via CQRS services:
  - we implement the "Command Query Responsibility Segregation" pattern
    to potentially increase scaling ability of reading, and allow distributed
    transactions via a service-based two-phase commit
  - no ID should be transmitted most of the time, but write commands have to
    follow a read query which would specify the corresponding item so that
    the ID would be stored during the process
  - I*Query interfaces should have some SelectBy*() methods to change the
    current selected aggregate, which could later on be retrieved by a
    Get(out aAggregate: T....) method
  - I*Command interfaces should have standard Add/Update/Delete methods,
    expecting a previous SelectBy*() call for Update/Delete: those methods
    should prepare the corresponding data change
  - I*Command interfaces should have Commit to perform all the pending
    changes (which may be implemented by transactions or via TSQLRestBatch)
  - I*Command interfaces should abort the process if the instance is
    released without any prior call to Commit (e.g. rollback the transaction
    or free any pending TSQLRestBatch instance)

  TODO:
   - manage Authentication expiration 

}

{ *********** Some Domain-Driven-Design Common Definitions }

type
  /// abstract ancestor for all Domain-Driven-Design related Exceptions
  EDDDException = class(ESynException);

  /// Exception type linked to CQRS persistence service methods
  ECQRSException = class(EDDDException); 
  

{ *********** Persistence Interfaces }

type
  /// result enumerate for I*Query/I*Command CQRS persistence service methods 
  // - cqrsSuccess will map the default TInterfaceStub returned value
  // - cqrsBadRequest would indicate that the method was not called in the
  // expected workflow sequence
  // - cqrsNotFound may appear for a I*Query SelectBy*() method
  // - cqrsInvalidContent for any I*Command method with invalid aggregate input
  // value (e.g. a missing field)
  // - cqrsAlreadyExists for a I*Command.Add method with a primay key conflict
  // - cqrsNoPriorQuery for a I*Command.Update/Delete method with no prior
  // call to SelectBy*()
  // - cqrsUnspecifiedError should be used for any other kind of error
  TCQRSResult =
    (cqrsSuccess, cqrsUnspecifiedError, cqrsBadRequest,
     cqrsNotFound,
     cqrsInvalidContent, cqrsAlreadyExists, cqrsNoPriorQuery);

  /// generic interface, to be used for CQRS I*Query types definition
  ICQRSQuery = interface(IInvokable)
    ['{923614C8-A639-45AD-A3A3-4548337923C9}']
    /// should return the last error as an enumerate
    function LastError: TCQRSResult;
    /// should return an error message corresponding to the last enumerate
    function LastErrorMessage: RawUTF8;
  end;
            
  /// to be inherited to implement CQRS I*Query services extended error message
  // - you should never assign directly a cqrs* value to a method result, but
  // rather use the Reset/SetResult() methods provided by this class:
  // ! function TMyService.MyMethod: TCQRSResult;
  // ! begin
  // !   Reset(result); // reset the error information to cqrsUnspecifiedError
  // !   ... // do some work
  // !   if error then
  // !     SetResult(cqrsUnspecifiedError,'Oups! For "%"',[name]) else
  // !     SetResult(cqrsSuccess); // instead of result := cqrsSuccess
  // !   end;
  TCQRSQueryObject = class(TInjectableObject, ICQRSQuery)
  protected
    fResultAddress: ^TCQRSResult;
    fLastError: TCQRSResult;
    fLastErrorMessage: RawUTF8;
    /// overloaded protected methods to be used for LastError process
    procedure Reset(var result: TCQRSResult; Error: TCQRSResult=cqrsUnspecifiedError);
    procedure SetResult(Error: TCQRSResult); overload;
    procedure SetResult(Error: TCQRSResult; const ErrorMessage: RawUTF8); overload;
    procedure SetResult(Error: TCQRSResult;
      ErrorMsgFmt: PUTF8Char; const ErrorMsgArgs: array of const); overload;
  public
    /// returns the last error as an enumerate
    function LastError: TCQRSResult; virtual;
    /// returns a textual error message corresponding to the last error enumerate 
    // - if no specific message has been defined, a generic English text will be
    // computed by this method 
    function LastErrorMessage: RawUTF8; virtual;
  end;


{ *********** Other Cross-Cutting Interfaces }

type
  /// the data type which will be returned during a password challenge
  // - in practice, will be e.g. Base-64 encoded SHA-256 binary hash
  TAuthQueryNonce = RawUTF8;

  /// used to store authentication information
  TAuthInfo = class(TPersistent)
  protected
    fLogonName: RawUTF8;
  published
    /// the textual identifier by which the user would recognize himself
    property LogonName: RawUTF8 read fLogonName write fLogonName;
  end;

  /// service to authenticate credentials via a dual pass challenge
  IAuthQuery = interface(ICQRSQuery)
    ['{5FB1E4A6-B432-413F-8958-1FA1857D1195}']
    /// initiate the first phase of a dual pass challenge authentication
    function ChallengeSelectFirst(const aLogonName: RawUTF8): TAuthQueryNonce;
    /// validate the first phase of a dual pass challenge authentication
    function ChallengeSelectFinal(const aChallengedPassword: TAuthQueryNonce): TCQRSResult;
    /// set the credential for Get() or further IAuthCommand.Update/Delete
    // - this method execution will be disabled for most clients
    function ChallengeSelectByName(const aLogonName: RawUTF8): TCQRSResult;
    /// retrieve some information about the current selected credential
    function Get(out aAggregate: TAuthInfo): TCQRSResult;
  end;

  /// service to update or register new authentication credentials
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


{ *********** Application Layer Implementation }

type
  /// abstract class for implementing an Application Layer service
  TDDDApplication = class(TInjectableObject)
  protected
  end;


{ *********** Cross-Cutting Layer Implementation}

{ ----- Authentication Implementation using SHA-256 dual step challenge }

type
  /// generic class for implementing authentication
  // - do not instantiate this abstract class, but e.g. TDDDAuthenticationSHA256
  // or TDDDAuthenticationMD5
  TDDDAuthenticationAbstract = class(TCQRSQueryObject,IAuthCommand)
  protected
    fCurrentLogonName: RawUTF8;
    fCurrentNonce: TAuthQueryNonce;
    class function ComputeHashPassword(const aLogonName,aPassword: RawUTF8): TAuthQueryNonce; 
    /// overriden classes should override this method with the proper algorithm
    class function DoHash(const aValue: TAuthQueryNonce): TAuthQueryNonce; virtual; abstract;
    // temporary abstract method - TODO: use the ORM as real repository 
    function GetHashedPasswordFromRepository(const aLogonName: RawUTF8): TAuthQueryNonce; virtual; abstract;
  public
    /// initiate the first phase of a dual pass challenge authentication
    function ChallengeSelectFirst(const aLogonName: RawUTF8): TAuthQueryNonce;
    /// validate the first phase of a dual pass challenge authentication
    function ChallengeSelectFinal(const aChallengedPassword: TAuthQueryNonce): TCQRSResult;
    /// set the credential for Get() or further IAuthCommand.Update/Delete
    // - this method execution will be disabled for most clients
    function ChallengeSelectByName(const aLogonName: RawUTF8): TCQRSResult;
    /// retrieve some information about the current selected credential
    function Get(out aAggregate: TAuthInfo): TCQRSResult;
    /// register a new credential, from its LogonName/HashedPassword values
    // - on success, the newly created credential will be the currently selected
    function Add(const aLogonName: RawUTF8; aHashedPassword: TAuthQueryNonce): TCQRSResult;
    /// update the current selected credential password
    function UpdatePassword(const aHashedPassword: TAuthQueryNonce): TCQRSResult;
    /// delete the current selected credential
    function Delete: TCQRSResult;
    /// write all pending changes prepared by Add/UpdatePassword/Delete methods
    function Commit: TCQRSResult;
    /// class method to be used on the client side to compute the password
    // - is basically  
    class function ClientComputeChallengedPassword(
      const aLogonName,aPlainPassword: RawUTF8;
      const aChallengeFromServer: TAuthQueryNonce): TAuthQueryNonce; virtual;
  end;

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

  
{ ----- Persistence Implementation using mORMot's ORM }


implementation


{ TCQRSQueryObject }

function TCQRSQueryObject.LastError: TCQRSResult;
begin
  result := fLastError;
end;

function TCQRSQueryObject.LastErrorMessage: RawUTF8;
begin
  if fLastErrorMessage='' then
    result := GetEnumNameTrimed(TypeInfo(TCQRSResult),fLastError) else
    result := fLastErrorMessage;
end;

procedure TCQRSQueryObject.Reset(var result: TCQRSResult; Error: TCQRSResult);
begin
  fResultAddress := @result;
  result := Error;
  fLastError := Error;
  fLastErrorMessage := '';
end;

procedure TCQRSQueryObject.SetResult(Error: TCQRSResult);
begin
  if fResultAddress=nil then
    raise ECQRSException.CreateUTF8('%.SetResult(%) with no prior Start',
      [self,GetEnumName(TypeInfo(TCQRSResult),ord(Error))^]);
  fResultAddress^ := Error;
  fLastError := Error;
end;

procedure TCQRSQueryObject.SetResult(Error: TCQRSResult;
  const ErrorMessage: RawUTF8);
begin                 
  SetResult(Error);
  fLastErrorMessage := ErrorMessage;
end;

procedure TCQRSQueryObject.SetResult(Error: TCQRSResult;
  ErrorMsgFmt: PUTF8Char; const ErrorMsgArgs: array of const);
begin
  SetResult(Error,FormatUTF8(ErrorMsgFmt,ErrorMsgArgs));
end;


{ TDDDAuthenticationAbstract }

function TDDDAuthenticationAbstract.ChallengeSelectFirst(
  const aLogonName: RawUTF8): TAuthQueryNonce;
begin
  fCurrentLogonName := aLogonName;
  fCurrentNonce := DoHash(aLogonName+NowToString);
end;

function TDDDAuthenticationAbstract.ChallengeSelectFinal(
  const aChallengedPassword: TAuthQueryNonce): TCQRSResult;
begin
  Reset(result);
  if (fCurrentLogonName='') or (fCurrentNonce='') then
    SetResult(cqrsBadRequest) else
    if DoHash(fCurrentLogonName+':'+fCurrentNonce+':'+
       GetHashedPasswordFromRepository(fCurrentLogonName))=aChallengedPassword then
      SetResult(cqrsSuccess) else
      SetResult(cqrsBadRequest,'Wrong Password');
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


function TDDDAuthenticationAbstract.ChallengeSelectByName(
  const aLogonName: RawUTF8): TCQRSResult;
begin
  Reset(result);

end;

function TDDDAuthenticationAbstract.Get(
  out aAggregate: TAuthInfo): TCQRSResult;
begin
  Reset(result);

end;

function TDDDAuthenticationAbstract.Add(const aLogonName: RawUTF8;
  aHashedPassword: TAuthQueryNonce): TCQRSResult;
begin
  Reset(result);

end;

function TDDDAuthenticationAbstract.Delete: TCQRSResult;
begin
  Reset(result);

end;

function TDDDAuthenticationAbstract.UpdatePassword(
  const aHashedPassword: TAuthQueryNonce): TCQRSResult;
begin
  Reset(result);

end;

function TDDDAuthenticationAbstract.Commit: TCQRSResult;
begin
  Reset(result);

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

end.
