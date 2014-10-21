/// implements MVC patterns over mORMot's ORM/SOA and SynMustache
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotMVC;

{
    This file is part of Synopse mORMot framework.

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

  
  Version 1.18
  - first public release, corresponding to mORMot Framework 1.18
    and feature request [bd94c11ab1]

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  SysUtils,
  Classes,
  Variants,
  SynCommons,
  SynCrypto,
  SynMustache,
  mORMot;

type

  { ====== Views ====== }

  /// define a particular rendered View
  // - as rendered by TMVCViewsAbtract.Render() method
  TMVCView = record
    /// the low-level content of this View
    Content: RawByteString;
    /// the MIME content type of this View
    ContentType: RawUTF8;
  end;

  /// an abstract class able to implement Views
  TMVCViewsAbtract = class
  protected
    fFactory: TInterfaceFactory;
    fLogClass: TSynLogClass;
    fViewTemplateFolder: TFileName;
    fFactoryErrorIndex: integer;
    /// overriden implementations should return the rendered content
    procedure Render(methodIndex: Integer; const Context: variant; var View: TMVCView); virtual; abstract;
  public
    /// initialize the class
    constructor Create(aInterface: PTypeInfo; aLogClass: TSynLogClass);
    /// read-only access to the associated factory for the implementation class
    property Factory: TInterfaceFactory read fFactory;
    /// read-only access to the local folder containing the Mustache views
    property ViewTemplateFolder: TFileName read fViewTemplateFolder;
  end;

  /// general parameters defining the Mustache Views process
  // - used as a separate value so that we would be able to store the
  // settings in a file, e.g. encoded as a JSON object
  TMVCViewsMustacheParameters = record
    /// where the mustache template files are stored
    // - if not set, will search in a 'Views' folder under the current executable
    Folder: TFileName;
    /// the file extensions to search in the given Folder, specified as CSV
    // - if not set, will search for 'html,json,css'
    CSVExtensions: TFileName;
    /// defines if the view files should be checked for modification
    // - default 0 would be slightly faster, since content would never be checked
    // - any value would automatically update the rendering template, if the file
    // changed after a given number of seconds
    FileTimestampMonitorAfterSeconds: cardinal;
    /// file extension (e.g. '.html') to be used to create void templates
    // - default '' will create no void template file in the given Folder
    ExtensionForNotExistingTemplate: TFileName;
  end;

  /// a class able to implement Views using Mustache templates
  TMVCViewsMustache = class(TMVCViewsAbtract)
  protected
    fViewTemplateFileTimestampMonitor: cardinal;
    fViewPartials: TSynMustachePartials;
    fViews: array of record // follows fFactory.Methods[]
      Mustache: TSynMustache;
      Template: RawUTF8;
      MethodName: TFileName;
      SearchPattern: TFileName;
      FileName: TFileName;
      ShortFileName: TFileName;
      FileExt: TFileName;
      ContentType: RawUTF8;
      FileTimeStamp: TDateTime;
      FileTimeStampCheckTick: Int64;
    end;
    /// overriden implementations should return the rendered content
    procedure Render(methodIndex: Integer; const Context: variant; var View: TMVCView); override;
  public
    /// create an instance of this ViewModel implementation class
    // - define the associated REST instance, the interface definition and the
    // local folder where the mustache template files are stored
    // - will search and parse the matching views (and associated *.partial)
    constructor Create(aInterface: PTypeInfo;
      const aParameters: TMVCViewsMustacheParameters;
      aLogClass: TSynLogClass=nil); reintroduce; overload; virtual;
    /// create an instance of this ViewModel implementation class
    // - this overloaded version will use default parameters (i.e. search for
    // html+json+css in the "Views" sub-folder under the executable)
    // - will search and parse the matching views (and associated *.partial),
    // optionally creating void templates for any missing view
    constructor Create(aInterface: PTypeInfo; aLogClass: TSynLogClass=nil;
      aExtensionForNotExistingTemplate: TFileName=''); overload;
    /// finalize the instance
    destructor Destroy; override;
  end;

  
  { ====== Sessions ====== }

  /// an abstract class able to implement ViewModel/Controller sessions
  // - see TMVCSessionWithCookies to implement cookie-based sessions
  TMVCSessionAbstract = class
  public
    /// create an instance of this ViewModel implementation class
    constructor Create; virtual;
    /// will create a new session
    // - setting an optional record data, and returning the internal session ID
    // - you can supply a time period, after which the session will expire -
    // default is 1 hour - note that overriden methods may not implement it
    function Initialize(PRecordData: pointer=nil; PRecordTypeInfo: pointer=nil;
      SessionTimeOut: TDateTime=1/24): integer; virtual; abstract;
    /// fast check if there is a session associated to the current context
    function Exists: boolean; virtual; abstract;
    /// retrieve the current session ID
    // - can optionally retrieve the associated record Data parameter
    function CheckAndRetrieve(PRecordData: pointer=nil; PRecordTypeInfo: pointer=nil): integer; virtual; abstract;
    /// retrieve the session information as a JSON object
    // - returned as a TDocVariant, including any associated record Data
    function CheckAndRetrieveInfo(PRecordDataTypeInfo: pointer): variant; virtual; abstract;
    /// clear the session
    procedure Finalize; virtual; abstract;
  end;

  /// a class able to implement ViewModel/Controller sessions with cookies
  // - this kind of ViewModel will implement cookie-based sessions, able to
  // store any (simple) record content in the cookie, on the browser client side
  TMVCSessionWithCookies = class(TMVCSessionAbstract)
  protected
    fSessionCount: integer;
    fCookieName: RawUTF8;
    function GetCookie: RawUTF8; virtual; abstract;
    procedure SetCookie(const cookie: RawUTF8); virtual; abstract;
  public
    /// create an instance of this ViewModel implementation class
    constructor Create; override;
    /// will initialize the session cookie
    // - setting an optional record data, which will be stored Base64-encoded
    // - will return the internal session ID
    // - you can supply a time period, after which the session will expire -
    // default is 1 hour
    function Initialize(PRecordData: pointer=nil; PRecordTypeInfo: pointer=nil;
      SessionTimeOut: TDateTime=1/24): integer; override;
    /// fast check if there is a cookie session associated to the current context
    function Exists: boolean; override;
    /// retrieve the session ID from the current cookie
    // - can optionally retrieve the record Data parameter stored in the cookie
    function CheckAndRetrieve(PRecordData: pointer=nil; PRecordTypeInfo: pointer=nil): integer; override;
    /// retrieve the session information as a JSON object from the current cookie
    // - returned as a TDocVariant, including any associated record Data
    function CheckAndRetrieveInfo(PRecordDataTypeInfo: pointer): variant; override;
    /// clear the session
    procedure Finalize; override;
  end;

  /// implement a ViewModel/Controller sessions in a TSQLRestServer instance
  // - will use ServiceContext.Request threadvar to access the client cookies
  TMVCSessionWithRestServer = class(TMVCSessionWithCookies)
  protected
    function GetCookie: RawUTF8; override;
    procedure SetCookie(const cookie: RawUTF8); override;
  end;

  /// implement a single ViewModel/Controller in-memory session
  // - this kind of session could be used in-process, e.g. for a VCL/FMX GUI
  // - do NOT use it with multiple clients, e.g. from HTTP remote access
  TMVCSessionSingle = class(TMVCSessionWithCookies)
  protected
    fSingleCookie: RawUTF8;
    function GetCookie: RawUTF8; override;
    procedure SetCookie(const cookie: RawUTF8); override;
  end;

  { ====== Application Run ====== }

  /// record type to define commands e.g. to redirect to another URI
  // - do NOT access those record property directly, but rather use
  // TMVCApplication.GotoView/GotoError/GotoDefault methods, e.g.
  // !  function TBlogApplication.Logout: TMVCAction;
  // !  begin
  // !    CurrentSession.Finalize;
  // !    GotoDefault(result);
  // !  end;
  // - this record type should match exactly TServiceCustomAnswer layout,
  // so that TServiceMethod.InternalExecute() would handle it directly
  TMVCAction = record
    /// the method name to be executed
    RedirectToMethodName: RawUTF8;
    /// may contain a JSON object which will be used to specify parameters
    // to the specified method
    RedirectToMethodParameters: RawUTF8;
    /// which HTML status code should be returned
    // - if RedirectMethodName is set, will return 307 HTML_TEMPORARYREDIRECT
    // by default, but you can set here the expected HTML status code, e.g.
    // 201 HTML_CREATED or 404 HTML_NOTFOUND
    ReturnedStatus: cardinal;
  end;

  TMVCApplication = class;

  /// abtract MVC rendering execution context
  // - you shoud not execute this abstract class, but any of the inherited class
  // - one instance inherited from this class would be allocated for each event 
  // - may return some data (when inheriting from TMVCRendererReturningData), or
  // even simply display the value in a VCL/FMX GUI, without any output
  TMVCRendererAbstract = class
  protected
    fApplication: TMVCApplication;
    fMethodIndex: integer;
    fMethodReturnsAction: boolean;
    fInput: RawUTF8;
    procedure Renders(outContext: variant; status: cardinal;
      forcesError: boolean); virtual; abstract;
    procedure CommandError(const ErrorName: RawUTF8; const ErrorValue: variant;
      ErrorCode: Integer); virtual;
  public
    /// initialize a rendering process for a given MVC Application/ViewModel
    constructor Create(aApplication: TMVCApplication); reintroduce;
    /// main execution method of the rendering process
    // - Input should have been set with the incoming execution context
    procedure ExecuteCommand(aMethodIndex: integer); virtual;
    /// incoming execution context, to be processed via ExecuteCommand() method
    // - should be specified as a raw JSON object
    property Input: RawUTF8 read fInput write fInput;
  end;

  /// how TMVCRendererReturningData should cache its content
  TMVCRendererCachePolicy = (
    cacheNone,
    cacheRootIgnoringSession, cacheRootIfSession, cacheRootIfNoSession,
    cacheRootWithSession,
    cacheWithParametersIgnoringSession, cacheWithParametersIfSession,
    cacheWithParametersIfNoSession);

  TMVCRunWithViews = class;

  /// abstract MVC rendering execution context, returning some content
  // - the Output property would contain the content to be returned
  // - can be used to return e.g. some rendered HTML or some raw JSON
  TMVCRendererReturningData = class(TMVCRendererAbstract)
  protected
    fRun: TMVCRunWithViews;
    fOutput: TServiceCustomAnswer;
    fCacheCurrent: (noCache, rootCache, inputCache);
    fCacheCurrentSec: cardinal;
    fCacheCurrentInputValueKey: RawUTF8;
  public
    /// initialize a rendering process for a given MVC Application/ViewModel
    // - you need to specify a MVC Views engine, e.g. TMVCViewsMustache instance
    constructor Create(aRun: TMVCRunWithViews); reintroduce;
    /// main execution method of the rendering process
    // - this overriden method would handle proper caching as defined by
    // TMVCRunWithViews.SetCache()
    procedure ExecuteCommand(aMethodIndex: integer); override;
    /// caller should retrieve this value after ExecuteCommand method execution
    property Output: TServiceCustomAnswer read fOutput;
  end;

  TMVCRendererReturningDataClass = class of TMVCRendererReturningData;

  /// MVC rendering execution context, returning some rendered View content
  // - will use an associated Views templates system, e.g. a Mustache renderer
  TMVCRendererFromViews = class(TMVCRendererReturningData)
  protected
    // Renders() will fill Output using the corresponding View, to be sent back
    procedure Renders(outContext: variant; status: cardinal; forcesError: boolean); override;
  end;

  /// MVC rendering execution context, returning some un-rendered JSON content
  // - may be used e.g. for debugging purpose
  // - for instance, TMVCRunOnRestServer will return such context with the
  // supplied URI ends with '/json' (e.g. for any /root/method/json request)
  TMVCRendererJson = class(TMVCRendererReturningData)
  protected
    // Renders() will fill Output with the outgoing JSON, to be sent back
    procedure Renders(outContext: variant; status: cardinal; forcesError: boolean); override;
  end;

  /// abstract class used by TMVCApplication to run
  // - a single TMVCApplication logic may handle several TMVCRun instances
  TMVCRun = class
  protected
    fApplication: TMVCApplication;
  public
    /// link this runner class to a specified MVC application
    // - will also reset the associated Application.Session instance
    constructor Create(aApplication: TMVCApplication); reintroduce;
    /// method called to flush the caching mechanism for all MVC commands
    procedure NotifyContentChanged; virtual;
    /// you may call this method to flush any caching mechanism for a MVC command
    procedure NotifyContentChangedForMethod(aMethodIndex: integer); overload; virtual;
    /// you may call this method to flush any caching mechanism for a MVC command
    procedure NotifyContentChangedForMethod(const aMethodName: RawUTF8); overload;
    /// read-write access to the associated MVC Application/ViewModel instance
    property Application: TMVCApplication read fApplication write fApplication;
  end;

  /// abstract class used by TMVCApplication to run TMVCViews-based process
  // - this inherited class will host a MVC Views instance, and handle
  // an optional simple in-memory cache
  TMVCRunWithViews = class(TMVCRun)
  protected
    fViews: TMVCViewsAbtract;
    fCache: array of record
      Policy: TMVCRendererCachePolicy;
      TimeOutSeconds: cardinal;
      RootValue: RawUTF8;
      RootValueExpirationTime: cardinal;
      InputValues: TSynNameValue;
    end;
  public
    /// link this runner class to a specified MVC application
    constructor Create(aApplication: TMVCApplication;
      aViews: TMVCViewsAbtract=nil); reintroduce;
    /// method called to flush the caching mechanism for a MVC command
    procedure NotifyContentChangedForMethod(aMethodIndex: integer); override;
    /// defines the caching policy for a given MVC command
    // - a time expiration period (up to 5 minutes) can also be defined per
    // MVC command - leaving default 0 will set to 5 minutes expiration delay
    // - function calls can be chained to create some fluent definition interface
    // like in TAnyBLogapplication.Create:
    // ! fMainRunner := TMVCRunWithViews.Create(self).SetCache('default',cacheRoot);
    function SetCache(const aMethodName: RawUTF8; aPolicy: TMVCRendererCachePolicy;
      aTimeOutSeconds: cardinal=0): TMVCRunWithViews; virtual;
    /// finalize this instance
    destructor Destroy; override;
    /// read-write access to the associated MVC Views instance
    property Views: TMVCViewsAbtract read fViews;
  end;


  /// run TMVCApplication directly within a TSQLRestServer method-based service
  // - this is the easiest way to host and publish a MVC Application, optionally
  // in conjunction with REST/AJAX client access
  TMVCRunOnRestServer = class(TMVCRunWithViews)
  protected
    fRestServer: TSQLRestServer;
    fPublishMvcInfo: boolean;
    fMvcInfoCache: RawUTF8;
    /// callback used for the rendering on the TSQLRestServer
    procedure RunOnRestServerRoot(Ctxt: TSQLRestServerURIContext);
    procedure RunOnRestServerSub(Ctxt: TSQLRestServerURIContext);
    procedure InternalRunOnRestServer(Ctxt: TSQLRestServerURIContext;
      const MethodName: RawUTF8);
  public
    /// this constructor will publish some views to a TSQLRestServer instance
    // - the associated RestModel can match the supplied TSQLRestServer, or be
    // another instance (if the data model is not part of the publishing server)
    // - all TMVCApplication methods would be registered to the TSQLRestServer,
    // as /root/methodName if aSubURI is '', or as /root/aSubURI/methodName
    // - if aApplication has no Views instance associated, this constructor will
    // initialize a Mustache renderer in its default folder, with '.html' void
    // template generation
    // - will also create a TMVCSessionWithRestServer for simple cookie sessions
    // - aPublishMvcInfo=TRUE will define a /root/[aSubURI/]mvc-info HTML page,
    // which is pretty convenient when working with views
    constructor Create(aApplication: TMVCApplication;
      aRestServer: TSQLRestServer=nil; const aSubURI: RawUTF8='';
      aViews: TMVCViewsAbtract=nil; aPublishMvcInfo: boolean=true); reintroduce;
  end;


  { ====== Application / ViewModel ====== }

  /// Exception class triggerred by mORMot MVC/MVVM applications internally
  // - those error are internal fatal errors of the server side process
  EMVCException = class(ESynException);

  /// Exception class triggerred by mORMot MVC/MVVM applications externally
  // - those error are external errors which should be notified to the client
  // - can be used to change the default view, e.g. on application error
  EMVCApplication = class(ESynException)
  protected
    fAction: TMVCAction;
  public
    /// same as calling TMVCApplication.GotoView()
    constructor CreateGotoView(const aMethod: RawUTF8;
      const aParametersNameValuePairs: array of const);
    /// same as calling TMVCApplication.GotoError()
    constructor CreateGotoError(const aErrorMessage: string;
      ErrorCode: integer=HTML_BADREQUEST); overload;
    /// same as calling TMVCApplication.GotoError()
    constructor CreateGotoError(aHtmlErrorCode: integer); overload;
    /// same as calling TMVCApplication.GotoDefault
    constructor CreateDefault;
  end;

  /// defines the main and error pages for the ViewModel of one application
  IMVCApplication = interface(IInvokable)
    ['{C48718BF-861B-448A-B593-8012DB51E15D}']
    /// the default main page
    // - whole data context is retrieved and returned as a TDocVariant
    procedure Default(var Scope: variant);
    /// the error page
    // - in addition to the error message, a whole data context is retrieved
    // and returned as a TDocVariant
    procedure Error(var Msg: RawUTF8; var Scope: variant);
  end;

  /// parent class to implement a MVC/MVVM application
  // - you should inherit from this class, then implement an interface inheriting
  // from IMVCApplication to define the various commands of the application
  // - here the Model would be a TSQLRest instance, Views will be defined by
  // TMVCViewsAbtract (e.g. TMVCViewsMustache), and the ViewModel/Controller
  // will be implemented with IMVCApplication methods of the inherited class
  TMVCApplication = class(TInterfacedObject)
  protected
    fFactory: TInterfaceFactory;
    fFactoryEntry: pointer;
    fFactoryErrorIndex: integer;
    fSession: TMVCSessionAbstract;
    fRestModel: TSQLRest;
    fRestServer: TSQLRestServer;
    // if any TMVCRun instance is store here, will be freed by Destroy
    // but note that a single TMVCApplication logic may handle several TMVCRun
    fMainRunner: TMVCRun;
    procedure SetSession(const Value: TMVCSessionAbstract);
    /// to be called when the data model did change to force content re-creation
    // - this default implementation will call fMainRunner.NotifyContentChanged
    procedure FlushAnyCache; virtual;
    /// generic IMVCApplication implementation
    procedure Error(var Msg: RawUTF8; var Scope: variant);
    /// every view will have this data context transmitted as "main":...
    function GetViewInfo(MethodIndex: integer): variant; virtual;
    /// compute the data context e.g. for the /mvc-info URI
    function GetMvcInfo: variant; virtual;
    /// wrappers to redirect to IMVCApplication standard methods
    class procedure GotoView(var Action: TMVCAction; const MethodName: string;
      const ParametersNameValuePairs: array of const; status: cardinal=0);
    class procedure GotoError(var Action: TMVCAction; const Msg: string;
      ErrorCode: integer=HTML_BADREQUEST); overload;
    class procedure GotoError(var Action: TMVCAction; ErrorCode: integer); overload;
    class procedure GotoDefault(var Action: TMVCAction);
  public
    /// create an instance of the MVC/MVVM application
    // - define the associated REST instance, and the interface definition for
    // application commands
    constructor Create(aRestModel: TSQLRest; aInterface: PTypeInfo); reintroduce; virtual;
    /// finalize the application
    // - and release any associated CurrentSession, Views, and fMainRunner
    destructor Destroy; override;

    /// read-only access to the associated mORMot REST data Model
    property RestModel: TSQLRest read fRestModel;
    /// read-only access to the associated factory for IMVCApplication interface
    property Factory: TInterfaceFactory read fFactory;
    /// read-write access to the associated Session instance
    property CurrentSession: TMVCSessionAbstract read fSession write SetSession;
    /// read-write access to the main associated TMVCRun instance
    // - if any TMVCRun instance is stored here, will be freed by Destroy
    // - but note that a single TMVCApplication logic may handle several TMVCRun
    property MainRunner: TMVCRun read fMainRunner;
  end;

const
  /// the pseudo-method name for the MVC information html page
  MVCINFO_URI = 'mvc-info';


implementation


{ TMVCViewsAbtract }

constructor TMVCViewsAbtract.Create(aInterface: PTypeInfo; aLogClass: TSynLogClass);
begin
  inherited Create;
  fFactory := TInterfaceFactory.Get(aInterface);
  fFactoryErrorIndex := fFactory.FindMethodIndex('Error');
  if aLogClass=nil then
    fLogClass := TSQLLog else
    fLogClass := aLogClass;
end;


{ TMVCViewsMustache }

const
  MUSTACHE_METHODPARTIAL =
  '{{<method}}{{verb}} {{methodName}}{{#asInParams}}({{#args}}{{^dirResult}}'+
  '{{dirName}} {{argName}}: {{typeDelphi}}{{commaArg}}{{/dirResult}}{{/args}})'+
  '{{/asInParams}}{{#args}}{{#dirResult}}: {{typeDelphi}}{{/dirResult}}'+
  '{{/args}};{{/method}}';

  MUSTACHE_VOIDVIEW = MUSTACHE_METHODPARTIAL+
  '<<! void template created for the {{interfaceName}}.{{methodName}} View:'#13#10+
  ' defined as'#13#10'   {{>method}}'#13#10' with the following data context:'#13#10+
  '   * Main: variant'#13#10'{{#args}}{{#dirOutput}}   * {{argName}}: {{typePascal}}'+
  #13#10'{{/dirOutput}}{{/args}}>>'#13#10;

  MUSTACHE_MVCINFO = MUSTACHE_METHODPARTIAL+
  '{{<url}}/{{root}}/{{methodName}}{{#asInParams}}?'+
  '{{#args}}{{#dirInput}}{{argName}}=</b>..[{{typePascal}}]..<b>'+
  '{{#commaInSingle}}&{{/commaInSingle}}{{/dirInput}}{{/args}}{{/asInParams}}{{/url}}'+
  '{{<mustache}}<b>&#123;{Main&#125;}</b>: variant{{#args}}<br><b>{&#123;'+
  '{{argName}}&#125;}</b>: {{typePascal}}{{/args}}{{/mustache}}'+
  '<html><head><title>{{Name}} Information</title></head><body '+
  'style="font-family:Verdana;"><h1>{{Name}} mORMotMVC Information</h1>'+
  '<p><strong>Generated by a <i>mORMot</i> {{mORMot}} server</strong><br>'+
  '<small>&copy;Synopse Informatique - <a href=http://synopse.info>'+
  'http://synopse.info</a></small></p><h2>Controller Definition</h2>'+
  '<p>Registered interface is:</p><pre>'#13#10+
  '  I{{name}} = interface(IInvokable)'#13#10'{{#methods}}'#13#10+
  '    {{>method}}'#13#10'{{/methods}}'#13#10'  end;'#13#10+
  '</pre><p>Use this page as reference when writing your <a href=http://blog.synopse.info'+
  '/post/2014/04/28/Mustache-Logic-less-templates-for-Delphi-part-1>Mustache</a> Views.</p>'+
  '<h2>Available Commands</h2><p>You can access the following commands:</p>'+
  '<ul>{{#methods}}<li><b>{{>url}}</b>{{/methods}}</ul><p>Any missing parameter '+
  'would be replaced by its default value.</p><h2>Available Views</h2>'+
  '<p>The following views are defined, with expected data context:</p><ul>'+
  '{{#methods}}{{^resultIsServiceCustomAnswer}}<li><b>{{>url}}</b><p>{{>mustache}}'+
  '</p></li>{{/resultIsServiceCustomAnswer}}{{/methods}}</ul><p>'+
  'Currently, all views are located in the <code>{{viewsFolder}}</code> folder.</p>';

  MUSTACHE_DEFAULTERROR =
  '<html><head><title>mORMotMVC Error</title></head><body style='+
  '"font-family:Verdana;"><h1>mORMotMVC Default Error Page</h1><p>A <code>'+
  '{{exceptionName}}</code> exception did raise during {{className}} process '+
  'with the following message:</p><pre>{{exceptionMessage}}</pre><p>'+
  'Triggered with the following context:</p><pre>{{originalErrorContext}}</pre>';


function MethodHasView(const aMethod: TServiceMethod): boolean;
begin // any method returning a TMVCAction do not have any associated view
  result := (aMethod.ArgsResultIndex<0) or
            (aMethod.Args[aMethod.ArgsResultIndex].ValueType<>smvRecord) or
            (aMethod.Args[aMethod.ArgsResultIndex].TypeInfo<>TypeInfo(TMVCAction));
end;

constructor TMVCViewsMustache.Create(aInterface: PTypeInfo;
  const aParameters: TMVCViewsMustacheParameters; aLogClass: TSynLogClass);
var m: integer;
    LowerExt: TFileName;
    partialName: RawUTF8;
    info: variant;
    SR: TSearchRec;
begin
  inherited Create(aInterface,aLogClass);
  // get views
  fViewTemplateFileTimestampMonitor := aParameters.FileTimestampMonitorAfterSeconds;
  if aParameters.Folder='' then
    fViewTemplateFolder := ExtractFilePath(ParamStr(0))+'Views\' else
    fViewTemplateFolder := IncludeTrailingPathDelimiter(aParameters.Folder);
  if not DirectoryExists(fViewTemplateFolder) then
    CreateDir(fViewTemplateFolder);
  if aParameters.CSVExtensions='' then
    LowerExt := ',html,json,css,' else
    LowerExt := ','+SysUtils.LowerCase(aParameters.CSVExtensions)+',';
  SetLength(fViews,fFactory.MethodsCount);
  for m := 0 to fFactory.MethodsCount-1 do
  if MethodHasView(fFactory.Methods[m]) then
  with fViews[m] do begin
    MethodName := UTF8ToString(fFactory.Methods[m].URI);
    SearchPattern := fViewTemplateFolder+MethodName+'.*';
    if FindFirst(SearchPattern,faAnyFile-faDirectory,SR)=0 then
      try
        repeat
          FileExt := SysUtils.LowerCase(copy(ExtractFileExt(SR.Name),2,100));
          if Pos(','+FileExt+',',LowerExt)>0 then
            break; // found a template with the right extension
        until FindNext(SR)<>0;
        ShortFileName := SR.Name;
        FileName := fViewTemplateFolder+ShortFileName;
        ContentType := GetMimeContentType(nil,0,ShortFileName);
      finally
        FindClose(SR);
      end else begin
        fLogClass.Add.Log(
          sllWarning,'%.Create: Missing View file in %',[self,SearchPattern]);
        if aParameters.ExtensionForNotExistingTemplate<>'' then begin
          ShortFileName := MethodName+aParameters.ExtensionForNotExistingTemplate;
          FileName := fViewTemplateFolder+ShortFileName;
          info := fFactory.Methods[m].ContextFromArguments(nil);
          info.interfaceName := fFactory.InterfaceTypeInfo^.Name;
          FileFromString(StringReplaceChars(StringReplaceChars(
            TSynMustache.Parse(MUSTACHE_VOIDVIEW).Render(info),'<','{'),'>','}'),
            FileName);
        end;
      end;
  end;
  // get partials
  fViewPartials := TSynMustachePartials.Create;
  if FindFirst(fViewTemplateFolder+'*.partial',faAnyFile,SR)=0 then
  try
    repeat
      StringToUTF8(GetFileNameWithoutExt(SR.Name),partialName);
      try
        fViewPartials.Add(partialName,StringFromFile(fViewTemplateFolder+SR.Name));
      except
        on E: Exception do
          fLogClass.Add.Log(
            sllError,'%.Create: Invalid Partial file % - %',[self,SR.Name,E]);
      end;
    until FindNext(SR)<>0;
  finally
    FindClose(SR);
  end;
end;

constructor TMVCViewsMustache.Create(aInterface: PTypeInfo;
  aLogClass: TSynLogClass; aExtensionForNotExistingTemplate: TFileName);
var params: TMVCViewsMustacheParameters;
begin
  fillchar(params,sizeof(params),0);
  params.ExtensionForNotExistingTemplate := aExtensionForNotExistingTemplate;
  Create(aInterface,params,aLogClass);
end;

destructor TMVCViewsMustache.Destroy;
begin
  inherited;
  fViewPartials.Free;
end;

procedure TMVCViewsMustache.Render(methodIndex: Integer; const Context: variant;
  var View: TMVCView);
var age: TDateTime;
begin
  if cardinal(methodIndex)>=fFactory.MethodsCount then
    raise EMVCException.CreateUTF8('%.Render(methodIndex=%)',[self,methodIndex]);
  with fViews[methodIndex] do begin
    if (Mustache=nil) and (FileName='') then 
      raise EMVCException.CreateUTF8('%.Render(''%''): Missing Template in ''%''',
        [self,MethodName,SearchPattern]);
    if (Mustache=nil) or ((fViewTemplateFileTimestampMonitor<>0) and
       (FileTimeStampCheckTick<GetTickCount64)) then begin
      age := FileAgeToDateTime(FileName);
      if (Mustache=nil) or (age<>FileTimeStamp) then begin
        Mustache := nil;
        FileTimeStamp := age;
        Template := StringFromFile(FileName);
        if Template<>'' then
        try
          Mustache := TSynMustache.Parse(Template);
        except
          on E: Exception do
            raise EMVCException.CreateUTF8('%.Render(''%''): Invalid Template: % - %',
              [self,FileName,E,E.Message]);
        end else
          raise EMVCException.CreateUTF8('%.Render(''%''): Missing Template in ''%''',
            [self,ShortFileName,SearchPattern]);
        if fViewTemplateFileTimestampMonitor<>0 then
          FileTimeStampCheckTick := GetTickCount64+
            Int64(fViewTemplateFileTimestampMonitor)*Int64(1000);
      end;
    end;
    View.Content := Mustache.Render(Context,fViewPartials);
    if trim(View.Content)='' then begin
      Mustache := nil; // force reload ASAP
      raise EMVCException.CreateUTF8(
        '%.Render(''%''): Void Template - please put some content!',[self,ShortFileName]);
    end;
    View.ContentType := ContentType;
  end;
end;


{ TMVCSessionAbstract }

constructor TMVCSessionAbstract.Create;
begin
  inherited;
end;


{ TMVCSessionWithCookies }

constructor TMVCSessionWithCookies.Create;
begin
  inherited Create;
  fCookieName := 'mORMot'+copy(SHA256(
    FormatUTF8('%%%',[self,GetTickCount64,MainThreadID])),1,14);
end;

function TMVCSessionWithCookies.Exists: boolean;
begin
  result := GetCookie<>'';
end;

// Cookie is session_expires_________optionalrecord___crc_____
//           UInt32  TTimeLog        ..base64..       UInt32
//           1       9               25               len-8

function TMVCSessionWithCookies.CheckAndRetrieve(
  PRecordData,PRecordTypeInfo: pointer): integer;
var cookie: RawUTF8;
    cookieLen, crc, sessionID: cardinal;
    Expires: TTimeLog;
begin
  cookie := GetCookie;
  cookieLen := length(cookie);
  if (cookieLen>=32) and // check cookie standard info
     HexDisplayToCardinal(@cookie[cookieLen-7],crc) and
     (crc32c(PtrUInt(self),pointer(cookie),cookieLen-8)=crc) and
     HexDisplayToCardinal(pointer(cookie),sessionID) and
     (sessionID<=cardinal(fSessionCount)) and
     HexDisplayToBin(@cookie[9],@Expires,sizeof(Expires)) and
     (Expires>=TimeLogNowUTC) then
    if PRecordData=nil then
      result := sessionID else
      if (PRecordTypeInfo<>nil) and (cookieLen>32) and
         RecordLoadBase64(@cookie[25],cookieLen-32,PRecordData^,PRecordTypeInfo,true) then
        result := sessionID else
        result := 0 else
    result := 0;
end;

function TMVCSessionWithCookies.CheckAndRetrieveInfo(
  PRecordDataTypeInfo: pointer): variant;
var rec: TByteDynArray; // to store locally any kind of record
    recJSON: RawUTF8;
    sessionID: integer;
begin
  SetLength(rec,RecordTypeInfoSize(PRecordDataTypeInfo));
  try
    SetVariantNull(result);
    sessionID := CheckAndRetrieve(pointer(rec),PRecordDataTypeInfo);
    if sessionID=0 then
      exit;
    if rec<>nil then begin
      recJSON := RecordSaveJSON(pointer(rec)^,PRecordDataTypeInfo);
      result := _JsonFast(recJSON);
    end;
    _ObjAddProps(['id',sessionID],result);
  finally
    if rec<>nil then // manual finalization of managed fields
      RecordClear(pointer(rec)^,PRecordDataTypeInfo);
  end;
end;

function TMVCSessionWithCookies.Initialize(
  PRecordData,PRecordTypeInfo: pointer; SessionTimeOut: TDateTime): integer;
var Expires: TTimeLogBits;
    cookie: RawUTF8;
begin
  result := InterlockedIncrement(fSessionCount);
  Expires.From(NowUTC+SessionTimeOut);
  cookie := CardinalToHex(result)+Int64ToHex(Expires.Value);
  if (PRecordData<>nil) and (PRecordTypeInfo<>nil) then
    cookie := cookie+RecordSaveBase64(PRecordData^,PRecordTypeInfo,true);
  cookie := cookie+CardinalToHex(crc32c(PtrUInt(self),pointer(cookie),length(cookie)));
  SetCookie(cookie);
end;

procedure TMVCSessionWithCookies.Finalize;
begin
  SetCookie(COOKIE_EXPIRED);
end;


{ TMVCSessionWithRestServer }

function TMVCSessionWithRestServer.GetCookie: RawUTF8;
begin
  result := ServiceContext.Request.InCookie[fCookieName];
end;

procedure TMVCSessionWithRestServer.SetCookie(const cookie: RawUTF8);
begin
  ServiceContext.Request.OutSetCookie := fCookieName+'='+cookie;
  ServiceContext.Request.InCookie[fCookieName] := cookie;
end;


{ TMVCSessionSingle }

function TMVCSessionSingle.GetCookie: RawUTF8;
begin
  result := fSingleCookie;
end;

procedure TMVCSessionSingle.SetCookie(const cookie: RawUTF8);
begin
  fSingleCookie := cookie;
end;


{ EMVCApplication }

constructor EMVCApplication.CreateDefault;
begin
  TMVCApplication.GotoDefault(fAction);
end;

constructor EMVCApplication.CreateGotoError(const aErrorMessage: string;
  ErrorCode: integer);
begin
  TMVCApplication.GotoError(fAction,aErrorMessage,ErrorCode);
end;

constructor EMVCApplication.CreateGotoError(aHtmlErrorCode: integer);
begin
  TMVCApplication.GotoError(fAction,aHtmlErrorCode);
end;

constructor EMVCApplication.CreateGotoView(const aMethod: RawUTF8;
  const aParametersNameValuePairs: array of const);
begin
  TMVCApplication.GotoView(fAction,aMethod,aParametersNameValuePairs);
end;


{ TMVCApplication }

constructor TMVCApplication.Create(aRestModel: TSQLRest; aInterface: PTypeInfo);
var m: integer;
    entry: PInterfaceEntry;
begin
  inherited Create;
  fRestModel := aRestModel;
  fFactory := TInterfaceFactory.Get(aInterface);
  fFactoryErrorIndex := fFactory.FindMethodIndex('Error');
  if fFactoryErrorIndex<0 then
    raise EMVCException.CreateUTF8(
      '% does not implement the IMVCApplication.Error() method',[aInterface.Name]);
  entry := GetInterfaceEntry(fFactory.InterfaceIID);
  if entry=nil then
    raise EMVCException.CreateUTF8('%.Create: this class should implement %',
      [self,fFactory.InterfaceTypeInfo^.Name]);
  fFactoryEntry := PAnsiChar(self)+entry^.IOffset;
  for m := 0 to fFactory.MethodsCount-1 do
    if not MethodHasView(fFactory.Methods[m]) then
    with fFactory.Methods[m] do
      if ArgsOutFirst<>ArgsResultIndex then
        raise EMVCException.CreateUTF8(
          '%.Create: %.% var/out parameters not allowed with TMVCAction result',
          [self,fFactory.InterfaceTypeInfo^.Name,URI]) else
        // TServiceCustomAnswer maps TMVCAction in TMVCApplication.RunOnRestServer
        ArgsResultIsServiceCustomAnswer := true;
end;

destructor TMVCApplication.Destroy;
begin
  inherited;
  fMainRunner.Free;
  fSession.Free;
end;

procedure TMVCApplication.Error(var Msg: RawUTF8; var Scope: variant);
begin // do nothing: just pass input error Msg and data Scope to the view
end;

class procedure TMVCApplication.GotoView(var Action: TMVCAction; const MethodName: string;
  const ParametersNameValuePairs: array of const; status: cardinal);
begin
  Action.ReturnedStatus := status;
  Action.RedirectToMethodName := MethodName;
  if high(ParametersNameValuePairs)<1 then
    Action.RedirectToMethodParameters := '' else
    Action.RedirectToMethodParameters := JSONEncode(ParametersNameValuePairs);
end;

class procedure TMVCApplication.GotoError(var Action: TMVCAction;
  const Msg: string; ErrorCode: integer);
begin
  GotoView(Action,'Error',['Msg',Msg],ErrorCode);
end;

class procedure TMVCApplication.GotoError(var Action: TMVCAction;
  ErrorCode: integer);
begin
  if ErrorCode<=HTML_CONTINUE then
    ErrorCode := HTML_BADREQUEST;
  GotoView(Action,'Error',['Msg',StatusCodeToErrorMsg(ErrorCode)],ErrorCode);
end;

class procedure TMVCApplication.GotoDefault(var Action: TMVCAction);
begin
  Action.ReturnedStatus := 0;
  Action.RedirectToMethodName := 'Default';
  Action.RedirectToMethodParameters := '';
end;

procedure TMVCApplication.SetSession(const Value: TMVCSessionAbstract);
begin
  FreeAndNil(fSession);
  fSession := Value;
end;

function TMVCApplication.GetViewInfo(MethodIndex: integer): variant;
begin
  result := _ObjFast(['pageName',fFactory.Methods[MethodIndex].URI]);
end;

function TMVCApplication.GetMvcInfo: variant;
begin
  Result := _ObjFast(['name',fFactory.InterfaceTypeInfo^.Name,
    'mORMot',SYNOPSE_FRAMEWORK_VERSION,'root',RestModel.Model.Root,
    'methods',fFactory.ContextFromMethods(nil)]);
end;

procedure TMVCApplication.FlushAnyCache;
begin
  if fMainRunner<>nil then
    fMainRunner.NotifyContentChanged;
end;


{ TMVCRendererAbstract }

constructor TMVCRendererAbstract.Create(aApplication: TMVCApplication);
begin
  fApplication := aApplication;
end;

procedure TMVCRendererAbstract.CommandError(const ErrorName: RawUTF8;
  const ErrorValue: variant; ErrorCode: Integer);
var renderContext: variant;
begin
  renderContext := _ObjFast([
    'main',fApplication.GetViewInfo(fMethodIndex),
    'msg',StatusCodeToErrorMsg(ErrorCode),
    'errorCode',ErrorCode,ErrorName,ErrorValue]);
  renderContext.originalErrorContext := JSONReformat(VariantToUTF8(renderContext));
  Renders(renderContext,ErrorCode,true);
end;

procedure TMVCRendererAbstract.ExecuteCommand(aMethodIndex: integer);
var action: TMVCAction;
    isAction: boolean;
    WR: TTextWriter;
    methodOutput: RawUTF8;
    renderContext: variant;
begin
  action.ReturnedStatus := HTML_SUCCESS;
  fMethodIndex := aMethodIndex;
  try
    if fMethodIndex>=0 then begin
      repeat
        try
          isAction := fApplication.fFactory.Methods[fMethodIndex].ArgsResultIsServiceCustomAnswer;
          WR := TJSONWriter.CreateOwnedStream;
          try
            WR.Add('{');
            with fApplication.fFactory do
            if not Methods[fMethodIndex].InternalExecute([fApplication.fFactoryEntry],
               pointer(fInput),WR,action.RedirectToMethodName,action.ReturnedStatus,
               [optVariantCopiedByReference],true,nil) then
              raise EMVCException.CreateUTF8('%.CommandRunMethod: %.%() execution error',
                [Self,InterfaceTypeInfo^.Name,Methods[fMethodIndex].URI]);
            if not isAction then
              WR.Add('}');
            WR.SetText(methodOutput);
          finally
            WR.Free;
          end;
          if isAction then
            // was a TMVCAction mapped in a TServiceCustomAnswer record
            action.RedirectToMethodParameters := methodOutput else begin
            // rendering, e.g. with fast Mustache {{template}}
            renderContext := _JsonFast(methodOutput);
            TDocVariantData(renderContext).AddValue(
              'main',fApplication.GetViewInfo(fMethodIndex));
            if fMethodIndex=fApplication.fFactoryErrorIndex then
              _ObjAddProps(['errorCode',action.ReturnedStatus,
                'originalErrorContext',JSONReformat(VariantToUTF8(renderContext))],
                renderContext);
            Renders(renderContext,action.ReturnedStatus,false);
            exit; // success
          end;
        except
          on E: EMVCApplication do
            action := E.fAction;
        end; // lower level exceptions will be handled below
        fInput := action.RedirectToMethodParameters;
        fMethodIndex := fApplication.fFactory.FindMethodIndex(action.RedirectToMethodName);
        if action.ReturnedStatus=0 then
          action.ReturnedStatus := HTML_SUCCESS;
      until fMethodIndex<0;
    end;
    // if we reached here, there was a wrong URI -> render the 404 error page
    CommandError('notfound',true,HTML_NOTFOUND);
  except
    on E: Exception do
      CommandError('exception',FormatUTF8('% raised in %.ExecuteCommand: %',
        [E,self,E.Message]),HTML_SERVERERROR);
  end;
end;


{ TMVCRendererFromViews }

procedure TMVCRendererFromViews.Renders(outContext: variant;
  status: cardinal; forcesError: boolean);
var view: TMVCView;
begin
  if forcesError or (fMethodIndex=fRun.fViews.fFactoryErrorIndex) then
    try // last change rendering of the error page
      fRun.fViews.Render(fRun.fViews.fFactoryErrorIndex,outContext,view);
    except // fallback to default HTML error template, if current did not work
      on E: Exception do begin
        _ObjAddProps(['exceptionName',E.ClassName,
          'exceptionMessage',E.Message,'className',ClassName],outContext);
        view.Content := TSynMustache.Parse(MUSTACHE_DEFAULTERROR).Render(outContext);
        view.ContentType := HTML_CONTENT_TYPE;
      end;
    end else
      fRun.fViews.Render(fMethodIndex,outContext,view);
  fOutput.Content := view.Content;
  fOutput.Header := HEADER_CONTENT_TYPE+view.ContentType;
  fOutput.Status := status;
end;


{ TMVCRendererJson }

procedure TMVCRendererJson.Renders(outContext: variant;
  status: cardinal; forcesError: boolean);
begin
  fOutput.Content := JSONReformat(VariantToUTF8(outContext));
  fOutput.Header := JSON_CONTENT_TYPE_HEADER;
  fOutput.Status := status;
end;


{ TMVCRun }

constructor TMVCRun.Create(aApplication: TMVCApplication);
begin
  fApplication := aApplication;
  fApplication.SetSession(nil);
end;

procedure TMVCRun.NotifyContentChangedForMethod(aMethodIndex: integer);
begin // do nothing at this abstract level
end;

procedure TMVCRun.NotifyContentChanged;
var m: integer;
begin
  for m := 0 to fApplication.fFactory.MethodsCount-1 do
    NotifyContentChangedForMethod(m)
end;

procedure TMVCRun.NotifyContentChangedForMethod(const aMethodName: RawUTF8);
begin
  NotifyContentChangedForMethod(fApplication.fFactory.FindMethodIndex(aMethodName));
end;


{ TMVCRunWithViews }

constructor TMVCRunWithViews.Create(aApplication: TMVCApplication;
  aViews: TMVCViewsAbtract);
begin
  inherited Create(aApplication);
  fViews := aViews;
end;

function TMVCRunWithViews.SetCache(const aMethodName: RawUTF8;
  aPolicy: TMVCRendererCachePolicy; aTimeOutSeconds: cardinal): TMVCRunWithViews;
const MAX_CACHE_TIMEOUT = 60*15; // 15 minutes
var aMethodIndex: integer;
begin
  aMethodIndex := fApplication.fFactory.CheckMethodIndex(aMethodName);
  if fCache=nil then
    SetLength(fCache,fApplication.fFactory.MethodsCount);
  with fCache[aMethodIndex] do begin
    Policy := aPolicy;
    if aTimeOutSeconds-1>=MAX_CACHE_TIMEOUT then
      TimeOutSeconds := MAX_CACHE_TIMEOUT else
      TimeOutSeconds := aTimeOutSeconds;
    NotifyContentChangedForMethod(aMethodIndex);
  end;
  result := self;
end;

destructor TMVCRunWithViews.Destroy;
begin
  fViews.Free;
  inherited;
end;

procedure TMVCRunWithViews.NotifyContentChangedForMethod(aMethodIndex: integer);
begin
  inherited;
  if cardinal(aMethodIndex)<cardinal(Length(fCache)) then
  with fCache[aMethodIndex] do
    case Policy of
    cacheRootIgnoringSession,cacheRootIfSession,cacheRootIfNoSession:
      RootValue := '';
    cacheRootWithSession,
    cacheWithParametersIgnoringSession,cacheWithParametersIfSession,
    cacheWithParametersIfNoSession:
      InputValues.Init(false);
    end;
end;


{ TMVCRunOnRestServer }

constructor TMVCRunOnRestServer.Create(aApplication: TMVCApplication;
  aRestServer: TSQLRestServer; const aSubURI: RawUTF8;
  aViews: TMVCViewsAbtract; aPublishMvcInfo: boolean);
var m: integer;
begin
  if aApplication=nil then
    raise EMVCException.CreateUTF8('%.Create(aApplication=nil)',[self]);
  if aRestServer=nil then
    fRestServer := aApplication.RestModel as TSQLRestServer else
    fRestServer := aRestServer;
  if aViews=nil then
    aViews := TMVCViewsMustache.Create(
      aApplication.fFactory.InterfaceTypeInfo,fRestServer.LogClass,'.html') else
    aViews.fLogClass := fRestServer.LogClass;
  inherited Create(aApplication,aViews);
  fPublishMvcInfo := aPublishMvcInfo;
  if aSubURI<>'' then
    fRestServer.ServiceMethodRegister(aSubURI,RunOnRestServerSub,true) else begin
    for m := 0 to fApplication.fFactory.MethodsCount-1 do
      fRestServer.ServiceMethodRegister(
        fApplication.fFactory.Methods[m].URI,RunOnRestServerRoot,true);
    if aPublishMvcInfo then
      fRestServer.ServiceMethodRegister(MVCINFO_URI,RunOnRestServerRoot,true);
  end;
  fApplication.SetSession(TMVCSessionWithRestServer.Create);
end;

procedure TMVCRunOnRestServer.InternalRunOnRestServer(
  Ctxt: TSQLRestServerURIContext; const MethodName: RawUTF8);
var mvcinfo, inputContext: variant;
    rawMethodName,rawFormat: RawUTF8;
    rendererClass: TMVCRendererReturningDataClass;
    renderer: TMVCRendererReturningData;
begin
  Split(MethodName,'/',rawMethodName,rawFormat);
  if fPublishMvcInfo and IdemPropNameU(rawMethodName,MVCINFO_URI) then begin
    if fMvcInfoCache='' then begin
      mvcinfo := fApplication.GetMvcInfo;
      mvcinfo.viewsFolder := fViews.ViewTemplateFolder;
      fMvcInfoCache := TSynMustache.Parse(MUSTACHE_MVCINFO).Render(mvcinfo);
    end;
    Ctxt.Returns(fMvcInfoCache,HTML_SUCCESS,HTML_CONTENT_TYPE_HEADER,True);
  end else begin
    if IdemPropNameU(rawFormat,'json') then
      rendererClass := TMVCRendererJSON else
      rendererClass := TMVCRendererFromViews;
    renderer := rendererClass.Create(self);
    try
      if Ctxt.Method in [mGET,mPOST] then begin
        inputContext := Ctxt.InputAsTDocVariant;
        if not VarIsEmpty(inputContext) then
          VariantSaveJSON(inputContext,twJSONEscape,renderer.fInput);
        renderer.ExecuteCommand(fViews.fFactory.FindMethodIndex(rawMethodName));
      end else
        renderer.CommandError('notfound',true,HTML_NOTFOUND);
      Ctxt.Returns(renderer.Output.Content,renderer.Output.Status,
        renderer.Output.Header,True,true);
    finally
      renderer.Free;
    end;
  end;
end;

procedure TMVCRunOnRestServer.RunOnRestServerRoot(Ctxt: TSQLRestServerURIContext);
begin
  InternalRunOnRestServer(Ctxt,Ctxt.URI+'/'+Ctxt.URIBlobFieldName);
end;

procedure TMVCRunOnRestServer.RunOnRestServerSub(Ctxt: TSQLRestServerURIContext);
begin
  InternalRunOnRestServer(Ctxt,Ctxt.URIBlobFieldName);
end;



{ TMVCRendererReturningData }

constructor TMVCRendererReturningData.Create(aRun: TMVCRunWithViews);
begin
  fRun := aRun;
  inherited Create(fRun.Application);
end;

procedure TMVCRendererReturningData.ExecuteCommand(aMethodIndex: integer);
  procedure SetOutputValue(const aValue: RawUTF8);
  begin
    fOutput.Status := HTML_SUCCESS;
    Split(aValue,#0,fOutput.Header,RawUTF8(fOutput.Content));
  end;
  function RetrievedFromInputValues(const aKey: RawUTF8;
    const aInputValues: TSynNameValue): boolean;
  var i: integer;
  begin
    i := aInputValues.Find(aKey);
    if (i>=0) and (aInputValues.List[i].Value<>'') and
       (fCacheCurrentSec<cardinal(aInputValues.List[i].Tag)) then begin
      SetOutputValue(aInputValues.List[i].Value);
      result := true;
    end else begin
      fCacheCurrent := inputCache;
      fCacheCurrentInputValueKey := aKey;
      result := false;
    end;
  end;
var sessionID: integer;
label doRoot,doInput;
begin
  fCacheCurrent := noCache;
  fCacheCurrentSec := GetTickCount64 div 1000;
  if cardinal(aMethodIndex)<cardinal(Length(fRun.fCache)) then
  with fRun.fCache[aMethodIndex] do begin
    case Policy of
    cacheRootIgnoringSession:
      if fInput='' then
 doRoot:if (RootValue<>'') and (fCacheCurrentSec<RootValueExpirationTime) then begin
          SetOutputValue(RootValue);
          exit;
        end else
          fCacheCurrent := rootCache;
    cacheRootIfSession:
      if (fInput='') and fApplication.CurrentSession.Exists then
        goto doRoot;
    cacheRootIfNoSession:
      if (fInput='') and not fApplication.CurrentSession.Exists then
        goto doRoot;
    cacheRootWithSession:
      if fInput='' then begin
        sessionID := fApplication.CurrentSession.CheckAndRetrieve;
        if sessionID=0 then
          goto doRoot else
        if RetrievedFromInputValues(UInt32ToUtf8(sessionID),InputValues) then
          exit;
      end;
    cacheWithParametersIgnoringSession:
doInput:if fInput='' then
          goto doRoot else
        if RetrievedFromInputValues(fInput,InputValues) then
          exit;
    cacheWithParametersIfSession:
      if fApplication.CurrentSession.Exists then
        goto doInput;
    cacheWithParametersIfNoSession:
      if not fApplication.CurrentSession.Exists then
        goto doInput;
    end;
  end;
  inherited ExecuteCommand(aMethodIndex);
  if fCacheCurrent<>noCache then
    with fRun.fCache[aMethodIndex] do begin
      inc(fCacheCurrentSec,TimeOutSeconds);
      case fCacheCurrent of
      rootCache:
        if fOutput.Status=HTML_SUCCESS then begin
          RootValue := fOutput.Header+#0+fOutput.Content;
          RootValueExpirationTime := fCacheCurrentSec;
        end else
          RootValue := '';
      inputCache:
        if fOutput.Status=HTML_SUCCESS then
          InputValues.Add(fCacheCurrentInputValueKey,fOutput.Header+#0+fOutput.Content,fCacheCurrentSec) else
          InputValues.Add(fCacheCurrentInputValueKey,'');
      end;
    end;
end;

initialization
  assert(sizeof(TMVCAction)=sizeof(TServiceCustomAnswer));
end.

