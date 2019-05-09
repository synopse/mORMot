/// implement TSynTable/TSynTableStatement and TSynFilter/TSynValidate process
// - licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynTable;

(*
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2019 Arnaud Bouchez
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

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2019
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
   - initial release
   - removed from SynCommons.pas, for better code clarity, and to reduce the
   number of source code lines of the unit, and circumvent the Delphi 5/6/7
   limitation of 65535 lines (internal error PRO-3006)


*)

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64

uses
  {$ifdef MSWINDOWS}
    Windows,
  {$endif}
  SysUtils,
  Classes,
  {$ifndef LVCL}
    Contnrs,  // for TObjectList
  {$endif}
  {$ifndef NOVARIANTS}
    Variants,
  {$endif}
  SynCommons;



{ ************ filtering and validation classes and functions ************** }

/// return TRUE if the supplied content is a valid email address
// - follows RFC 822, to validate local-part@domain email format
function IsValidEmail(P: PUTF8Char): boolean;

/// return TRUE if the supplied content is a valid IP v4 address
function IsValidIP4Address(P: PUTF8Char): boolean;

/// return TRUE if the supplied content matchs a glob pattern
// - ?  Matches any single characer
// - *	Matches any contiguous characters
// - [abc]  Matches a or b or c at that position
// - [^abc]	Matches anything but a or b or c at that position
// - [!abc]	Matches anything but a or b or c at that position
// - [a-e]  Matches a through e at that position
// - [abcx-z]  Matches a or b or c or x or y or or z, as does [a-cx-z]
// - 'ma?ch.*'	would match match.exe, mavch.dat, march.on, etc..
// - 'this [e-n]s a [!zy]est' would match 'this is a test', but would not
// match 'this as a test' nor 'this is a zest'
// - consider using TMatch or TMatchs if you expect to reuse the pattern
function IsMatch(const Pattern, Text: RawUTF8; CaseInsensitive: boolean=false): boolean;

/// return TRUE if the supplied content matchs a glob pattern, using VCL strings
// - is a wrapper around IsMatch() with fast UTF-8 conversion
function IsMatchString(const Pattern, Text: string; CaseInsensitive: boolean=false): boolean;

type
  PMatch = ^TMatch;
  TMatchSearchFunction = function(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;

  /// low-level structure used by IsMatch() for actual glob search
  // - you can use this object to prepare a given pattern, e.g. in a loop
  // - implemented as a fast brute-force state-machine without any heap allocation
  // - some common patterns ('exactmatch', 'startwith*', '*endwith', '*contained*')
  // are handled with dedicated code, optionally with case-insensitive search
  // - consider using TMatchs (or SetMatchs/TMatchDynArray) if you expect to
  // search for several patterns, or even TExprParserMatch for expression search
  {$ifdef UNICODE}TMatch = record{$else}TMatch = object{$endif}
  private
    Pattern, Text: PUTF8Char;
    P, T, PMax, TMax: PtrInt;
    Upper: PNormTable;
    State: (sNONE, sABORT, sEND, sLITERAL, sPATTERN, sRANGE, sVALID);
    procedure MatchAfterStar;
    procedure MatchMain;
  public
    /// published for proper inlining
    Search: TMatchSearchFunction;
    /// initialize the internal fields for a given glob search pattern
    // - note that the aPattern instance should remain in memory, since it will
    // be pointed to by the Pattern private field of this object
    procedure Prepare(const aPattern: RawUTF8; aCaseInsensitive, aReuse: boolean); overload;
    /// initialize the internal fields for a given glob search pattern
    // - note that the aPattern buffer should remain in memory, since it will
    // be pointed to by the Pattern private field of this object
    procedure Prepare(aPattern: PUTF8Char; aPatternLen: integer;
      aCaseInsensitive, aReuse: boolean); overload;
    /// initialize low-level internal fields for'*aPattern*' search
    // - this method is faster than a regular Prepare('*' + aPattern + '*')
    // - warning: the supplied aPattern variable may be modified in-place to be
    // filled with some lookup buffer, for length(aPattern) in [2..31] range
    procedure PrepareContains(var aPattern: RawUTF8; aCaseInsensitive: boolean); overload;
    /// initialize low-level internal fields for a custom search algorithm
    procedure PrepareRaw(aPattern: PUTF8Char; aPatternLen: integer;
      aSearch: TMatchSearchFunction);
    /// returns TRUE if the supplied content matches the prepared glob pattern
    // - this method is not thread-safe
    function Match(const aText: RawUTF8): boolean; overload;
      {$ifdef FPC}inline;{$endif}
    /// returns TRUE if the supplied content matches the prepared glob pattern
    // - this method is not thread-safe
    function Match(aText: PUTF8Char; aTextLen: PtrInt): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns TRUE if the supplied content matches the prepared glob pattern
    // - this method IS thread-safe, and won't lock
    function MatchThreadSafe(const aText: RawUTF8): boolean;
    /// returns TRUE if the supplied VCL/LCL content matches the prepared glob pattern
    // - this method IS thread-safe, will use stack to UTF-8 temporary conversion
    // if possible, and won't lock
    function MatchString(const aText: string): boolean;
    /// returns TRUE if this search pattern matches another
    function Equals(const aAnother{$ifndef DELPHI5OROLDER}: TMatch{$endif}): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// access to the pattern length as stored in PMax + 1
    function PatternLength: integer; {$ifdef HASINLINE}inline;{$endif}
    /// access to the pattern text as stored in Pattern
    function PatternText: PUTF8Char; {$ifdef HASINLINE}inline;{$endif}
  end;
  /// use SetMatchs() to initialize such an array from a CSV pattern text
  TMatchDynArray = array of TMatch;

  /// TMatch descendant owning a copy of the Pattern string to avoid GPF issues
  TMatchStore = record
    /// access to the research criteria
    // - defined as a nested record (and not an object) to circumvent Delphi bug
    Pattern: TMatch;
    /// Pattern.Pattern PUTF8Char will point to this instance
    PatternInstance: RawUTF8;
  end;
  TMatchStoreDynArray = array of TMatchStore;

  /// stores several TMatch instances, from a set of glob patterns
  TMatchs = class(TSynPersistent)
  protected
    fMatch: TMatchStoreDynArray;
    fMatchCount: integer;
  public
    /// add once some glob patterns to the internal TMach list
    // - aPatterns[] follows the IsMatch() syntax
    constructor Create(const aPatterns: TRawUTF8DynArray; CaseInsensitive: Boolean); reintroduce; overload;
    /// add once some glob patterns to the internal TMach list
    // - aPatterns[] follows the IsMatch() syntax
    procedure Subscribe(const aPatterns: TRawUTF8DynArray; CaseInsensitive: Boolean); overload; virtual;
    /// add once some glob patterns to the internal TMach list
    // - each CSV item in aPatterns follows the IsMatch() syntax
    procedure Subscribe(const aPatternsCSV: RawUTF8; CaseInsensitive: Boolean); overload;
    /// search patterns in the supplied UTF-8 text
    // - returns -1 if no filter has been subscribed
    // - returns -2 if there is no match on any previous pattern subscription
    // - returns fMatch[] index, i.e. >= 0 number on first matching pattern
    // - this method is thread-safe
    function Match(const aText: RawUTF8): integer; overload; {$ifdef HASINLINE}inline;{$endif}
    /// search patterns in the supplied UTF-8 text buffer
    function Match(aText: PUTF8Char; aLen: integer): integer; overload;
    /// search patterns in the supplied VCL/LCL text
    // - could be used on a TFileName for instance
    // - will avoid any memory allocation if aText is small enough
    function MatchString(const aText: string): integer;
  end;

/// fill the Match[] dynamic array with all glob patterns supplied as CSV
// - returns how many patterns have been set in Match[|]
// - note that the CSVPattern instance should remain in memory, since it will
// be pointed to by the Match[].Pattern private field
function SetMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  out Match: TMatchDynArray): integer; overload;

/// fill the Match[0..MatchMax] static array with all glob patterns supplied as CSV
// - note that the CSVPattern instance should remain in memory, since it will
// be pointed to by the Match[].Pattern private field
function SetMatchs(CSVPattern: PUTF8Char; CaseInsensitive: boolean;
  Match: PMatch; MatchMax: integer): integer; overload;

/// search if one TMach is already registered in the Several[] dynamic array
function MatchExists(const One: TMatch; const Several: TMatchDynArray): boolean;

/// add one TMach if not already registered in the Several[] dynamic array
function MatchAdd(const One: TMatch; var Several: TMatchDynArray): boolean;

/// returns TRUE if Match=nil or if any Match[].Match(Text) is TRUE
function MatchAny(const Match: TMatchDynArray; const Text: RawUTF8): boolean;

/// apply the CSV-supplied glob patterns to an array of RawUTF8
// - any text not maching the pattern will be deleted from the array
procedure FilterMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  var Values: TRawUTF8DynArray);


type
  TSynFilterOrValidate = class;

  TSynFilterOrValidateObjArray = array of TSynFilterOrValidate;
  TSynFilterOrValidateObjArrayArray = array of TSynFilterOrValidateObjArray;

  /// will define a filter (transformation) or a validation process to be
  // applied to a database Record content (typicaly a TSQLRecord)
  // - the optional associated parameters are to be supplied JSON-encoded
  TSynFilterOrValidate = class
  protected
    fParameters: RawUTF8;
    /// children must override this method in order to parse the JSON-encoded
    // parameters, and store it in protected field values
    procedure SetParameters(const Value: RawUTF8); virtual;
  public
    /// add the filter or validation process to a list, checking if not present
    // - if an instance with the same class type and parameters is already
    // registered, will call aInstance.Free and return the exising instance
    // - if there is no similar instance, will add it to the list and return it
    function AddOnce(var aObjArray: TSynFilterOrValidateObjArray;
      aFreeIfAlreadyThere: boolean=true): TSynFilterOrValidate;
  public
    /// initialize the filter (transformation) or validation instance
    // - most of the time, optional parameters may be specified as JSON,
    // possibly with the extended MongoDB syntax
    constructor Create(const aParameters: RawUTF8=''); overload; virtual;
    /// initialize the filter or validation instance
    /// - this overloaded constructor will allow to easily set the parameters
    constructor CreateUTF8(const Format: RawUTF8; const Args, Params: array of const); overload;
    /// the optional associated parameters, supplied as JSON-encoded
    property Parameters: RawUTF8 read fParameters write SetParameters;
  end;

  /// will define a validation to be applied to a Record (typicaly a TSQLRecord)
  // field content
  // - a typical usage is to validate an email or IP adress e.g.
  // - the optional associated parameters are to be supplied JSON-encoded
  TSynValidate = class(TSynFilterOrValidate)
  public
    /// perform the validation action to the specified value
    // - the value is expected by be UTF-8 text, as generated by
    // TPropInfo.GetValue e.g.
    // - if the validation failed, must return FALSE and put some message in
    // ErrorMsg (translated into the current language: you could e.g. use
    // a resourcestring and a SysUtils.Format() call for automatic translation
    // via the mORMoti18n unit - you can leave ErrorMsg='' to trigger a
    // generic error message from clas name ('"Validate email" rule failed'
    // for TSynValidateEmail class e.g.)
    // - if the validation passed, will return TRUE
    function Process(FieldIndex: integer; const Value: RawUTF8; var ErrorMsg: string): boolean;
      virtual; abstract;
  end;

  /// points to a TSynValidate variable
  // - used e.g. as optional parameter to TSQLRecord.Validate/FilterAndValidate
  PSynValidate = ^TSynValidate;

  /// IP v4 address validation to be applied to a Record field content
  // (typicaly a TSQLRecord)
  // - this versions expect no parameter
  TSynValidateIPAddress = class(TSynValidate)
  protected
  public
    /// perform the IP Address validation action to the specified value
    function Process(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string): boolean; override;
  end;

  /// IP address validation to be applied to a Record field content
  // (typicaly a TSQLRecord)
  // - optional JSON encoded parameters are "AllowedTLD" or "ForbiddenTLD",
  // expecting a CSV lis of Top-Level-Domain (TLD) names, e.g.
  // $ '{"AllowedTLD":"com,org,net","ForbiddenTLD":"fr"}'
  // $ '{AnyTLD:true,ForbiddenDomains:"mailinator.com,yopmail.com"}'
  // - this will process a validation according to RFC 822 (calling the
  // IsValidEmail() function) then will check for the TLD to be in one of
  // the Top-Level domains ('.com' and such) or a two-char country, and
  // then will check the TLD according to AllowedTLD and ForbiddenTLD
  TSynValidateEmail = class(TSynValidate)
  private
    fAllowedTLD: RawUTF8;
    fForbiddenTLD: RawUTF8;
    fForbiddenDomains: RawUTF8;
    fAnyTLD: boolean;
  protected
    /// decode all published properties from their JSON representation
    procedure SetParameters(const Value: RawUTF8); override;
  public
    /// perform the Email Address validation action to the specified value
    // - call IsValidEmail() function and check for the supplied TLD
    function Process(aFieldIndex: integer; const Value: RawUTF8; var ErrorMsg: string): boolean; override;
    /// allow any TLD to be allowed, even if not a generic TLD (.com,.net ...)
    // - this may be mandatory since already over 1,300 new gTLD names or
    // "strings" could become available in the next few years: there is a
    // growing list of new gTLDs available at
    // @http://newgtlds.icann.org/en/program-status/delegated-strings
    // - the only restriction is that it should be ascii characters
    property AnyTLD: boolean read fAnyTLD write fAnyTLD;
    /// a CSV list of allowed TLD
    // - if accessed directly, should be set as lower case values
    // - e.g. 'com,org,net'
    property AllowedTLD: RawUTF8 read fAllowedTLD write fAllowedTLD;
    /// a CSV list of forbidden TLD
    // - if accessed directly, should be set as lower case values
    // - e.g. 'fr'
    property ForbiddenTLD: RawUTF8 read fForbiddenTLD write fForbiddenTLD;
    /// a CSV list of forbidden domain names
    // - if accessed directly, should be set as lower case values
    // - not only the TLD, but whole domains like 'cracks.ru,hotmail.com' or such
    property ForbiddenDomains: RawUTF8 read fForbiddenDomains write fForbiddenDomains;
  end;

  /// glob case-sensitive pattern validation of a Record field content
  // - parameter is NOT JSON encoded, but is some basic TMatch glob pattern
  // - ?	   	Matches any single characer
  // - *	   	Matches any contiguous characters
  // - [abc]  Matches a or b or c at that position
  // - [^abc]	Matches anything but a or b or c at that position
  // - [!abc]	Matches anything but a or b or c at that position
  // - [a-e]  Matches a through e at that position
  // - [abcx-z] Matches a or b or c or x or y or or z, as does [a-cx-z]
  // - 'ma?ch.*'	would match match.exe, mavch.dat, march.on, etc..
  // - 'this [e-n]s a [!zy]est' would match 'this is a test', but would not
  //   match 'this as a test' nor 'this is a zest'
  // - pattern check IS case sensitive (TSynValidatePatternI is not)
  // - this class is not as complete as PCRE regex for example,
  // but code overhead is very small, and speed good enough in practice
  TSynValidatePattern = class(TSynValidate)
  protected
    fMatch: TMatch;
    procedure SetParameters(const Value: RawUTF8); override;
  public
    /// perform the pattern validation to the specified value
    // - pattern can be e.g. '[0-9][0-9]:[0-9][0-9]:[0-9][0-9]'
    // - this method will implement both TSynValidatePattern and
    // TSynValidatePatternI, checking the current class
    function Process(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string): boolean; override;
  end;

  /// glob case-insensitive pattern validation of a text field content
  // (typicaly a TSQLRecord)
  // - parameter is NOT JSON encoded, but is some basic TMatch glob pattern
  // - same as TSynValidatePattern, but is NOT case sensitive
  TSynValidatePatternI = class(TSynValidatePattern);

  /// text validation to ensure that to any text field would not be ''
  TSynValidateNonVoidText = class(TSynValidate)
  public
    /// perform the non void text validation action to the specified value
    function Process(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string): boolean; override;
  end;

  TSynValidateTextProps = array[0..15] of cardinal;

{$M+} // to have existing RTTI for published properties
  /// text validation to be applied to any Record field content
  // - default MinLength value is 1, MaxLength is maxInt: so a blank
  // TSynValidateText.Create('') is the same as TSynValidateNonVoidText
  // - MinAlphaCount, MinDigitCount, MinPunctCount, MinLowerCount and
  // MinUpperCount allow you to specify the minimal count of respectively
  // alphabetical [a-zA-Z], digit [0-9], punctuation [_!;.,/:?%$="#@(){}+-*],
  // lower case or upper case characters
  // - expects optional JSON parameters of the allowed text length range as
  // $ '{"MinLength":5,"MaxLength":10,"MinAlphaCount":1,"MinDigitCount":1,
  // $ "MinPunctCount":1,"MinLowerCount":1,"MinUpperCount":1}
  TSynValidateText = class(TSynValidate)
  private
    /// used to store all associated validation properties by index
    fProps: TSynValidateTextProps;
    fUTF8Length: boolean;
  protected
    /// use sInvalidTextChar resourcestring to create a translated error message
    procedure SetErrorMsg(fPropsIndex, InvalidTextIndex, MainIndex: integer;
      var result: string);
    /// decode "MinLength", "MaxLength", and other parameters into fProps[]
    procedure SetParameters(const Value: RawUTF8); override;
  public
    /// perform the text length validation action to the specified value
    function Process(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string): boolean; override;
  published
    /// Minimal length value allowed for the text content
    // - the length is calculated with UTF-16 Unicode codepoints, unless
    // UTF8Length has been set to TRUE so that the UTF-8 byte count is checked
    // - default is 1, i.e. a void text will not pass the validation
    property MinLength: cardinal read fProps[0] write fProps[0];
    /// Maximal length value allowed for the text content
    // - the length is calculated with UTF-16 Unicode codepoints, unless
    // UTF8Length has been set to TRUE so that the UTF-8 byte count is checked
    // - default is maxInt, i.e. no maximum length is set
    property MaxLength: cardinal read fProps[1] write fProps[1];
    /// Minimal alphabetical character [a-zA-Z] count
    // - default is 0, i.e. no minimum set
    property MinAlphaCount: cardinal read fProps[2] write fProps[2];
    /// Maximal alphabetical character [a-zA-Z] count
    // - default is maxInt, i.e. no Maximum set
    property MaxAlphaCount: cardinal read fProps[10] write fProps[10];
    /// Minimal digit character [0-9] count
    // - default is 0, i.e. no minimum set
    property MinDigitCount: cardinal read fProps[3] write fProps[3];
    /// Maximal digit character [0-9] count
    // - default is maxInt, i.e. no Maximum set
    property MaxDigitCount: cardinal read fProps[11] write fProps[11];
    /// Minimal punctuation sign [_!;.,/:?%$="#@(){}+-*] count
    // - default is 0, i.e. no minimum set
    property MinPunctCount: cardinal read fProps[4] write fProps[4];
    /// Maximal punctuation sign [_!;.,/:?%$="#@(){}+-*] count
    // - default is maxInt, i.e. no Maximum set
    property MaxPunctCount: cardinal read fProps[12] write fProps[12];
    /// Minimal alphabetical lower case character [a-z] count
    // - default is 0, i.e. no minimum set
    property MinLowerCount: cardinal read fProps[5] write fProps[5];
    /// Maximal alphabetical lower case character [a-z] count
    // - default is maxInt, i.e. no Maximum set
    property MaxLowerCount: cardinal read fProps[13] write fProps[13];
    /// Minimal alphabetical upper case character [A-Z] count
    // - default is 0, i.e. no minimum set
    property MinUpperCount: cardinal read fProps[6] write fProps[6];
    /// Maximal alphabetical upper case character [A-Z] count
    // - default is maxInt, i.e. no Maximum set
    property MaxUpperCount: cardinal read fProps[14] write fProps[14];
    /// Minimal space count inside the value text
    // - default is 0, i.e. any space number allowed
    property MinSpaceCount: cardinal read fProps[7] write fProps[7];
    /// Maximal space count inside the value text
    // - default is maxInt, i.e. any space number allowed
    property MaxSpaceCount: cardinal read fProps[15] write fProps[15];
    /// Maximal space count allowed on the Left side
    // - default is maxInt, i.e. any Left space allowed
    property MaxLeftTrimCount: cardinal read fProps[8] write fProps[8];
    /// Maximal space count allowed on the Right side
    // - default is maxInt, i.e. any Right space allowed
    property MaxRightTrimCount: cardinal read fProps[9] write fProps[9];
    /// defines if lengths parameters expects UTF-8 or UTF-16 codepoints number
    // - with default FALSE, the length is calculated with UTF-16 Unicode
    // codepoints - MaxLength may not match the UCS4 glyphs number, in case of
    // UTF-16 surrogates
    // - you can set this property to TRUE so that the UTF-8 byte count would
    // be used for truncation againts the MaxLength parameter
    property UTF8Length: boolean read fUTF8Length write fUTF8Length;
  end;
{$M-}

  /// strong password validation for a Record field content (typicaly a TSQLRecord)
  // - the following parameters are set by default to
  // $ '{"MinLength":5,"MaxLength":20,"MinAlphaCount":1,"MinDigitCount":1,
  // $ "MinPunctCount":1,"MinLowerCount":1,"MinUpperCount":1,"MaxSpaceCount":0}'
  // - you can specify some JSON encoded parameters to change this default
  // values, which will validate the text field only if it contains from 5 to 10
  // characters, with at least one digit, one upper case letter, one lower case
  // letter, and one ponctuation sign, with no space allowed inside
  TSynValidatePassWord = class(TSynValidateText)
  protected
    /// set password specific parameters
    procedure SetParameters(const Value: RawUTF8); override;
  end;

  { C++Builder doesn't support array elements as properties (RSP-12595).
    For now, simply exclude the relevant classes from C++Builder. }
  {$NODEFINE TSynValidateTextProps}
  {$NODEFINE TSynValidateText }
  {$NODEFINE TSynValidatePassWord }

  /// will define a transformation to be applied to a Record field content
  // (typicaly a TSQLRecord)
  // - here "filter" means that content would be transformed according to a
  // set of defined rules
  // - a typical usage is to convert to lower or upper case, or
  // trim any time or date value in a TDateTime field
  // - the optional associated parameters are to be supplied JSON-encoded
  TSynFilter = class(TSynFilterOrValidate)
  protected
  public
    /// perform the transformation to the specified value
    // - the value is converted into UTF-8 text, as expected by
    // TPropInfo.GetValue / TPropInfo.SetValue e.g.
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); virtual; abstract;
  end;

  /// class-refrence type (metaclass) for a TSynFilter or a TSynValidate
  TSynFilterOrValidateClass = class of TSynFilterOrValidate;

  /// class-reference type (metaclass) of a record filter (transformation)
  TSynFilterClass = class of TSynFilter;

  /// convert the value into ASCII Upper Case characters
  // - UpperCase conversion is made for ASCII-7 only, i.e. 'a'..'z' characters
  // - this version expects no parameter
  TSynFilterUpperCase = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// convert the value into WinAnsi Upper Case characters
  // - UpperCase conversion is made for all latin characters in the WinAnsi
  // code page only, e.g. 'e' acute will be converted to 'E'
  // - this version expects no parameter
  TSynFilterUpperCaseU = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// convert the value into ASCII Lower Case characters
  // - LowerCase conversion is made for ASCII-7 only, i.e. 'A'..'Z' characters
  // - this version expects no parameter
  TSynFilterLowerCase = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// convert the value into WinAnsi Lower Case characters
  // - LowerCase conversion is made for all latin characters in the WinAnsi
  // code page only, e.g. 'E' acute will be converted to 'e'
  // - this version expects no parameter
  TSynFilterLowerCaseU = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// trim any space character left or right to the value
  // - this versions expect no parameter
  TSynFilterTrim = class(TSynFilter)
  public
    /// perform the space triming conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// truncate a text above a given maximum length
  // - expects optional JSON parameters of the allowed text length range as
  // $ '{MaxLength":10}
  TSynFilterTruncate = class(TSynFilter)
  protected
    fMaxLength: cardinal;
    fUTF8Length: boolean;
    /// decode the MaxLength: and UTF8Length: parameters
    procedure SetParameters(const Value: RawUTF8); override;
  public
    /// perform the length truncation of the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
    /// Maximal length value allowed for the text content
    // - the length is calculated with UTF-16 Unicode codepoints, unless
    // UTF8Length has been set to TRUE so that the UTF-8 byte count is checked
    // - default is 0, i.e. no maximum length is forced
    property MaxLength: cardinal read fMaxLength write fMaxLength;
    /// defines if MaxLength is stored as UTF-8 or UTF-16 codepoints number
    // - with default FALSE, the length is calculated with UTF-16 Unicode
    // codepoints - MaxLength may not match the UCS4 glyphs number, in case of
    // UTF-16 surrogates
    // - you can set this property to TRUE so that the UTF-8 byte count would
    // be used for truncation againts the MaxLength parameter
    property UTF8Length: boolean read fUTF8Length write fUTF8Length;
  end;


{ ************ Database types and classes ************************** }

type
  /// handled field/parameter/column types for abstract database access
  // - this will map JSON-compatible low-level database-level access types, not
  // high-level Delphi types as TSQLFieldType defined in mORMot.pas
  // - it does not map either all potential types as defined in DB.pas (which
  // are there for compatibility with old RDBMS, and are not abstract enough)
  // - those types can be mapped to standard SQLite3 generic types, i.e.
  // NULL, INTEGER, REAL, TEXT, BLOB (with the addition of a ftCurrency and
  // ftDate type, for better support of most DB engines)
  // see @http://www.sqlite.org/datatype3.html
  // - the only string type handled here uses UTF-8 encoding (implemented
  // using our RawUTF8 type), for cross-Delphi true Unicode process
  TSQLDBFieldType =
    (ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob);

  /// set of field/parameter/column types for abstract database access
  TSQLDBFieldTypes = set of TSQLDBFieldType;

  /// array of field/parameter/column types for abstract database access
  TSQLDBFieldTypeDynArray = array of TSQLDBFieldType;

  /// array of field/parameter/column types for abstract database access
  // - this array as a fixed size, ready to handle up to MAX_SQLFIELDS items
  TSQLDBFieldTypeArray = array[0..MAX_SQLFIELDS-1] of TSQLDBFieldType;

  /// how TSQLVar may be processed
  // - by default, ftDate will use seconds resolution unless svoDateWithMS is set
  TSQLVarOption = (svoDateWithMS);

  /// defines how TSQLVar may be processed
  TSQLVarOptions = set of TSQLVarOption;

  /// memory structure used for database values by reference storage
  // - used mainly by SynDB, mORMot, mORMotDB and mORMotSQLite3 units
  // - defines only TSQLDBFieldType data types (similar to those handled by
  // SQLite3, with the addition of ftCurrency and ftDate)
  // - cleaner/lighter dedicated type than TValue or variant/TVarData, strong
  // enough to be marshalled as JSON content
  // - variable-length data (e.g. UTF-8 text or binary BLOB) are never stored
  // within this record, but VText/VBlob will point to an external (temporary)
  // memory buffer
  // - date/time is stored as ISO-8601 text (with milliseconds if svoDateWithMS
  // option is set and the database supports it), and currency as double or BCD
  // in most databases
  TSQLVar = record
    /// how this value should be processed
    Options: TSQLVarOptions;
    /// the type of the value stored
    case VType: TSQLDBFieldType of
    ftInt64: (
      VInt64: Int64);
    ftDouble: (
      VDouble: double);
    ftDate: (
      VDateTime: TDateTime);
    ftCurrency: (
      VCurrency: Currency);
    ftUTF8: (
      VText: PUTF8Char);
    ftBlob: (
      VBlob: pointer;
      VBlobLen: Integer)
  end;

  /// dynamic array of database values by reference storage
  TSQLVarDynArray = array of TSQLVar;

  /// used to store bit set for all available fields in a Table
  // - with current MAX_SQLFIELDS value, 64 bits uses 8 bytes of memory
  // - see also IsZero() and IsEqual() functions
  // - you can also use ALL_FIELDS as defined in mORMot.pas
  TSQLFieldBits = set of 0..MAX_SQLFIELDS-1;

  /// used to store a field index in a Table
  // - note that -1 is commonly used for the ID/RowID field so the values should
  // be signed
  // - even if ShortInt (-128..127) may have been enough, we define a 16 bit
  // safe unsigned integer to let the source compile with Delphi 5
  TSQLFieldIndex = SmallInt; // -32768..32767

  /// used to store field indexes in a Table
  // - same as TSQLFieldBits, but allowing to store the proper order
  TSQLFieldIndexDynArray = array of TSQLFieldIndex;

  /// points to a bit set used for all available fields in a Table
  PSQLFieldBits = ^TSQLFieldBits;

  /// generic parameter types, as recognized by SQLParamContent() and
  // ExtractInlineParameters() functions
  TSQLParamType = (sptUnknown, sptInteger, sptFloat, sptText, sptBlob, sptDateTime);

  /// array of parameter types, as recognized by SQLParamContent() and
  // ExtractInlineParameters() functions
  TSQLParamTypeDynArray = array of TSQLParamType;

  /// simple writer to a Stream, specialized for the JSON format and SQL export
  // - use an internal buffer, faster than string+string
  TJSONWriter = class(TTextWriter)
  protected
    /// used to store output format
    fExpand: boolean;
    /// used to store output format for TSQLRecord.GetJSONValues()
    fWithID: boolean;
    /// used to store field for TSQLRecord.GetJSONValues()
    fFields: TSQLFieldIndexDynArray;
    /// if not Expanded format, contains the Stream position of the first
    // useful Row of data; i.e. ',val11' position in:
    // & { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    fStartDataPosition: integer;
  public
    /// used internally to store column names and count for AddColumns
    ColNames: TRawUTF8DynArray;
    /// the data will be written to the specified Stream
    // - if no Stream is supplied, a temporary memory stream will be created
    // (it's faster to supply one, e.g. any TSQLRest.TempMemoryStream)
    constructor Create(aStream: TStream; Expand, withID: boolean;
      const Fields: TSQLFieldBits; aBufSize: integer=8192); overload;
    /// the data will be written to the specified Stream
    // - if no Stream is supplied, a temporary memory stream will be created
    // (it's faster to supply one, e.g. any TSQLRest.TempMemoryStream)
    constructor Create(aStream: TStream; Expand, withID: boolean;
      const Fields: TSQLFieldIndexDynArray=nil; aBufSize: integer=8192); overload;
    /// rewind the Stream position and write void JSON object
    procedure CancelAllVoid;
    /// write or init field names for appropriate JSON Expand later use
    // - ColNames[] must have been initialized before calling this procedure
    // - if aKnownRowsCount is not null, a "rowCount":... item will be added
    // to the generated JSON stream (for faster unserialization of huge content)
    procedure AddColumns(aKnownRowsCount: integer=0);
    /// allow to change on the fly an expanded format column layout
    // - by definition, a non expanded format will raise a ESynException
    // - caller should then set ColNames[] and run AddColumns()
    procedure ChangeExpandedFields(aWithID: boolean; const aFields: TSQLFieldIndexDynArray); overload;
    /// end the serialized JSON object
    // - cancel last ','
    // - close the JSON object ']' or ']}'
    // - write non expanded postlog (,"rowcount":...), if needed
    // - flush the internal buffer content if aFlushFinal=true
    procedure EndJSONObject(aKnownRowsCount,aRowsCount: integer; aFlushFinal: boolean=true);
      {$ifdef HASINLINE}inline;{$endif}
    /// the first data row is erased from the content
    // - only works if the associated storage stream is TMemoryStream
    // - expect not Expanded format
    procedure TrimFirstRow;
    /// is set to TRUE in case of Expanded format
    property Expand: boolean read fExpand write fExpand;
    /// is set to TRUE if the ID field must be appended to the resulting JSON
    // - this field is used only by TSQLRecord.GetJSONValues
    // - this field is ignored by TSQLTable.GetJSONValues
    property WithID: boolean read fWithID;
    /// Read-Only access to the field bits set for each column to be stored
    property Fields: TSQLFieldIndexDynArray read fFields;
    /// if not Expanded format, contains the Stream position of the first
    // useful Row of data; i.e. ',val11' position in:
    // & { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    property StartDataPosition: integer read fStartDataPosition;
  end;

/// returns TRUE if no bit inside this TSQLFieldBits is set
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value
function IsZero(const Fields: TSQLFieldBits): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast comparison of two TSQLFieldBits values
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value
function IsEqual(const A,B: TSQLFieldBits): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast initialize a TSQLFieldBits with 0
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value
procedure FillZero(var Fields: TSQLFieldBits); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a TSQLFieldBits set of bits into an array of integers
procedure FieldBitsToIndex(const Fields: TSQLFieldBits;
  var Index: TSQLFieldIndexDynArray; MaxLength: integer=MAX_SQLFIELDS;
  IndexStart: integer=0); overload;

/// convert a TSQLFieldBits set of bits into an array of integers
function FieldBitsToIndex(const Fields: TSQLFieldBits;
  MaxLength: integer=MAX_SQLFIELDS): TSQLFieldIndexDynArray; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// add a field index to an array of field indexes
// - returns the index in Indexes[] of the newly appended Field value
function AddFieldIndex(var Indexes: TSQLFieldIndexDynArray; Field: integer): integer;

/// convert an array of field indexes into a TSQLFieldBits set of bits
procedure FieldIndexToBits(const Index: TSQLFieldIndexDynArray; var Fields: TSQLFieldBits); overload;

// search a field index in an array of field indexes
// - returns the index in Indexes[] of the given Field value, -1 if not found
function SearchFieldIndex(var Indexes: TSQLFieldIndexDynArray; Field: integer): integer;

/// convert an array of field indexes into a TSQLFieldBits set of bits
function FieldIndexToBits(const Index: TSQLFieldIndexDynArray): TSQLFieldBits; overload;
  {$ifdef HASINLINE}inline;{$endif}
  
/// returns the stored size of a TSQLVar database value
// - only returns VBlobLen / StrLen(VText) size, 0 otherwise
function SQLVarLength(const Value: TSQLVar): integer;

{$ifndef NOVARIANTS}

/// convert any Variant into a database value
// - ftBlob kind won't be handled by this function
// - complex variant types would be converted into ftUTF8 JSON object/array
procedure VariantToSQLVar(const Input: variant; var temp: RawByteString;
  var Output: TSQLVar);

/// guess the correct TSQLDBFieldType from a variant value
function VariantTypeToSQLDBFieldType(const V: Variant): TSQLDBFieldType;

{$endif NOVARIANTS}

  /// convert a date to a ISO-8601 string format for SQL '?' inlined parameters
// - will return the date encoded as '\uFFF1YYYY-MM-DD' - therefore
// ':("\uFFF12012-05-04"):' pattern will be recognized as a sftDateTime
// inline parameter in  SQLParamContent() / ExtractInlineParameters() functions
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum=?',[DateToSQL(EncodeDate(2012,5,4))]);
function DateToSQL(Date: TDateTime): RawUTF8; overload;

/// convert a date to a ISO-8601 string format for SQL '?' inlined parameters
// - will return the date encoded as '\uFFF1YYYY-MM-DD' - therefore
// ':("\uFFF12012-05-04"):' pattern will be recognized as a sftDateTime
// inline parameter in  SQLParamContent() / ExtractInlineParameters() functions
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum=?',[DateToSQL(2012,5,4)]);
function DateToSQL(Year,Month,Day: cardinal): RawUTF8; overload;

/// convert a date/time to a ISO-8601 string format for SQL '?' inlined parameters
// - if DT=0, returns ''
// - if DT contains only a date, returns the date encoded as '\uFFF1YYYY-MM-DD'
// - if DT contains only a time, returns the time encoded as '\uFFF1Thh:mm:ss'
// - otherwise, returns the ISO-8601 date and time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss'
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - if WithMS is TRUE, will append '.sss' for milliseconds resolution
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum<=?',[DateTimeToSQL(Now)]);
// - see TimeLogToSQL() if you are using TTimeLog/TModTime/TCreateTime values
function DateTimeToSQL(DT: TDateTime; WithMS: boolean=false): RawUTF8;

/// decode a SQL '?' inlined parameter (i.e. with JSON_SQLDATE_MAGIC prefix)
// - as generated by DateToSQL/DateTimeToSQL/TimeLogToSQL functions
function SQLToDateTime(const ParamValueWithMagic: RawUTF8): TDateTime;

/// convert a TTimeLog value into a ISO-8601 string format for SQL '?' inlined
// parameters
// - handle TTimeLog bit-encoded Int64 format
// - follows the same pattern as DateToSQL or DateTimeToSQL functions, i.e.
// will return the date or time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss' -
// therefore ':("\uFFF12012-05-04T20:12:13"):' pattern will be recognized as a
// sftDateTime inline parameter in  SQLParamContent() / ExtractInlineParameters()
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum<=?',[TimeLogToSQL(TimeLogNow)]);
function TimeLogToSQL(const Timestamp: TTimeLog): RawUTF8;

/// convert a Iso8601 encoded string into a ISO-8601 string format for SQL
// '?' inlined parameters
// - follows the same pattern as DateToSQL or DateTimeToSQL functions, i.e.
// will return the date or time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss' -
// therefore ':("\uFFF12012-05-04T20:12:13"):' pattern will be recognized as a
// sftDateTime inline parameter in  SQLParamContent() / ExtractInlineParameters()
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - in practice, just append the JSON_SQLDATE_MAGIC prefix to the supplied text
function Iso8601ToSQL(const S: RawByteString): RawUTF8;


/// guess the content type of an UTF-8 SQL value, in :(....): format
// - will be used e.g. by ExtractInlineParameters() to un-inline a SQL statement
// - sftInteger is returned for an INTEGER value, e.g. :(1234):
// - sftFloat is returned for any floating point value (i.e. some digits
// separated by a '.' character), e.g. :(12.34): or :(12E-34):
// - sftUTF8Text is returned for :("text"): or :('text'):, with double quoting
// inside the value
// - sftBlob will be recognized from the ':("\uFFF0base64encodedbinary"):'
// pattern, and return raw binary (for direct blob parameter assignment)
// - sftDateTime will be recognized from ':(\uFFF1"2012-05-04"):' pattern,
// i.e. JSON_SQLDATE_MAGIC-prefixed string as returned by DateToSQL() or
// DateTimeToSQL() functions
// - sftUnknown is returned on invalid content, or if wasNull is set to TRUE
// - if ParamValue is not nil, the pointing RawUTF8 string is set with the
// value inside :(...): without double quoting in case of sftUTF8Text
// - wasNull is set to TRUE if P was ':(null):' and ParamType is sftUnknwown
function SQLParamContent(P: PUTF8Char; out ParamType: TSQLParamType; out ParamValue: RawUTF8;
  out wasNull: boolean): PUTF8Char;

/// this function will extract inlined :(1234): parameters into Types[]/Values[]
// - will return the generic SQL statement with ? place holders for inlined
// parameters and setting Values with SQLParamContent() decoded content
// - will set maxParam=0 in case of no inlined parameters
// - recognized types are sptInteger, sptFloat, sptDateTime ('\uFFF1...'),
// sptUTF8Text and sptBlob ('\uFFF0...')
// - sptUnknown is returned on invalid content
function ExtractInlineParameters(const SQL: RawUTF8;
  var Types: TSQLParamTypeDynArray; var Values: TRawUTF8DynArray;
  var maxParam: integer; var Nulls: TSQLFieldBits): RawUTF8;

/// returns a 64-bit value as inlined ':(1234):' text
function InlineParameter(ID: Int64): shortstring; overload;

/// returns a string value as inlined ':("value"):' text
function InlineParameter(const value: RawUTF8): RawUTF8; overload;

type
  /// SQL Query comparison operators
  // - used e.g. by CompareOperator() functions in SynTable.pas or vt_BestIndex()
  // in mORMotSQLite3.pas
  TCompareOperator = (
     soEqualTo,
     soNotEqualTo,
     soLessThan,
     soLessThanOrEqualTo,
     soGreaterThan,
     soGreaterThanOrEqualTo,
     soBeginWith,
     soContains,
     soSoundsLikeEnglish,
     soSoundsLikeFrench,
     soSoundsLikeSpanish);

const
  /// convert identified field types into high-level ORM types
  // - as will be implemented in unit mORMot.pas
  SQLDBFIELDTYPE_TO_DELPHITYPE: array[TSQLDBFieldType] of RawUTF8 = (
    '???','???', 'Int64', 'Double', 'Currency', 'TDateTime', 'RawUTF8', 'TSQLRawBlob');



{ ************ TSynTable types and classes ************************** }

{$define SORTCOMPAREMETHOD}
{ if defined, the field content comparison will use a method instead of fixed
  functions - could be mandatory for tftArray field kind }

type
  /// exception raised by all TSynTable related code
  ETableDataException = class(ESynException);

  /// the available types for any TSynTable field property
  // - this is used in our so-called SBF compact binary format
  // (similar to BSON or Protocol Buffers)
  // - those types are used for both storage and JSON conversion
  // - basic types are similar to SQLite3, i.e. Int64/Double/UTF-8/Blob
  // - storage can be of fixed size, or of variable length
  // - you can specify to use WinAnsi encoding instead of UTF-8 for string storage
  // (it can use less space on disk than UTF-8 encoding)
  // - BLOB fields can be either internal (i.e. handled by TSynTable like a
  // RawByteString text storage), either external (i.e. must be stored in a dedicated
  // storage structure - e.g. another TSynBigTable instance)
  TSynTableFieldType =
    (// unknown or not defined field type
     tftUnknown,
     // some fixed-size field value
     tftBoolean, tftUInt8, tftUInt16, tftUInt24, tftInt32, tftInt64,
     tftCurrency, tftDouble,
     // some variable-size field value
     tftVarUInt32, tftVarInt32, tftVarUInt64,
     // text storage
     tftWinAnsi, tftUTF8,
     // BLOB fields
     tftBlobInternal, tftBlobExternal,
     // other variable-size field value
     tftVarInt64);

  /// set of available field types for TSynTable
  TSynTableFieldTypes = set of TSynTableFieldType;

  /// available option types for a field property
  // - tfoIndex is set if an index must be created for this field
  // - tfoUnique is set if field values must be unique (if set, the tfoIndex
  // will be always forced)
  // - tfoCaseInsensitive can be set to make no difference between 'a' and 'A'
  // (by default, comparison is case-sensitive) - this option has an effect
  // not only if tfoIndex or tfoUnique is set, but also for iterating search
  TSynTableFieldOption = (
    tfoIndex, tfoUnique, tfoCaseInsensitive);

  /// set of option types for a field
  TSynTableFieldOptions = set of TSynTableFieldOption;

  /// used to store bit set for all available fiels in a Table
  // - with current format, maximum field count is 64
  TSynTableFieldBits = set of 0..63;

  /// an custom RawByteString type used to store internaly a data in
  // our SBF compact binary format
  TSBFString = type RawByteString;

  /// function prototype used to retrieve the index of a specified property name
  // - 'ID' is handled separately: here must be available only the custom fields
  TSynTableFieldIndex = function(const PropName: RawUTF8): integer of object;

  /// the recognized operators for a TSynTableStatement where clause
  TSynTableStatementOperator = (
     opEqualTo,
     opNotEqualTo,
     opLessThan,
     opLessThanOrEqualTo,
     opGreaterThan,
     opGreaterThanOrEqualTo,
     opIn,
     opIsNull,
     opIsNotNull,
     opLike,
     opContains,
     opFunction);

  TSynTableFieldProperties = class;

  /// one recognized SELECT expression for TSynTableStatement
  TSynTableStatementSelect = record
    /// the column SELECTed for the SQL statement, in the expected order
    // - contains 0 for ID/RowID, or the RTTI field index + 1
    Field: integer;
    /// an optional integer to be added
    // - recognized from .. +123 .. -123 patterns in the select
    ToBeAdded: integer;
    /// the optional column alias, e.g. 'MaxID' for 'max(id) as MaxID'
    Alias: RawUTF8;
    /// the optional function applied to the SELECTed column
    // - e.g. Max(RowID) would store 'Max' and SelectField[0]=0
    // - but Count(*) would store 'Count' and SelectField[0]=0, and
    // set FunctionIsCountStart = TRUE
    FunctionName: RawUTF8;
    /// if the function needs a special process
    // - e.g. funcCountStar for the special Count(*) expression or
    // funcDistinct, funcMax for distinct(...)/max(...) aggregation
    FunctionKnown: (funcNone, funcCountStar, funcDistinct, funcMax);
  end;

  /// the recognized SELECT expressions for TSynTableStatement
  TSynTableStatementSelectDynArray = array of TSynTableStatementSelect;

  /// one recognized WHERE expression for TSynTableStatement
  TSynTableStatementWhere = record
    /// any '(' before the actual expression
    ParenthesisBefore: RawUTF8;
    /// any ')' after the actual expression
    ParenthesisAfter: RawUTF8;
    /// expressions are evaluated as AND unless this field is set to TRUE
    JoinedOR: boolean;
    /// if this expression is preceded by a NOT modifier
    NotClause: boolean;
    /// the index of the field used for the WHERE expression
    // - WhereField=0 for ID, 1 for field # 0, 2 for field #1,
    // and so on... (i.e. WhereField = RTTI field index +1)
    Field: integer;
    /// the operator of the WHERE expression
    Operator: TSynTableStatementOperator;
    /// the SQL function name associated to a Field and Value
    // - e.g. 'INTEGERDYNARRAYCONTAINS' and Field=0 for
    // IntegerDynArrayContains(RowID,10) and ValueInteger=10
    // - Value does not contain anything
    FunctionName: RawUTF8;
    /// the value used for the WHERE expression
    Value: RawUTF8;
    /// the raw value SQL buffer used for the WHERE expression
    ValueSQL: PUTF8Char;
    /// the raw value SQL buffer length used for the WHERE expression
    ValueSQLLen: integer;
    /// an integer representation of WhereValue (used for ID check e.g.)
    ValueInteger: integer;
    /// used to fast compare with SBF binary compact formatted data
    ValueSBF: TSBFString;
    {$ifndef NOVARIANTS}
    /// the value used for the WHERE expression, encoded as Variant
    // - may be a TDocVariant for the IN operator
    ValueVariant: variant;
    {$endif}
  end;

  /// the recognized WHERE expressions for TSynTableStatement
  TSynTableStatementWhereDynArray = array of TSynTableStatementWhere;

  /// used to parse a SELECT SQL statement, following the SQlite3 syntax
  // - handle basic REST commands, i.e. a SELECT over a single table (no JOIN)
  // with its WHERE clause, and result column aliases
  // - handle also aggregate functions like "SELECT Count(*) FROM TableName"
  // - will also parse any LIMIT, OFFSET, ORDER BY, GROUP BY statement clause
  TSynTableStatement = class
  protected
    fSQLStatement: RawUTF8;
    fSelect: TSynTableStatementSelectDynArray;
    fSelectFunctionCount: integer;
    fTableName: RawUTF8;
    fWhere: TSynTableStatementWhereDynArray;
    fOrderByField: TSQLFieldIndexDynArray;
    fGroupByField: TSQLFieldIndexDynArray;
    fWhereHasParenthesis: boolean;
    fOrderByDesc: boolean;
    fLimit: integer;
    fOffset: integer;
    fWriter: TJSONWriter;
  public
    /// parse the given SELECT SQL statement and retrieve the corresponding
    // parameters into this class read-only properties
    // - the supplied GetFieldIndex() method is used to populate the
    // SelectedFields and Where[].Field properties
    // - SimpleFieldsBits is used for '*' field names
    // - SQLStatement is left '' if the SQL statement is not correct
    // - if SQLStatement is set, the caller must check for TableName to match
    // the expected value, then use the Where[] to retrieve the content
    // - if FieldProp is set, then the Where[].ValueSBF property is initialized
    // with the SBF equivalence of the Where[].Value
    constructor Create(const SQL: RawUTF8; GetFieldIndex: TSynTableFieldIndex;
      SimpleFieldsBits: TSQLFieldBits=[0..MAX_SQLFIELDS-1];
      FieldProp: TSynTableFieldProperties=nil);
    /// compute the SELECT column bits from the SelectFields array
    procedure SelectFieldBits(var Fields: TSQLFieldBits; var withID: boolean);

    /// the SELECT SQL statement parsed
    // - equals '' if the parsing failed
    property SQLStatement: RawUTF8 read fSQLStatement;
    /// the column SELECTed for the SQL statement, in the expected order
    property Select: TSynTableStatementSelectDynArray read fSelect;
    /// if the SELECTed expression of this SQL statement have any function defined
    property SelectFunctionCount: integer read fSelectFunctionCount;
    /// the retrieved table name
    property TableName: RawUTF8 read fTableName;
    /// the WHERE clause of this SQL statement
    property Where: TSynTableStatementWhereDynArray read fWhere;
    /// if the WHERE clause contains any ( ) parenthesis expression
    property WhereHasParenthesis: boolean read fWhereHasParenthesis;
    /// recognize an GROUP BY clause with one or several fields
    // - here 0 = ID, otherwise RTTI field index +1
    property GroupByField: TSQLFieldIndexDynArray read fGroupByField;
    /// recognize an ORDER BY clause with one or several fields
    // - here 0 = ID, otherwise RTTI field index +1
    property OrderByField: TSQLFieldIndexDynArray read fOrderByField;
    /// false for default ASC order, true for DESC attribute
    property OrderByDesc: boolean read fOrderByDesc;
    /// the number specified by the optional LIMIT ... clause
    // - set to 0 by default (meaning no LIMIT clause)
    property Limit: integer read fLimit;
    /// the number specified by the optional OFFSET ... clause
    // - set to 0 by default (meaning no OFFSET clause)
    property Offset: integer read fOffset;
    /// optional associated writer
    property Writer: TJSONWriter read fWriter write fWriter;
  end;

  /// function prototype used to retrieve the RECORD data of a specified Index
  // - the index is not the per-ID index, but the "physical" index, i.e. the
  // index value used to retrieve data from low-level (and faster) method
  // - should return nil if Index is out of range
  // - caller must provide a temporary storage buffer to be used optionally
  TSynTableGetRecordData = function(
    Index: integer; var aTempData: RawByteString): pointer of object;

  TSynTable = class;

  {$ifdef SORTCOMPAREMETHOD}
  /// internal value used by TSynTableFieldProperties.SortCompare() method to
  // avoid stack allocation
  TSortCompareTmp = record
    PB1, PB2: PByte;
    L1,L2: integer;
  end;
  {$endif}

  /// store the type properties of a given field / database column
  TSynTableFieldProperties = class
  protected
    /// used during OrderedIndexSort to prevent stack usage
    SortPivot: pointer;
    {$ifdef SORTCOMPAREMETHOD}
    /// internal value used by SortCompare() method to avoid stack allocation
    SortCompareTmp: TSortCompareTmp;
    {$endif}
    /// these two temporary buffers are used to call TSynTableGetRecordData
    DataTemp1, DataTemp2: RawByteString;
    /// the associated table which own this field property
    Owner: TSynTable;
    /// the global size of a default field value, as encoded
    // in our SBF compact binary format
    fDefaultFieldLength: integer;
    /// a default field data, as encoded in our SBF compact binary format
    fDefaultFieldData: TSBFString;
    /// last >=0 value returned by the last OrderedIndexFindAdd() call
    fOrderedIndexFindAdd: integer;
    /// used for internal QuickSort of OrderedIndex[]
    // - call SortCompare() for sorting the items
    procedure OrderedIndexSort(L,R: PtrInt);
    /// retrieve an index from OrderedIndex[] of the given value
    // - call SortCompare() to compare to the reference value
    function OrderedIndexFind(Value: pointer): PtrInt;
    /// retrieve an index where a Value must be added into OrderedIndex[]
    // - call SortCompare() to compare to the reference value
    // - returns -1 if Value is there, or the index where to insert
    // - the returned value (if >= 0) will be stored in fOrderedIndexFindAdd
    function OrderedIndexFindAdd(Value: pointer): PtrInt;
    /// set OrderedIndexReverse[OrderedIndex[aOrderedIndex]] := aOrderedIndex;
    procedure OrderedIndexReverseSet(aOrderedIndex: integer);
  public
    /// the field name
    Name: RawUTF8;
    /// kind of field (defines both value type and storage to be used)
    FieldType: TSynTableFieldType;
    /// the fixed-length size, or -1 for a varInt, -2 for a variable string
    FieldSize: integer;
    /// options of this field
    Options: TSynTableFieldOptions;
    /// contains the offset of this field, in case of fixed-length field
    // - normaly, fixed-length fields are stored in the beginning of the record
    // storage: in this case, a value >= 0 will point to the position of the
    // field value of this field
    // - if the value is < 0, its absolute will be the field number to be counted
    // after TSynTable.fFieldVariableOffset (-1 for first item)
    Offset: integer;
    /// number of the field in the table (starting at 0)
    FieldNumber: integer;
    /// if allocated, contains the storage indexes of every item, in sorted order
    // - only available if tfoIndex is in Options
    // - the index is not the per-ID index, but the "physical" index, i.e. the
    // index value used to retrieve data from low-level (and faster) method
    OrderedIndex: TIntegerDynArray;
    /// if allocated, contains the reverse storage index of OrderedIndex
    // - i.e. OrderedIndexReverse[OrderedIndex[i]] := i;
    // - used to speed up the record update procedure with huge number of
    // records
    OrderedIndexReverse: TIntegerDynArray;
    /// number of items in OrderedIndex[]
    // - is set to 0 when the content has been modified (mark force recreate)
    OrderedIndexCount: integer;
    /// if set to TRUE after an OrderedIndex[] refresh but with not sorting
    // - OrderedIndexSort(0,OrderedIndexCount-1) must be called before using
    // the OrderedIndex[] array
    // - you should call OrderedIndexRefresh method to ensure it is sorted
    OrderedIndexNotSorted: boolean;
    /// all TSynValidate instances registered per each field
    Filters: TObjectList;
    /// all TSynValidate instances registered per each field
    Validates: TObjectList;
    /// low-level binary comparison used by IDSort and TSynTable.IterateJSONValues
    // - P1 and P2 must point to the values encoded in our SBF compact binary format
    {$ifdef SORTCOMPAREMETHOD}
    function SortCompare(P1,P2: PUTF8Char): PtrInt;
    {$else}
    SortCompare: TUTF8Compare;
    {$endif}

    /// read entry from a specified file reader
    constructor CreateFrom(var RD: TFileBufferReader);
    /// release associated memory and objects
    destructor Destroy; override;
    /// save entry to a specified file writer
    procedure SaveTo(WR: TFileBufferWriter);
    /// decode the value from our SBF compact binary format into UTF-8 JSON
    // - returns the next FieldBuffer value
    function GetJSON(FieldBuffer: pointer; W: TTextWriter): pointer;
    /// decode the value from our SBF compact binary format into UTF-8 text
    // - this method does not check for FieldBuffer to be not nil -> caller
    // should check this explicitely
    function GetValue(FieldBuffer: pointer): RawUTF8;
    /// decode the value from a record buffer into an Boolean
    // - will call Owner.GetData to retrieve then decode the field SBF content
    function GetBoolean(RecordBuffer: pointer): Boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// decode the value from a record buffer into an integer
    // - will call Owner.GetData to retrieve then decode the field SBF content
    function GetInteger(RecordBuffer: pointer): Integer;
    /// decode the value from a record buffer into an Int64
    // - will call Owner.GetData to retrieve then decode the field SBF content
    function GetInt64(RecordBuffer: pointer): Int64;
    /// decode the value from a record buffer into an floating-point value
    // - will call Owner.GetData to retrieve then decode the field SBF content
    function GetDouble(RecordBuffer: pointer): Double;
    /// decode the value from a record buffer into an currency value
    // - will call Owner.GetData to retrieve then decode the field SBF content
    function GetCurrency(RecordBuffer: pointer): Currency;
    /// decode the value from a record buffer into a RawUTF8 string
    // - will call Owner.GetData to retrieve then decode the field SBF content
    function GetRawUTF8(RecordBuffer: pointer): RawUTF8;
    {$ifndef NOVARIANTS}
    /// decode the value from our SBF compact binary format into a Variant
    function GetVariant(FieldBuffer: pointer): Variant; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// decode the value from our SBF compact binary format into a Variant
    procedure GetVariant(FieldBuffer: pointer; var result: Variant); overload;
    {$endif}
    /// retrieve the binary length (in bytes) of some SBF compact binary format
    function GetLength(FieldBuffer: pointer): Integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// create some SBF compact binary format from a Delphi binary value
    // - will return '' if the field type doesn't match a boolean
    function SBF(const Value: Boolean): TSBFString; overload;
    /// create some SBF compact binary format from a Delphi binary value
    // - will encode any byte, word, integer, cardinal, Int64 value
    // - will return '' if the field type doesn't match an integer
    function SBF(const Value: Int64): TSBFString; overload;
    /// create some SBF compact binary format from a Delphi binary value
    // - will encode any byte, word, integer, cardinal value
    // - will return '' if the field type doesn't match an integer
    function SBF(const Value: Integer): TSBFString; overload;
    /// create some SBF compact binary format from a Delphi binary value
    // - will return '' if the field type doesn't match a currency
    // - we can't use SBF() method name because of Currency/Double ambiguity
    function SBFCurr(const Value: Currency): TSBFString;
    /// create some SBF compact binary format from a Delphi binary value
    // - will return '' if the field type doesn't match a floating-point
    // - we can't use SBF() method name because of Currency/Double ambiguity
    function SBFFloat(const Value: Double): TSBFString;
    /// create some SBF compact binary format from a Delphi binary value
    // - expect a RawUTF8 string: will be converted to WinAnsiString
    // before storage, for tftWinAnsi
    // - will return '' if the field type doesn't match a string
    function SBF(const Value: RawUTF8): TSBFString; overload;
    /// create some SBF compact binary format from a BLOB memory buffer
    // - will return '' if the field type doesn't match tftBlobInternal
    function SBF(Value: pointer; ValueLen: integer): TSBFString; overload;
    /// convert any UTF-8 encoded value into our SBF compact binary format
    // - can be used e.g. from a WHERE clause, for fast comparison in
    // TSynTableStatement.WhereValue content using OrderedIndex[]
    // - is the reverse of GetValue/GetRawUTF8 methods above
    function SBFFromRawUTF8(const aValue: RawUTF8): TSBFString;
    {$ifndef NOVARIANTS}
    /// create some SBF compact binary format from a Variant value
    function SBF(const Value: Variant): TSBFString; overload;
    {$endif}

    /// will update then sort the array of indexes used for the field index
    // - the OrderedIndex[] array is first refreshed according to the
    // aOldIndex, aNewIndex parameters: aOldIndex=-1 for Add, aNewIndex=-1 for
    // Delete, or both >= 0 for update
    // - call with both indexes = -1 will sort the existing OrderedIndex[] array
    // - GetData property must have been set with a method returning a pointer
    // to the field data for a given index (this index is not the per-ID index,
    // but the "physical" index, i.e. the index value used to retrieve data
    // from low-level (and fast) GetData method)
    // - aOldRecordData and aNewRecordData can be specified in order to guess
    // if the field data has really been modified (speed up the update a lot
    // to only sort indexed fields if its content has been really modified)
    // - returns FALSE if any parameter is invalid
    function OrderedIndexUpdate(aOldIndex, aNewIndex: integer;
      aOldRecordData, aNewRecordData: pointer): boolean;
    /// retrieve one or more "physical" indexes matching a WHERE Statement
    // - is faster than O(1) GetIteraring(), because will use O(log(n)) binary
    // search using the OrderedIndex[] array
    // - returns the resulting indexes as a a sorted list in MatchIndex/MatchIndexCount
    // - if the indexes are already present in the list, won't duplicate them
    // - WhereSBFValue must be a valid SBF formated field buffer content
    // - the Limit parameter is similar to the SQL LIMIT clause: if greater than 0,
    // an upper bound on the number of rows returned is placed (e.g. set Limit=1
    // to only retrieve the first match)
    // - GetData property must have been set with a method returning a pointer
    // to the field data for a given index (this index is not the per-ID index,
    // but the "physical" index, i.e. the index value used to retrieve data
    // from low-level (and fast) GetData method)
    // - in this method, indexes are not the per-ID indexes, but the "physical"
    // indexes, i.e. each index value used to retrieve data from low-level
    // (and fast) GetData method
    function OrderedIndexMatch(WhereSBFValue: pointer;
      var MatchIndex: TIntegerDynArray; var MatchIndexCount: integer;
      Limit: Integer=0): Boolean;
    /// will force refresh the OrderedIndex[] array
    // - to be called e.g. if OrderedIndexNotSorted = TRUE, if you want to
    // access to the OrderedIndex[] array
    procedure OrderedIndexRefresh;
    /// register a custom filter or validation rule to the class for this field
    // - this will be used by Filter() and Validate() methods
    // - will return the specified associated TSynFilterOrValidate instance
    // - a TSynValidateTableUniqueField is always added by
    // TSynTable.AfterFieldModif if tfoUnique is set in Options
    function AddFilterOrValidate(aFilter: TSynFilterOrValidate): TSynFilterOrValidate;
    /// check the registered constraints
    // - returns '' on success
    // - returns an error message e.g. if a tftUnique constraint failed
    // - RecordIndex=-1 in case of adding, or the physical index of the updated record
    function Validate(RecordBuffer: pointer; RecordIndex: integer): string;
    /// some default SBF compact binary format content
    property SBFDefault: TSBFString read fDefaultFieldData;
  end;


{$ifndef DELPHI5OROLDER}

  /// a pointer to structure used to store a TSynTable record
  PSynTableData = ^TSynTableData;

  {$A-} { packet object not allowed since Delphi 2009 :( }
  /// used to store a TSynTable record using our SBF compact binary format
  // - this object can be created on the stack
  // - it is mapped into a variant TVarData, to be retrieved by the
  // TSynTable.Data method - but direct allocation of a TSynTableData on the
  // stack is faster (due to the Variant overhead)
  // - is defined either as an object either as a record, due to a bug
  // in Delphi 2009/2010 compiler (at least): this structure is not initialized
  // if defined as an object on the stack, but will be as a record :(
  {$ifdef UNICODE}TSynTableData = record{$else}TSynTableData = object{$endif UNICODE}
  {$ifdef UNICODE}private{$else}protected{$endif UNICODE}
    VType: TVarType;
    Filler: array[1..SizeOf(TVarData)-SizeOf(TVarType)-SizeOf(pointer)*2-4] of byte;
    VID: integer;
    VTable: TSynTable;
    VValue: TSBFString;
    {$ifndef NOVARIANTS}
    function GetFieldValue(const FieldName: RawUTF8): Variant; overload;
    procedure GetFieldVariant(const FieldName: RawUTF8; var result: Variant);
    procedure SetFieldValue(const FieldName: RawUTF8; const Value: Variant); overload;
    {$endif}
    /// raise an exception if VTable=nil
    procedure CheckVTableInitialized;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize a record data content for a specified table
    // - a void content is set
    procedure Init(aTable: TSynTable; aID: Integer=0); overload; {$ifdef HASINLINE}inline;{$endif}
    /// initialize a record data content for a specified table
    // - the specified SBF content is store inside this TSynTableData
    procedure Init(aTable: TSynTable; aID: Integer; RecordBuffer: pointer;
      RecordBufferLen: integer); overload;
    /// the associated record ID
    property ID: integer read VID write VID;
    /// the associated TSynTable instance
    property Table: TSynTable read VTable write VTable;
    /// the record content, SBF compact binary format encoded
    property SBF: TSBFString read VValue;
    {$ifndef NOVARIANTS}
    /// set or retrieve a field value from a variant data
    property Field[const FieldName: RawUTF8]: Variant read GetFieldValue write SetFieldValue;
    /// get a field value for a specified field
    // - this method is faster than Field[], because it won't look for the field name
    function GetFieldValue(aField: TSynTableFieldProperties): Variant; overload;
    /// set a field value for a specified field
    // - this method is faster than Field[], because it won't look for the field name
    procedure SetFieldValue(aField: TSynTableFieldProperties; const Value: Variant); overload;
      {$ifdef HASINLINE}inline;{$endif}
    {$endif}
    /// set a field value for a specified field, from SBF-encoded data
    // - this method is faster than the other, because it won't look for the field
    // name nor make any variant conversion
    procedure SetFieldSBFValue(aField: TSynTableFieldProperties; const Value: TSBFString);
    /// get a field value for a specified field, into SBF-encoded data
    // - this method is faster than the other, because it won't look for the field
    // name nor make any variant conversion
    function GetFieldSBFValue(aField: TSynTableFieldProperties): TSBFString;
    /// filter the SBF buffer record content with all registered filters
    // - all field values are filtered in-place, following our SBF compact
    // binary format encoding for this record
    procedure FilterSBFValue; {$ifdef HASINLINE}inline;{$endif}
    /// check the registered constraints according to a record SBF buffer
    // - returns '' on success
    // - returns an error message e.g. if a tftUnique constraint failed
    // - RecordIndex=-1 in case of adding, or the physical index of the updated record
    function ValidateSBFValue(RecordIndex: integer): string;
  end;
  {$A+} { packet object not allowed since Delphi 2009 :( }
{$endif DELPHI5OROLDER}

  PUpdateFieldEvent = ^TUpdateFieldEvent;

  /// an opaque structure used for TSynTable.UpdateFieldEvent method
  TUpdateFieldEvent = record
    /// the number of record added
    Count: integer;
    /// the list of IDs added
    // - this list is already in increasing order, because GetIterating was
    // called with the ioID order
    IDs: TIntegerDynArray;
    /// the offset of every record added
    // - follows the IDs[] order
    Offsets64: TInt64DynArray;
    /// previous indexes: NewIndexs[oldIndex] := newIndex
    NewIndexs: TIntegerDynArray;
    /// the list of existing field in the previous data
    AvailableFields: TSQLFieldBits;
    /// where to write the updated data
    WR: TFileBufferWriter;
  end;

  /// will define a validation to be applied to a TSynTableFieldProperties field
  // - a typical usage is to validate a value to be unique in the table
  // (implemented in the TSynValidateTableUniqueField class)
  // - the optional associated parameters are to be supplied JSON-encoded
  // - ProcessField and ProcessRecordIndex properties will be filled before
  // Process method call by TSynTableFieldProperties.Validate()
  TSynValidateTable = class(TSynValidate)
  protected
    fProcessField: TSynTableFieldProperties;
    fProcessRecordIndex: integer;
  public
    /// the associated TSQLRest instance
    // - this value is filled by TSynTableFieldProperties.Validate with its
    // self value to be used for the validation
    // - it can be used in the overridden Process method
    property ProcessField: TSynTableFieldProperties read fProcessField write fProcessField;
    /// the associated record index (in case of update)
    // - is set to -1 in case of adding, or the physical index of the updated record
    // - this value is filled by TSynTableFieldProperties.Validate
    // - it can be used in the overridden Process method
    property ProcessRecordIndex: integer read fProcessRecordIndex write fProcessRecordIndex;
  end;

  /// will define a validation for a TSynTableFieldProperties Unique field
  // - implement constraints check e.g. if tfoUnique is set in Options
  // - it will check that the field value is not void
  // - it will check that the field value is not a duplicate
  TSynValidateTableUniqueField = class(TSynValidateTable)
  public
    /// perform the unique field validation action to the specified value
    // - duplication value check will use the ProcessField  and
    // ProcessRecordIndex properties, which will be filled before call by
    // TSynTableFieldProperties.Validate()
    // - aFieldIndex parameter is not used here, since we have already the
    // ProcessField property set
    // - here the Value is expected to be UTF-8 text, as converted from our SBF
    // compact binary format via e.g. TSynTableFieldProperties.GetValue /
    // GetRawUTF8: this is mandatory to have the validation rule fit with other
    // TSynValidateTable classes
    function Process(aFieldIndex: integer; const Value: RawUTF8; var ErrorMsg: string): boolean; override;
  end;

  /// store the description of a table with records, to implement a Database
  // - can be used with several storage engines, for instance TSynBigTableRecord
  // - each record can have up to 64 fields
  // - a mandatory ID field must be handled by the storage engine itself
  // - will handle the storage of records into our SBF compact binary format, in
  // which fixed-length fields are stored leftmost side, then variable-length
  // fields follow
  TSynTable = class
  protected
    fTableName: RawUTF8;
    /// list of TSynTableFieldProperties instances
    fField: TObjectList;
    /// offset of the first variable length value field
    fFieldVariableOffset: PtrUInt;
    /// index of the first variable length value field
    // - equals -1 if no variable length field exists
    fFieldVariableIndex: integer;
    /// bit is set for a tftWinAnsi, tftUTF8 or tftBlobInternal kind of field
    // - these kind of field are encoded as a VarInt length, then the data
    fFieldIsVarString: TSynTableFieldBits;
    /// bit is set for a tftBlobExternal kind of field e.g.
    fFieldIsExternal: TSynTableFieldBits;
    /// event used for proper data retrieval of a given record buffer
    fGetRecordData: TSynTableGetRecordData;
    /// the global size of a default value, as encoded in our SBF compact binary format
    fDefaultRecordLength: integer;
    /// a default record data, as encoded in our SBF compact binary format
    fDefaultRecordData: TSBFString;
    /// list of TSynTableFieldProperties added via all AddField() call
    fAddedField: TList;
    /// true if any field has a tfoUnique option set
    fFieldHasUniqueIndexes: boolean;
    function GetFieldType(Index: integer): TSynTableFieldProperties;
    function GetFieldCount: integer;
    function GetFieldFromName(const aName: RawUTF8): TSynTableFieldProperties;
    function GetFieldIndexFromName(const aName: RawUTF8): integer;
    /// this method matchs the TSynTableFieldIndex event type
    function GetFieldIndexFromShortName(const aName: ShortString): integer;
    /// refresh Offset,FieldNumber,FieldSize and fFieldVariableIndex,fFieldVariableOffset
    procedure AfterFieldModif;
  public
    /// create a table definition instance
    constructor Create(const aTableName: RawUTF8);
    /// create a table definition instance from a specified file reader
    procedure LoadFrom(var RD: TFileBufferReader);
    /// release used memory
    destructor Destroy; override;
    /// save field properties to a specified file writer
    procedure SaveTo(WR: TFileBufferWriter);

    /// retrieve to the corresponding data address of a given field
    function GetData(RecordBuffer: PUTF8Char; Field: TSynTableFieldProperties): pointer;
    /// add a field description to the table
    // - warning: the class responsible of the storage itself must process the
    // data already stored when a field is created, e.g. in
    // TSynBigTableRecord.AddFieldUpdate method
    // - physical order does not necessary follow the AddField() call order:
    // for better performance, it will try to store fixed-sized record first,
    // multiple of 4 bytes first (access is faster if dat is 4 byte aligned),
    // then variable-length after fixed-sized fields; in all case, a field
    // indexed will be put first
    function AddField(const aName: RawUTF8; aType: TSynTableFieldType;
      aOptions: TSynTableFieldOptions=[]): TSynTableFieldProperties;
    /// update a record content
    // - return the updated record data, in our SBF compact binary format
    // - if NewFieldData is not specified, a default 0 or '' value is appended
    // - if NewFieldData is set, it must match the field value kind
    // - warning: this method will update result in-place, so RecordBuffer MUST
    // be <> pointer(result) or data corruption may occur
    procedure UpdateFieldData(RecordBuffer: PUTF8Char; RecordBufferLen,
      FieldIndex: integer; var result: TSBFString; const NewFieldData: TSBFString='');
    /// update a record content after any AddfieldUpdate, to refresh the data
    // - AvailableFields must contain the list of existing fields in the previous data
    function UpdateFieldRecord(RecordBuffer: PUTF8Char; var AvailableFields: TSQLFieldBits): TSBFString;
    /// this Event is to be called for all data records (via a GetIterating method)
    // after any AddfieldUpdate, to refresh the data
    // - Opaque is in fact a pointer to a TUpdateFieldEvent record, and will contain
    // all parameters set by TSynBigTableRecord.AddFieldUpdate, including a
    // TFileBufferWriter instance to use to write the recreated data
    // - it will work with either any newly added field, handly also field data
    // order change in SBF record (e.g. when a fixed-sized field has been added
    // on a record containing variable-length fields)
    function UpdateFieldEvent(Sender: TObject; Opaque: pointer; ID, Index: integer;
      Data: pointer; DataLen: integer): boolean;
    /// event which must be called by the storage engine when some values are modified
    // - if aOldIndex and aNewIndex are both >= 0, the corresponding aOldIndex
    // will be replaced by aNewIndex value (i.e. called in case of a data Update)
    // - if aOldIndex is -1 and aNewIndex is >= 0, aNewIndex refers to a just
    // created item (i.e. called in case of a data Add)
    // - if aOldIndex is >= 0 and aNewIndex is -1, aNewIndex refers to a just
    // deleted item (i.e. called in case of a data Delete)
    // - will update then sort all existing TSynTableFieldProperties.OrderedIndex
    // values
    // - the GetDataBuffer protected virtual method must have been overridden to
    // properly return the record data for a given "physical/stored" index
    // - aOldRecordData and aNewRecordData can be specified in order to guess
    // if the field data has really been modified (speed up the update a lot
    // to only sort indexed fields if its content has been really modified)
    procedure FieldIndexModify(aOldIndex, aNewIndex: integer;
      aOldRecordData, aNewRecordData: pointer);
    /// return the total length of the given record buffer, encoded in our SBF
    // compact binary format
    function DataLength(RecordBuffer: pointer): integer;
    {$ifndef NOVARIANTS}
    /// create a Variant able to access any field content via late binding
    // - i.e. you can use Var.Name to access the 'Name' field of record Var
    // - if you leave ID and RecordBuffer void, a void record is created
    function Data(aID: integer=0; RecordBuffer: pointer=nil;
      RecordBufferLen: Integer=0): Variant; overload;
    {$endif NOVARIANTS}
    /// return a default content for ALL record fields
    // - uses our SBF compact binary format
    property DefaultRecordData: TSBFString read fDefaultRecordData;
    /// list of TSynTableFieldProperties added via all AddField() call
    // - this list will allow TSynBigTableRecord.AddFieldUpdate to refresh
    // the data on disk according to the new field configuration
    property AddedField: TList read fAddedField write fAddedField;
    /// offset of the first variable length value field
    property FieldVariableOffset: PtrUInt read fFieldVariableOffset;
  public
    {$ifndef DELPHI5OROLDER}
    /// create a TJSONWriter, ready to be filled with GetJSONValues(W) below
    // - will initialize all TJSONWriter.ColNames[] values according to the
    // specified Fields index list, and initialize the JSON content
    function CreateJSONWriter(JSON: TStream; Expand, withID: boolean;
      const Fields: TSQLFieldIndexDynArray): TJSONWriter; overload;
    /// create a TJSONWriter, ready to be filled with GetJSONValues(W) below
    // - will initialize all TJSONWriter.ColNames[] values according to the
    // specified Fields bit set, and initialize the JSON content
    function CreateJSONWriter(JSON: TStream; Expand, withID: boolean;
      const Fields: TSQLFieldBits): TJSONWriter; overload;
    (** return the UTF-8 encoded JSON objects for the values contained
      in the specified RecordBuffer encoded in our SBF compact binary format,
      according to the Expand/WithID/Fields parameters of W
      - if W.Expand is true, JSON data is an object, for direct use with any Ajax or .NET client:
      ! {"col1":val11,"col2":"val12"}
      - if W.Expand is false, JSON data is serialized (as used in TSQLTableJSON)
      ! { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
      - only fields with a bit set in W.Fields will be appended
      - if W.WithID is true, then the first ID field value is included *)
    procedure GetJSONValues(aID: integer; RecordBuffer: PUTF8Char; W: TJSONWriter);
    /// can be used to retrieve all values matching a preparated TSynTableStatement
    // - this method matchs the TSynBigTableIterateEvent callback definition
    // - Sender will be the TSynBigTable instance, and Opaque will point to a
    // TSynTableStatement instance (with all fields initialized, including Writer)
    function IterateJSONValues(Sender: TObject; Opaque: pointer; ID: integer;
      Data: pointer; DataLen: integer): boolean;
    {$endif DELPHI5OROLDER}
    /// check the registered constraints according to a record SBF buffer
    // - returns '' on success
    // - returns an error message e.g. if a tftUnique constraint failed
    // - RecordIndex=-1 in case of adding, or the physical index of the updated record
    function Validate(RecordBuffer: pointer; RecordIndex: integer): string;
    /// filter the SBF buffer record content with all registered filters
    // - all field values are filtered in-place, following our SBF compact
    // binary format encoding for this record
    procedure Filter(var RecordBuffer: TSBFString);

    /// event used for proper data retrieval of a given record buffer, according
    // to the physical/storage index value (not per-ID index)
    // - if not set, field indexes won't work
    // - will be mapped e.g. to TSynBigTable.GetPointerFromPhysicalIndex
    property GetRecordData: TSynTableGetRecordData read fGetRecordData write fGetRecordData;
  public
    /// the internal Table name used to identify it (e.g. from JSON or SQL)
    // - similar to the SQL Table name
    property TableName: RawUTF8 read fTableName write fTableName;
    /// number of fields in this table
    property FieldCount: integer read GetFieldCount;
    /// retrieve the properties of a given field
    // - returns nil if the specified Index is out of range
    property Field[Index: integer]: TSynTableFieldProperties read GetFieldType;
    /// retrieve the properties of a given field
    // - returns nil if the specified Index is out of range
    property FieldFromName[const aName: RawUTF8]: TSynTableFieldProperties read GetFieldFromName; default;
    /// retrieve the index of a given field
    // - returns -1 if the specified Index is out of range
    property FieldIndexFromName[const aName: RawUTF8]: integer read GetFieldIndexFromName;
    /// read-only access to the Field list
    property FieldList: TObjectList read fField;
    /// true if any field has a tfoUnique option set
    property HasUniqueIndexes: boolean read fFieldHasUniqueIndexes;
  end;

{$ifndef NOVARIANTS}
  /// a custom variant type used to have direct access to a record content
  // - use TSynTable.Data method to retrieve such a Variant
  // - this variant will store internaly a SBF compact binary format
  // representation of the record content
  // - uses internally a TSynTableData object
  TSynTableVariantType = class(TSynInvokeableVariantType)
  protected
    procedure IntGet(var Dest: TVarData; const V: TVarData; Name: PAnsiChar); override;
    procedure IntSet(const V, Value: TVarData; Name: PAnsiChar); override;
  public
    /// retrieve the SBF compact binary format representation of a record content
    class function ToSBF(const V: Variant): TSBFString;
    /// retrieve the ID value associated to a record content
    class function ToID(const V: Variant): integer;
    /// retrieve the TSynTable instance associated to a record content
    class function ToTable(const V: Variant): TSynTable;
    /// clear the content
    procedure Clear(var V: TVarData); override;
    /// copy two record content
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
  end;
{$endif NOVARIANTS}

const
  /// used by TSynTableStatement.WhereField for "SELECT .. FROM TableName WHERE ID=?"
  SYNTABLESTATEMENTWHEREID = 0;


/// low-level integer comparison according to a specified operator
// - SBF must point to the values encoded in our SBF compact binary format
// - Value must contain the plain integer value
// - Value can be a Currency accessed via a PInt64
// - will work only for tftBoolean, tftUInt8, tftUInt16, tftUInt24,
// tftInt32, tftInt64 and tftCurrency field types
// - will handle only soEqualTo...soGreaterThanOrEqualTo operators
// - if SBFEnd is not nil, it will test for all values until SBF>=SBFEnd
// (can be used for tftArray)
// - returns true if both values match, or false otherwise
function CompareOperator(FieldType: TSynTableFieldType; SBF, SBFEnd: PUTF8Char;
  Value: Int64; Oper: TCompareOperator): boolean; overload;

/// low-level floating-point comparison according to a specified operator
// - SBF must point to the values encoded in our SBF compact binary format
// - Value must contain the plain floating-point value
// - will work only for tftDouble field type
// - will handle only soEqualTo...soGreaterThanOrEqualTo operators
// - if SBFEnd is not nil, it will test for all values until SBF>=SBFEnd
// (can be used for tftArray)
// - returns true if both values match, or false otherwise
function CompareOperator(SBF, SBFEnd: PUTF8Char;
  Value: double; Oper: TCompareOperator): boolean; overload;

/// low-level text comparison according to a specified operator
// - SBF must point to the values encoded in our SBF compact binary format
// - Value must contain the plain text value, in the same encoding (either
// WinAnsi either UTF-8, as FieldType defined for the SBF value)
// - will work only for tftWinAnsi and tftUTF8 field types
// - will handle all kind of operators (including soBeginWith, soContains and
// soSoundsLike*) but soSoundsLike* won't make use of the CaseSensitive parameter
// - for soSoundsLikeEnglish, soSoundsLikeFrench and soSoundsLikeSpanish
// operators, Value is not a real PUTF8Char but a prepared PSynSoundEx
// - if SBFEnd is not nil, it will test for all values until SBF>=SBFEnd
// (can be used for tftArray)
// - returns true if both values match, or false otherwise
function CompareOperator(FieldType: TSynTableFieldType; SBF, SBFEnd: PUTF8Char;
  Value: PUTF8Char; ValueLen: integer; Oper: TCompareOperator;
  CaseSensitive: boolean): boolean; overload;

/// convert any AnsiString content into our SBF compact binary format storage
procedure ToSBFStr(const Value: RawByteString; out Result: TSBFString);


{ ************ low-level buffer processing functions ************************* }

type
  /// implements a thread-safe Bloom Filter storage
  // - a "Bloom Filter" is a space-efficient probabilistic data structure,
  // that is used to test whether an element is a member of a set. False positive
  // matches are possible, but false negatives are not. Elements can be added to
  // the set, but not removed. Typical use cases are to avoid unecessary
  // slow disk or network access if possible, when a lot of items are involved.
  // - memory use is very low, when compared to storage of all values: fewer
  // than 10 bits per element are required for a 1% false positive probability,
  // independent of the size or number of elements in the set - for instance,
  // storing 10,000,000 items presence with 1% of false positive ratio
  // would consume only 11.5 MB of memory, using 7 hash functions
  // - use Insert() methods to add an item to the internal bits array, and
  // Reset() to clear all bits array, if needed
  // - MayExist() function would check if the supplied item was probably set
  // - SaveTo() and LoadFrom() methods allow transmission of the bits array,
  // for a disk/database storage or transmission over a network
  // - internally, several (hardware-accelerated) crc32c hash functions will be
  // used, with some random seed values, to simulate several hashing functions
  // - Insert/MayExist/Reset methods are thread-safe
  TSynBloomFilter = class(TSynPersistentLock)
  private
    fSize: cardinal;
    fFalsePositivePercent: double;
    fBits: cardinal;
    fHashFunctions: cardinal;
    fInserted: cardinal;
    fStore: RawByteString;
    function GetInserted: cardinal;
  public
    /// initialize the internal bits storage for a given number of items
    // - by default, internal bits array size will be guess from a 1 % false
    // positive rate - but you may specify another value, to reduce memory use
    // - this constructor would compute and initialize Bits and HashFunctions
    // corresponding to the expected false positive ratio
    constructor Create(aSize: integer; aFalsePositivePercent: double = 1); reintroduce; overload;
    /// initialize the internal bits storage from a SaveTo() binary buffer
    // - this constructor will initialize the internal bits array calling LoadFrom()
    constructor Create(const aSaved: RawByteString; aMagic: cardinal=$B1003F11); reintroduce; overload;
    /// add an item in the internal bits array storage
    // - this method is thread-safe
    procedure Insert(const aValue: RawByteString); overload;
    /// add an item in the internal bits array storage
    // - this method is thread-safe
    procedure Insert(aValue: pointer; aValueLen: integer); overload; virtual;
    /// clear the internal bits array storage
    // - you may call this method after some time, if some items may have
    // been removed, to reduce false positives
    // - this method is thread-safe
    procedure Reset; virtual;
    /// returns TRUE if the supplied items was probably set via Insert()
    // - some false positive may occur, but not much than FalsePositivePercent
    // - this method is thread-safe
    function MayExist(const aValue: RawByteString): boolean; overload;
    /// returns TRUE if the supplied items was probably set via Insert()
    // - some false positive may occur, but not much than FalsePositivePercent
    // - this method is thread-safe
    function MayExist(aValue: pointer; aValueLen: integer): boolean; overload;
    /// store the internal bits array into a binary buffer
    // - may be used to transmit or store the state of a dataset, avoiding
    // to recompute all Insert() at program startup, or to synchronize
    // networks nodes information and reduce the number of remote requests
    function SaveTo(aMagic: cardinal=$B1003F11): RawByteString; overload;
    /// store the internal bits array into a binary buffer
    // - may be used to transmit or store the state of a dataset, avoiding
    // to recompute all Insert() at program startup, or to synchronize
    // networks nodes information and reduce the number of remote requests
    procedure SaveTo(aDest: TFileBufferWriter; aMagic: cardinal=$B1003F11); overload;
    /// read the internal bits array from a binary buffer
    // - as previously serialized by the SaveTo method
    // - may be used to transmit or store the state of a dataset
    function LoadFrom(const aSaved: RawByteString; aMagic: cardinal=$B1003F11): boolean; overload;
    /// read the internal bits array from a binary buffer
    // - as previously serialized by the SaveTo method
    // - may be used to transmit or store the state of a dataset
    function LoadFrom(P: PByte; PLen: integer; aMagic: cardinal=$B1003F11): boolean; overload; virtual;
  published
    /// maximum number of items which are expected to be inserted
    property Size: cardinal read fSize;
    /// expected percentage (1..100) of false positive results for MayExists()
    property FalsePositivePercent: double read fFalsePositivePercent;
    /// number of bits stored in the internal bits array
    property Bits: cardinal read fBits;
    /// how many hash functions would be applied for each Insert()
    property HashFunctions: cardinal read fHashFunctions;
    /// how many times the Insert() method has been called
    property Inserted: cardinal read GetInserted;
  end;

  /// implements a thread-safe differential Bloom Filter storage
  // - this inherited class is able to compute incremental serialization of
  // its internal bits array, to reduce network use
  // - an obfuscated revision counter is used to identify storage history
  TSynBloomFilterDiff = class(TSynBloomFilter)
  protected
    fRevision: Int64;
    fSnapShotAfterMinutes: cardinal;
    fSnapshotAfterInsertCount: cardinal;
    fSnapshotTimestamp: Int64;
    fSnapshotInsertCount: cardinal;
    fKnownRevision: Int64;
    fKnownStore: RawByteString;
  public
    /// add an item in the internal bits array storage
    // - this overloaded thread-safe method would compute fRevision
    procedure Insert(aValue: pointer; aValueLen: integer); override;
    /// clear the internal bits array storage
    // - this overloaded thread-safe method would reset fRevision
    procedure Reset; override;
    /// store the internal bits array into an incremental binary buffer
    // - here the difference from a previous SaveToDiff revision will be computed
    // - if aKnownRevision is outdated (e.g. if equals 0), the whole bits array
    // would be returned, and around 10 bits per item would be transmitted
    // (for 1% false positive ratio)
    // - incremental retrieval would then return around 10 bytes per newly added
    // item since the last snapshot reference state (with 1% ratio, i.e. 7 hash
    // functions)
    function SaveToDiff(const aKnownRevision: Int64): RawByteString;
    /// use the current internal bits array state as known revision
    // - is done the first time SaveToDiff() is called, then after 1/32th of
    // the filter size has been inserted (see SnapshotAfterInsertCount property),
    // or after SnapShotAfterMinutes property timeout period
    procedure DiffSnapshot;
    /// retrieve the revision number from an incremental binary buffer
    // - returns 0 if the supplied binary buffer does not match this bloom filter
    function DiffKnownRevision(const aDiff: RawByteString): Int64;
    /// read the internal bits array from an incremental binary buffer
    // - as previously serialized by the SaveToDiff() method
    // - may be used to transmit or store the state of a dataset
    // - returns false if the supplied content is incorrect, e.g. if the known
    // revision is deprecated
    function LoadFromDiff(const aDiff: RawByteString): boolean;
    /// the opaque revision number of this internal storage
    // - is in fact the Unix timestamp shifted by 31 bits, and an incremental
    // counter: this pattern will allow consistent IDs over several ServPanels
    property Revision: Int64 read fRevision;
    /// after how many Insert() the internal bits array storage should be
    // promoted as known revision
    // - equals Size div 32 by default
    property SnapshotAfterInsertCount: cardinal read fSnapshotAfterInsertCount
      write fSnapshotAfterInsertCount;
    /// after how many time the internal bits array storage should be
    // promoted as known revision
    // - equals 30 minutes by default
    property SnapShotAfterMinutes: cardinal read fSnapShotAfterMinutes
      write fSnapShotAfterMinutes;
  end;


/// RLE compression of a memory buffer containing mostly zeros
// - will store the number of consecutive zeros instead of plain zero bytes
// - used for spare bit sets, e.g. TSynBloomFilter serialization
// - will also compute the crc32c of the supplied content
// - use ZeroDecompress() to expand the compressed result
// - resulting content would be at most 14 bytes bigger than the input
// - you may use this function before SynLZ compression
procedure ZeroCompress(P: PAnsiChar; Len: integer; Dest: TFileBufferWriter);

/// RLE uncompression of a memory buffer containing mostly zeros
// - returns Dest='' if P^ is not a valid ZeroCompress() function result
// - used for spare bit sets, e.g. TSynBloomFilter serialization
// - will also check the crc32c of the supplied content
procedure ZeroDecompress(P: PByte; Len: integer; {$ifdef FPC}var{$else}out{$endif} Dest: RawByteString);

/// RLE compression of XORed memory buffers resulting in mostly zeros
// - will perform ZeroCompress(Dest^ := New^ xor Old^) without any temporary
// memory allocation
// - is used  e.g. by TSynBloomFilterDiff.SaveToDiff() in incremental mode
// - will also compute the crc32c of the supplied content
procedure ZeroCompressXor(New,Old: PAnsiChar; Len: cardinal; Dest: TFileBufferWriter);

/// RLE uncompression and ORing of a memory buffer containing mostly zeros
// - will perform Dest^ := Dest^ or ZeroDecompress(P^) without any temporary
// memory allocation
// - is used  e.g. by TSynBloomFilterDiff.LoadFromDiff() in incremental mode
// - returns false if P^ is not a valid ZeroCompress/ZeroCompressXor() result
// - will also check the crc32c of the supplied content
function ZeroDecompressOr(P,Dest: PAnsiChar; Len,DestLen: integer): boolean;


const
  /// normal pattern search depth for DeltaCompress()
  // - gives good results on most content
  DELTA_LEVEL_FAST = 100;
  /// brutal pattern search depth for DeltaCompress()
  // - may become very slow, with minor benefit, on huge content
  DELTA_LEVEL_BEST = 500;
  /// 2MB as internal chunks/window default size for DeltaCompress()
  // - will use up to 9 MB of RAM during DeltaCompress() - none in DeltaExtract()
  DELTA_BUF_DEFAULT = 2 shl 20;

/// compute difference of two binary buffers
// - returns '=' for equal buffers, or an optimized binary delta
// - DeltaExtract() could be used later on to compute New from Old + Delta
function DeltaCompress(const New, Old: RawByteString;
  Level: integer=DELTA_LEVEL_FAST; BufSize: integer=DELTA_BUF_DEFAULT): RawByteString; overload;

/// compute difference of two binary buffers
// - returns '=' for equal buffers, or an optimized binary delta
// - DeltaExtract() could be used later on to compute New from Old
function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  Level: integer=DELTA_LEVEL_FAST; BufSize: integer=DELTA_BUF_DEFAULT): RawByteString; overload;

/// compute difference of two binary buffers
// - returns '=' for equal buffers, or an optimized binary delta
// - DeltaExtract() could be used later on to compute New from Old + Delta
// - caller should call Freemem(Delta) once finished with the output buffer
function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  out Delta: PAnsiChar; Level: integer=DELTA_LEVEL_FAST; BufSize: integer=DELTA_BUF_DEFAULT): integer; overload;

type
  /// result of function DeltaExtract()
  TDeltaError = (
    dsSuccess, dsCrcCopy, dsCrcComp, dsCrcBegin, dsCrcEnd, dsCrcExtract, dsFlag, dsLen);

/// returns how many bytes a DeltaCompress() result will expand to
function DeltaExtractSize(const Delta: RawByteString): integer; overload;

/// returns how many bytes a DeltaCompress() result will expand to
function DeltaExtractSize(Delta: PAnsiChar): integer; overload;

/// apply the delta binary as computed by DeltaCompress()
// - decompression don't use any RAM, will perform crc32c check, and is very fast
// - return dsSuccess if was uncompressed to aOutUpd as expected
function DeltaExtract(const Delta,Old: RawByteString; out New: RawByteString): TDeltaError; overload;

/// low-level apply the delta binary as computed by DeltaCompress()
// - New should already be allocated with DeltaExtractSize(Delta) bytes
// - as such, expect Delta, Old and New to be <> nil, and Delta <> '='
// - return dsSuccess if was uncompressed to aOutUpd as expected
function DeltaExtract(Delta,Old,New: PAnsiChar): TDeltaError; overload;

function ToText(err: TDeltaError): PShortString; overload;


type
  /// item as stored in a TRawByteStringGroup instance
  TRawByteStringGroupValue = record
    Position: integer;
    Value: RawByteString;
  end;
  PRawByteStringGroupValue = ^TRawByteStringGroupValue;
  /// items as stored in a TRawByteStringGroup instance
  TRawByteStringGroupValueDynArray = array of TRawByteStringGroupValue;

  /// store several RawByteString content with optional concatenation
  {$ifdef UNICODE}TRawByteStringGroup = record{$else}TRawByteStringGroup = object{$endif}
  public
    /// actual list storing the data
    Values: TRawByteStringGroupValueDynArray;
    /// how many items are currently stored in Values[]
    Count: integer;
    /// the current size of data stored in Values[]
    Position: integer;
    /// naive but efficient cache for Find()
    LastFind: integer;
    /// add a new item to Values[]
    procedure Add(const aItem: RawByteString); overload;
    /// add a new item to Values[]
    procedure Add(aItem: pointer; aItemLen: integer); overload;
    {$ifndef DELPHI5OROLDER}
    /// add another TRawByteStringGroup to Values[]
    procedure Add(const aAnother: TRawByteStringGroup); overload;
    /// low-level method to abort the latest Add() call
    // - warning: will work only once, if an Add() has actually been just called:
    // otherwise, the behavior is unexpected, and may wrongly truncate data
    procedure RemoveLastAdd;
    /// compare two TRawByteStringGroup instance stored text
    function Equals(const aAnother: TRawByteStringGroup): boolean;
    {$endif DELPHI5OROLDER}
    /// clear any stored information
    procedure Clear;
    /// append stored information into another RawByteString, and clear content
    procedure AppendTextAndClear(var aDest: RawByteString);
    // compact the Values[] array into a single item
    // - is also used by AsText to compute a single RawByteString
    procedure Compact;
    /// return all content as a single RawByteString
    // - will also compact the Values[] array into a single item (which is returned)
    function AsText: RawByteString;
    /// return all content as a single TByteDynArray
    function AsBytes: TByteDynArray;
    /// save all content into a TTextWriter instance
    procedure Write(W: TTextWriter; Escape: TTextWriterKind=twJSONEscape); overload;
    /// save all content into a TFileBufferWriter instance
    procedure WriteBinary(W: TFileBufferWriter); overload;
    /// save all content as a string into a TFileBufferWriter instance
    // - storing the length as WriteVarUInt32() prefix
    procedure WriteString(W: TFileBufferWriter);
    /// add another TRawByteStringGroup previously serialized via WriteString()
    procedure AddFromReader(var aReader: TFastReader);
    /// returns a pointer to Values[] containing a given position
    // - returns nil if not found
    function Find(aPosition: integer): PRawByteStringGroupValue; overload;
    /// returns a pointer to Values[].Value containing a given position and length
    // - returns nil if not found
    function Find(aPosition, aLength: integer): pointer; overload;
    /// returns the text at a given position in Values[]
    // - text should be in a single Values[] entry
    procedure FindAsText(aPosition, aLength: integer; out aText: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the text at a given position in Values[]
    // - text should be in a single Values[] entry
    function FindAsText(aPosition, aLength: integer): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    {$ifndef NOVARIANTS}
    /// returns the text at a given position in Values[]
    // - text should be in a single Values[] entry
    // - explicitly returns null if the supplied text was not found
    procedure FindAsVariant(aPosition, aLength: integer; out aDest: variant);
      {$ifdef HASINLINE}inline;{$endif}
    {$endif}
    /// append the text at a given position in Values[], JSON escaped by default
    // - text should be in a single Values[] entry
    procedure FindWrite(aPosition, aLength: integer; W: TTextWriter;
      Escape: TTextWriterKind=twJSONEscape; TrailingCharsToIgnore: integer=0);
      {$ifdef HASINLINE}inline;{$endif}
    /// append the blob at a given position in Values[], base-64 encoded
    // - text should be in a single Values[] entry
    procedure FindWriteBase64(aPosition, aLength: integer; W: TTextWriter;
      withMagic: boolean);
      {$ifdef HASINLINE}inline;{$endif}
    /// copy the text at a given position in Values[]
    // - text should be in a single Values[] entry
    procedure FindMove(aPosition, aLength: integer; aDest: pointer);
  end;
  /// pointer reference to a TRawByteStringGroup
  PRawByteStringGroup = ^TRawByteStringGroup;


{ ************  Security and Identifier classes ************************** }

type
  /// class-reference type (metaclass) of an authentication class
  TSynAuthenticationClass = class of TSynAuthenticationAbstract;

  /// abstract authentication class, implementing safe token/challenge security
  // and a list of active sessions
  // - do not use this class, but plain TSynAuthentication
  TSynAuthenticationAbstract = class
  protected
    fSessions: TIntegerDynArray;
    fSessionsCount: Integer;
    fSessionGenerator: integer;
    fTokenSeed: Int64;
    fSafe: TSynLocker;
    function ComputeCredential(previous: boolean; const UserName,PassWord: RawUTF8): cardinal; virtual;
    function GetPassword(const UserName: RawUTF8; out Password: RawUTF8): boolean; virtual; abstract;
    function GetUsersCount: integer; virtual; abstract;
  public
    /// initialize the authentication scheme
    constructor Create;
    /// finalize the authentation
    destructor Destroy; override;
    /// register one credential for a given user
    // - this abstract method will raise an exception: inherited classes should
    // implement them as expected
    procedure AuthenticateUser(const aName, aPassword: RawUTF8); virtual;
    /// unregister one credential for a given user
    // - this abstract method will raise an exception: inherited classes should
    // implement them as expected
    procedure DisauthenticateUser(const aName: RawUTF8); virtual;
    /// create a new session
    // - should return 0 on authentication error, or an integer session ID
    // - this method will check the User name and password, and create a new session
    function CreateSession(const User: RawUTF8; Hash: cardinal): integer; virtual;
    /// check if the session exists in the internal list
    function SessionExists(aID: integer): boolean;
    /// delete a session
    procedure RemoveSession(aID: integer);
    /// returns the current identification token
    // - to be sent to the client for its authentication challenge
    function CurrentToken: Int64;
    /// the number of current opened sessions
    property SessionsCount: integer read fSessionsCount;
    /// the number of registered users
    property UsersCount: integer read GetUsersCount;
    /// to be used to compute a Hash on the client sude, for a given Token
    // - the token should have been retrieved from the server, and the client
    // should compute and return this hash value, to perform the authentication
    // challenge and create the session
    // - internal algorithm is not cryptographic secure, but fast and safe
    class function ComputeHash(Token: Int64; const UserName,PassWord: RawUTF8): cardinal; virtual;
  end;

  /// simple authentication class, implementing safe token/challenge security
  // - maintain a list of user / name credential pairs, and a list of sessions
  // - is not meant to handle authorization, just plain user access validation
  // - used e.g. by TSQLDBConnection.RemoteProcessMessage (on server side) and
  // TSQLDBProxyConnectionPropertiesAbstract (on client side) in SynDB.pas
  TSynAuthentication = class(TSynAuthenticationAbstract)
  protected
    fCredentials: TSynNameValue; // store user/password pairs
    function GetPassword(const UserName: RawUTF8; out Password: RawUTF8): boolean; override;
    function GetUsersCount: integer; override;
  public
    /// initialize the authentication scheme
    // - you can optionally register one user credential
    constructor Create(const aUserName: RawUTF8=''; const aPassword: RawUTF8=''); reintroduce;
    /// register one credential for a given user
    procedure AuthenticateUser(const aName, aPassword: RawUTF8); override;
    /// unregister one credential for a given user
    procedure DisauthenticateUser(const aName: RawUTF8); override;
  end;


{ ************ Expression Search Engine ************************** }

type
  /// exception type used by TExprParser
  EExprParser = class(ESynException);

  /// identify an expression search engine node type, as used by TExprParser
  TExprNodeType = (entWord, entNot, entOr, entAnd);

  /// results returned by TExprParserAbstract.Parse method
  TExprParserResult = (
    eprSuccess, eprNoExpression,
    eprMissingParenthesis, eprTooManyParenthesis, eprMissingFinalWord,
    eprInvalidExpression, eprUnknownVariable, eprUnsupportedOperator,
    eprInvalidConstantOrVariable);

  TParserAbstract = class;

  /// stores an expression search engine node, as used by TExprParser
  TExprNode = class(TSynPersistent)
  protected
    fNext: TExprNode;
    fNodeType: TExprNodeType;
    function Append(node: TExprNode): boolean;
  public
    /// initialize a node for the search engine
    constructor Create(nodeType: TExprNodeType); reintroduce;
    /// recursively destroys the linked list of nodes (i.e. Next)
    destructor Destroy; override;
    /// browse all nodes until Next = nil
    function Last: TExprNode;
    /// points to the next node in the parsed tree
    property Next: TExprNode read fNext;
    /// what is actually stored in this node
    property NodeType: TExprNodeType read fNodeType;
  end;

  /// abstract class to handle word search, as used by TExprParser
  TExprNodeWordAbstract = class(TExprNode)
  protected
    fOwner: TParserAbstract;
    fWord: RawUTF8;
    /// should be set from actual data before TExprParser.Found is called
    fFound: boolean;
    function ParseWord: TExprParserResult; virtual; abstract;
  public
    /// you should override this virtual constructor for proper initialization
    constructor Create(aOwner: TParserAbstract; const aWord: RawUTF8); reintroduce; virtual;
  end;

  /// class-reference type (metaclass) for a TExprNode
  // - allow to customize the actual searching process for entWord
  TExprNodeWordClass = class of TExprNodeWordAbstract;

  /// parent class of TExprParserAbstract
  TParserAbstract = class(TSynPersistent)
  protected
    fExpression, fCurrentWord, fAndWord, fOrWord, fNotWord: RawUTF8;
    fCurrent: PUTF8Char;
    fCurrentError: TExprParserResult;
    fFirstNode: TExprNode;
    fWordClass: TExprNodeWordClass;
    fWords: array of TExprNodeWordAbstract;
    fWordCount: integer;
    fNoWordIsAnd: boolean;
    fFoundStack: array[byte] of boolean; // simple stack-based virtual machine
    procedure ParseNextCurrentWord; virtual; abstract;
    function ParseExpr: TExprNode;
    function ParseFactor: TExprNode;
    function ParseTerm: TExprNode;
    procedure Clear; virtual;
    // override this method to initialize fWordClass and fAnd/Or/NotWord
    procedure Initialize; virtual; abstract;
    /// perform the expression search over TExprNodeWord.fFound flags
    // - warning: caller should check that fFirstNode<>nil (e.g. WordCount>0)
    function Execute: boolean; {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize an expression parser
    constructor Create; override;
    /// finalize the expression parser
    destructor Destroy; override;
    /// initialize the parser from a given text expression
    function Parse(const aExpression: RawUTF8): TExprParserResult;
    /// try this parser class on a given text expression
    // - returns '' on success, or an explicit error message (e.g.
    // 'Missing parenthesis')
    class function ParseError(const aExpression: RawUTF8): RawUTF8;
    /// the associated text expression used to define the search
    property Expression: RawUTF8 read fExpression;
    /// how many words did appear in the search expression
    property WordCount: integer read fWordCount;
  end;

  /// abstract class to parse a text expression into nodes
  // - you should inherit this class to provide actual text search
  // - searched expressions can use parenthesis and &=AND -=WITHOUT +=OR operators,
  // e.g. '((w1 & w2) - w3) + w4' means ((w1 and w2) without w3) or w4
  // - no operator is handled like a AND, e.g. 'w1 w2' = 'w1 & w2'
  TExprParserAbstract = class(TParserAbstract)
  protected
    procedure ParseNextCurrentWord; override;
    // may be overriden to provide custom words escaping (e.g. handle quotes)
    procedure ParseNextWord; virtual;
    procedure Initialize; override;
  end;

  /// search expression engine using TMatch for the actual word searches
  TExprParserMatch = class(TExprParserAbstract)
  protected
    fCaseSensitive: boolean;
    fMatchedLastSet: integer;
    procedure Initialize; override;
  public
    /// initialize the search engine
    constructor Create(aCaseSensitive: boolean = true); reintroduce;
    /// returns TRUE if the expression is within the text buffer
    function Search(aText: PUTF8Char; aTextLen: PtrInt): boolean; overload;
    /// returns TRUE if the expression is within the text buffer
    function Search(const aText: RawUTF8): boolean; overload; {$ifdef HASINLINE}inline;{$endif}
  end;

const
  /// may be used when overriding TExprParserAbstract.ParseNextWord method
  PARSER_STOPCHAR = ['&', '+', '-', '(', ')'];

function ToText(r: TExprParserResult): PShortString; overload;
function ToUTF8(r: TExprParserResult): RawUTF8; overload;


implementation

{ ************ TSynTable generic types and classes ************************** }

{$ifndef NOVARIANTS}

{ TSynTableVariantType }

var
  SynTableVariantType: TCustomVariantType = nil;

procedure TSynTableVariantType.Clear(var V: TVarData);
begin
  //Assert(V.VType=SynTableVariantType.VarType);
  TSynTableData(V).VValue := ''; // clean memory release
  PPtrUInt(@V)^ := 0; // will set V.VType := varEmpty
end;

procedure TSynTableVariantType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  //Assert(Source.VType=SynTableVariantType.VarType);
  inherited Copy(Dest,Source,Indirect); // copy VType+VID+VTable
  if not Indirect then
    with TSynTableData(Dest) do begin
      PtrInt(VValue) := 0; // avoid GPF
      VValue := TSynTableData(Source).VValue; // copy by reference
    end;
end;

procedure TSynTableVariantType.IntGet(var Dest: TVarData;
  const V: TVarData; Name: PAnsiChar);
begin
  TSynTableData(V).GetFieldVariant(RawByteString(Name),variant(Dest));
end;

procedure TSynTableVariantType.IntSet(const V, Value: TVarData;
  Name: PAnsiChar);
begin
  TSynTableData(V).SetFieldValue(RawByteString(Name),Variant(Value));
end;

class function TSynTableVariantType.ToID(const V: Variant): integer;
var Data: TSynTableData absolute V;
begin
  if Data.VType<>SynTableVariantType.VarType then
    result := 0 else
    result := Data.VID;
end;

class function TSynTableVariantType.ToSBF(const V: Variant): TSBFString;
var Data: TSynTableData absolute V;
begin
  if Data.VType<>SynTableVariantType.VarType then
    result := '' else
    result := Data.VValue;
end;

class function TSynTableVariantType.ToTable(const V: Variant): TSynTable;
var Data: TSynTableData absolute V;
begin
  if Data.VType<>SynTableVariantType.VarType then
    result := nil else
    result := Data.VTable;
end;

{$endif NOVARIANTS}


{ TSynTable }

{$ifdef CPUX86}
function SortQWord(const A,B: QWord): integer;
asm // Delphi x86 compiler is not efficient, and oldest even incorrect
        mov     ecx, [eax]
        mov     eax, [eax + 4]
        cmp     eax, [edx + 4]
        jnz     @nz
        cmp     ecx, [edx]
        jz      @0
@nz:    jnb     @p
        or      eax, -1
        ret
@0:     xor     eax, eax
        ret
@p:     mov     eax, 1
end;

function SortInt64(const A,B: Int64): integer;
asm // Delphi x86 compiler is not efficient at compiling below code
        mov     ecx, [eax]
        mov     eax, [eax + 4]
        cmp     eax, [edx + 4]
        jnz     @nz
        cmp     ecx, [edx]
        jz      @0
        jnb     @p
@n:     or      eax, -1
        ret
@0:     xor     eax, eax
        ret
@nz:    jl      @n
@p:     mov     eax, 1
end;
{$endif}

{$ifndef SORTCOMPAREMETHOD}

function SortU8(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PByte(P1)^-PByte(P2)^;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortU16(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PWord(P1)^-PWord(P2)^;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortI32(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PInteger(P1)^-PInteger(P2)^;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortDouble(P1,P2: PUTF8Char): PtrInt;
var V: Double;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        V := PDouble(P1)^-PDouble(P2)^;
        if V<0 then
          result := -1 else
        if V=0 then
          result := 0 else
          result := 1;
       end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortU24(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PtrInt(PWord(P1)^)+PtrInt(P1[2])shl 16
          -PtrInt(PWord(P2)^)-PtrInt(P2[2]) shl 16;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarUInt32(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := FromVarUInt32(PByte(P1))-FromVarUInt32(PByte(P2));
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarInt32(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := FromVarInt32(PByte(P1))-FromVarInt32(PByte(P2));
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

{$ifdef CPU64} // PtrInt = Int64 -> so direct substraction works

function SortI64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := PInt64(P1)^-PInt64(P2)^ else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarUInt64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := FromVarUInt64(PByte(P1))-FromVarUInt64(PByte(P2)) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarInt64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := FromVarInt64(PByte(P1))-FromVarInt64(PByte(P2)) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

{$else}

{$ifdef CPUX86} // circumvent comparison slowness (and QWord bug)

function SortI64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := SortInt64(PInt64(P1)^,PInt64(P2)^) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarUInt64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := SortQWord(FromVarUInt64(PByte(P1)),FromVarUInt64(PByte(P2))) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarInt64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := SortInt64(FromVarInt64(PByte(P1)),FromVarInt64(PByte(P2))) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

{$else}

function SortI64(P1,P2: PUTF8Char): PtrInt;
var V: Int64;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        V := PInt64(P1)^-PInt64(P2)^;
        if V<0 then
          result := -1 else
        if V>0 then
          result := 1 else
          result := 0;
       end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarUInt64(P1,P2: PUTF8Char): PtrInt;
var V1,V2: QWord;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        V1 := FromVarUInt64(PByte(P1));
        V2 := FromVarUInt64(PByte(P2));
        if V1>V2 then
          result := 1 else
        if V1=V2 then
          result := 0 else
          result := -1;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarInt64(P1,P2: PUTF8Char): PtrInt;
var V1,V2: Int64;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        V1 := FromVarInt64(PByte(P1));
        V2 := FromVarInt64(PByte(P2));
        if V1>V2 then
          result := 1 else
        if V1=V2 then
          result := 0 else
          result := -1;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

{$endif CPUX86}

{$endif CPU64}

function SortStr(P1,P2: PUTF8Char): PtrInt;
var L1, L2, L, i: PtrInt;
    PB1, PB2: PByte;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        if PtrInt(P1^)<=$7F then begin
          L1 := PtrInt(P1^);
          inc(P1);
        end else begin
          PB1 := pointer(P1);
          L1 := FromVarUInt32High(PB1);
          P1 := pointer(PB1);
        end;
        if PtrInt(P2^)<=$7F then begin
          L2 := PtrInt(P2^);
          inc(P2);
        end else begin
          PB2 := pointer(P2);
          L2 := FromVarUInt32High(PB2);
          P2 := pointer(PB2);
        end;
        L := L1;
        if L2>L then
          L := L2;
        for i := 0 to L-1 do begin
          result := PtrInt(P1[i])-PtrInt(P2[i]);
          if Result<>0 then
            exit;
        end;
        result := L1-L2;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortIStr(P1,P2: PUTF8Char): PtrInt;
var L1, L2, L, i: PtrInt;
    PB1, PB2: PByte;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        if PtrInt(P1^)<=$7F then begin
          L1 := PtrInt(P1^);
          inc(P1);
        end else begin
          PB1 := pointer(P1);
          L1 := FromVarUInt32High(PB1);
          P1 := pointer(PB1);
        end;
        if PtrInt(P2^)<=$7F then begin
          L2 := PtrInt(P2^);
          inc(P2);
        end else begin
          PB2 := pointer(P2);
          L2 := FromVarUInt32High(PB2);
          P2 := pointer(PB2);
        end;
        if L2>L1 then
          L := L2 else
          L := L1;
        for i := 0 to L-1 do // NormToUpperAnsi7 works for both WinAnsi & UTF-8
          if NormToUpperAnsi7[P1[i]]<>NormToUpperAnsi7[P2[i]] then begin
            result := PtrInt(P1[i])-PtrInt(P2[i]);
            exit;
          end;
        result := L1-L2;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

const
  FIELD_SORT: array[TSynTableFieldType] of TUTF8Compare = (
    nil, // tftUnknown,
    SortU8,    SortU8,  SortU16,  SortU24,  SortI32, SortI64,
 // tftBoolean,tftUInt8,tftUInt16,tftUInt24,tftInt32,tftInt64,
    SortI64,  SortDouble, SortVarUInt32,SortVarInt32,SortVarUInt64,
 // tftCurrency,tftDouble, tftVarUInt32, tftVarInt32,tftVarUInt64,
    SortStr,   SortStr, SortStr,        nil,           SortVarInt64);
 // tftWinAnsi,tftUTF8, tftBlobInternal,tftBlobExternal,tftVarInt64);

{$endif SORTCOMPAREMETHOD}

const
  FIELD_FIXEDSIZE: array[TSynTableFieldType] of Integer = (
     0, // tftUnknown,
     1, 1, 2, 3, 4, 8, 8, 8,
     // tftBoolean, tftUInt8, tftUInt16, tftUInt24, tftInt32, tftInt64, tftCurrency, tftDouble
     -1, -1, -1, // tftVarUInt32, tftVarInt32, tftVarUInt64 have -1 as size
     -2, -2, -2, // tftWinAnsi, tftUTF8, tftBlobInternal have -2 as size
     -3,  // tftBlobExternal has -3 as size
     -1); //tftVarInt64

  // note: boolean is not in this set, because it can be 'true' or 'false'
  FIELD_INTEGER: TSynTableFieldTypes = [
    tftUInt8, tftUInt16, tftUInt24, tftInt32, tftInt64,
    tftVarUInt32, tftVarInt32, tftVarUInt64, tftVarInt64];

function TSynTable.AddField(const aName: RawUTF8;
  aType: TSynTableFieldType; aOptions: TSynTableFieldOptions): TSynTableFieldProperties;
var aSize: Integer;
begin
  result := nil;
  aSize := FIELD_FIXEDSIZE[aType];
  if (self=nil) or (aSize=0) or IsRowID(pointer(aName)) or
     not PropNameValid(pointer(aName)) or (GetFieldFromName(aName)<>nil) then
    exit;
  result := TSynTableFieldProperties.Create;
  if fAddedField=nil then
    fAddedField := TList.Create;
  fAddedField.Add(result);
  result.Name := aName;
  result.FieldType := aType;
  if tfoUnique in aOptions then
    Include(aOptions,tfoIndex); // create an index for faster Unique field
  if aSize=-3 then // external field has no index available
    aOptions := aOptions-[tfoIndex,tfoUnique];
  result.Options := aOptions;
  if aSize>0 then begin
    // fixed-size field should be inserted left-side of the stream
    if (tfoIndex in aOptions) or (aSize and 3=0) then begin
      // indexed field or size is alignment friendly: put left side
      if not ((tfoIndex in aOptions) and (aSize and 3=0)) then
        // indexed+aligned field -> set first, otherwise at variable or not indexed
        while result.FieldNumber<fField.Count do
          with TSynTableFieldProperties(fField.List[result.FieldNumber]) do
          if (Offset<0) or not (tfoIndex in Options) then
            break else
            inc(result.FieldNumber);
      end else
      // not indexed field: insert after previous fixed-sized fields
      if fFieldVariableIndex>=0 then
        result.FieldNumber := fFieldVariableIndex else
        result.FieldNumber := fField.Count;
    fField.Insert(result.FieldNumber,result);
  end else begin
    if (tfoIndex in aOptions) and (fFieldVariableIndex>=0) then begin
      // indexed field should be added left side (faster access for sort)
      result.FieldNumber := fFieldVariableIndex;
      while result.FieldNumber<fField.Count do
        with TSynTableFieldProperties(fField.List[result.FieldNumber]) do
        if not (tfoIndex in Options) then
          break else
          inc(result.FieldNumber);
      fField.Insert(result.FieldNumber,result);
    end else
      // not indexed field: just add at the end of the field list
      result.FieldNumber := fField.Add(result);
  end;
  if tfoUnique in aOptions then begin
    fFieldHasUniqueIndexes := true;
    result.AddFilterOrValidate(TSynValidateTableUniqueField.Create);
  end;
  AfterFieldModif; // set Offset,FieldNumber,FieldSize fFieldVariableIndex/Offset
end;

procedure TSynTable.UpdateFieldData(RecordBuffer: PUTF8Char; RecordBufferLen,
  FieldIndex: integer; var result: TSBFString; const NewFieldData: TSBFString='');
var NewSize, DestOffset, OldSize: integer;
    F: TSynTableFieldProperties;
    NewData, Dest: PAnsiChar;
begin
  if (self<>nil) and ((RecordBuffer=nil) or (RecordBufferLen=0)) then begin
    // no data yet -> use default
    RecordBuffer := pointer(fDefaultRecordData);
    RecordBufferLen := fDefaultRecordLength;
  end;
  if RecordBuffer=pointer(result) then
    // update content code below will fail -> please correct calling code
    raise ETableDataException.CreateUTF8('In-place call of %.UpdateFieldData',[self]);
  if (self=nil) or (cardinal(FieldIndex)>=cardinal(fField.Count)) then begin
    SetString(result,PAnsiChar(RecordBuffer),RecordBufferLen);
    exit;
  end;
  F := TSynTableFieldProperties(fField.List[FieldIndex]);
  NewSize := length(NewFieldData);
  if NewSize=0 then begin
    // no NewFieldData specified -> use default field data to be inserted
    NewData := pointer(F.fDefaultFieldData);
    NewSize := F.fDefaultFieldLength;
  end else
    NewData := pointer(NewFieldData);
  Dest := GetData(RecordBuffer,F);
  DestOffset := Dest-RecordBuffer;
  // update content
  OldSize :=  F.GetLength(Dest);
  dec(RecordBufferLen,OldSize);
  SetLength(Result,RecordBufferLen+NewSize);
  MoveFast(RecordBuffer^,PByteArray(result)[0],DestOffset);
  MoveFast(NewData^,PByteArray(result)[DestOffset],NewSize);
  MoveFast(Dest[OldSize],PByteArray(result)[DestOffset+NewSize],RecordBufferLen-DestOffset);
end;

constructor TSynTable.Create(const aTableName: RawUTF8);
begin
  if not PropNameValid(pointer(aTableName)) then
    raise ETableDataException.CreateUTF8('Invalid %.Create(%)',[self,aTableName]);
  fTableName := aTableName;
  fField := TObjectList.Create;
  fFieldVariableIndex := -1;
end;

procedure TSynTable.LoadFrom(var RD: TFileBufferReader);
var n, i: integer;
    aTableName: RawUTF8;
begin
  fField.Clear;
  RD.Read(aTableName);
  if not PropNameValid(pointer(aTableName)) then
    RD.ErrorInvalidContent;
  fTableName := aTableName;
  n := RD.ReadVarUInt32;
  if cardinal(n)>=MAX_SQLFIELDS then
    RD.ErrorInvalidContent;
  for i := 0 to n-1 do
    fField.Add(TSynTableFieldProperties.CreateFrom(RD));
  AfterFieldModif;
end;

destructor TSynTable.Destroy;
begin
  fField.Free;
  fAddedField.Free;
  inherited;
end;

function TSynTable.GetFieldCount: integer;
begin
  if self=nil then
    result := 0 else
    result := fField.Count;
end;

function TSynTable.GetFieldFromName(const aName: RawUTF8): TSynTableFieldProperties;
var i: integer;
begin
  if self<>nil then
    for i := 0 to fField.Count-1 do begin
      result := TSynTableFieldProperties(fField.List[i]);
      if IdemPropNameU(result.Name,aName) then
        exit;
    end;
  result := nil;
end;

function TSynTable.GetFieldIndexFromName(const aName: RawUTF8): integer;
begin
  if self<>nil then
    for result := 0 to fField.Count-1 do
      if IdemPropNameU(TSynTableFieldProperties(fField.List[result]).Name,aName) then
        exit;
  result := -1;
end;

function TSynTable.GetFieldIndexFromShortName(const aName: ShortString): integer;
begin
  if self<>nil then
    for result := 0 to fField.Count-1 do
      with TSynTableFieldProperties(fField.List[result]) do
      if IdemPropName(aName,pointer(Name),length(Name)) then
        exit;
  result := -1;
end;

function TSynTable.GetFieldType(Index: integer): TSynTableFieldProperties;
begin
  if (self=nil) or (cardinal(Index)>=cardinal(fField.Count)) then
    result := nil else // avoid GPF
    result := fField.List[Index];
end;

{$ifndef DELPHI5OROLDER}

function TSynTable.CreateJSONWriter(JSON: TStream; Expand, withID: boolean;
  const Fields: TSQLFieldBits): TJSONWriter;
begin
  result := CreateJSONWriter(JSON,Expand,withID,FieldBitsToIndex(Fields,fField.Count));
end;

function TSynTable.CreateJSONWriter(JSON: TStream; Expand, withID: boolean;
  const Fields: TSQLFieldIndexDynArray): TJSONWriter;
var i,nf,n: integer;
begin
  if (self=nil) or ((Fields=nil) and not withID) then begin
    result := nil; // no data to retrieve
    exit;
  end;
  result := TJSONWriter.Create(JSON,Expand,withID,Fields);
  // set col names
  if withID then
    n := 1 else
    n := 0;
  nf := length(Fields);
  SetLength(result.ColNames,nf+n);
  if withID then
    result.ColNames[0] := 'ID';
  for i := 0 to nf-1 do
    result.ColNames[i+n] := TSynTableFieldProperties(fField.List[Fields[i]]).Name;
  result.AddColumns; // write or init field names for appropriate JSON Expand
end;

procedure TSynTable.GetJSONValues(aID: integer; RecordBuffer: PUTF8Char;
  W: TJSONWriter);
var i,n: integer;
    buf: array[0..MAX_SQLFIELDS-1] of PUTF8Char;
begin
  if (self=nil) or (RecordBuffer=nil) or (W=nil) then
    exit; // avoid GPF
  if W.Expand then begin
    W.Add('{');
    if W.WithID then
      W.AddString(W.ColNames[0]);
  end;
  if W.WithID then begin
    W.Add(aID);
    W.Add(',');
    n := 1;
  end else
    n := 0;
  for i := 0 to fField.Count-1 do begin
    buf[i] := RecordBuffer;
    inc(RecordBuffer,TSynTableFieldProperties(fField.List[i]).GetLength(RecordBuffer));
  end;
  for i := 0 to length(W.Fields)-1 do begin
    if W.Expand then begin
      W.AddString(W.ColNames[n]); // '"'+ColNames[]+'":'
      inc(n);
    end;
    TSynTableFieldProperties(fField.List[W.Fields[i]]).GetJSON(buf[i],W);
    W.Add(',');
  end;
  W.CancelLastComma; // cancel last ','
  if W.Expand then
    W.Add('}');
end;

function TSynTable.IterateJSONValues(Sender: TObject; Opaque: pointer;
  ID: integer; Data: pointer; DataLen: integer): boolean;
var Statement: TSynTableStatement absolute Opaque;
    F: TSynTableFieldProperties;
    nWhere,fIndex: cardinal;
begin  // note: we should have handled -2 (=COUNT) case already
  nWhere := length(Statement.Where);
  if (self=nil) or (Statement=nil) or (Data=nil) or
     (Statement.Select=nil) or (nWhere>1) or
     ((nWhere=1)and(Statement.Where[0].ValueSBF='')) then begin
    result := false;
    exit;
  end;
  result := true;
  if nWhere=1 then begin // Where=nil -> all rows
    fIndex := Statement.Where[0].Field;
    if fIndex=SYNTABLESTATEMENTWHEREID then begin
      if ID<>Statement.Where[0].ValueInteger then
        exit;
    end else begin
      dec(fIndex); // 0 is ID, 1 for field # 0, 2 for field #1, and so on...
      if fIndex<cardinal(fField.Count) then begin
        F := TSynTableFieldProperties(fField.List[fIndex]);
        if F.SortCompare(GetData(Data,F),pointer(Statement.Where[0].ValueSBF))<>0 then
          exit;
      end;
    end;
  end;
  GetJSONValues(ID,Data,Statement.Writer);
end;

{$endif DELPHI5OROLDER}

function TSynTable.GetData(RecordBuffer: PUTF8Char; Field: TSynTableFieldProperties): pointer;
var i: integer;
    PB: PByte;
begin
  if Field.Offset>=0 then
    result := RecordBuffer+Field.Offset else begin
    result := RecordBuffer+fFieldVariableOffset;
    for i := fFieldVariableIndex to Field.FieldNumber-1 do
      if i in fFieldIsVarString then begin
        // inlined result := GotoNextVarString(result);
        if PByte(result)^<=$7f then
          inc(PtrUInt(result),PByte(result)^+1) else begin
          PB := result;
          inc(PtrUInt(result),FromVarUInt32High(PB)+PtrUInt(PB)-PtrUInt(result));
        end;
      end else
      if not (i in fFieldIsExternal) then begin
        // inlined result := GotoNextVarInt(result)
        while PByte(result)^>$7f do inc(PtrUInt(result));
        inc(PtrUInt(result));
      end;
  end;
end;

procedure TSynTable.SaveTo(WR: TFileBufferWriter);
var i: Integer;
begin
  WR.Write(fTableName);
  WR.WriteVarUInt32(fField.Count);
  for i := 0 to fField.Count-1 do
    TSynTableFieldProperties(fField.List[i]).SaveTo(WR);
end;

procedure TSynTable.AfterFieldModif;
var i, Offs: integer;
begin
  PInt64(@fFieldIsVarString)^ := 0;
  PInt64(@fFieldIsExternal)^ := 0;
  fFieldVariableIndex := -1;
  fDefaultRecordLength := 0;
  fFieldHasUniqueIndexes := false;
  Offs := 0;
  for i := 0 to fField.Count-1 do
  with TSynTableFieldProperties(fField.List[i]) do begin
    FieldNumber := i;
    {$ifndef SORTCOMPAREMETHOD}
    SortCompare := FIELD_SORT[FieldType];
    {$endif}
    Owner := self;
    FieldSize := FIELD_FIXEDSIZE[FieldType];
    if FieldSize>=0 then begin
      //assert(Offs>=0);
      Offset := Offs;
      inc(Offs,FieldSize);
      inc(fDefaultRecordLength,FieldSize);
      fDefaultFieldLength := FieldSize;
    end else begin
      if FieldSize=-3 then
        Include(fFieldIsExternal,i) else begin
        fDefaultFieldLength := 1;
        inc(fDefaultRecordLength);
        if FieldSize=-2 then
          Include(fFieldIsVarString,i);
        {$ifndef SORTCOMPAREMETHOD}
        if (FieldType in [tftWinAnsi,tftUTF8]) and
           (tfoCaseInsensitive in Options) then
          SortCompare := SortIStr; // works for both WinAnsi and UTF-8 encodings
        {$endif}
      end;
      // we need the Offset even for tftBlobExternal (FieldSize=-3)
      if fFieldVariableIndex<0 then begin
        fFieldVariableIndex := i;
        fFieldVariableOffset := Offs;
        Offs := -1;
      end;
      Offset := Offs;
      dec(Offs);
    end;
    SetLength(fDefaultFieldData,fDefaultFieldLength);
    FillcharFast(pointer(fDefaultFieldData)^,fDefaultFieldLength,0);
  end;
  SetLength(fDefaultRecordData,fDefaultRecordLength);
  FillcharFast(pointer(fDefaultRecordData)^,fDefaultRecordLength,0);
end;

procedure TSynTable.FieldIndexModify(aOldIndex, aNewIndex: integer;
  aOldRecordData, aNewRecordData: pointer);
var F: integer;
begin
  for F := 0 to fField.Count-1 do
    with TSynTableFieldProperties(fField.List[F]) do
      if tfoIndex in Options then
        OrderedIndexUpdate(aOldIndex,aNewIndex,aOldRecordData,aNewRecordData);
end;

procedure TSynTable.Filter(var RecordBuffer: TSBFString);
var Old, New: RawUTF8;
    NewRecord: TSBFString; // UpdateFieldData update result in-place
    F, i: integer;
begin
  for F := 0 to fField.Count-1 do
    with TSynTableFieldProperties(fField.List[F]) do
    if Filters<>nil then begin
      Old := GetRawUTF8(pointer(RecordBuffer));
      New := Old;
      for i := 0 to Filters.Count-1 do
        TSynFilter(Filters.List[i]).Process(F,New);
      if Old<>New then begin
        // value was changed -> store modified
        UpdateFieldData(pointer(RecordBuffer),length(RecordBuffer),F,
          NewRecord,SBFFromRawUTF8(New));
        RecordBuffer := NewRecord;
      end;
    end;
end;

{$ifndef NOVARIANTS}
function TSynTable.Data(aID: integer; RecordBuffer: pointer; RecordBufferLen: Integer): Variant;
var data: TSynTableData absolute result;
begin
  if SynTableVariantType=nil then
    SynTableVariantType := SynRegisterCustomVariantType(TSynTableVariantType);
  {$ifndef FPC}
  if data.VType and VTYPE_STATIC<>0 then
  {$endif}
    VarClear(result);
  data.VType := SynTableVariantType.VarType;
  data.VID := aID;
  data.VTable := self;
  pointer(data.VValue) := nil; // avoid GPF
  if RecordBuffer=nil then
    data.VValue := DefaultRecordData else begin
    if RecordBufferLen=0 then
      RecordBufferLen := DataLength(RecordBuffer);
    SetString(data.VValue,PAnsiChar(RecordBuffer),RecordBufferLen);
  end;
end;
{$endif NOVARIANTS}

function TSynTable.DataLength(RecordBuffer: pointer): integer;
var F: Integer;
    PC: PUTF8Char;
begin
  if (Self<>nil) and (RecordBuffer<>nil) then begin
    PC := RecordBuffer;
    for F := 0 to fField.Count-1 do
      inc(PC,TSynTableFieldProperties(fField.List[F]).GetLength(PC));
    result := PC-RecordBuffer;
  end else
    result := 0;
end;

function TSynTable.UpdateFieldEvent(Sender: TObject; Opaque: pointer;
  ID, Index: integer; Data: pointer; DataLen: integer): boolean;
var Added: PUpdateFieldEvent absolute Opaque;
    F, aSize: integer;
begin // in practice, this data processing is very fast (thanks to WR speed)
  with Added^ do begin
    result := Count<length(IDs);
    if not result then
      exit;
    for F := 0 to fField.Count-1 do
    with TSynTableFieldProperties(fField.List[F]) do
    if F in AvailableFields then begin
      // add previous field content: will handle any field offset change in record
      aSize := Getlength(Data);
      WR.Write(Data,aSize);
      inc(PtrUInt(Data),aSize);
    end else
      // add default field content for a newly added field
      WR.Write(Pointer(fDefaultFieldData),fDefaultFieldLength);
    if WR.TotalWritten>1 shl 30 then
      raise ETableDataException.CreateUTF8('%: File size too big (>1GB)',[self]) else
      Offsets64[Count] := WR.TotalWritten;
    IDs[Count] := ID;
    NewIndexs[Index] := Count;
    inc(Count);
  end;
end;

function TSynTable.UpdateFieldRecord(RecordBuffer: PUTF8Char;
  var AvailableFields: TSQLFieldBits): TSBFString;
var Lens: array[0..MAX_SQLFIELDS-1] of Integer;
    F, Len, TotalLen: integer;
    P: PUTF8Char;
    Dest: PByte;
begin
  // retrieve all field buffer lengths, to speed up record content creation
  TotalLen := 0;
  P := RecordBuffer;
  for F := 0 to fField.Count-1 do
  with TSynTableFieldProperties(fField.List[F]) do
  if F in AvailableFields then begin
    Len := GetLength(P);
    inc(P,Len);
    inc(TotalLen,Len);
    Lens[F] := Len;
  end else
    inc(TotalLen,fDefaultFieldLength);
  // create new record content
  P := RecordBuffer;
  SetString(Result,nil,TotalLen);
  Dest := pointer(Result);
  for F := 0 to fField.Count-1 do
  with TSynTableFieldProperties(fField.List[F]) do
    if F in AvailableFields then begin
      Len := Lens[F];
      {$ifdef FPC}Move{$else}MoveFast{$endif}(P^,Dest^,Len);
      inc(P,Len);
      inc(Dest,Len);
    end else begin
      FillcharFast(Dest^,fDefaultFieldLength,0);
      inc(Dest,fDefaultFieldLength);
    end;
  //Assert(PtrUInt(Dest)-PtrUInt(result)=PtrUInt(TotalLen));
end;

function TSynTable.Validate(RecordBuffer: pointer; RecordIndex: integer): string;
var F: integer;
begin
  result := '';
  for F := 0 to fField.Count-1 do
    with TSynTableFieldProperties(fField.List[F]) do
      if Validates<>nil then begin
        result := Validate(RecordBuffer,RecordIndex);
        if result<>'' then
          exit;
      end;
end;


{ TSynTableFieldProperties }

constructor TSynTableFieldProperties.CreateFrom(var RD: TFileBufferReader);
begin
  fOrderedIndexFindAdd := -1;
  RD.Read(Name);
  if not PropNameValid(pointer(Name)) then
    RD.ErrorInvalidContent;
  RD.Read(@FieldType,SizeOf(FieldType));
  RD.Read(@Options,SizeOf(Options));
  if (FieldType>high(FieldType)) then
    RD.ErrorInvalidContent;
  OrderedIndexCount := RD.ReadVarUInt32Array(OrderedIndex);
  if OrderedIndexCount>0 then begin
    if tfoIndex in Options then begin
      //assert(OrderedIndexReverse=nil);
      OrderedIndexReverseSet(-1); // compute whole OrderedIndexReverse[] array
    end else
      RD.ErrorInvalidContent;
  end;
  // we allow a void OrderedIndex[] array from disk
end;

destructor TSynTableFieldProperties.Destroy;
begin
  Filters.Free;
  Validates.Free;
  inherited;
end;

function TSynTableFieldProperties.GetJSON(FieldBuffer: pointer;
  W: TTextWriter): pointer;
var len: integer;
    tmp: RawUTF8;
begin
  case FieldType of
  // fixed-sized field value
  tftBoolean:
    W.Add(PBoolean(FieldBuffer)^);
  tftUInt8:
    W.Add(PByte(FieldBuffer)^);
  tftUInt16:
    W.Add(PWord(FieldBuffer)^);
  tftUInt24:
    // PInteger()^ and $ffffff -> possible GPF on Memory Mapped file
    W.Add(PWord(FieldBuffer)^+integer(PByteArray(FieldBuffer)^[2])shl 16);
  tftInt32:
    W.Add(PInteger(FieldBuffer)^);
  tftInt64:
    W.Add(PInt64(FieldBuffer)^);
  tftCurrency:
    W.AddCurr64(PInt64(FieldBuffer)^);
  tftDouble:
    W.AddDouble(PDouble(FieldBuffer)^);
  // some variable-size field value
  tftVarUInt32:
    W.Add(FromVarUInt32(PByte(FieldBuffer)));
  tftVarInt32:
    W.Add(FromVarInt32(PByte(FieldBuffer)));
  tftVarUInt64:
    W.AddQ(FromVarUInt64(PByte(FieldBuffer)));
  tftVarInt64:
    W.Add(FromVarInt64(PByte(FieldBuffer)));
  // text storage - WinAnsi could use less space than UTF-8
  tftWinAnsi, tftUTF8: begin
    W.Add('"');
    len := FromVarUInt32(PByte(FieldBuffer));
    if len>0 then
      if FieldType=tftUTF8 then
        W.AddJSONEscape(PAnsiChar(FieldBuffer),len) else begin
        SetLength(tmp,len*3); // in-place decoding and appending
        W.AddJSONEscape(pointer(tmp),WinAnsiBufferToUtf8(pointer(tmp),PAnsiChar(FieldBuffer),len)-pointer(tmp));
      end;
    W.Add('"');
    result := PAnsiChar(FieldBuffer)+len;
    exit;
  end;
  tftBlobInternal: begin
    W.AddShort('"X''');
    len := FromVarUInt32(PByte(FieldBuffer));
    W.AddBinToHex(PByte(FieldBuffer),len);
    W.Add('''','"');
  end;
  tftBlobExternal:
    ; // BLOB fields are not handled here, but must be directly accessed
  end;
  result := PAnsiChar(FieldBuffer)+FieldSize; // // tftWinAnsi,tftUTF8 already done
end;

function TSynTableFieldProperties.GetLength(FieldBuffer: pointer): Integer;
var PB: PByte;
begin
  if FieldSize>=0 then
    result := FieldSize else
    case FieldSize of
    -1: begin // variable-length data
      result := 0;
      while PByteArray(FieldBuffer)^[result]>$7f do inc(result);
      inc(result);
    end;
    -2: begin // tftWinAnsi, tftUTF8, tftBlobInternal records
      result := PByte(FieldBuffer)^;
      if result<=$7F then
        inc(Result) else begin
        PB := FieldBuffer;
        result := FromVarUInt32High(PB)+PtrUInt(PB)-PtrUInt(FieldBuffer);
      end;
    end;
    else
      result := 0; // tftBlobExternal is not stored in FieldBuffer
    end;
end;

{$ifndef NOVARIANTS}
function TSynTableFieldProperties.GetVariant(FieldBuffer: pointer): Variant;
begin
  GetVariant(FieldBuffer,result);
end;

procedure TSynTableFieldProperties.GetVariant(FieldBuffer: pointer; var result: Variant);
var len: integer;
    PB: PByte absolute FieldBuffer;
    PA: PAnsiChar absolute FieldBuffer;
    PU: PUTF8Char absolute FieldBuffer;
    tmp: RawByteString;
    {$ifndef UNICODE}
    WS: WideString;
    {$endif}
begin
  case FieldType of
  // fixed-sized field value
  tftBoolean:
    result := PBoolean(FieldBuffer)^;
  tftUInt8:
    result := PB^;
  tftUInt16:
    result := PWord(FieldBuffer)^;
  tftUInt24:
    // PInteger()^ and $ffffff -> possible GPF on Memory Mapped file
    result := PWord(FieldBuffer)^+integer(PByteArray(FieldBuffer)^[2])shl 16;
  tftInt32:
    result := PInteger(FieldBuffer)^;
  tftInt64:
    result := PInt64(FieldBuffer)^;
  tftCurrency:
    result := PCurrency(FieldBuffer)^;
  tftDouble:
    result := PDouble(FieldBuffer)^;
  // some variable-size field value
  tftVarUInt32:
    result := FromVarUInt32(PB);
  tftVarInt32:
    result := FromVarInt32(PB);
  tftVarUInt64:
    result := FromVarUInt64(PB);
  tftVarInt64:
    result := FromVarInt64(PB);
  // text storage - WinAnsi could use less space than UTF-8
  tftWinAnsi: begin
    len := FromVarUInt32(PB);
    if len>0 then
      {$ifdef UNICODE}
      result := WinAnsiToUnicodeString(PA,len)
      {$else}
      result := CurrentAnsiConvert.AnsiToAnsi(WinAnsiConvert,PA,len)
      {$endif} else
      result := '';
  end;
  tftUTF8: begin
    len := FromVarUInt32(PB);
    if len>0 then
      {$ifdef UNICODE}
      result := UTF8DecodeToUnicodeString(PU,len)
      {$else} begin
        UTF8ToSynUnicode(PU,len,WS);
        result := WS;
      end
      {$endif} else
      result := '';
  end;
  tftBlobInternal: begin
    len := FromVarUInt32(PB);
    SetString(tmp,PA,len);
    result := tmp; // return internal BLOB content as string
  end
  else
    result := ''; // tftBlobExternal fields e.g. must be directly accessed
  end;
end;
{$endif}

{$ifdef ISDELPHI20062007}
  {$WARNINGS OFF} // circument Delphi 2007 false positive warning
{$endif}

function TSynTableFieldProperties.GetValue(FieldBuffer: pointer): RawUTF8;
var len: integer;
    PB: PByte absolute FieldBuffer;
    PC: PAnsiChar absolute FieldBuffer;
begin
  result := '';
  case FieldType of
  // fixed-sized field value
  tftBoolean:
    JSONBoolean(PBoolean(FieldBuffer)^,result);
  tftUInt8:
    UInt32ToUtf8(PB^,result);
  tftUInt16:
    UInt32ToUtf8(PWord(FieldBuffer)^,result);
  tftUInt24:
    // PInteger()^ and $ffffff -> possible GPF on Memory Mapped file
    UInt32ToUtf8(PWord(FieldBuffer)^+integer(PByteArray(FieldBuffer)^[2])shl 16,result);
  tftInt32:
    Int32ToUtf8(PInteger(FieldBuffer)^,result);
  tftInt64:
    Int64ToUtf8(PInt64(FieldBuffer)^,result);
  tftCurrency:
    Curr64ToStr(PInt64(FieldBuffer)^,result);
  tftDouble:
    ExtendedToStr(PDouble(FieldBuffer)^,DOUBLE_PRECISION,result);
  // some variable-size field value
  tftVarUInt32:
    UInt32ToUtf8(FromVarUInt32(PB),result);
  tftVarInt32:
    Int32ToUtf8(FromVarInt32(PB),result);
  tftVarUInt64:
    UInt64ToUtf8(FromVarUInt64(PB),result);
  tftVarInt64:
    Int64ToUtf8(FromVarInt64(PB),result);
  // text storage - WinAnsi could use less space than UTF-8
  tftWinAnsi, tftUTF8, tftBlobInternal: begin
    len := FromVarUInt32(PB);
    if len>0 then
      if FieldType<>tftWinAnsi then
        SetString(result,PC,len) else
        result := WinAnsiConvert.AnsiBufferToRawUTF8(PC,len);
  end;
  // tftBlobExternal fields e.g. must be directly accessed
  end;
end;

{$ifdef ISDELPHI20062007}
  {$WARNINGS ON} // circument Delphi 2007 false positive warning
{$endif}

procedure TSynTableFieldProperties.OrderedIndexReverseSet(aOrderedIndex: integer);
var nrev, ndx, n: PtrInt;
begin
  n := length(OrderedIndex);
  nrev := length(OrderedIndexReverse);
  if nrev=0 then
    if n=0 then
      exit else begin
      // void OrderedIndexReverse[]
      nrev := MaxInteger(OrderedIndex,OrderedIndexCount,n)+1;
      SetLength(OrderedIndexReverse,nrev);
      FillcharFast(OrderedIndexReverse[0],nrev*4,255); // all to -1
      Reverse(OrderedIndex,OrderedIndexCount,pointer(OrderedIndexReverse));
    end;
  if PtrUInt(aOrderedIndex)>=PtrUInt(OrderedIndexCount) then
    exit; // e.g. CreateFrom() will call OrderedIndexReverseSet(-1)
  if nrev<n then begin
    SetLength(OrderedIndexReverse,n); // resize if needed
    nrev := n;
  end;
  ndx := OrderedIndex[aOrderedIndex];
  if ndx>=nrev then
    SetLength(OrderedIndexReverse,ndx+256) else
  OrderedIndexReverse[ndx] := aOrderedIndex;
end;

procedure TSynTableFieldProperties.OrderedIndexSort(L, R: PtrInt);
var I, J, P: PtrInt;
    TmpI, TmpJ: integer;
begin
  if (L<R) and Assigned(Owner.GetRecordData) then
  repeat
    I := L; J := R;
    P := (L + R) shr 1;
    repeat
      with Owner do begin
        SortPivot := GetData(GetRecordData(OrderedIndex[P],DataTemp1),self);
        while SortCompare(GetData(GetRecordData(OrderedIndex[I],DataTemp2),self),
          SortPivot)<0 do inc(I);
        while SortCompare(GetData(GetRecordData(OrderedIndex[J],DataTemp2),self),
          SortPivot)>0 do dec(J);
      end;
      if I <= J then begin
        if I < J then begin
          TmpJ := OrderedIndex[J];
          TmpI := OrderedIndex[I];
          OrderedIndex[J] := TmpI;
          OrderedIndex[I] := TmpJ;
          // keep OrderedIndexReverse[OrderedIndex[i]]=i
          OrderedIndexReverse[TmpJ] := I;
          OrderedIndexReverse[TmpI] := J;
        end;
        if P = I then P := J else if P = J then P := I;
        inc(I); dec(J);
      end;
    until I > J;
    if J - L < R - I then begin // use recursion only for smaller range
      if L < J then
        OrderedIndexSort(L, J);
      L := I;
    end else begin
      if I < R then
        OrderedIndexSort(I, R);
      R := J;
    end;
  until L >= R;
end;

procedure TSynTableFieldProperties.OrderedIndexRefresh;
begin
  if (self=nil) or not OrderedIndexNotSorted then
    exit; // already sorted
  OrderedIndexSort(0,OrderedIndexCount-1);
  OrderedIndexNotSorted := false;
end;

function TSynTableFieldProperties.OrderedIndexFind(Value: pointer): PtrInt;
var L,R: PtrInt;
    cmp: PtrInt;
begin
  if OrderedIndexNotSorted then
    OrderedIndexRefresh;
  L := 0;
  R := OrderedIndexCount-1;
  with Owner do
    if (R>=0) and Assigned(GetRecordData) then
    repeat
      result := (L + R) shr 1;
      cmp := SortCompare(GetData(GetRecordData(OrderedIndex[result],DataTemp1),self),Value);
      if cmp=0 then
        exit;
      if cmp<0 then
        L := result + 1 else
        R := result - 1;
    until (L > R);
  result := -1
end;

function TSynTableFieldProperties.OrderedIndexFindAdd(Value: pointer): PtrInt;
var L,R,i: PtrInt;
    cmp: PtrInt;
begin
  if OrderedIndexNotSorted then
    OrderedIndexRefresh;
  R := OrderedIndexCount-1;
  if R<0 then
    result := 0 else
    with Owner do begin
      fOrderedIndexFindAdd := -1;
      L := 0;
      result := -1; // return -1 if found
      repeat
        i := (L + R) shr 1;
        cmp := SortCompare(GetData(GetRecordData(OrderedIndex[i],DataTemp1),self),Value);
        if cmp=0 then
          exit;
        if cmp<0 then
          L := i + 1 else
          R := i - 1;
      until (L > R);
      while (i>=0) and
            (SortCompare(GetData(GetRecordData(OrderedIndex[i],DataTemp1),self),Value)>=0) do
        dec(i);
      result := i+1; // return the index where to insert
    end;
  fOrderedIndexFindAdd := result; // store inserting index for OrderedIndexUpdate
end;

function TSynTableFieldProperties.OrderedIndexMatch(WhereSBFValue: pointer;
  var MatchIndex: TIntegerDynArray; var MatchIndexCount: integer; Limit: Integer=0): Boolean;
var i, L,R: PtrInt;
begin
  result := false;
  if (self=nil) or (WhereSBFValue=nil) or not Assigned(Owner.GetRecordData) or
     (OrderedIndex=nil) or not (tfoIndex in Options) then
    exit;
  i := OrderedIndexFind(WhereSBFValue);
  if i<0 then
    exit; // WHERE value not found
  if (tfoUnique in Options) or (Limit=1) then begin
    // unique index: direct fastest O(log(n)) binary search
    AddSortedInteger(MatchIndex,MatchIndexCount,OrderedIndex[i]);
    // AddSortedInteger() will fail if OrderedIndex[i] already exists
  end else
  with Owner do begin
    // multiple index matches possible: add matching range
    L := i;
    repeat
      dec(L);
    until (L<0) or (SortCompare(GetData(GetRecordData(
      OrderedIndex[L],DataTemp1),self),WhereSBFValue)<>0);
    R := i;
    repeat
      inc(R);
    until (R>=OrderedIndexCount) or
      (SortCompare(GetData(GetRecordData(OrderedIndex[R],DataTemp1),self),WhereSBFValue)<>0);
    if Limit=0 then
      Limit := MaxInt; // no LIMIT set -> retrieve all rows
    for i := L+1 to R-1 do begin
      AddSortedInteger(MatchIndex,MatchIndexCount,OrderedIndex[i]);
      dec(Limit);
      if Limit=0 then
        Break; // reach LIMIT upperbound result count
    end;
  end;
  result := true;
end;

function TSynTableFieldProperties.OrderedIndexUpdate(aOldIndex, aNewIndex: integer;
  aOldRecordData, aNewRecordData: pointer): boolean;
var aOldIndexIndex: integer;
begin
  result := false;
  if (self=nil) or not Assigned(Owner.GetRecordData) then
    exit; // avoid GPF
  // update content
  if aOldIndex<0 then
    if aNewIndex<0 then begin
      // both indexes equal -1 -> force sort
      OrderedIndexSort(0,OrderedIndexCount-1);
      OrderedIndexNotSorted := false;
    end else begin
      // added record
      if tfoUnique in Options then begin
        if fOrderedIndexFindAdd<0 then
          raise ETableDataException.CreateUTF8(
            '%.CheckConstraint call needed before %.OrderedIndexUpdate',[self,Name]);
        OrderedIndexReverseSet(InsertInteger(OrderedIndex,OrderedIndexCount,
          aNewIndex,fOrderedIndexFindAdd));
      end else begin
        AddInteger(OrderedIndex,OrderedIndexCount,aNewIndex);
        OrderedIndexReverseSet(OrderedIndexCount-1);
        OrderedIndexNotSorted := true; // -> OrderedIndexSort() call on purpose
      end;
    end else begin
    // aOldIndex>=0: update a value
    // retrieve position in OrderedIndex[] to be deleted/updated
    if OrderedIndexReverse=nil then
      OrderedIndexReverseSet(0) else // do OrderedIndexReverse[OrderedIndex[i]] := i
      {assert(aOldIndex<length(OrderedIndexReverse))};
    //assert(IntegerScanIndex(Pointer(OrderedIndex),OrderedIndexCount,aOldIndex)=OrderedIndexReverse[aOldIndex]);
    aOldIndexIndex := OrderedIndexReverse[aOldIndex]; // use FAST reverse array
    if aOldIndexIndex<0 then
      exit; // invalid Old index
    if aNewIndex<0 then begin
      // deleted record
      DeleteInteger(OrderedIndex,OrderedIndexCount,aOldIndexIndex);
      Reverse(OrderedIndex,OrderedIndexCount,pointer(OrderedIndexReverse));
      // no need to refresh OrderedIndex[], since data will remain sorted
    end else begin
      // updated record
      OrderedIndex[aOldIndexIndex] := aNewIndex;
      OrderedIndexReverseSet(aOldIndexIndex);
      if (aOldRecordData<>nil) or (aOldIndex<>aNewIndex) then // not in-place update
        with Owner do begin
          if aOldRecordData=nil then
            aOldRecordData := GetRecordData(aOldIndex,DataTemp1);
          if aNewRecordData=nil then
            aNewRecordData := GetRecordData(aNewIndex,DataTemp2);
          if SortCompare(GetData(aOldRecordData,self),GetData(aNewRecordData,self))=0 then begin
            // only sort if field content was modified -> MUCH faster in most case
            result := true;
            exit;
          end;
        end;
      if tfoUnique in Options then begin
        if fOrderedIndexFindAdd>=0 then begin
          // we know which OrderedIndex[] has to be changed -> manual update
          // - this is still a bottleneck in the current implementation, but
          // I was not able to find out how to make it faster, and still
          // being able to check unique field constraints without changing the
          // OrderedIndex[] content from a simple list into e.g. a red-black
          // tree: such a structure performs better, but uses much more memory
          // and is to be implemented
          // - it's still fast, faster than any DB AFAIK, around 500 updates
          // per second with 1,000,000 records on a Core i7
          // - it's still faster to refresh OrderedIndex[] than iterating
          // through all items to validate the unique constraint
          DeleteInteger(OrderedIndex,OrderedIndexCount,aOldIndexIndex);
          if fOrderedIndexFindAdd>aOldIndexIndex then
            dec(fOrderedIndexFindAdd);
          InsertInteger(OrderedIndex,OrderedIndexCount,aNewIndex,fOrderedIndexFindAdd);
          Reverse(OrderedIndex,OrderedIndexCount,pointer(OrderedIndexReverse));
        end else
          // slow full sort - with 1,000,000 items it's about 100 times slower
          // (never called with common usage in SynBigTable unit)
          OrderedIndexSort(0,OrderedIndexCount-1);
      end else
        OrderedIndexNotSorted := true; // will call OrderedIndexSort() on purpose
    end;
  end;
  fOrderedIndexFindAdd := -1; // consume this value
  result := true;
end;

procedure TSynTableFieldProperties.SaveTo(WR: TFileBufferWriter);
begin
  WR.Write(Name);
  WR.Write(@FieldType,SizeOf(FieldType));
  WR.Write(@Options,SizeOf(Options));
  WR.WriteVarUInt32Array(OrderedIndex,OrderedIndexCount,wkVarUInt32);
end;

function TSynTableFieldProperties.SBF(const Value: Int64): TSBFString;
var tmp: array[0..15] of AnsiChar;
begin
  case FieldType of
    tftInt32: begin // special version for handling negative values
      PInteger(@tmp)^ := Value;
      SetString(Result,tmp,SizeOf(Integer));
    end;
    tftUInt8, tftUInt16, tftUInt24, tftInt64:
      SetString(Result,PAnsiChar(@Value),FieldSize);
    tftVarUInt32:
      SetString(Result,tmp,PAnsiChar(ToVarUInt32(Value,@tmp))-tmp);
    tftVarInt32:
      SetString(Result,tmp,PAnsiChar(ToVarInt32(Value,@tmp))-tmp);
    tftVarUInt64:
      SetString(Result,tmp,PAnsiChar(ToVarUInt64(Value,@tmp))-tmp);
    tftVarInt64:
      SetString(Result,tmp,PAnsiChar(ToVarInt64(Value,@tmp))-tmp);
    else
      result := '';
  end;
end;

function TSynTableFieldProperties.SBF(const Value: Integer): TSBFString;
var tmp: array[0..15] of AnsiChar;
begin
  case FieldType of
    tftUInt8, tftUInt16, tftUInt24, tftInt32:
      SetString(Result,PAnsiChar(@Value),FieldSize);
    tftInt64: begin // special version for handling negative values
      PInt64(@tmp)^ := Value;
      SetString(Result,tmp,SizeOf(Int64));
    end;
    tftVarUInt32:
      if Value<0 then // expect an unsigned integer
        result := '' else
        SetString(Result,tmp,PAnsiChar(ToVarUInt32(Value,@tmp))-tmp);
    tftVarInt32:
      SetString(Result,tmp,PAnsiChar(ToVarInt32(Value,@tmp))-tmp);
    tftVarUInt64:
      if cardinal(Value)>cardinal(maxInt) then
        result := '' else // expect a 32 bit integer
        SetString(Result,tmp,PAnsiChar(ToVarUInt64(Value,@tmp))-tmp);
    tftVarInt64:
      SetString(Result,tmp,PAnsiChar(ToVarInt64(Value,@tmp))-tmp);
    else
      result := '';
  end;
end;

const
  SBF_BOOL: array[boolean] of TSBFString =
   (#0,#1);

{$ifndef NOVARIANTS}
function TSynTableFieldProperties.SBF(const Value: Variant): TSBFString;
var V64: Int64;
    VC: Currency absolute V64;
    VD: Double absolute V64;
begin // VarIsOrdinal/VarIsFloat/VarIsStr are buggy -> use field type
  case FieldType of
    tftBoolean:
      result := SBF_BOOL[boolean(Value)];
    tftUInt8, tftUInt16, tftUInt24, tftInt32, tftInt64,
    tftVarUInt32, tftVarInt32, tftVarUInt64, tftVarInt64: begin
      if not VariantToInt64(Value,V64) then
        V64 := 0;
      result := SBF(V64);
    end;
    tftCurrency: begin
      VC := Value;
      SetString(result,PAnsiChar(@VC),SizeOf(VC));
    end;
    tftDouble: begin
      VD := Value;
      SetString(result,PAnsiChar(@VD),SizeOf(VD));
    end;
    tftWinAnsi:
      ToSBFStr(WinAnsiConvert.UTF8ToAnsi(VariantToUTF8(Value)),result);
    tftUTF8:
      ToSBFStr(VariantToUTF8(Value),result);
    else
      result := '';
  end;
  if result='' then
    result := SBFDefault;
end;
{$endif}

function TSynTableFieldProperties.SBF(const Value: Boolean): TSBFString;
begin
  if FieldType<>tftBoolean then
    result := '' else
    result := SBF_BOOL[Value];
end;

function TSynTableFieldProperties.SBFCurr(const Value: Currency): TSBFString;
begin
  if FieldType<>tftCurrency then
    result := '' else
    SetString(Result,PAnsiChar(@Value),SizeOf(Value));
end;

procedure ToSBFStr(const Value: RawByteString; out Result: TSBFString);
var tmp: array[0..15] of AnsiChar;
    Len, Head: integer;
begin
  if PtrUInt(Value)=0 then
    Result := #0 else begin
    Len := {$ifdef FPC}length(Value){$else}PInteger(PtrUInt(Value)-SizeOf(integer))^{$endif};
    Head := PAnsiChar(ToVarUInt32(Len,@tmp))-tmp;
    SetLength(Result,Len+Head);
    {$ifdef FPC}Move{$else}MoveFast{$endif}(tmp,PByteArray(Result)[0],Head);
    {$ifdef FPC}Move{$else}MoveFast{$endif}(pointer(Value)^,PByteArray(Result)[Head],Len);
  end;
end;

function TSynTableFieldProperties.SBF(const Value: RawUTF8): TSBFString;
begin
  case FieldType of
    tftUTF8:
      ToSBFStr(Value,Result);
    tftWinAnsi:
      ToSBFStr(Utf8ToWinAnsi(Value),Result);
    else
      result := '';
  end;
end;

function TSynTableFieldProperties.SBF(Value: pointer; ValueLen: integer): TSBFString;
var tmp: array[0..15] of AnsiChar;
    Head: integer;
begin
  if FieldType<>tftBlobInternal then
    result := '' else
  if (Value=nil) or (ValueLen=0) then
    result := #0 else begin // inlined ToSBFStr() code
    Head := PAnsiChar(ToVarUInt32(ValueLen,@tmp))-tmp;
    SetString(Result,nil,ValueLen+Head);
    {$ifdef FPC}Move{$else}MoveFast{$endif}(tmp,PByteArray(Result)[0],Head);
    {$ifdef FPC}Move{$else}MoveFast{$endif}(Value^,PByteArray(Result)[Head],ValueLen);
  end;
end;

function TSynTableFieldProperties.SBFFloat(const Value: Double): TSBFString;
begin
  if FieldType<>tftDouble then
    result := '' else
    SetString(Result,PAnsiChar(@Value),SizeOf(Value));
end;

function TSynTableFieldProperties.SBFFromRawUTF8(const aValue: RawUTF8): TSBFString;
var Curr: Currency;
begin
  case FieldType of
  tftBoolean:
    if (SynCommons.GetInteger(pointer(aValue))<>0) or IdemPropNameU(aValue,'true') then
      result := #1 else
      result := #0; // store false by default
  tftUInt8, tftUInt16, tftUInt24, tftInt32, tftVarInt32:
    result := SBF(SynCommons.GetInteger(pointer(aValue)));
  tftVarUInt32, tftInt64, tftVarUInt64, tftVarInt64:
    result := SBF(SynCommons.GetInt64(pointer(aValue)));
  tftCurrency: begin
    PInt64(@Curr)^ := StrToCurr64(pointer(aValue));
    result := SBFCurr(Curr);
  end;
  tftDouble:
    result := SBFFloat(GetExtended(pointer(aValue)));
  // text storage - WinAnsi could use less space than UTF-8
  tftUTF8, tftWinAnsi:
    result := SBF(aValue);
  else
    result := ''; // tftBlob* fields e.g. must be handled directly
  end;
end;

function TSynTableFieldProperties.GetInteger(RecordBuffer: pointer): Integer;
begin
  if (self=nil) or (RecordBuffer=nil) or (Owner=nil) then
    result := 0 else begin
    RecordBuffer := Owner.GetData(RecordBuffer,self);
    case FieldType of
    tftBoolean, tftUInt8:
      result := PByte(RecordBuffer)^;
    tftUInt16:
      result := PWord(RecordBuffer)^;
    tftUInt24:
      // PInteger()^ and $ffffff -> possible GPF on Memory Mapped file
      result := PWord(RecordBuffer)^+integer(PByteArray(RecordBuffer)^[2])shl 16;
    tftInt32:
      result := PInteger(RecordBuffer)^;
    tftInt64:
      result := PInt64(RecordBuffer)^;
    // some variable-size field value
    tftVarUInt32:
      result := FromVarUInt32(PByte(RecordBuffer));
    tftVarInt32:
      result := FromVarInt32(PByte(RecordBuffer));
    tftVarUInt64:
      result := FromVarUInt64(PByte(RecordBuffer));
    tftVarInt64:
      result := FromVarInt64(PByte(RecordBuffer));
    else
      result := 0;
    end;
  end;
end;

function TSynTableFieldProperties.GetInt64(RecordBuffer: pointer): Int64;
var PB: PByte;
begin
  if (self=nil) or (RecordBuffer=nil) or (Owner=nil) then
    result := 0 else begin
    PB := Owner.GetData(RecordBuffer,self);
    case FieldType of
    tftInt64:
      result := PInt64(PB)^;
    tftVarUInt64:
      result := FromVarUInt64(PB);
    tftVarInt64:
      result := FromVarInt64(PB);
    else
      result := GetInteger(RecordBuffer);
    end;
  end;
end;

function TSynTableFieldProperties.GetBoolean(RecordBuffer: pointer): Boolean;
begin
  result := boolean(GetInteger(RecordBuffer));
end;

function TSynTableFieldProperties.GetCurrency(RecordBuffer: pointer): Currency;
begin
  if (self=nil) or (RecordBuffer=nil) or (Owner=nil) then
    result := 0 else
    case FieldType of
    tftCurrency:
      result := PCurrency(Owner.GetData(RecordBuffer,self))^;
    else
      result := GetInt64(RecordBuffer);
    end;
end;

function TSynTableFieldProperties.GetDouble(RecordBuffer: pointer): Double;
begin
  if (self=nil) or (RecordBuffer=nil) or (Owner=nil) then
    result := 0 else
    case FieldType of
    tftDouble:
      result := PDouble(Owner.GetData(RecordBuffer,self))^;
    else
      result := GetInt64(RecordBuffer);
    end;
end;

function TSynTableFieldProperties.GetRawUTF8(RecordBuffer: pointer): RawUTF8;
begin
  if (self=nil) or (RecordBuffer=nil) or (Owner=nil) then
    result := '' else begin
    RecordBuffer := Owner.GetData(RecordBuffer,self);
    if RecordBuffer<>nil then
      result := GetValue(RecordBuffer) else // will do conversion to text
      result := '';
  end;
end;

function TSynTableFieldProperties.AddFilterOrValidate(aFilter: TSynFilterOrValidate): TSynFilterOrValidate;
procedure Add(var List: TObjectList);
begin
  if List=nil then
    List := TObjectList.Create;
  List.Add(result);
end;
begin
  result := aFilter;
  if (self=nil) or (result=nil) then
    result := nil else
  if aFilter.InheritsFrom(TSynFilter) then
    Add(Filters) else
  if aFilter.InheritsFrom(TSynValidate) then
    Add(Validates) else
    result := nil;
end;

function TSynTableFieldProperties.Validate(RecordBuffer: pointer;
  RecordIndex: integer): string;
var i: integer;
    Value: RawUTF8;
    aValidate: TSynValidate;
    aValidateTable: TSynValidateTable absolute aValidate;
begin
  result := '';
  if (self=nil) or (Validates=nil) then
    exit;
  Value := GetRawUTF8(RecordBuffer); // TSynTableValidate needs RawUTF8 text
  for i := 0 to Validates.Count-1 do begin
    aValidate := Validates.List[i];
    if aValidate.InheritsFrom(TSynValidateTable) then begin
      aValidateTable.ProcessField := self;
      aValidateTable.ProcessRecordIndex := RecordIndex;
    end;
    if not aValidate.Process(FieldNumber,Value,result) then begin
      if result='' then
        // no custom message -> show a default message
        result := format(sValidationFailed,[
          GetCaptionFromClass(aValidate.ClassType)]);
      break;
    end;
  end;
end;

{$ifdef SORTCOMPAREMETHOD}
function TSynTableFieldProperties.SortCompare(P1, P2: PUTF8Char): PtrInt;
var i, L: integer;
label minus,plus,zer;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
      case FieldType of
      tftBoolean, tftUInt8:
        result := PByte(P1)^-PByte(P2)^;
      tftUInt16:
        result := PWord(P1)^-PWord(P2)^;
      tftUInt24:
        result := PtrInt(PWord(P1)^)+PtrInt(P1[2])shl 16
          -PtrInt(PWord(P2)^)-PtrInt(P2[2]) shl 16;
      tftInt32:
        result := PInteger(P1)^-PInteger(P2)^;
      tftDouble: begin
        PDouble(@SortCompareTmp)^ := PDouble(P1)^-PDouble(P2)^;
        if PDouble(@SortCompareTmp)^<0 then
          goto minus else
        if PDouble(@SortCompareTmp)^>0 then
          goto plus else
          goto zer;
      end;
      tftVarUInt32:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        result := FromVarUInt32(PB1)-FromVarUInt32(PB2);
      end;
      tftVarInt32:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        result := FromVarInt32(PB1)-FromVarInt32(PB2);
      end;
      {$ifdef CPUX86} // circumvent comparison slowness (and QWord bug)
      tftInt64, tftCurrency:
        result := SortInt64(PInt64(P1)^,PInt64(P2)^);
      tftVarUInt64:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        result := SortQWord(FromVarUInt64(PB1),FromVarUInt64(PB2));
      end;
      tftVarInt64:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        result := SortInt64(FromVarInt64(PB1),FromVarInt64(PB2));
      end;
      {$else}
      {$ifdef CPU64} // PtrInt = Int64
      tftInt64, tftCurrency:
        result := PInt64(P1)^-PInt64(P2)^;
      tftVarUInt64:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        result := FromVarUInt64(PB1)-FromVarUInt64(PB2);
      end;
      tftVarInt64:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        result := FromVarInt64(PB1)-FromVarInt64(PB2);
      end;
      {$else}
      tftInt64, tftCurrency: begin
        PInt64(@SortCompareTmp)^ := PInt64(P1)^-PInt64(P2)^;
        if PInt64(@SortCompareTmp)^<0 then
          goto minus else
        if PInt64(@SortCompareTmp)^>0 then
          goto plus else
          goto zer;
      end;
      tftVarUInt64:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        PInt64(@SortCompareTmp)^ := FromVarUInt64(PB1)-FromVarUInt64(PB2);
        if PInt64(@SortCompareTmp)^<0 then
          goto minus else
        if PInt64(@SortCompareTmp)^>0 then
          goto plus else
          goto zer;
      end;
      tftVarInt64:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        PInt64(@SortCompareTmp)^ := FromVarInt64(PB1)-FromVarInt64(PB2);
        if PInt64(@SortCompareTmp)^<0 then
          goto minus else
        if PInt64(@SortCompareTmp)^>0 then
          goto plus else
          goto zer;
      end;
      {$endif}
      {$endif}
      tftWinAnsi, tftUTF8, tftBlobInternal:
      begin
        with SortCompareTmp do begin
          if PtrInt(P1^)<=$7F then begin
            L1 := PtrInt(P1^);
            inc(P1);
          end else begin
            PB1 := pointer(P1);
            L1 := FromVarUInt32High(PB1);
            P1 := pointer(PB1);
          end;
          if PtrInt(P2^)<=$7F then begin
            L2 := PtrInt(P2^);
            inc(P2);
          end else begin
            PB2 := pointer(P2);
            L2 := FromVarUInt32High(PB2);
            P2 := pointer(PB2);
          end;
        end;
        with SortCompareTmp do begin
          L := L1;
          if L2>L then
            L := L2;
        end;
        if tfoCaseInsensitive in Options then begin
          i := 0;
          while i<L do begin
            result := PtrInt(NormToUpperAnsi7[P1[i]])-PtrInt(NormToUpperAnsi7[P2[i]]);
            if result<>0 then
              exit else
              inc(i);
          end;
        end else begin
          i := 0;
          while i<L do begin
            result := PtrInt(P1[i])-PtrInt(P2[i]);
            if result<>0 then
              exit else
              inc(i);
          end;
        end;
        with SortCompareTmp do
          result := L1-L2;
      end;
      else
        goto zer;
      end else
plus:   result := 1 else  // P2=nil
minus:result := -1 else // P1=nil
zer:result := 0;      // P1=P2
end;
{$endif}

function CompareOperator(FieldType: TSynTableFieldType; SBF, SBFEnd: PUTF8Char;
  Value: Int64; Oper: TCompareOperator): boolean;
var V: Int64;
    PB: PByte absolute SBF;
begin
  result := true;
  if PB<>nil then
  repeat
    case FieldType of
      tftBoolean, tftUInt8:
        V := PB^;
      tftUInt16:
        V := PWord(PB)^;
      tftUInt24:
        // PInteger()^ and $ffffff -> possible GPF on Memory Mapped file
        V := PWord(PB)^+integer(PByteArray(PB)^[2])shl 16;
      tftInt32:
        V := PInteger(PB)^;
      tftInt64:
        V := PInt64(PB)^;
      // some variable-size field value
      tftVarUInt32:
        V := FromVarUInt32(PB);
      tftVarInt32:
        V := FromVarInt32(PB);
      tftVarUInt64:
        V := FromVarUInt64(PB);
      tftVarInt64:
        V := FromVarInt64(PB);
      else V := 0;  // makes compiler happy
    end;
    case Oper of
      soEqualTo:              if V=Value then exit;
      soNotEqualTo:           if V<>Value then exit;
      soLessThan:             if V<Value then exit;
      soLessThanOrEqualTo:    if V<=Value then exit;
      soGreaterThan:          if V>Value then exit;
      soGreaterThanOrEqualTo: if V>=Value then exit;
      else break;
    end;
    // not found: go to next value
    if SBFEnd=nil then
      break; // only one value to be checked
    if FIELD_FIXEDSIZE[FieldType]>0 then
      inc(SBF,FIELD_FIXEDSIZE[FieldType]); // FromVar*() already updated PB/SBF
  until SBF>=SBFEnd;
  result := false; // not found
end;

function CompareOperator(SBF, SBFEnd: PUTF8Char;
  Value: double; Oper: TCompareOperator): boolean;
begin
  result := true;
  if SBF<>nil then
  repeat
    case Oper of
      soEqualTo:              if PDouble(SBF)^=Value then exit;
      soNotEqualTo:           if PDouble(SBF)^<>Value then exit;
      soLessThan:             if PDouble(SBF)^<Value then exit;
      soLessThanOrEqualTo:    if PDouble(SBF)^<=Value then exit;
      soGreaterThan:          if PDouble(SBF)^>Value then exit;
      soGreaterThanOrEqualTo: if PDouble(SBF)^>=Value then exit;
      else break;
    end;
    // not found: go to next value
    if SBFEnd=nil then
      break; // only one value to be checked
    inc(SBF,SizeOf(Value));
  until SBF>=SBFEnd;
  result := false; // not found
end;

function CompareOperator(FieldType: TSynTableFieldType; SBF, SBFEnd: PUTF8Char;
  Value: PUTF8Char; ValueLen: integer; Oper: TCompareOperator;
  CaseSensitive: boolean): boolean; overload;
var L, Cmp: PtrInt;
    PB: PByte;
    tmp: array[byte] of AnsiChar;
begin
  result := true;
  if SBF<>nil then
  repeat
    // get length of text in the SBF encoded buffer
    if integer(SBF^)<=$7f then begin
      L := integer(SBF^);
      inc(SBF);
    end else begin
      PB := Pointer(SBF);
      L := FromVarUInt32(PB);
      SBF := pointer(PB);
    end;
    // perform comparison: returns nil on match
    case Oper of
      soEqualTo..soGreaterThanOrEqualTo: begin
        Cmp := L-ValueLen;
        if Cmp<0 then
          L := ValueLen;
        if CaseSensitive then
          Cmp := StrCompL(SBF,Value,L,Cmp) else
          Cmp := StrCompIL(SBF,Value,L,Cmp);
        case Oper of
          soEqualTo:              if Cmp=0 then exit;
          soNotEqualTo:           if Cmp<>0 then exit;
          soLessThan:             if Cmp<0 then exit;
          soLessThanOrEqualTo:    if Cmp<=0 then exit;
          soGreaterThan:          if Cmp>0 then exit;
          soGreaterThanOrEqualTo: if Cmp>=0 then exit;
        end;
      end;
      soBeginWith:
        if ValueLen>=L then
          if CaseSensitive then begin
            if StrCompL(SBF,Value,ValueLen,0)=0 then
              exit;
          end else
            if StrCompIL(SBF,Value,ValueLen,0)=0 then
              exit;
      soContains: begin
        dec(L,ValueLen);
        while L>=0 do begin
          while (L>=0) and not(byte(SBF^) in IsWord) do begin
            dec(L);
            inc(SBF);
          end; // begin of next word reached
          if L<0 then
            Break; // not enough chars to contain the Value
          if CaseSensitive then begin
            if StrCompL(SBF,Value,ValueLen,0)=0 then
              exit;
          end else
            if StrCompIL(SBF,Value,ValueLen,0)=0 then
              exit;
          while (L>=0) and (byte(SBF^) in IsWord) do begin
            dec(L);
            inc(SBF);
          end; // end of word reached
        end;
        if SBFEnd=nil then
          break; // only one value to be checked
        inc(SBF,ValueLen); // custom inc(SBF,L);
        if SBF<SBFEnd then
          continue else break;
      end;
      soSoundsLikeEnglish,
      soSoundsLikeFrench,
      soSoundsLikeSpanish: begin
        if L>high(tmp) then
          Cmp := high(tmp) else
          Cmp := L;
        tmp[Cmp] := #0; // TSynSoundEx expect the buffer to be #0 terminated
        {$ifdef FPC}Move{$else}MoveFast{$endif}(SBF^,tmp,Cmp);
        case FieldType of
        tftWinAnsi:
          if PSynSoundEx(Value)^.Ansi(tmp) then
            exit;
        tftUTF8:
          if PSynSoundEx(Value)^.UTF8(tmp) then
            exit;
        else break;
        end;
      end;
      else break;
    end;
    // no match -> go to the end of the SBF buffer
    if SBFEnd=nil then
      exit; // only one value to be checked
    inc(SBF,L);
    if SBF>=SBFEnd then
      break;
  until false;
end;


{ TSynValidateTableUniqueField }

function TSynValidateTableUniqueField.Process(aFieldIndex: integer;
  const Value: RawUTF8; var ErrorMsg: string): boolean;
var S: TSBFString;
begin
  result := false;
  if (self=nil) or (Value='') or (ProcessField=nil) then
    exit; // void field can't be unique
  if not (tfoIndex in ProcessField.Options) then
    exit; // index should be always created by TSynTable.AfterFieldModif
  S := ProcessField.SBFFromRawUTF8(Value);
  if S='' then
    exit; // void field can't be unique
  if ProcessField.OrderedIndexFindAdd(Pointer(S))>=0 then
    // there is some place to insert the Value -> not existing yet -> OK
    result := true else begin
    // RecordIndex=-1 in case of adding, or the physical index of the updated record
    if (ProcessRecordIndex>=0) and
       (ProcessField.OrderedIndex[ProcessField.OrderedIndexFind(Pointer(S))]=
         ProcessRecordIndex) then
      // allow update of the record
      result := true else
      // found a dupplicated value
      ErrorMsg := sValidationFieldDuplicate;
  end;
end;


{ TSynTableStatement }

const
  NULL_UPP  = ord('N')+ord('U')shl 8+ord('L')shl 16+ord('L')shl 24;

constructor TSynTableStatement.Create(const SQL: RawUTF8;
  GetFieldIndex: TSynTableFieldIndex; SimpleFieldsBits: TSQLFieldBits;
  FieldProp: TSynTableFieldProperties);
var Prop, whereBefore: RawUTF8;
    P, B: PUTF8Char;
    ndx,err,len,selectCount,whereCount: integer;
    whereWithOR,whereNotClause: boolean;

function GetPropIndex: integer;
begin
  if not GetNextFieldProp(P,Prop) then
    result := -1 else
  if IsRowID(pointer(Prop)) then
    result := 0 else begin // 0 = ID field
    result := GetFieldIndex(Prop);
    if result>=0 then // -1 = no valid field name
      inc(result);  // otherwise: PropertyIndex+1
  end;
end;
function SetFields: boolean;
var select: TSynTableStatementSelect;
begin
  result := false;
  FillcharFast(select,SizeOf(select),0);
  select.Field := GetPropIndex; // 0 = ID, otherwise PropertyIndex+1
  if select.Field<0 then begin
    if P^<>'(' then // Field not found -> try function(field)
      exit;
    P := GotoNextNotSpace(P+1);
    select.FunctionName := Prop;
    inc(fSelectFunctionCount);
    if IdemPropNameU(Prop,'COUNT') and (P^='*') then begin
      select.Field := 0; // count(*) -> count(ID)
      select.FunctionKnown := funcCountStar;
      P := GotoNextNotSpace(P+1);
    end else begin
      if IdemPropNameU(Prop,'DISTINCT') then
        select.FunctionKnown := funcDistinct else
      if IdemPropNameU(Prop,'MAX') then
        select.FunctionKnown := funcMax;
      select.Field := GetPropIndex;
      if select.Field<0 then
        exit;
    end;
    if P^<>')' then
      exit;
    P := GotoNextNotSpace(P+1);
  end;
  if P^ in ['+','-'] then begin
    select.ToBeAdded := GetNextItemInteger(P,' ');
    if select.ToBeAdded=0 then
      exit;
    P := GotoNextNotSpace(P);
  end;
  if IdemPChar(P,'AS ') then begin
    inc(P,3);
    if not GetNextFieldProp(P,select.Alias) then
      exit;
  end;
  SetLength(fSelect,selectCount+1);
  fSelect[selectCount] := select;
  inc(selectCount);
  result := true;
end;
function GetWhereValue(var Where: TSynTableStatementWhere): boolean;
var B: PUTF8Char;
begin
  result := false;
  P := GotoNextNotSpace(P);
  Where.ValueSQL := P;
  if PWord(P)^=ord(':')+ord('(') shl 8 then
    inc(P,2); // ignore :(...): parameter (no prepared statements here)
  if P^ in ['''','"'] then begin
    // SQL String statement
    P := UnQuoteSQLStringVar(P,Where.Value);
    if P=nil then
      exit; // end of string before end quote -> incorrect
    {$ifndef NOVARIANTS}
    RawUTF8ToVariant(Where.Value,Where.ValueVariant);
    {$endif}
    if FieldProp<>nil then
      // create a SBF formatted version of the WHERE value
      Where.ValueSBF := FieldProp.SBFFromRawUTF8(Where.Value);
  end else
  if (PInteger(P)^ and $DFDFDFDF=NULL_UPP) and (P[4] in [#0..' ',';']) then begin
    // NULL statement
    Where.Value := NULL_STR_VAR; // not void
    {$ifndef NOVARIANTS}
    SetVariantNull(Where.ValueVariant);
    {$endif}
  end else begin
    // numeric statement or 'true' or 'false' (OK for NormalizeValue)
    B := P;
    repeat
      inc(P);
    until P^ in [#0..' ',';',')',','];
    SetString(Where.Value,B,P-B);
    {$ifndef NOVARIANTS}
    Where.ValueVariant := VariantLoadJSON(Where.Value);
    {$endif}
    Where.ValueInteger := GetInteger(pointer(Where.Value),err);
    if FieldProp<>nil then
      if Where.Value<>'?' then
        if (FieldProp.FieldType in FIELD_INTEGER) and (err<>0) then
          // we expect a true INTEGER value here
          Where.Value := '' else
          // create a SBF formatted version of the WHERE value
          Where.ValueSBF := FieldProp.SBFFromRawUTF8(Where.Value);
  end;
  if PWord(P)^=ord(')')+ord(':')shl 8 then
    inc(P,2); // ignore :(...): parameter
  Where.ValueSQLLen := P-Where.ValueSQL;
  P := GotoNextNotSpace(P);
  if (P^=')') and (Where.FunctionName='') then begin
    B := P;
    repeat
      inc(P);
    until not (P^ in [#1..' ',')']);
    while P[-1]=' ' do dec(P); // trim right space
    SetString(Where.ParenthesisAfter,B,P-B);
    P := GotoNextNotSpace(P);
  end;
  result := true;
end;
{$ifndef NOVARIANTS}
function GetWhereValues(var Where: TSynTableStatementWhere): boolean;
var v: TSynTableStatementWhereDynArray;
    n, w: integer;
    tmp: RawUTF8;
begin
  result := false;
  if Where.ValueSQLLen<=2 then
    exit;
  SetString(tmp,PAnsiChar(Where.ValueSQL)+1,Where.ValueSQLLen-2);
  P := pointer(tmp); // parse again the IN (...,...,... ) expression
  n := 0;
  try
    repeat
      if n=length(v) then
        SetLength(v,NextGrow(n));
      if not GetWhereValue(v[n]) then
        exit;
      inc(n);
      if P^=#0 then
        break;
      if P^<>',' then
        exit;
      inc(P);
    until false;
  finally
    P := Where.ValueSQL+Where.ValueSQLLen; // continue parsing as usual
  end;
  with TDocVariantData(Where.ValueVariant) do begin
    InitFast(n,dvArray);
    for w := 0 to n-1 do
      AddItem(v[w].ValueVariant);
    Where.Value := ToJSON;
  end;
  result := true;
end;
{$endif}
function GetWhereExpression(FieldIndex: integer; var Where: TSynTableStatementWhere): boolean;
begin
  result := false;
  Where.ParenthesisBefore := whereBefore;
  Where.JoinedOR := whereWithOR;
  Where.NotClause := whereNotClause;
  Where.Field := FieldIndex; // 0 = ID, otherwise PropertyIndex+1
  case P^ of
  '=': Where.Operator := opEqualTo;
  '>': if P[1]='=' then begin
         inc(P);
         Where.Operator := opGreaterThanOrEqualTo;
       end else
         Where.Operator := opGreaterThan;
  '<': case P[1] of
       '=': begin
         inc(P);
         Where.Operator := opLessThanOrEqualTo;
       end;
       '>': begin
         inc(P);
         Where.Operator := opNotEqualTo;
       end;
       else
         Where.Operator := opLessThan;
       end;
  'i','I':
    case P[1] of
    's','S': begin
      P := GotoNextNotSpace(P+2);
      if IdemPChar(P,'NULL') then begin
        Where.Value := NULL_STR_VAR;
        Where.Operator := opIsNull;
        Where.ValueSQL := P;
        Where.ValueSQLLen := 4;
        {$ifndef NOVARIANTS}
        TVarData(Where.ValueVariant).VType := varNull;
        {$endif}
        inc(P,4);
        result := true;
      end else
      if IdemPChar(P,'NOT NULL') then begin
        Where.Value := 'not null';
        Where.Operator := opIsNotNull;
        Where.ValueSQL := P;
        Where.ValueSQLLen := 8;
        {$ifndef NOVARIANTS}
        TVarData(Where.ValueVariant).VType := varNull;
        {$endif}
        inc(P,8);
        result := true; // leave ValueVariant=unassigned
      end;
      exit;
    end;
    {$ifndef NOVARIANTS}
    'n','N': begin
       Where.Operator := opIn;
       P := GotoNextNotSpace(P+2);
       if P^<>'(' then
         exit; // incorrect SQL statement
       B := P; // get the IN() clause as JSON
       inc(P);
       while (P^<>')') or (P[1]=':') do // handle :(...): within the clause
         if P^=#0 then
           exit else
           inc(P);
       inc(P);
       SetString(Where.Value,PAnsiChar(B),P-B);
       Where.ValueSQL := B;
       Where.ValueSQLLen := P-B;
       result := GetWhereValues(Where);
       exit;
    end;
    {$endif}
    end; // 'i','I':
  'l','L':
    if IdemPChar(P+1,'IKE') then begin
      inc(P,3);
      Where.Operator := opLike;
    end else
    exit;
  else exit; // unknown operator
  end;
  // we got 'WHERE FieldName operator ' -> handle value
  inc(P);
  result := GetWhereValue(Where);
end;

label lim,lim2;
begin
  P := pointer(SQL);
  if (P=nil) or (self=nil) then
    exit; // avoid GPF
  P := GotoNextNotSpace(P); // trim left
  if not IdemPChar(P,'SELECT ') then
    exit else // handle only SELECT statement
    inc(P,7);
  // 1. get SELECT clause: set bits in Fields from CSV field IDs in SQL
  selectCount := 0;
  P := GotoNextNotSpace(P); // trim left
  if P^=#0 then
    exit; // no SQL statement
  if P^='*' then begin // all simple (not TSQLRawBlob/TSQLRecordMany) fields
    inc(P);
    len := GetBitsCount(SimpleFieldsBits,MAX_SQLFIELDS)+1;
    SetLength(fSelect,len);
    selectCount := 1; // Select[0].Field := 0 -> ID
    for ndx := 0 to MAX_SQLFIELDS-1 do
      if ndx in SimpleFieldsBits then begin
        fSelect[selectCount].Field := ndx+1;
        inc(selectCount);
        if selectCount=len then
          break;
      end;
    GetNextFieldProp(P,Prop);
  end else
  if not SetFields then
    exit else // we need at least one field name
    if P^<>',' then
      GetNextFieldProp(P,Prop) else
      repeat
        while P^ in [',',#1..' '] do inc(P); // trim left
      until not SetFields; // add other CSV field names
  // 2. get FROM clause
  if not IdemPropNameU(Prop,'FROM') then exit; // incorrect SQL statement
  GetNextFieldProp(P,Prop);
  fTableName := Prop;
  // 3. get WHERE clause
  whereCount := 0;
  whereWithOR := false;
  whereNotClause := false;
  whereBefore := '';
  GetNextFieldProp(P,Prop);
  if IdemPropNameU(Prop,'WHERE') then begin
    repeat
      B := P;
      if P^='(' then begin
        fWhereHasParenthesis := true;
        repeat
          inc(P);
        until not (P^ in [#1..' ','(']);
        while P[-1]=' ' do dec(P); // trim right space
        SetString(whereBefore,B,P-B);
        B := P;
      end;
      ndx := GetPropIndex;
      if ndx<0 then begin
        if IdemPropNameU(Prop,'NOT') then begin
          whereNotClause := true;
          continue;
        end;
        if P^='(' then begin
          inc(P);
          SetLength(fWhere,whereCount+1);
          with fWhere[whereCount] do begin
            ParenthesisBefore := whereBefore;
            JoinedOR := whereWithOR;
            NotClause := whereNotClause;
            FunctionName := UpperCase(Prop);
            // Byte/Word/Integer/Cardinal/Int64/CurrencyDynArrayContains(BlobField,I64)
            len := length(Prop);
            if (len>16) and
               IdemPropName('DynArrayContains',PUTF8Char(@PByteArray(Prop)[len-16]),16) then
              Operator := opContains else
              Operator := opFunction;
            B := P;
            Field := GetPropIndex;
            if Field<0 then
              P := B else
              if P^<>',' then
                break else
                P := GotoNextNotSpace(P+1);
            if (P^=')') or
               (GetWhereValue(fWhere[whereCount]) and (P^=')')) then begin
              inc(P);
              break;
            end;
          end;
        end;
        P := B;
        break;
      end;
      SetLength(fWhere,whereCount+1);
      if not GetWhereExpression(ndx,fWhere[whereCount]) then
        exit; // invalid SQL statement
      inc(whereCount);
      GetNextFieldProp(P,Prop);
      if IdemPropNameU(Prop,'OR') then
        whereWithOR := true else
      if IdemPropNameU(Prop,'AND') then
        whereWithOR := false else
        goto lim2;
      whereNotClause := false;
      whereBefore := '';
    until false;
    // 4. get optional LIMIT/OFFSET/ORDER clause
lim:P := GotoNextNotSpace(P);
    while (P<>nil) and not(P^ in [#0,';']) do begin
      GetNextFieldProp(P,Prop);
lim2: if IdemPropNameU(Prop,'LIMIT') then
        fLimit := GetNextItemCardinal(P,' ') else
      if IdemPropNameU(Prop,'OFFSET') then
        fOffset := GetNextItemCardinal(P,' ') else
      if IdemPropNameU(Prop,'ORDER') then begin
        GetNextFieldProp(P,Prop);
        if IdemPropNameU(Prop,'BY') then begin
          repeat
            ndx := GetPropIndex; // 0 = ID, otherwise PropertyIndex+1
            if ndx<0 then
              exit; // incorrect SQL statement
            AddFieldIndex(fOrderByField,ndx);
            if P^<>',' then begin // check ORDER BY ... ASC/DESC
              B := P;
              if GetNextFieldProp(P,Prop) then
                if IdemPropNameU(Prop,'DESC') then
                  fOrderByDesc := true else
                if not IdemPropNameU(Prop,'ASC') then
                  P := B;
              break;
            end;
            P := GotoNextNotSpace(P+1);
          until P^ in [#0,';'];
        end else
        exit; // incorrect SQL statement
      end else
      if IdemPropNameU(Prop,'GROUP') then begin
        GetNextFieldProp(P,Prop);
        if IdemPropNameU(Prop,'BY') then begin
          repeat
            ndx := GetPropIndex; // 0 = ID, otherwise PropertyIndex+1
            if ndx<0 then
              exit; // incorrect SQL statement
            AddFieldIndex(fGroupByField,ndx);
            if P^<>',' then
              break;
            P := GotoNextNotSpace(P+1);
          until P^ in [#0,';'];
        end else
        exit; // incorrect SQL statement
      end else
      if Prop<>'' then
        exit else // incorrect SQL statement
        break; // reached the end of the statement
    end;
  end else
  if Prop<>'' then
    goto lim2; // handle LIMIT OFFSET ORDER
  fSQLStatement := SQL; // make a private copy e.g. for Where[].ValueSQL
end;

procedure TSynTableStatement.SelectFieldBits(var Fields: TSQLFieldBits; var withID: boolean);
var i: integer;
begin
  FillcharFast(Fields,SizeOf(Fields),0);
  withID := false;
  for i := 0 to Length(Select)-1 do
    if Select[i].Field=0 then
      withID := true else
      include(Fields,Select[i].Field-1);
end;


{$ifndef DELPHI5OROLDER}

{ TSynTableData }

procedure TSynTableData.CheckVTableInitialized;
begin
  if VTable=nil then
    raise ETableDataException.Create('TSynTableData non initialized');
end;

{$ifndef NOVARIANTS}

function TSynTableData.GetFieldValue(const FieldName: RawUTF8): Variant;
begin
  GetFieldVariant(FieldName,result);
end;

procedure TSynTableData.GetFieldVariant(const FieldName: RawUTF8; var result: Variant);
var aField: TSynTableFieldProperties;
begin
  if IsRowID(Pointer(FieldName)) then
    result := VID else begin
    CheckVTableInitialized;
    aField := VTable.FieldFromName[FieldName];
    if aField=nil then
      raise ETableDataException.CreateUTF8('Unknown % property',[FieldName]) else
    aField.GetVariant(VTable.GetData(pointer(VValue),aField),result);
  end;
end;

function TSynTableData.GetFieldValue(aField: TSynTableFieldProperties): Variant;
begin
  CheckVTableInitialized;
  aField.GetVariant(VTable.GetData(pointer(VValue),aField),result);
end;

{$endif NOVARIANTS}

procedure TSynTableData.FilterSBFValue;
begin
  CheckVTableInitialized;
  VTable.Filter(VValue);
end;

function TSynTableData.GetFieldSBFValue(aField: TSynTableFieldProperties): TSBFString;
var FieldBuffer: PAnsiChar;
begin
  CheckVTableInitialized;
  FieldBuffer := VTable.GetData(pointer(VValue),aField);
  SetString(Result,FieldBuffer,aField.GetLength(FieldBuffer));
end;

procedure TSynTableData.Init(aTable: TSynTable; aID: Integer);
begin
  VTable := aTable;
  VID := aID;
  VValue := VTable.DefaultRecordData;
  {$ifdef UNICODE}FillcharFast(Filler,SizeOf(Filler),0);{$endif}
end;

procedure TSynTableData.Init(aTable: TSynTable; aID: Integer;
  RecordBuffer: pointer; RecordBufferLen: integer);
begin
  VTable := aTable;
  if (RecordBufferLen=0) or (RecordBuffer=nil) then begin
    VID := 0;
    VValue := VTable.DefaultRecordData;
  end else begin
    VID := aID;
    SetString(VValue,PAnsiChar(RecordBuffer),RecordBufferLen);
  end;
end;

{$ifndef NOVARIANTS}
procedure TSynTableData.SetFieldValue(const FieldName: RawUTF8;
  const Value: Variant);
var F: TSynTableFieldProperties;
begin
  CheckVTableInitialized;
  if IsRowID(Pointer(FieldName)) then
    VID := Value else begin
    F := VTable.FieldFromName[FieldName];
    if F=nil then
      raise ETableDataException.CreateUTF8('Unknown % property',[FieldName]) else
      SetFieldValue(F,Value);
  end;
end;

procedure TSynTableData.SetFieldValue(aField: TSynTableFieldProperties; const Value: Variant);
begin
  SetFieldSBFValue(aField,aField.SBF(Value));
end;
{$endif}

procedure TSynTableData.SetFieldSBFValue(aField: TSynTableFieldProperties;
  const Value: TSBFString);
var NewValue: TSBFString;
begin
  CheckVTableInitialized;
  if (aField.FieldSize>0) and (VValue<>'') then begin
    // fixed size content: fast in-place update
    {$ifdef FPC}Move{$else}MoveFast{$endif}(pointer(Value)^,VValue[aField.Offset+1],aField.FieldSize)
    // VValue[F.Offset+1] above will call UniqueString(VValue), even under FPC
  end else begin
    // variable-length update
    VTable.UpdateFieldData(pointer(VValue),length(VValue),
      aField.FieldNumber,NewValue,Value);
    VValue := NewValue;
  end;
end;

function TSynTableData.ValidateSBFValue(RecordIndex: integer): string;
begin
  CheckVTableInitialized;
  Result := VTable.Validate(Pointer(VValue),RecordIndex);
end;

{$endif DELPHI5OROLDER}


{ ************ filtering and validation classes and functions *************** }

function IsValidIP4Address(P: PUTF8Char): boolean;
var ndot: PtrInt;
    V: PtrUInt;
begin
  result := false;
  if (P=nil) or not (P^ in ['0'..'9']) then
    exit;
  V := 0;
  ndot := 0;
  repeat
    case P^ of
      #0: break;
      '.': if (P[-1]='.') or (V>255) then
        exit else begin
        inc(ndot);
        V := 0;
      end;
      '0'..'9': V := (V*10)+ord(P^)-48;
      else exit;
    end;
    inc(P);
  until false;
  if (ndot=3) and (V<=255) and (P[-1]<>'.') then
    result := true;
end;

function IsValidEmail(P: PUTF8Char): boolean;
// Initial Author: Ernesto D'Spirito - UTF-8 version by AB
// http://www.howtodothings.com/computers/a1169-validating-email-addresses-in-delphi.html
const
  // Valid characters in an "atom"
  atom_chars: TSynAnsicharSet = [#33..#255] -
     ['(', ')', '<', '>', '@', ',', ';', ':', '\', '/', '"', '.', '[', ']', #127];
  // Valid characters in a "quoted-string"
  quoted_string_chars: TSynAnsicharSet = [#0..#255] - ['"', #13, '\'];
  // Valid characters in a subdomain
  letters_digits: TSynAnsicharSet = ['0'..'9', 'A'..'Z', 'a'..'z'];
type
  States = (STATE_BEGIN, STATE_ATOM, STATE_QTEXT, STATE_QCHAR,
    STATE_QUOTE, STATE_LOCAL_PERIOD, STATE_EXPECTING_SUBDOMAIN,
    STATE_SUBDOMAIN, STATE_HYPHEN);
var
  State: States;
  subdomains: integer;
  c: AnsiChar;
  ch: PtrInt;
begin
  State := STATE_BEGIN;
  subdomains := 1;
  if P<>nil then
  repeat
    ch := ord(P^);
    if ch and $80=0 then
      inc(P) else
      ch := GetHighUTF8UCS4(P);
    if (ch<=255) and (WinAnsiConvert.AnsiToWide[ch]<=255) then
      // convert into WinAnsi char
      c := AnsiChar(ch) else
      // invalid char
      c := #127;
    case State of
    STATE_BEGIN:
      if c in atom_chars then
        State := STATE_ATOM else
      if c='"' then
        State := STATE_QTEXT else
        break;
    STATE_ATOM:
      if c='@' then
        State := STATE_EXPECTING_SUBDOMAIN else
      if c='.' then
        State := STATE_LOCAL_PERIOD else
      if not (c in atom_chars) then
        break;
    STATE_QTEXT:
      if c='\' then
        State := STATE_QCHAR else
      if c='"' then
        State := STATE_QUOTE else
      if not (c in quoted_string_chars) then
        break;
    STATE_QCHAR:
      State := STATE_QTEXT;
    STATE_QUOTE:
      if c='@' then
        State := STATE_EXPECTING_SUBDOMAIN else
      if c='.' then
        State := STATE_LOCAL_PERIOD else
        break;
    STATE_LOCAL_PERIOD:
      if c in atom_chars then
        State := STATE_ATOM else
      if c='"' then
        State := STATE_QTEXT else
        break;
    STATE_EXPECTING_SUBDOMAIN:
      if c in letters_digits then
        State := STATE_SUBDOMAIN else
        break;
    STATE_SUBDOMAIN:
      if c='.' then begin
        inc(subdomains);
        State := STATE_EXPECTING_SUBDOMAIN
      end else
      if c='-' then
        State := STATE_HYPHEN else
      if not (c in letters_digits) then
        break;
    STATE_HYPHEN:
      if c in letters_digits then
        State := STATE_SUBDOMAIN else
      if c<>'-' then
        break;
    end;
    if P^=#0 then begin
      P := nil;
      break;
    end;
  until false;
  result := (State = STATE_SUBDOMAIN) and (subdomains >= 2);
end;

// code below adapted from ZMatchPattern.pas - http://www.zeoslib.sourceforge.net

procedure TMatch.MatchMain;
var RangeStart, RangeEnd: PtrInt;
    c: AnsiChar;
    flags: set of(Invert, MemberMatch);
begin
  while ((State = sNONE) and (P <= PMax)) do begin
    c := Upper[Pattern[P]];
    if T > TMax then begin
      if (c = '*') and (P + 1 > PMax) then
        State := sVALID else
        State := sABORT;
      exit;
    end else
    case c of
      '?': ;
      '*':
        MatchAfterStar;
      '[': begin
        inc(P);
        byte(flags) := 0;
        if Pattern[P] in ['!','^'] then begin
          include(flags, Invert);
          inc(P);
        end;
        if (Pattern[P] = ']') then begin
          State := sPATTERN;
          exit;
        end;
        c := Upper[Text[T]];
        while Pattern[P] <> ']' do begin
          RangeStart := P;
          RangeEnd := P;
          inc(P);
          if P > PMax then begin
            State := sPATTERN;
            exit;
          end;
          if Pattern[P] = '-' then begin
            inc(P);
            RangeEnd := P;
            if (P > PMax) or (Pattern[RangeEnd] = ']') then begin
              State := sPATTERN;
              exit;
            end;
            inc(P);
          end;
          if P > PMax then begin
            State := sPATTERN;
            exit;
          end;
          if RangeStart < RangeEnd then begin
            if (c >= Upper[Pattern[RangeStart]]) and (c <= Upper[Pattern[RangeEnd]]) then begin
              include(flags, MemberMatch);
              break;
            end;
          end
          else
            if (c >= Upper[Pattern[RangeEnd]]) and (c <= Upper[Pattern[RangeStart]]) then begin
              include(flags, MemberMatch);
              break;
            end;
        end;
        if ((Invert in flags) and (MemberMatch in flags)) or
           not ((Invert in flags) or (MemberMatch in flags)) then begin
          State := sRANGE;
          exit;
        end;
        if MemberMatch in flags then
          while (P <= PMax) and (Pattern[P] <> ']') do
            inc(P);
        if P > PMax then begin
          State := sPATTERN;
          exit;
        end;
      end;
    else
      if c <> Upper[Text[T]] then
        State := sLITERAL;
    end;
    inc(P);
    inc(T);
  end;
  if State = sNONE then
    if T <= TMax then
      State := sEND else
      State := sVALID;
end;

procedure TMatch.MatchAfterStar;
var retryT, retryP: PtrInt;
begin
  if (TMax = 1) or (P = PMax) then begin
    State := sVALID;
    exit;
  end else
  if (PMax = 0) or (TMax = 0) then begin
    State := sABORT;
    exit;
  end;
  while ((T <= TMax) and (P < PMax)) and (Pattern[P] in ['?', '*']) do begin
    if Pattern[P] = '?' then
      inc(T);
    inc(P);
  end;
  if T >= TMax then begin
    State := sABORT;
    exit;
  end else
  if P >= PMax then begin
    State := sVALID;
    exit;
  end;
  repeat
    if (Upper[Pattern[P]] = Upper[Text[T]]) or (Pattern[P] = '[') then begin
      retryT := T;
      retryP := P;
      MatchMain;
      if State = sVALID then
        break;
      State := sNONE; // retry until end of Text, (check below) or State valid
      T := retryT;
      P := retryP;
    end;
    inc(T);
    if (T > TMax) or (P > PMax) then begin
      State := sABORT;
      exit;
    end;
  until State <> sNONE;
end;

function SearchAny(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  aMatch.State := sNONE;
  aMatch.P := 0;
  aMatch.T := 0;
  aMatch.Text := aText;
  aMatch.TMax := aTextLen - 1;
  aMatch.MatchMain;
  result := aMatch.State = sVALID;
end;

// faster alternative (without recursion) for only * ? (but not [...])

function SearchNoRange(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
{$ifdef CPUX86}
var
  c: AnsiChar;
  pat, txt: PtrInt; // use local registers
begin
  aMatch.T := 0; // aMatch.P/T are used for retry positions after *
  aMatch.Text := aText;
  aMatch.TMax := aTextLen - 1;
  pat := 0;
  txt := 0;
  repeat
    if pat <= aMatch.PMax then begin
      c := aMatch.Pattern[pat];
      case c of
        '?':
          if txt <= aMatch.TMax then begin
            inc(pat);
            inc(txt);
            continue;
          end;
        '*': begin
          aMatch.P := pat;
          aMatch.T := txt + 1;
          inc(pat);
          continue;
        end;
        else if (txt <= aMatch.TMax) and (c = aMatch.Text[txt]) then begin
          inc(pat);
          inc(txt);
          continue;
        end;
      end;
    end
    else if txt > aMatch.TMax then
      break;
    txt := aMatch.T;
    if (txt > 0) and (txt <= aMatch.TMax + 1) then begin
      inc(aMatch.T);
      pat := aMatch.P+1;
      continue;
    end;
    result := false;
    exit;
  until false;
  result := true;
end;
{$else} // optimized for x86_64/ARM with more registers
var
  c: AnsiChar;
  pat, patend, txtend, txtretry, patretry: PUTF8Char;
label
  fin;
begin
  pat := pointer(aMatch.Pattern);
  if pat = nil then
    goto fin;
  patend := pat + aMatch.PMax;
  patretry := nil;
  txtend := aText + aTextLen - 1;
  txtretry := nil;
  repeat
    if pat <= patend then begin
      c := pat^;
      if c <> '*' then
        if c <> '?' then begin
          if (aText <= txtend) and (c = aText^) then begin
            inc(pat);
            inc(aText);
            continue;
          end;
        end
        else begin // '?'
          if aText <= txtend then begin
            inc(pat);
            inc(aText);
            continue;
          end;
        end
        else begin // '*'
          inc(pat);
          txtretry := aText + 1;
          patretry := pat;
          continue;
        end;
    end
    else if aText > txtend then
      break;
    if (PtrInt(txtretry)> 0) and (txtretry <= txtend + 1) then begin
      aText := txtretry;
      inc(txtretry);
      pat := patretry;
      continue;
    end;
fin:result := false;
    exit;
  until false;
  result := true;
end;
{$endif CPUX86}

function SearchNoRangeU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
var
  c: AnsiChar;
  pat, txt: PtrInt;
begin
  aMatch.T := 0;
  aMatch.Text := aText;
  aMatch.TMax := aTextLen - 1;
  pat := 0;
  txt := 0;
  repeat
    if pat <= aMatch.PMax then begin
      c := aMatch.Pattern[pat];
      case c of
        '?':
          if txt <= aMatch.TMax then begin
            inc(pat);
            inc(txt);
            continue;
          end;
        '*': begin
          aMatch.P := pat;
          aMatch.T := txt + 1;
          inc(pat);
          continue;
        end;
        else if (txt <= aMatch.TMax) and
           (aMatch.Upper[c] = aMatch.Upper[aMatch.Text[txt]]) then begin
          inc(pat);
          inc(txt);
          continue;
        end;
      end;
    end
    else if txt > aMatch.TMax then
      break;
    txt := aMatch.T;
    if (txt > 0) and (txt <= aMatch.TMax + 1) then begin
      inc(aMatch.T);
      pat := aMatch.P+1;
      continue;
    end;
    result := false;
    exit;
  until false;
  result := true;
end;

function SimpleContainsU(t, tend, p: PUTF8Char; pmax: PtrInt; up: PNormTable): boolean;
  {$ifdef HASINLINE}inline;{$endif}
// brute force case-insensitive search p[0..pmax] in t..tend-1
var first: AnsiChar;
    i: PtrInt;
label next;
begin
  first := up[p^];
  repeat
    if up[t^] <> first then begin
next: inc(t);
      if t < tend then
        continue else
        break;
    end;
    for i := 1 to pmax do
      if up[t[i]] <> up[p[i]] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

{$ifdef CPU64} // naive but very efficient code generation on FPC x86-64
function SimpleContains8(t, tend, p: PUTF8Char; pmax: PtrInt): boolean; inline;
label next;
var i, first: PtrInt;
begin
  first := PPtrInt(p)^;
  repeat
    if PPtrInt(t)^ <> first then begin
next: inc(t);
      if t < tend then
        continue else
        break;
    end;
    for i := 8 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;
{$endif CPU64}

function SimpleContains4(t, tend, p: PUTF8Char; pmax: PtrInt): boolean;
  {$ifdef HASINLINE}inline;{$endif}
label next;
var i: PtrInt;
{$ifdef CPUX86} // circumvent lack of registers for this CPU
begin
  repeat
    if PCardinal(t)^ <> PCardinal(p)^ then begin
{$else}
    first: cardinal;
begin
  first := PCardinal(p)^;
  repeat
    if PCardinal(t)^ <> first then begin
{$endif}
next: inc(t);
      if t < tend then
        continue else
        break;
    end;
    for i := 4 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

function SimpleContains1(t, tend, p: PUTF8Char; pmax: PtrInt): boolean;
  {$ifdef HASINLINE}inline;{$endif}
label next;
var i: PtrInt;
{$ifdef CPUX86}
begin
  repeat
    if t^ <> p^ then begin
{$else}
    first: AnsiChar;
begin
  first := p^;
  repeat
    if t^ <> first then begin
{$endif}
next: inc(t);
      if t < tend then
        continue else
        break;
    end;
    for i := 1 to pmax do
      if t[i] <> p[i] then
       goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

function CompareMemU(P1, P2: PUTF8Char; len: PtrInt; U: PNormTable): Boolean;
  {$ifdef FPC}inline;{$endif}
begin // here we know that len>0
  result := false;
  repeat
    dec(len);
    if U[P1[len]] <> U[P2[len]] then
      exit;
  until len = 0;
  result := true;
end;

function SearchVoid(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := aTextLen = 0;
end;

function SearchNoPattern(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax + 1 = aTextlen) and CompareMem(aText, aMatch.Pattern, aTextLen);
end;

function SearchNoPatternU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax + 1 = aTextlen) and CompareMemU(aText, aMatch.Pattern, aTextLen, aMatch.Upper);
end;

function SearchContainsValid(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := true;
end;

function SearchContainsU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContainsU(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax, aMatch.Upper)
  else
    result := false;
end;

function SearchContains1(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContains1(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax)
  else
    result := false;
end;

function SearchContains4(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContains4(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax)
  else
    result := false;
end;

{$ifdef CPU64}
function SearchContains8(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin // optimized e.g. to search an IP address as '*12.34.56.78*' in logs
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContains8(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax)
  else
    result := false;
end;
{$endif CPU64}

function SearchStartWith(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax < aTextlen) and CompareMem(aText, aMatch.Pattern, aMatch.PMax + 1);
end;

function SearchStartWithU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax < aTextlen) and CompareMemU(aText, aMatch.Pattern, aMatch.PMax + 1, aMatch.Upper);
end;

function SearchEndWith(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  result := (aTextlen >= 0) and CompareMem(aText + aTextLen, aMatch.Pattern, aMatch.PMax);
end;

function SearchEndWithU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  result := (aTextlen >= 0) and CompareMemU(aText + aTextLen, aMatch.Pattern, aMatch.PMax, aMatch.Upper);
end;

procedure TMatch.Prepare(const aPattern: RawUTF8; aCaseInsensitive, aReuse: boolean);
begin
  Prepare(pointer(aPattern), length(aPattern), aCaseInsensitive, aReuse);
end;

procedure TMatch.Prepare(aPattern: PUTF8Char; aPatternLen: integer;
  aCaseInsensitive, aReuse: boolean);
const SPECIALS: PUTF8Char = '*?[';
begin
  Pattern := aPattern;
  PMax := aPatternLen - 1; // search in Pattern[0..PMax]
  if Pattern = nil then begin
    Search := SearchVoid;
    exit;
  end;
  if aCaseInsensitive and not IsCaseSensitive(aPattern,aPatternLen) then
    aCaseInsensitive := false; // don't slow down e.g. number or IP search
  if aCaseInsensitive then
    Upper := @NormToUpperAnsi7 else
    Upper := @NormToNorm;
  Search := nil;
  if aReuse then
    if strcspn(Pattern, SPECIALS) > PMax then
      if aCaseInsensitive then
        Search := SearchNoPatternU
      else
        Search := SearchNoPattern
    else if PMax > 0 then begin
      if Pattern[PMax] = '*' then begin
        if strcspn(Pattern + 1, SPECIALS) = PMax - 1 then
          case Pattern[0] of
            '*': begin // *something*
              inc(Pattern);
              dec(PMax, 2); // trim trailing and ending *
              if PMax < 0 then
                Search := SearchContainsValid
              else if aCaseInsensitive then
                Search := SearchContainsU
              {$ifdef CPU64}
              else if PMax >= 7 then
                Search := SearchContains8
              {$endif}
              else if PMax >= 3 then
                Search := SearchContains4
              else
                Search := SearchContains1;
            end;
            '?':
              if aCaseInsensitive then
                Search := SearchNoRangeU
              else
                Search := SearchNoRange;
            '[':
              Search := SearchAny;
            else begin
              dec(PMax); // trim trailing *
              if aCaseInsensitive then
                Search := SearchStartWithU
              else
                Search := SearchStartWith;
            end;
          end;
      end
      else if (Pattern[0] = '*') and (strcspn(Pattern + 1, SPECIALS) >= PMax) then begin
        inc(Pattern); // jump leading *
        if aCaseInsensitive then
          Search := SearchEndWithU
        else
          Search := SearchEndWith;
      end;
    end else
      if Pattern[0] in ['*','?'] then
        Search := SearchContainsValid;
  if not Assigned(Search) then begin
    aPattern := PosChar(Pattern, '[');
    if (aPattern = nil) or (aPattern - Pattern > PMax) then
      if aCaseInsensitive then
        Search := SearchNoRangeU
      else
        Search := SearchNoRange
    else
      Search := SearchAny;
  end;
end;

type // Holub and Durian (2005) SBNDM2 algorithm
  // see http://www.cri.haifa.ac.il/events/2005/string/presentations/Holub.pdf
  TSBNDMQ2Mask = array[AnsiChar] of cardinal;
  PSBNDMQ2Mask = ^TSBNDMQ2Mask;

function SearchSBNDMQ2ComputeMask(const Pattern: RawUTF8; u: PNormTable): RawByteString;
var
  i: PtrInt;
  p: PAnsiChar absolute Pattern;
  m: PSBNDMQ2Mask absolute result;
  c: PCardinal;
begin
  SetLength(result, SizeOf(m^));
  {$ifdef FPC}FillChar{$else}FillCharFast{$endif}(m^, SizeOf(m^), 0);
  for i := 0 to length(Pattern) - 1 do begin
    c := @m^[u[p[i]]]; // for FPC code generation
    c^ := c^ or (1 shl i);
  end;
end;

function SearchSBNDMQ2(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
var
  mask: PSBNDMQ2Mask;
  max, i, j: PtrInt;
  state: cardinal;
begin
  mask := pointer(aMatch^.Pattern); // was filled by SearchSBNDMQ2ComputeMask()
  max := aMatch^.PMax;
  i := max - 1;
  dec(aTextLen);
  if i < aTextLen then begin
    repeat
      state := mask[aText[i+1]] shr 1; // in two steps for better FPC codegen
      state := state and mask[aText[i]];
      if state = 0 then begin
        inc(i, max); // fast skip
        if i >= aTextLen then
          break;
        continue;
      end;
      j := i - max;
      repeat
        dec(i);
        if i < 0 then
          break;
        state := (state shr 1) and mask[aText[i]];
      until state = 0;
      if i = j then begin
        result := true;
        exit;
      end;
      inc(i, max);
      if i >= aTextLen then
        break;
    until false;
  end;
  result := false;
end;

function SearchSBNDMQ2U(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
var
  u: PNormTable;
  mask: PSBNDMQ2Mask;
  max, i, j: PtrInt;
  state: cardinal;
begin
  mask := pointer(aMatch^.Pattern);
  max := aMatch^.PMax;
  u := aMatch^.Upper;
  i := max - 1;
  dec(aTextLen);
  if i < aTextLen then begin
    repeat
      state := mask[u[aText[i+1]]] shr 1;
      state := state and mask[u[aText[i]]];
      if state = 0 then begin
        inc(i, max);
        if i >= aTextLen then
          break;
        continue;
      end;
      j := i - max;
      repeat
        dec(i);
        if i < 0 then
          break;
        state := (state shr 1) and mask[u[aText[i]]];
      until state = 0;
      if i = j then begin
        result := true;
        exit;
      end;
      inc(i, max);
      if i >= aTextLen then
        break;
    until false;
  end;
  result := false;
end;

procedure TMatch.PrepareContains(var aPattern: RawUTF8;
  aCaseInsensitive: boolean);
begin
  PMax := length(aPattern) - 1;
  if aCaseInsensitive and not IsCaseSensitive(pointer(aPattern), PMax + 1) then
    aCaseInsensitive := false;
  if aCaseInsensitive then
    Upper := @NormToUpperAnsi7
  else
    Upper := @NormToNorm;
  if PMax < 0 then
    Search := SearchContainsValid
  else if PMax > 30 then
    if aCaseInsensitive then
      Search := SearchContainsU
    else
      Search := {$ifdef CPU64}SearchContains8{$else}SearchContains4{$endif}
  else if PMax >= 1 then begin // len in [2..31] = PMax in [1..30]
    aPattern := SearchSBNDMQ2ComputeMask(aPattern, Upper); // lookup table
    if aCaseInsensitive then
      Search := SearchSBNDMQ2U
    else
      Search := SearchSBNDMQ2;
  end
  else if aCaseInsensitive then
    Search := SearchContainsU
  else
    Search := SearchContains1; // todo: use IndexByte() on FPC?
  Pattern := pointer(aPattern);
end;

procedure TMatch.PrepareRaw(aPattern: PUTF8Char; aPatternLen: integer;
  aSearch: TMatchSearchFunction);
begin
  Pattern := aPattern;
  PMax := aPatternLen - 1; // search in Pattern[0..PMax]
  Search := aSearch;
end;

function TMatch.Match(const aText: RawUTF8): boolean;
begin
  if aText <> '' then
    result := Search(@self, pointer(aText), length(aText))
  else
    result := PMax < 0;
end;

function TMatch.Match(aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  if (aText <> nil) and (aTextLen > 0) then
    result := Search(@self, aText, aTextLen)
  else
    result := PMax < 0;
end;

function TMatch.MatchThreadSafe(const aText: RawUTF8): boolean;
var local: TMatch; // thread-safe with no lock!
begin
  local := self;
  if aText <> '' then
    result := local.Search(@local, pointer(aText), length(aText))
  else
    result := local.PMax < 0;
end;

function TMatch.MatchString(const aText: string): boolean;
var
  local: TMatch; // thread-safe with no lock!
  temp: TSynTempBuffer;
  len: integer;
begin
  if aText = '' then begin
    result := PMax < 0;
    exit;
  end;
  len := length(aText);
  temp.Init(len * 3);
  {$ifdef UNICODE}
  len := RawUnicodeToUtf8(temp.buf, temp.len + 1, pointer(aText), len, [ccfNoTrailingZero]);
  {$else}
  len := CurrentAnsiConvert.AnsiBufferToUTF8(temp.buf, pointer(aText), len) - temp.buf;
  {$endif}
  local := self;
  result := local.Search(@local, temp.buf, len);
  temp.Done;
end;

function TMatch.Equals(const aAnother{$ifndef DELPHI5OROLDER}: TMatch{$endif}): boolean;
begin
  result := (PMax = TMatch(aAnother).PMax) and (Upper = TMatch(aAnother).Upper) and
    CompareMem(Pattern, TMatch(aAnother).Pattern, PMax + 1);
end;

function TMatch.PatternLength: integer;
begin
  result := PMax + 1;
end;

function TMatch.PatternText: PUTF8Char;
begin
  result := Pattern;
end;

function IsMatch(const Pattern, Text: RawUTF8; CaseInsensitive: boolean): boolean;
var match: TMatch;
begin
  match.Prepare(Pattern, CaseInsensitive, {reuse=}false);
  result := match.Match(Text);
end;

function IsMatchString(const Pattern, Text: string; CaseInsensitive: boolean=false): boolean;
var match: TMatch;
begin
  match.Prepare(StringToUTF8(Pattern), CaseInsensitive, {reuse=}false);
  result := match.Match(StringToUTF8(Text));
end;

function SetMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  out Match: TMatchDynArray): integer;
var P, S: PUTF8Char;
begin
  result := 0;
  P := pointer(CSVPattern);
  if P <> nil then
    repeat
      S := P;
      while not (P^ in [#0, ',']) do
        inc(P);
      if P <> S then begin
        SetLength(Match, result + 1);
        Match[result].Prepare(S, P - S, CaseInsensitive, {reuse=}true);
        inc(result);
      end;
      if P^ = #0 then
        break;
      inc(P);
    until false;
end;

function SetMatchs(CSVPattern: PUTF8Char; CaseInsensitive: boolean;
  Match: PMatch; MatchMax: integer): integer;
var S: PUTF8Char;
begin
  result := 0;
  if (CSVPattern <> nil) and (MatchMax >= 0) then
    repeat
      S := CSVPattern;
      while not (CSVPattern^ in [#0, ',']) do
        inc(CSVPattern);
      if CSVPattern <> S then begin
        Match^.Prepare(S, CSVPattern - S, CaseInsensitive, {reuse=}true);
        inc(result);
        if result > MatchMax then
          break;
        inc(Match);
      end;
      if CSVPattern^ = #0 then
        break;
      inc(CSVPattern);
    until false;
end;

function MatchExists(const One: TMatch; const Several: TMatchDynArray): boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to high(Several) do
    if Several[i].Equals(One) then
      exit;
  result := false;
end;

function MatchAdd(const One: TMatch; var Several: TMatchDynArray): boolean;
var
  n: integer;
begin
  result := not MatchExists(One, Several);
  if result then begin
    n := length(Several);
    SetLength(Several, n + 1);
    Several[n] := One;
  end;
end;

function MatchAny(const Match: TMatchDynArray; const Text: RawUTF8): boolean;
var
  m: PMatch;
  i: integer;
begin
  result := true;
  if Match = nil then
    exit;
  m := pointer(Match);
  for i := 1 to length(Match) do
    if m^.Match(Text) then
      exit
    else
      inc(m);
  result := false;
end;

procedure FilterMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  var Values: TRawUTF8DynArray);
var
  match: TMatchDynArray;
  m, n, i: integer;
begin
  if SetMatchs(CSVPattern, CaseInsensitive, match) = 0 then
    exit;
  n := 0;
  for i := 0 to high(Values) do
    for m := 0 to high(match) do
      if match[m].Match(Values[i]) then begin
        if i <> n then
          Values[n] := Values[i];
        inc(n);
        break;
      end;
  if n <> length(Values) then
    SetLength(Values, n);
end;


{ TMatchs }

constructor TMatchs.Create(const aPatterns: TRawUTF8DynArray; CaseInsensitive: Boolean);
begin
  inherited Create;
  Subscribe(aPatterns, CaseInsensitive);
end;

function TMatchs.Match(const aText: RawUTF8): integer;
begin
  result := Match(pointer(aText), length(aText));
end;

function TMatchs.Match(aText: PUTF8Char; aLen: integer): integer;
var
  one: ^TMatchStore;
  local: TMatch; // thread-safe with no lock!
begin
  if (self = nil) or (fMatch = nil) then
    result := -1 // no filter by name -> allow e.g. to process everything
  else begin
    one := pointer(fMatch);
    if aLen <> 0 then begin
      for result := 0 to fMatchCount - 1 do begin
        local := one^.Pattern;
        if local.Search(@local, aText, aLen) then
          exit;
        inc(one);
      end;
    end
    else
      for result := 0 to fMatchCount - 1 do
        if one^.Pattern.PMax < 0 then
          exit
        else
          inc(one);
    result := -2;
  end;
end;

function TMatchs.MatchString(const aText: string): integer;
var
  temp: TSynTempBuffer;
  len: integer;
begin
  len := length(aText);
  temp.Init(len * 3);
  {$ifdef UNICODE}
  len := RawUnicodeToUtf8(temp.buf, temp.len + 1, pointer(aText), len, [ccfNoTrailingZero]);
  {$else}
  len := CurrentAnsiConvert.AnsiBufferToUTF8(temp.buf, pointer(aText), len, true) - temp.buf;
  {$endif}
  result := Match(temp.buf, len);
  temp.Done;
end;

procedure TMatchs.Subscribe(const aPatternsCSV: RawUTF8; CaseInsensitive: Boolean);
var
  patterns: TRawUTF8DynArray;
begin
  CSVToRawUTF8DynArray(pointer(aPatternsCSV), patterns);
  Subscribe(patterns, CaseInsensitive);
end;

procedure TMatchs.Subscribe(const aPatterns: TRawUTF8DynArray; CaseInsensitive: Boolean);
var
  i, j, m, n: integer;
  found: ^TMatchStore;
  pat: PRawUTF8;
begin
  m := length(aPatterns);
  if m = 0 then
    exit;
  n := fMatchCount;
  SetLength(fMatch, n + m);
  pat := pointer(aPatterns);
  for i := 1 to m do begin
    found := pointer(fMatch);
    for j := 1 to n do
      if StrComp(pointer(found^.PatternInstance), pointer(pat^)) = 0 then begin
        found := nil;
        break;
      end
      else
        inc(found);
    if found <> nil then
      with fMatch[n] do begin
        PatternInstance := pat^; // avoid GPF if aPatterns[] is released
        Pattern.Prepare(PatternInstance, CaseInsensitive, {reuse=}true);
        inc(n);
      end;
    inc(pat);
  end;
  fMatchCount := n;
  if n <> length(fMatch) then
    SetLength(fMatch, n);
end;


{ TSynFilterOrValidate }

constructor TSynFilterOrValidate.Create(const aParameters: RawUTF8);
begin
  inherited Create;
  SetParameters(aParameters); // should parse the JSON-encoded parameters
end;

constructor TSynFilterOrValidate.CreateUTF8(const Format: RawUTF8;
  const Args, Params: array of const);
begin
  Create(FormatUTF8(Format,Args,Params,true));
end;

procedure TSynFilterOrValidate.SetParameters(const value: RawUTF8);
begin
  fParameters := value;
end;

function TSynFilterOrValidate.AddOnce(var aObjArray: TSynFilterOrValidateObjArray;
  aFreeIfAlreadyThere: boolean): TSynFilterOrValidate;
var i: integer;
begin
  if self<>nil then begin
    for i := 0 to length(aObjArray)-1 do
      if (PPointer(aObjArray[i])^=PPointer(self)^) and
         (aObjArray[i].fParameters=fParameters) then begin
        if aFreeIfAlreadyThere then
          Free;
        result := aObjArray[i];
        exit;
      end;
    ObjArrayAdd(aObjArray,self);
  end;
  result := self;
end;


{ TSynFilterUpperCase }

procedure TSynFilterUpperCase.Process(aFieldIndex: integer; var value: RawUTF8);
begin
  value := SynCommons.UpperCase(value);
end;


{ TSynFilterUpperCaseU }

procedure TSynFilterUpperCaseU.Process(aFieldIndex: integer; var value: RawUTF8);
begin
  value := UpperCaseU(value);
end;


{ TSynFilterLowerCase }

procedure TSynFilterLowerCase.Process(aFieldIndex: integer; var value: RawUTF8);
begin
  value := LowerCase(value);
end;


{ TSynFilterLowerCaseU }

procedure TSynFilterLowerCaseU.Process(aFieldIndex: integer; var value: RawUTF8);
begin
  value := LowerCaseU(value);
end;


{ TSynFilterTrim }

procedure TSynFilterTrim.Process(aFieldIndex: integer; var value: RawUTF8);
begin
  value := Trim(value);
end;


{ TSynFilterTruncate}

procedure TSynFilterTruncate.SetParameters(const value: RawUTF8);
var V: array[0..1] of TValuePUTF8Char;
    tmp: TSynTempBuffer;
begin
  tmp.Init(value);
  JSONDecode(tmp.buf,['MaxLength','UTF8Length'],@V);
  fMaxLength := GetCardinalDef(V[0].Value,0);
  fUTF8Length := V[1].Idem('1') or V[1].Idem('true');
  tmp.Done;
end;

procedure TSynFilterTruncate.Process(aFieldIndex: integer; var value: RawUTF8);
begin
  if fMaxLength-1<cardinal(maxInt) then
    if fUTF8Length then
      Utf8TruncateToLength(value,fMaxLength) else
      Utf8TruncateToUnicodeLength(value,fMaxLength);
end;


{ TSynValidateIPAddress }

function TSynValidateIPAddress.Process(aFieldIndex: integer; const value: RawUTF8;
  var ErrorMsg: string): boolean;
begin
  result := IsValidIP4Address(pointer(value));
  if not result then
    ErrorMsg := Format(sInvalidIPAddress,[UTF8ToString(value)]);
end;


{ TSynValidateEmail }

function TSynValidateEmail.Process(aFieldIndex: integer; const value: RawUTF8;
  var ErrorMsg: string): boolean;
var TLD,DOM: RawUTF8;
    i: integer;
const TopLevelTLD: array[0..19] of PUTF8Char = (
  // see http://en.wikipedia.org/wiki/List_of_Internet_top-level_domains
  'aero','asia','biz','cat','com','coop','edu','gov','info','int','jobs',
  'mil','mobi','museum','name','net','org','pro','tel','travel'); // no xxx !
begin
  if IsValidEmail(pointer(value)) then
  repeat
    DOM := lowercase(copy(value,PosEx('@',value)+1,100));
    if length(DOM)>63 then
      break; // exceeded 63-character limit of a DNS name
    if (ForbiddenDomains<>'') and (FindCSVIndex(pointer(ForbiddenDomains),DOM)>=0) then
      break;
    i := length(value);
    while (i>0) and (value[i]<>'.') do dec(i);
    TLD := lowercase(copy(value,i+1,100));
    if (AllowedTLD<>'') and (FindCSVIndex(pointer(AllowedTLD),TLD)<0) then
      break;
    if (ForbiddenTLD<>'') and (FindCSVIndex(pointer(ForbiddenTLD),TLD)>=0) then
      break;
    if not fAnyTLD then
      if FastFindPUTF8CharSorted(@TopLevelTLD,high(TopLevelTLD),pointer(TLD))<0 then
        if length(TLD)<>2 then
          break; // assume a two chars string is a ISO 3166-1 alpha-2 code
    result := true;
    exit;
  until true;
  ErrorMsg := Format(sInvalidEmailAddress,[UTF8ToString(value)]);
  result := false;
end;

procedure TSynValidateEmail.SetParameters(const value: RawUTF8);
var V: array[0..3] of TValuePUTF8Char;
    tmp: TSynTempBuffer;
begin
  inherited;
  tmp.Init(value);
  JSONDecode(tmp.buf,['AllowedTLD','ForbiddenTLD','ForbiddenDomains','AnyTLD'],@V);
  LowerCaseCopy(V[0].Value,V[0].ValueLen,fAllowedTLD);
  LowerCaseCopy(V[1].Value,V[1].ValueLen,fForbiddenTLD);
  LowerCaseCopy(V[2].Value,V[2].ValueLen,fForbiddenDomains);
  AnyTLD := V[3].Idem('1') or V[3].Idem('true');
  tmp.Done;
end;


{ TSynValidatePattern }

procedure TSynValidatePattern.SetParameters(const Value: RawUTF8);
begin
  inherited SetParameters(Value);
  fMatch.Prepare(Value, ClassType=TSynValidatePatternI, {reuse=}true);
end;

function TSynValidatePattern.Process(aFieldIndex: integer; const value: RawUTF8;
  var ErrorMsg: string): boolean;
  procedure SetErrorMsg;
  begin
    ErrorMsg := Format(sInvalidPattern,[UTF8ToString(value)]);
  end;
begin
  result := fMatch.Match(value);
  if not result then
    SetErrorMsg;
end;


{ TSynValidateNonVoidText }

function Character01n(n: integer): string;
begin
  if n<0 then
    n := 0 else
  if n>1 then
    n := 2;
  result := GetCSVItemString(pointer(string(sCharacter01n)),n);
end;

procedure InvalidTextLengthMin(min: integer; var result: string);
begin
  result := Format(sInvalidTextLengthMin,[min,Character01n(min)]);
end;

function TSynValidateNonVoidText.Process(aFieldIndex: integer; const value: RawUTF8;
  var ErrorMsg: string): boolean;
begin
  if value='' then begin
    InvalidTextLengthMin(1,ErrorMsg);
    result := false;
  end else
    result := true;
end;


{ TSynValidateText }

procedure TSynValidateText.SetErrorMsg(fPropsIndex, InvalidTextIndex,
  MainIndex: integer; var result: string);
var P: PChar;
begin
  P := pointer(string(sInvalidTextChar));
  result := GetCSVItemString(P,MainIndex);
  if fPropsIndex>0 then
    result := Format(result,
      [fProps[fPropsIndex],GetCSVItemString(P,InvalidTextIndex),
       Character01n(fProps[fPropsIndex])]);
end;

function TSynValidateText.Process(aFieldIndex: integer; const value: RawUTF8;
  var ErrorMsg: string): boolean;
var i, L: cardinal;
    Min: array[2..7] of cardinal;
begin
  result := false;
  if fUTF8Length then
    L := length(value) else
    L := Utf8ToUnicodeLength(pointer(value));
  if L<MinLength then
    InvalidTextLengthMin(MinLength,ErrorMsg) else
  if L>MaxLength then
    ErrorMsg := Format(sInvalidTextLengthMax,[MaxLength,Character01n(MaxLength)]) else begin
    {$ifdef FPC}FillChar{$else}FillCharFast{$endif}(Min,SizeOf(Min),0);
    L := length(value);
    for i := 1 to L do
    case value[i] of
      ' ':
        inc(Min[7]);
      'a'..'z': begin
        inc(Min[2]);
        inc(Min[5]);
      end;
      'A'..'Z': begin
        inc(Min[2]);
        inc(Min[6]);
      end;
      '0'..'9':
        inc(Min[3]);
      '_','!',';','.',',','/',':','?','%','$','=','"','#','@','(',')','{','}',
      '+','''','-','*':
        inc(Min[4]);
    end;
    for i := 2 to 7 do
      if Min[i]<fProps[i] then begin
        SetErrorMsg(i,i,0,ErrorMsg);
        exit;
      end else
      if Min[i]>fProps[i+8] then begin
        SetErrorMsg(i+8,i,1,ErrorMsg);
        exit;
      end;
    if value<>'' then begin
      if MaxLeftTrimCount<cardinal(maxInt) then begin
        // if MaxLeftTrimCount is set, check against Value
        i := 0;
        while (i<L) and (value[i+1]=' ') do inc(i);
        if i>MaxLeftTrimCount then begin
          SetErrorMsg(0,0,8,ErrorMsg);
          exit;
        end;
      end;
      if MaxRightTrimCount<cardinal(maxInt) then begin
        // if MaxRightTrimCount is set, check against Value
        i := 0;
        while (i<L) and (value[L-i]=' ') do dec(i);
        if i>MaxRightTrimCount then begin
          SetErrorMsg(0,0,9,ErrorMsg);
          exit;
        end;
      end;
    end;
    result := true;
  end;
end;

procedure TSynValidateText.SetParameters(const value: RawUTF8);
var V: array[0..high(TSynValidateTextProps)+1] of TValuePUTF8Char;
    i: integer;
    tmp: TSynTempBuffer;
const DEFAULT: TSynValidateTextProps = (
  1,maxInt,0,0,0,0,0,0,maxInt,maxInt,maxInt,maxInt,maxInt,maxInt,maxInt,maxInt);
begin
  if (MinLength=0) and (MaxLength=0) then  // if not previously set
    fProps := DEFAULT;
  inherited SetParameters(value);
  if value='' then
    exit;
  tmp.Init(value);
  try
    JSONDecode(tmp.buf,['MinLength','MaxLength',
      'MinAlphaCount','MinDigitCount','MinPunctCount',
      'MinLowerCount','MinUpperCount','MinSpaceCount',
      'MaxLeftTrimCount','MaxRightTrimCount',
      'MaxAlphaCount','MaxDigitCount','MaxPunctCount',
      'MaxLowerCount','MaxUpperCount','MaxSpaceCount',
      'UTF8Length'],@V);
    for i := 0 to high(fProps) do
      fProps[i] := GetCardinalDef(V[i].Value,fProps[i]);
    with V[high(V)] do
      fUTF8Length := Idem('1') or Idem('true');
  finally
    tmp.Done;
  end;
end;


{ TSynValidatePassWord }

procedure TSynValidatePassWord.SetParameters(const value: RawUTF8);
const DEFAULT: TSynValidateTextProps = (
  5,20,1,1,1,1,1,0,maxInt,maxInt,maxInt,maxInt,maxInt,maxInt,maxInt,0);
begin
  // set default values for validating a strong password
  fProps := DEFAULT;
  // read custom parameters
  inherited;
end;


{ ************ low-level buffer processing functions************************* }

{ TSynBloomFilter }

const
  BLOOM_VERSION = 0;
  BLOOM_MAXHASH = 32; // only 7 is needed for 1% false positive ratio

constructor TSynBloomFilter.Create(aSize: integer; aFalsePositivePercent: double);
const LN2 = 0.69314718056;
begin
  inherited Create;
  if aSize < 0 then
    fSize := 1000 else
    fSize := aSize;
  if aFalsePositivePercent<=0 then
    fFalsePositivePercent := 1 else
  if aFalsePositivePercent>100 then
    fFalsePositivePercent := 100 else
    fFalsePositivePercent := aFalsePositivePercent;
  // see http://stackoverflow.com/a/22467497
  fBits := Round(-ln(fFalsePositivePercent/100)*aSize/(LN2*LN2));
  fHashFunctions := Round(fBits/fSize*LN2);
  if fHashFunctions=0 then
    fHashFunctions := 1 else
  if fHashFunctions>BLOOM_MAXHASH then
    fHashFunctions := BLOOM_MAXHASH;
  Reset;
end;

constructor TSynBloomFilter.Create(const aSaved: RawByteString; aMagic: cardinal);
begin
  inherited Create;
  if not LoadFrom(aSaved,aMagic) then
    raise ESynException.CreateUTF8('%.Create with invalid aSaved content',[self]);
end;

procedure TSynBloomFilter.Insert(const aValue: RawByteString);
begin
  Insert(pointer(aValue),length(aValue));
end;

procedure TSynBloomFilter.Insert(aValue: pointer; aValueLen: integer);
var h: integer;
    h1,h2: cardinal; // https://goo.gl/Pls5wi
begin
  if (self=nil) or (aValueLen<=0) or (fBits=0) then
    exit;
  h1 := crc32c(0,aValue,aValueLen);
  if fHashFunctions=1 then
    h2 := 0 else
    h2 := crc32c(h1,aValue,aValueLen);
  Safe.Lock;
  try
    for h := 0 to fHashFunctions-1 do begin
      SetBitPtr(pointer(fStore),h1 mod fBits);
      inc(h1,h2);
    end;
    inc(fInserted);
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilter.GetInserted: cardinal;
begin
  Safe.Lock;
  try
    result := fInserted; // Delphi 5 does not support LockedInt64[]
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilter.MayExist(const aValue: RawByteString): boolean;
begin
  result := MayExist(pointer(aValue),length(aValue));
end;

function TSynBloomFilter.MayExist(aValue: pointer; aValueLen: integer): boolean;
var h: integer;
    h1,h2: cardinal; // https://goo.gl/Pls5wi
begin
  result := false;
  if (self=nil) or (aValueLen<=0) or (fBits=0) then
    exit;
  h1 := crc32c(0,aValue,aValueLen);
  if fHashFunctions=1 then
    h2 := 0 else
    h2 := crc32c(h1,aValue,aValueLen);
  Safe.Lock;
  try
    for h := 0 to fHashFunctions-1 do
      if GetBitPtr(pointer(fStore),h1 mod fBits) then
        inc(h1,h2) else
        exit;
  finally
    Safe.UnLock;
  end;
  result := true;
end;

procedure TSynBloomFilter.Reset;
begin
  Safe.Lock;
  try
    if fStore='' then
      SetLength(fStore,(fBits shr 3)+1);
    FillcharFast(pointer(fStore)^,length(fStore),0);
    fInserted := 0;
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilter.SaveTo(aMagic: cardinal): RawByteString;
var W: TFileBufferWriter;
    BufLen: integer;
    temp: array[word] of byte;
begin
  BufLen := length(fStore)+100;
  if BufLen<=SizeOf(temp) then
    W := TFileBufferWriter.Create(TRawByteStringStream,@temp,SizeOf(temp)) else
    W := TFileBufferWriter.Create(TRawByteStringStream,BufLen);
  try
    SaveTo(W,aMagic);
    W.Flush;
    result := TRawByteStringStream(W.Stream).DataString;
  finally
    W.Free;
  end;
end;

procedure TSynBloomFilter.SaveTo(aDest: TFileBufferWriter; aMagic: cardinal=$B1003F11);
begin
  aDest.Write4(aMagic);
  aDest.Write1(BLOOM_VERSION);
  Safe.Lock;
  try
    aDest.Write8(fFalsePositivePercent);
    aDest.Write4(fSize);
    aDest.Write4(fBits);
    aDest.Write1(fHashFunctions);
    aDest.Write4(fInserted);
    ZeroCompress(pointer(fStore),Length(fStore),aDest);
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilter.LoadFrom(const aSaved: RawByteString; aMagic: cardinal): boolean;
begin
  result := LoadFrom(pointer(aSaved),length(aSaved));
end;

function TSynBloomFilter.LoadFrom(P: PByte; PLen: integer; aMagic: cardinal): boolean;
var start: PByte;
    version: integer;
begin
  result := false;
  start := P;
  if (P=nil) or (PLen<32) or (PCardinal(P)^<>aMagic) then
    exit;
  inc(P,4);
  version := P^; inc(P);
  if version>BLOOM_VERSION then
    exit;
  Safe.Lock;
  try
    fFalsePositivePercent := PDouble(P)^; inc(P,8);
    if (fFalsePositivePercent<=0) or (fFalsePositivePercent>100) then
      exit;
    fSize := PCardinal(P)^; inc(P,4);
    fBits := PCardinal(P)^; inc(P,4);
    if fBits<fSize then
      exit;
    fHashFunctions := P^; inc(P);
    if fHashFunctions-1>=BLOOM_MAXHASH then
      exit;
    Reset;
    fInserted := PCardinal(P)^; inc(P,4);
    ZeroDecompress(P,PLen-(PAnsiChar(P)-PAnsiChar(start)),fStore);
    result := length(fStore)=integer(fBits shr 3)+1;
  finally
    Safe.UnLock;
  end;
end;


{ TSynBloomFilterDiff }

type
  TBloomDiffHeader = packed record
    kind: (bdDiff,bdFull,bdUpToDate);
    size: cardinal;
    inserted: cardinal;
    revision: Int64;
    crc: cardinal;
  end;

procedure TSynBloomFilterDiff.Insert(aValue: pointer; aValueLen: integer);
begin
  Safe.Lock;
  try
    inherited Insert(aValue,aValueLen);
    inc(fRevision);
    inc(fSnapshotInsertCount);
  finally
    Safe.UnLock;
  end;
end;

procedure TSynBloomFilterDiff.Reset;
begin
  Safe.Lock;
  try
    inherited Reset;
    fSnapshotAfterInsertCount := fSize shr 5;
    fSnapShotAfterMinutes := 30;
    fSnapshotTimestamp := 0;
    fSnapshotInsertCount := 0;
    fRevision := UnixTimeUTC shl 31;
    fKnownRevision := 0;
    fKnownStore := '';
  finally
    Safe.UnLock;
  end;
end;

procedure TSynBloomFilterDiff.DiffSnapshot;
begin
  Safe.Lock;
  try
    fKnownRevision := fRevision;
    fSnapshotInsertCount := 0;
    SetString(fKnownStore,PAnsiChar(pointer(fStore)),length(fStore));
    if fSnapShotAfterMinutes=0 then
      fSnapshotTimestamp := 0 else
      fSnapshotTimestamp := GetTickCount64+fSnapShotAfterMinutes*60000;
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilterDiff.SaveToDiff(const aKnownRevision: Int64): RawByteString;
var head: TBloomDiffHeader;
    W: TFileBufferWriter;
    temp: array[word] of byte;
begin
  Safe.Lock;
  try
    if aKnownRevision=fRevision then
      head.kind := bdUpToDate else
    if (fKnownRevision=0) or
       (fSnapshotInsertCount>fSnapshotAfterInsertCount) or
       ((fSnapshotInsertCount>0) and (fSnapshotTimestamp<>0) and
        (GetTickCount64>fSnapshotTimestamp)) then begin
      DiffSnapshot;
      head.kind := bdFull;
    end else
    if (aKnownRevision<fKnownRevision) or (aKnownRevision>fRevision) then
      head.kind := bdFull else
      head.kind := bdDiff;
    head.size := length(fStore);
    head.inserted := fInserted;
    head.revision := fRevision;
    head.crc := crc32c(0,@head,SizeOf(head)-SizeOf(head.crc));
    if head.kind=bdUpToDate then begin
      SetString(result,PAnsiChar(@head),SizeOf(head));
      exit;
    end;
    if head.size+100<=SizeOf(temp) then
      W := TFileBufferWriter.Create(TRawByteStringStream,@temp,SizeOf(temp)) else
      W := TFileBufferWriter.Create(TRawByteStringStream,head.size+100);
    try
      W.Write(@head,SizeOf(head));
      case head.kind of
      bdFull:
        SaveTo(W);
      bdDiff:
        ZeroCompressXor(pointer(fStore),pointer(fKnownStore),head.size,W);
      end;
      W.Flush;
      result := TRawByteStringStream(W.Stream).DataString;
    finally
      W.Free;
    end;
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilterDiff.DiffKnownRevision(const aDiff: RawByteString): Int64;
var head: ^TBloomDiffHeader absolute aDiff;
begin
  if (length(aDiff)<SizeOf(head^)) or (head.kind>high(head.kind)) or
     (head.size<>cardinal(length(fStore))) or
     (head.crc<>crc32c(0,pointer(head),SizeOf(head^)-SizeOf(head.crc))) then
    result := 0 else
    result := head.Revision;
end;

function TSynBloomFilterDiff.LoadFromDiff(const aDiff: RawByteString): boolean;
var head: ^TBloomDiffHeader absolute aDiff;
    P: PByte;
    PLen: integer;
begin
  result := false;
  P := pointer(aDiff);
  PLen := length(aDiff);
  if (PLen<SizeOf(head^)) or (head.kind>high(head.kind)) or
     (head.crc<>crc32c(0,pointer(head),SizeOf(head^)-SizeOf(head.crc))) then
    exit;
  if (fStore<>'') and (head.size<>cardinal(length(fStore))) then
    exit;
  inc(P,SizeOf(head^));
  dec(PLen,SizeOf(head^));
  Safe.Lock;
  try
    case head.kind of
    bdFull:
      result := LoadFrom(P,PLen);
    bdDiff:
      if fStore<>'' then
        result := ZeroDecompressOr(pointer(P),Pointer(fStore),PLen,head.size);
    bdUpToDate:
      result := true;
    end;
    if result then begin
      fRevision := head.revision;
      fInserted := head.inserted;
    end;
  finally
    Safe.UnLock;
  end;
end;

procedure ZeroCompress(P: PAnsiChar; Len: integer; Dest: TFileBufferWriter);
var PEnd,beg,zero: PAnsiChar;
    crc: cardinal;
begin
  Dest.WriteVarUInt32(Len);
  PEnd := P+Len;
  beg := P;
  crc := 0;
  while P<PEnd do begin
    while (P^<>#0) and (P<PEnd) do inc(P);
    zero := P;
    while (P^=#0) and (P<PEnd) do inc(P);
    if P-zero>3 then begin
      Len := zero-beg;
      crc := crc32c(crc,beg,Len);
      Dest.WriteVarUInt32(Len);
      Dest.Write(beg,Len);
      Len := P-zero;
      crc := crc32c(crc,@Len,SizeOf(Len));
      Dest.WriteVarUInt32(Len-3);
      beg := P;
    end;
  end;
  Len := P-beg;
  if Len>0 then begin
    crc := crc32c(crc,beg,Len);
    Dest.WriteVarUInt32(Len);
    Dest.Write(beg,Len);
  end;
  Dest.Write4(crc);
end;

procedure ZeroCompressXor(New,Old: PAnsiChar; Len: cardinal; Dest: TFileBufferWriter);
var beg,same,index,crc,L: cardinal;
begin
  Dest.WriteVarUInt32(Len);
  beg := 0;
  index := 0;
  crc := 0;
  while index<Len do begin
    while (New[index]<>Old[index]) and (index<Len) do inc(index);
    same := index;
    while (New[index]=Old[index]) and (index<Len) do inc(index);
    L := index-same;
    if L>3 then begin
      Dest.WriteVarUInt32(same-beg);
      Dest.WriteXor(New+beg,Old+beg,same-beg,@crc);
      crc := crc32c(crc,@L,SizeOf(L));
      Dest.WriteVarUInt32(L-3);
      beg := index;
    end;
  end;
  L := index-beg;
  if L>0 then begin
    Dest.WriteVarUInt32(L);
    Dest.WriteXor(New+beg,Old+beg,L,@crc);
  end;
  Dest.Write4(crc);
end;

procedure ZeroDecompress(P: PByte; Len: integer; {$ifdef FPC}var{$else}out{$endif} Dest: RawByteString);
var PEnd,D,DEnd: PAnsiChar;
    DestLen,crc: cardinal;
begin
  PEnd := PAnsiChar(P)+Len-4;
  DestLen := FromVarUInt32(P);
  SetString(Dest,nil,DestLen); // FPC uses var
  D := pointer(Dest);
  DEnd := D+DestLen;
  crc := 0;
  while PAnsiChar(P)<PEnd do begin
    Len := FromVarUInt32(P);
    if D+Len>DEnd then
      break;
    {$ifdef FPC}Move{$else}MoveFast{$endif}(P^,D^,Len);
    crc := crc32c(crc,D,Len);
    inc(P,Len);
    inc(D,Len);
    if PAnsiChar(P)>=PEnd then
      break;
    Len := FromVarUInt32(P)+3;
    if D+Len>DEnd then
      break;
    {$ifdef FPC}FillChar{$else}FillCharFast{$endif}(D^,Len,0);
    crc := crc32c(crc,@Len,SizeOf(Len));
    inc(D,Len);
  end;
  if crc<>PCardinal(P)^ then
    Dest := '';
end;

function ZeroDecompressOr(P,Dest: PAnsiChar; Len,DestLen: integer): boolean;
var PEnd,DEnd: PAnsiChar;
    crc: cardinal;
begin
  PEnd := P+Len-4;
  if cardinal(DestLen)<>FromVarUInt32(PByte(P)) then begin
    result := false;
    exit;
  end;
  DEnd := Dest+DestLen;
  crc := 0;
  while (P<PEnd) and (Dest<DEnd) do begin
    Len := FromVarUInt32(PByte(P));
    if Dest+Len>DEnd then
      break;
    crc := crc32c(crc,P,Len);
    OrMemory(pointer(Dest),pointer(P),Len);
    inc(P,Len);
    inc(Dest,Len);
    if P>=PEnd then
      break;
    Len := FromVarUInt32(PByte(P))+3;
    crc := crc32c(crc,@Len,SizeOf(Len));
    inc(Dest,Len);
  end;
  result := crc=PCardinal(P)^;
end;


function Max(a,b: PtrInt): PtrInt; {$ifdef HASINLINE}inline;{$endif}
begin
  if a > b then
    result := a else
    result := b;
end;

function Min(a,b: PtrInt): PtrInt; {$ifdef HASINLINE}inline;{$endif}
begin
  if a < b then
    result := a else
    result := b;
end;

function Comp(a,b: PAnsiChar; len: PtrInt): PtrInt;
{$ifdef HASINLINE} inline;
var lenptr: PtrInt;
begin
  result := 0;
  lenptr := len-SizeOf(PtrInt);
  if lenptr>=0 then
    repeat
      if PPtrInt(a+result)^<>PPtrInt(b+result)^ then
        break;
      inc(result,SizeOf(PtrInt));
    until result>lenptr;
  if result<len then
    repeat
      if a[result]<>b[result] then
        exit;
      inc(result);
    until result=len;
end;
{$else} // eax = a, edx = b, ecx = len
asm // the 'rep cmpsb' version is slower on Intel Core CPU (not AMD)
     or ecx,ecx
     push ebx
     push ecx
     jz @ok
@1:  mov bx,[eax]
     lea eax,[eax+2]
     cmp bl,[edx]
     jne @ok
     dec ecx
     jz @ok
     cmp bh,[edx+1]
     lea edx,[edx+2]
     jne @ok
     dec ecx
     jnz @1
@ok: pop eax
     sub eax,ecx
     pop ebx
end;
{$endif}

function CompReverse(a,b: pointer; len: PtrInt): PtrInt;
begin
  result := 0;
  if len>0 then
    repeat
      if PByteArray(a)[-result]<>PByteArray(b)[-result] then
        exit;
      inc(result);
    until result=len;
end;

procedure movechars(s,d: PAnsiChar; t: PtrUInt);
  {$ifdef HASINLINE}inline;{$endif}
// this code is sometimes used rather than MoveFast() for overlapping copy
begin
  dec(PtrUInt(s), PtrUInt(d));
  inc(t, PtrUInt(d));
  repeat
    d^ := s[PtrUInt(d)];
    inc(d);
  until PtrUInt(d)=t;
end;

function WriteCurOfs(curofs,curlen,curofssize: integer; sp: PAnsiChar): PAnsiChar;
begin
  if curlen=0 then begin
    sp^ := #0;
    inc(sp);
  end else begin
    sp := Pointer(ToVarUInt32(curlen,PByte(sp)));
    PInteger(sp)^ := curofs;
    inc(sp,curofssize);
  end;
  result := sp;
end;

{$ifdef CPUINTEL} // crc32c SSE4.2 hardware accellerated dword hash
function crc32csse42(buf: pointer): cardinal;
{$ifdef CPUX86}
asm
        mov     edx, eax
        xor     eax, eax
        {$ifdef ISDELPHI2010}
        crc32   eax, dword ptr[edx]
        {$else}
        db $F2, $0F, $38, $F1, $02
        {$endif}
end;
{$else} {$ifdef FPC}nostackframe; assembler; asm {$else}
asm // ecx=buf (Linux: edi=buf)
        .noframe
{$endif FPC}
        xor     eax, eax
        crc32   eax, dword ptr[buf]
end;
{$endif CPUX86}
{$endif CPUINTEL}

function hash32prime(buf: pointer): cardinal;
begin // xxhash32-inspired - and won't pollute L1 cache with lookup tables
  result := PCardinal(buf)^;
  result := result xor (result shr 15);
  result := result * 2246822519;
  result := result xor (result shr 13);
  result := result * 3266489917;
  result := result xor (result shr 16);
end;

const
  HTabBits = 18; // fits well with DeltaCompress(..,BufSize=2MB)
  HTabMask = (1 shl HTabBits)-1; // =$3ffff
  HListMask = $ffffff; // HTab[]=($ff,$ff,$ff)

type
  PHTab = ^THTab; // HTabBits=18 -> SizeOf=767KB
  THTab = packed array[0..HTabMask] of array[0..2] of byte;

function DeltaCompute(NewBuf, OldBuf, OutBuf, WorkBuf: PAnsiChar;
  NewBufSize, OldBufSize, MaxLevel: PtrInt; HList, HTab: PHTab): PAnsiChar;
var i, curofs, curlen, curlevel, match, curofssize, h, oldh: PtrInt;
    sp, pInBuf, pOut: PAnsiChar;
    ofs: cardinal;
    spb: PByte absolute sp;
    hash: function(buf: pointer): cardinal;
begin
  // 1. fill HTab[] with hashes for all old data
  {$ifdef CPUINTEL}
  if cfSSE42 in CpuFeatures then
    hash := @crc32csse42 else
  {$endif}
    hash := @hash32prime;
  FillCharFast(HTab^,SizeOf(HTab^),$ff); // HTab[]=HListMask by default
  pInBuf := OldBuf;
  oldh := -1; // force calculate first hash
  sp := pointer(HList);
  for i := 0 to OldBufSize-3 do begin
    h := hash(pInBuf) and HTabMask;
    inc(pInBuf);
    if h=oldh then
      continue;
    oldh := h;
    h := PtrInt(@HTab^[h]); // fast 24-bit data process
    PCardinal(sp)^ := PCardinal(h)^;
    PCardinal(h)^ := cardinal(i) or (PCardinal(h)^ and $ff000000);
    inc(sp,3);
  end;
  // 2. compression init
  if OldBufSize<=$ffff then
    curofssize := 2 else
    curofssize := 3;
  curlen := -1;
  curofs := 0;
  pOut := OutBuf+7;
  sp := WorkBuf;
  // 3. handle identical leading bytes
  match := Comp(OldBuf,NewBuf,Min(OldBufSize,NewBufSize));
  if match>2 then begin
    sp := WriteCurOfs(0,match,curofssize,sp);
    sp^ := #0; inc(sp);
    inc(NewBuf,match);
    dec(NewBufSize,match);
  end;
  pInBuf := NewBuf;
  // 4. main loop: identify longest sequences using hash, and store reference
  if NewBufSize>=8 then
  repeat
    // hash 4 next bytes from NewBuf, and find longest match in OldBuf
    ofs := PCardinal(@HTab^[hash(NewBuf) and HTabMask])^ and HListMask;
    if ofs<>HListMask then begin // brute force search loop of best hash match
      curlevel := MaxLevel;
      repeat
        with PHash128Rec(OldBuf+ofs)^ do
        {$ifdef CPU64} // test 8 bytes
        if PHash128Rec(NewBuf)^.Lo=Lo then begin
        {$else}
        if (PHash128Rec(NewBuf)^.c0=c0) and (PHash128Rec(NewBuf)^.c1=c1) then begin
        {$endif}
          match := Comp(@PHash128Rec(NewBuf)^.c2,@c2,Min(PtrUInt(OldBufSize)-ofs,NewBufSize)-8);
          if match>curlen then begin // found a longer sequence
            curlen := match;
            curofs := ofs;
          end;
        end;
        dec(curlevel);
        ofs := PCardinal(@HList^[ofs])^ and HListMask;
      until (ofs=HListMask) or (curlevel=0);
    end;
    // curlen = longest sequence length
    if curlen<0 then begin // no sequence found -> copy one byte
      dec(NewBufSize);
      pOut^ := NewBuf^;
      inc(NewBuf);
      inc(pOut);
      if NewBufSize>8 then // >=8 may overflow
        continue else
        break;
    end;
    inc(curlen,8);
    sp := WriteCurOfs(curofs,curlen,curofssize,sp);
    spb := ToVarUInt32(NewBuf-pInBuf,spb);
    inc(NewBuf,curlen); // continue to search after the sequence
    dec(NewBufSize,curlen);
    curlen := -1;
    pInBuf := NewBuf;
    if NewBufSize>8 then // >=8 may overflow
      continue else
      break;
  until false;
  // 5. write remaining bytes
  if NewBufSize>0 then begin
    {$ifdef FPC}Move{$else}MoveFast{$endif}(NewBuf^,pOut^,NewBufSize);
    inc(pOut,NewBufSize);
    inc(newBuf,NewBufSize);
  end;
  sp^ := #0; inc(sp);
  spb := ToVarUInt32(NewBuf-pInBuf,spb);
  // 6. write header
  PInteger(OutBuf)^ := pOut-OutBuf-7;
  h := sp-WorkBuf;
  PInteger(OutBuf+3)^ := h;
  OutBuf[6] := AnsiChar(curofssize);
  // 7. copy commands
  {$ifdef FPC}Move{$else}MoveFast{$endif}(WorkBuf^,pOut^,h);
  result := pOut+h;
end;

function ExtractBuf(GoodCRC: cardinal; p: PAnsiChar; var aUpd, Delta: PAnsiChar;
  Old: PAnsiChar): TDeltaError;
var pEnd, buf, upd, src: PAnsiChar;
    bufsize, datasize, leading, srclen: PtrUInt;
    curofssize: byte;
begin
  // 1. decompression init
  upd := aUpd;
  bufsize :=  PCardinal(p)^ and $00ffffff; inc(p,3);
  datasize := PCardinal(p)^ and $00ffffff; inc(p,3);
  curofssize := ord(p^); inc(p);
  buf := p; inc(p,bufsize);
  pEnd := p+datasize;
  src := nil;
  // 2. main loop
  while p<pEnd do begin
    // src/srclen = sequence to be copied
    srclen := FromVarUInt32(PByte(P));
    if srclen>0 then
      if curofssize=2 then begin
        src := Old+PWord(p)^;
        inc(p,2);
      end else begin
        src := Old+PCardinal(p)^ and $00ffffff;
        inc(p,3);
      end;
    // copy leading bytes
    leading := FromVarUInt32(PByte(P));
    if leading<>0 then begin
      {$ifdef FPC}Move{$else}MoveFast{$endif}(buf^,upd^,leading);
      inc(buf,leading);
      inc(upd,leading);
    end;
    // copy sequence
    if srclen<>0 then begin
      if PtrUInt(upd-src)<srclen then
        movechars(src,upd,srclen) else
        {$ifdef FPC}Move{$else}MoveFast{$endif}(src^,upd^,srclen);
      inc(upd,srclen);
    end;
  end;
  // 3. result check
  Delta := p;
  if (p=pEnd) and (crc32c(0,aUpd,upd-aUpd)=GoodCRC) then // whole CRC is faster
    result := dsSuccess else
    result := dsCrcExtract;
  aUpd := upd;
end;

procedure WriteByte(var P: PAnsiChar; V: Byte); {$ifdef HASINLINE}inline;{$endif}
begin
  PByte(P)^ := V;
  inc(P);
end;

procedure WriteInt(var P: PAnsiChar; V: Cardinal); {$ifdef HASINLINE}inline;{$endif}
begin
  PCardinal(P)^ := V;
  inc(P,4);
end;

const
  FLAG_COPIED   = 0;
  FLAG_COMPRESS = 1;
  FLAG_BEGIN    = 2;
  FLAG_END      = 3;

function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  out Delta: PAnsiChar; Level, BufSize: integer): integer;
var HTab, HList: PHTab;
    d, workbuf: PAnsiChar;
    db: PByte absolute d;
    BufRead, OldRead, Trailing, NewSizeSave: PtrInt;
    bigfile: boolean;
  procedure CreateCopied;
  begin
    Getmem(Delta,NewSizeSave+17);  // 17 = 4*Integer + 1*Byte
    d := Delta;
    db := ToVarUInt32(0,ToVarUInt32(NewSizeSave,db));
    WriteByte(d,FLAG_COPIED); // block copied flag
    db := ToVarUInt32(NewSizeSave,db);
    WriteInt(d,crc32c(0,New,NewSizeSave));
    {$ifdef FPC}Move{$else}MoveFast{$endif}(New^,d^,NewSizeSave);
    inc(d,NewSizeSave);
    result := d-Delta;
  end;
begin
  // 1. special cases
  if (NewSize=OldSize) and CompareMem(Old,New,NewSize) then begin
    Getmem(Delta,1);
    Delta^ := '=';
    result := 1;
    exit;
  end;
  NewSizeSave := NewSize;
  if OldSize=0 then begin // Delta from nothing -> direct copy of whole block
    CreateCopied;
    exit;
  end;
  // 2. compression init
  bigfile := OldSize>BufSize;
  if BufSize>NewSize then
    BufSize := NewSize;
  if BufSize>$ffffff then
    BufSize := $ffffff; // we store offsets with 2..3 bytes -> max 16MB chunk
  Trailing := 0;
  Getmem(workbuf,BufSize); // compression temporary buffers
  Getmem(HList,BufSize*SizeOf(HList[0]));
  Getmem(HTab,SizeOf(HTab^));
  Getmem(Delta,Max(NewSize,OldSize)+4096); // Delta size max evalulation
  try
    d := Delta;
    db := ToVarUInt32(NewSize,db); // Destination Size
    // 3. handle leading and trailing identical bytes (for biggest files)
    if bigfile then begin
      BufRead := Comp(New,Old,Min(NewSize,OldSize));   // test 1st same chars
      if BufRead>9 then begin // it happens very often
        db := ToVarUInt32(BufRead,db); // blockSize = Size BufIdem
        WriteByte(d,FLAG_BEGIN);
        WriteInt(d,crc32c(0,New,BufRead));
        inc(New,BufRead);
        dec(NewSize,BufRead);
        inc(Old,BufRead);
        dec(OldSize,BufRead);
      end;                                             // test last same chars
      BufRead := CompReverse(New+NewSize-1,Old+OldSize-1,Min(NewSize,OldSize));
      if BufRead>5 then begin
        if NewSize=BufRead then
          dec(BufRead); // avoid block overflow
        dec(OldSize,BufRead);
        dec(NewSize,BufRead);
        Trailing := BufRead;
      end;
    end;
    // 4. main loop
    repeat
      BufRead := Min(BufSize,NewSize);
      dec(NewSize,BufRead);
      if (BufRead=0) and (Trailing>0) then begin
        db := ToVarUInt32(Trailing,db);
        WriteByte(d,FLAG_END); // block idem end flag -> BufRead := 0 not necessary
        WriteInt(d,crc32c(0,New,Trailing));
        break;
      end;
      OldRead := Min(BufSize,OldSize);
      dec(OldSize,OldRead);
      db := ToVarUInt32(OldRead,db);
      If (BufRead<4) or (OldRead<4) or (BufRead div 4>OldRead) then begin
        WriteByte(d,FLAG_COPIED); // block copied flag
        db := ToVarUInt32(BufRead,db);
        if BufRead=0 then
          break;
        WriteInt(d,crc32c(0,New,BufRead));
        {$ifdef FPC}Move{$else}MoveFast{$endif}(New^,d^,BufRead);
        inc(New,BufRead);
        inc(d,BufRead);
      end else begin
        WriteByte(d,FLAG_COMPRESS); // block compressed flag
        WriteInt(d,crc32c(0,New,BufRead));
        WriteInt(d,crc32c(0,Old,OldRead));
        d := DeltaCompute(New,Old,d,workbuf,BufRead,OldRead,Level,HList,HTab);
        inc(New,BufRead);
        inc(Old,OldRead);
      end;
    until false;
  // 5. release temp memory
  finally
    result := d-Delta;
    Freemem(HTab);
    Freemem(HList);
    Freemem(workbuf);
  end;
  if result>=NewSizeSave+17 then begin
    // Delta didn't compress well -> store it (with 17 bytes overhead)
    Freemem(Delta);
    CreateCopied;
  end;
end;

function DeltaCompress(const New, Old: RawByteString;
  Level, BufSize: integer): RawByteString;
begin
  result := DeltaCompress(pointer(New),pointer(Old),
    length(New),length(Old),Level,BufSize);
end;

function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  Level, BufSize: integer): RawByteString;
var Delta: PAnsiChar;
    DeltaLen: integer;
begin
  DeltaLen := DeltaCompress(New,Old,Newsize,OldSize,Delta,Level,BufSize);
  SetString(result,Delta,DeltaLen);
  Freemem(Delta);
end;

function DeltaExtract(Delta,Old,New: PAnsiChar): TDeltaError;
var BufCRC: Cardinal;
    Code: Byte;
    Len, BufRead, OldRead: PtrInt;
    db: PByte absolute Delta;
    Upd: PAnsiChar;
begin
  Result := dsSuccess;
  Len := FromVarUInt32(db);
  Upd := New;
  repeat
    OldRead := FromVarUInt32(db);
    Code := db^; inc(db);
    Case Code of
    FLAG_COPIED: begin // block copied flag - copy new from Delta
      BufRead := FromVarUInt32(db);
      If BufRead=0 then
        break;
      If crc32c(0,Delta+4,BufRead)<>PCardinal(Delta)^ then begin
        result := dsCrcCopy;
        exit;
      end;
      inc(Delta,4);
      {$ifdef FPC}Move{$else}MoveFast{$endif}(Delta^,New^,BufRead);
      if BufRead>=Len then
        exit; // if Old=nil -> only copy new
      inc(Delta,BufRead);
      inc(New,BufRead);
    end;
    FLAG_COMPRESS: begin // block compressed flag - extract Delta from Old
      BufCRC := PCardinal(Delta)^; inc(Delta,4);
      if crc32c(0,Old,OldRead)<>PCardinal(Delta)^ then begin
        result := dsCrcComp;
        exit;
      end;
      inc(Delta,4);
      result := ExtractBuf(BufCRC,Delta,New,Delta,Old);
      if result<>dsSuccess then
        exit;
    end;
    FLAG_BEGIN: begin // block idem begin flag
      if crc32c(0,Old,OldRead)<>PCardinal(Delta)^ then begin
        result := dsCrcBegin;
        exit;
      end;
      inc(Delta,4);
      {$ifdef FPC}Move{$else}MoveFast{$endif}(Old^,New^,OldRead);
      inc(New,OldRead);
    end;
    FLAG_END: begin // block idem end flag
      if crc32c(0,Old,OldRead)<>PCardinal(Delta)^ then
        Result := dsCrcEnd;
      {$ifdef FPC}Move{$else}MoveFast{$endif}(Old^,New^,OldRead);
      inc(New,OldRead);
      break;
    end;
    else begin
      result := dsFlag;
      exit;
    end;
    end; // Case Code of
    inc(Old,OldRead);
  until false;
  if New-Upd<>Len then
    result := dsLen;
end;

function DeltaExtract(const Delta,Old: RawByteString; out New: RawByteString): TDeltaError;
begin
  if (Delta='') or (Delta='=') then begin
    New := Old;
    result := dsSuccess;
  end else begin
    SetLength(New,DeltaExtractSize(pointer(Delta)));
    result := DeltaExtract(pointer(Delta),pointer(Old),pointer(New));
  end;
end;

function DeltaExtractSize(const Delta: RawByteString): integer;
begin
  result := DeltaExtractSize(pointer(Delta));
end;

function DeltaExtractSize(Delta: PAnsiChar): integer;
begin
  if Delta=nil then
    result := 0 else
    result := FromVarUInt32(PByte(Delta));
end;

function ToText(err: TDeltaError): PShortString;
begin
  result := GetEnumName(TypeInfo(TDeltaError),ord(err));
end;



{ TRawByteStringGroup }

procedure TRawByteStringGroup.Add(const aItem: RawByteString);
begin
  if Values=nil then
    Clear; // ensure all fields are initialized, even if on stack
  if Count=Length(Values) then
    SetLength(Values,NextGrow(Count));
  with Values[Count] do begin
    Position := self.Position;
    Value := aItem;
  end;
  LastFind := Count;
  inc(Count);
  inc(Position,Length(aItem));
end;

procedure TRawByteStringGroup.Add(aItem: pointer; aItemLen: integer);
var tmp: RawByteString;
begin
  SetString(tmp,PAnsiChar(aItem),aItemLen);
  Add(tmp);
end;

{$ifndef DELPHI5OROLDER} // circumvent Delphi 5 compiler bug

procedure TRawByteStringGroup.Add(const aAnother: TRawByteStringGroup);
var i: integer;
    s,d: PRawByteStringGroupValue;
begin
  if aAnother.Values=nil then
    exit;
  if Values=nil then
    Clear; // ensure all fields are initialized, even if on stack
  if Count+aAnother.Count>Length(Values) then
    SetLength(Values,Count+aAnother.Count);
  s := pointer(aAnother.Values);
  d := @Values[Count];
  for i := 1 to aAnother.Count do begin
    d^.Position := Position;
    d^.Value := s^.Value;
    inc(Position,length(s^.Value));
    inc(s);
    inc(d);
  end;
  inc(Count,aAnother.Count);
  LastFind := Count-1;
end;

procedure TRawByteStringGroup.RemoveLastAdd;
begin
  if Count>0 then begin
    dec(Count);
    dec(Position,Length(Values[Count].Value));
    Values[Count].Value := ''; // release memory
    LastFind := Count-1;
  end;
end;

function TRawByteStringGroup.Equals(const aAnother: TRawByteStringGroup): boolean;
begin
  if ((Values=nil) and (aAnother.Values<>nil)) or ((Values<>nil) and (aAnother.Values=nil)) or
     (Position<>aAnother.Position) then
    result := false else
    if (Count<>1) or (aAnother.Count<>1) or (Values[0].Value<>aAnother.Values[0].Value) then
      result := AsText=aAnother.AsText else
      result := true;
end;

{$endif DELPHI5OROLDER}

procedure TRawByteStringGroup.Clear;
begin
  Values := nil;
  Position := 0;
  Count := 0;
  LastFind := 0;
end;

procedure TRawByteStringGroup.AppendTextAndClear(var aDest: RawByteString);
var d,i: integer;
    v: PRawByteStringGroupValue;
begin
  d := length(aDest);
  SetLength(aDest,d+Position);
  v := pointer(Values);
  for i := 1 to Count do begin
    {$ifdef FPC}Move{$else}MoveFast{$endif}(
      pointer(v^.Value)^,PByteArray(aDest)[d+v^.Position],length(v^.Value));
    inc(v);
  end;
  Clear;
end;

function TRawByteStringGroup.AsText: RawByteString;
begin
  if Values=nil then
    result := '' else begin
    if Count>1 then
      Compact;
    result := Values[0].Value;
  end;
end;

procedure TRawByteStringGroup.Compact;
var i: integer;
    v: PRawByteStringGroupValue;
    tmp: RawByteString;
begin
  if (Values<>nil) and (Count>1) then begin
    SetString(tmp,nil,Position);
    v := pointer(Values);
    for i := 1 to Count do begin
      {$ifdef FPC}Move{$else}MoveFast{$endif}(
        pointer(v^.Value)^,PByteArray(tmp)[v^.Position],length(v^.Value));
      {$ifdef FPC}Finalize(v^.Value){$else}v^.Value := ''{$endif}; // free chunks
      inc(v);
    end;
    Values[0].Value := tmp; // use result for absolute compaction ;)
    if Count>128 then
      SetLength(Values,128);
    Count := 1;
    LastFind := 0;
  end;
end;

function TRawByteStringGroup.AsBytes: TByteDynArray;
var i: integer;
begin
  result := nil;
  if Values=nil then
    exit;
  SetLength(result,Position);
  for i := 0 to Count-1 do
  with Values[i] do
    {$ifdef FPC}Move{$else}MoveFast{$endif}(
      pointer(Value)^,PByteArray(result)[Position],length(Value));
end;

procedure TRawByteStringGroup.Write(W: TTextWriter; Escape: TTextWriterKind);
var i: integer;
begin
  if Values<>nil then
    for i := 0 to Count-1 do
    with Values[i] do
      W.Add(PUTF8Char(pointer(Value)),length(Value),Escape);
end;

procedure TRawByteStringGroup.WriteBinary(W: TFileBufferWriter);
var i: integer;
begin
  if Values<>nil then
    for i := 0 to Count-1 do
      W.WriteBinary(Values[i].Value);
end;

procedure TRawByteStringGroup.WriteString(W: TFileBufferWriter);
begin
  if Values=nil then begin
    W.Write1(0);
    exit;
  end;
  W.WriteVarUInt32(Position);
  WriteBinary(W);
end;

procedure TRawByteStringGroup.AddFromReader(var aReader: TFastReader);
var complexsize: integer;
begin
  complexsize := aReader.VarUInt32;
  if complexsize>0 then // directly create a RawByteString from aReader buffer
    Add(aReader.Next(complexsize),complexsize);
end;

function TRawByteStringGroup.Find(aPosition: integer): PRawByteStringGroupValue;
var i: integer;
begin
  if (pointer(Values)<>nil) and (cardinal(aPosition)<cardinal(Position)) then begin
    result := @Values[LastFind]; // this cache is very efficient in practice
    if (aPosition>=result^.Position) and (aPosition<result^.Position+length(result^.Value)) then
      exit;
    result := @Values[1]; // seldom O(n) brute force search (in CPU L1 cache)
    for i := 0 to Count-2 do
      if result^.Position>aPosition then begin
        dec(result);
        LastFind := i;
        exit;
      end else
        inc(result);
    dec(result);
    LastFind := Count-1;
  end
  else
    result := nil;
end;

function TRawByteStringGroup.Find(aPosition, aLength: integer): pointer;
var P: PRawByteStringGroupValue;
    i: integer;
label found;
begin
  if (pointer(Values)<>nil) and (cardinal(aPosition)<cardinal(Position)) then begin
    P := @Values[LastFind]; // this cache is very efficient in practice
    i := aPosition-P^.Position;
    if (i>=0) and (i+aLength<length(P^.Value)) then begin
      result := @PByteArray(P^.Value)[i];
      exit;
    end;
    P := @Values[1]; // seldom O(n) brute force search (in CPU L1 cache)
    for i := 0 to Count-2 do
      if P^.Position>aPosition then begin
        LastFind := i;
found:  dec(P);
        dec(aPosition,P^.Position);
        if aLength-aPosition<=length(P^.Value) then
          result := @PByteArray(P^.Value)[aPosition] else
          result := nil;
        exit;
      end else
        inc(P);
    LastFind := Count-1;
    goto found;
  end
  else
    result := nil;
end;

procedure TRawByteStringGroup.FindAsText(aPosition, aLength: integer; out aText: RawByteString);
var P: PRawByteStringGroupValue;
begin
  P := Find(aPosition);
  if P=nil then
    exit;
  dec(aPosition,P^.Position);
  if (aPosition=0) and (length(P^.Value)=aLength) then
    aText := P^.Value else // direct return if not yet compacted
    if aLength-aPosition<=length(P^.Value) then
      SetString(aText,PAnsiChar(@PByteArray(P^.Value)[aPosition]),aLength);
end;

function TRawByteStringGroup.FindAsText(aPosition, aLength: integer): RawByteString;
begin
  FindAsText(aPosition,aLength,result);
end;

{$ifndef NOVARIANTS}
procedure TRawByteStringGroup.FindAsVariant(aPosition, aLength: integer; out aDest: variant);
var tmp: RawByteString;
begin
  tmp := FindAsText(aPosition,aLength);
  if tmp <> '' then
    RawUTF8ToVariant(tmp,aDest);
end;
{$endif NOVARIANTS}

procedure TRawByteStringGroup.FindWrite(aPosition, aLength: integer;
  W: TTextWriter; Escape: TTextWriterKind; TrailingCharsToIgnore: integer);
var P: pointer;
begin
  P := Find(aPosition,aLength);
  if P<>nil then
    W.Add(PUTF8Char(P)+TrailingCharsToIgnore,aLength-TrailingCharsToIgnore,Escape);
end;

procedure TRawByteStringGroup.FindWriteBase64(aPosition, aLength: integer;
  W: TTextWriter; withMagic: boolean);
var P: pointer;
begin
  P := Find(aPosition,aLength);
  if P<>nil then
    W.WrBase64(P,aLength,withMagic);
end;

procedure TRawByteStringGroup.FindMove(aPosition, aLength: integer; aDest: pointer);
var P: pointer;
begin
  P := Find(aPosition,aLength);
  if P<>nil then
    {$ifdef FPC}Move{$else}MoveFast{$endif}(P^,aDest^,aLength);
end;


{ ************  Security classes ************************** }

{ TSynAuthenticationAbstract }

constructor TSynAuthenticationAbstract.Create;
begin
  fSafe.Init;
  fTokenSeed := Random32;
  fSessionGenerator := abs(fTokenSeed*PPtrInt(self)^);
  fTokenSeed := abs(fTokenSeed*Random32);
end;

destructor TSynAuthenticationAbstract.Destroy;
begin
  fSafe.Done;
  inherited;
end;

class function TSynAuthenticationAbstract.ComputeHash(Token: Int64;
  const UserName,PassWord: RawUTF8): cardinal;
begin // rough authentication - xxHash32 is less reversible than crc32c
  result := xxHash32(xxHash32(xxHash32(Token,@Token,SizeOf(Token)),
    pointer(UserName),length(UserName)),pointer(Password),length(PassWord));
end;

function TSynAuthenticationAbstract.ComputeCredential(previous: boolean;
  const UserName,PassWord: RawUTF8): cardinal;
var tok: Int64;
begin
  tok := GetTickCount64 div 10000;
  if previous then
    dec(tok);
  result := ComputeHash(tok xor fTokenSeed,UserName,PassWord);
end;

function TSynAuthenticationAbstract.CurrentToken: Int64;
begin
  result := (GetTickCount64 div 10000) xor fTokenSeed;
end;

procedure TSynAuthenticationAbstract.AuthenticateUser(const aName, aPassword: RawUTF8);
begin
  raise ESynException.CreateFmt('%.AuthenticateUser() is not implemented',[self]);
end;

procedure TSynAuthenticationAbstract.DisauthenticateUser(const aName: RawUTF8);
begin
  raise ESynException.CreateFmt('%.DisauthenticateUser() is not implemented',[self]);
end;

function TSynAuthenticationAbstract.CreateSession(const User: RawUTF8; Hash: cardinal): integer;
var password: RawUTF8;
begin
  result := 0;
  fSafe.Lock;
  try
    // check the given Hash challenge, against stored credentials
    if not GetPassword(User,password) then
      exit;
    if (ComputeCredential(false,User,password)<>Hash) and
       (ComputeCredential(true,User,password)<>Hash) then
      exit;
    // create the new session
    repeat
      result := fSessionGenerator;
      inc(fSessionGenerator);
    until result<>0;
    AddSortedInteger(fSessions,fSessionsCount,result);
  finally
    fSafe.UnLock;
  end;
end;

function TSynAuthenticationAbstract.SessionExists(aID: integer): boolean;
begin
  fSafe.Lock;
  try
    result := FastFindIntegerSorted(pointer(fSessions),fSessionsCount-1,aID)>=0;
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynAuthenticationAbstract.RemoveSession(aID: integer);
var i: integer;
begin
  fSafe.Lock;
  try
    i := FastFindIntegerSorted(pointer(fSessions),fSessionsCount-1,aID);
    if i>=0 then
      DeleteInteger(fSessions,fSessionsCount,i);
  finally
    fSafe.UnLock;
  end;
end;


{ TSynAuthentication }

constructor TSynAuthentication.Create(const aUserName,aPassword: RawUTF8);
begin
  inherited Create;
  fCredentials.Init(true);
  if aUserName<>'' then
    AuthenticateUser(aUserName,aPassword);
end;

function TSynAuthentication.GetPassword(const UserName: RawUTF8;
  out Password: RawUTF8): boolean;
var i: integer;
begin // caller did protect this method via fSafe.Lock
  i := fCredentials.Find(UserName);
  if i<0 then begin
    result := false;
    exit;
  end;
  password := fCredentials.List[i].Value;
  result := true;
end;

function TSynAuthentication.GetUsersCount: integer;
begin
  fSafe.Lock;
  try
    result := fCredentials.Count;
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynAuthentication.AuthenticateUser(const aName, aPassword: RawUTF8);
begin
  fSafe.Lock;
  try
    fCredentials.Add(aName,aPassword);
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynAuthentication.DisauthenticateUser(const aName: RawUTF8);
begin
  fSafe.Lock;
  try
    fCredentials.Delete(aName);
  finally
    fSafe.UnLock;
  end;
end;


{ ************ Database types and classes ************************** }

{$ifdef FPC}{$push}{$endif}
{$WARNINGS OFF} // yes, we know there will be dead code below: we rely on it ;)

function IsZero(const Fields: TSQLFieldBits): boolean;
var f: TPtrIntArray absolute Fields;
begin
  {$ifdef CPU64}
  if MAX_SQLFIELDS=64 then
    result := (f[0]=0) else
  if MAX_SQLFields=128 then
    result := (f[0]=0) and (f[1]=0) else
  if MAX_SQLFields=192 then
    result := (f[0]=0) and (f[1]=0) and (f[2]=0) else
  if MAX_SQLFields=256 then
    result := (f[0]=0) and (f[1]=0) and (f[2]=0) and (f[3]=0) else
  {$else}
  if MAX_SQLFIELDS=64 then
    result := (f[0]=0) and (f[1]=0) else
  if MAX_SQLFields=128 then
    result := (f[0]=0) and (f[1]=0) and (f[2]=0) and (f[3]=0) else
  if MAX_SQLFields=192 then
    result := (f[0]=0) and (f[1]=0) and (f[2]=0) and (f[3]=0)
          and (f[4]=0) and (f[5]=0) else
  if MAX_SQLFields=256 then
    result := (f[0]=0) and (f[1]=0) and (f[2]=0) and (f[3]=0)
          and (f[4]=0) and (f[5]=0) and (f[6]=0) and (f[7]=0) else
  {$endif}
    result := IsZero(@Fields,SizeOf(Fields))
end;

function IsEqual(const A,B: TSQLFieldBits): boolean;
var a_: TPtrIntArray absolute A;
    b_: TPtrIntArray absolute B;
begin
  {$ifdef CPU64}
  if MAX_SQLFIELDS=64 then
    result := (a_[0]=b_[0]) else
  if MAX_SQLFields=128 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) else
  if MAX_SQLFields=192 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) and (a_[2]=b_[2]) else
  if MAX_SQLFields=256 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) and (a_[2]=b_[2]) and (a_[3]=b_[3]) else
  {$else}
  if MAX_SQLFIELDS=64 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) else
  if MAX_SQLFields=128 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) and (a_[2]=b_[2]) and (a_[3]=b_[3]) else
  if MAX_SQLFields=192 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) and (a_[2]=b_[2]) and (a_[3]=b_[3])
          and (a_[4]=b_[4]) and (a_[5]=b_[5]) else
  if MAX_SQLFields=256 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) and (a_[2]=b_[2]) and (a_[3]=b_[3])
          and (a_[4]=b_[4]) and (a_[5]=b_[5]) and (a_[6]=b_[6]) and (a_[7]=b_[7]) else
  {$endif}
    result := CompareMemFixed(@A,@B,SizeOf(TSQLFieldBits))
end;

procedure FillZero(var Fields: TSQLFieldBits);
begin
  if MAX_SQLFIELDS=64 then
    PInt64(@Fields)^ := 0 else
  if MAX_SQLFields=128 then begin
    PInt64Array(@Fields)^[0] := 0;
    PInt64Array(@Fields)^[1] := 0;
  end else
  if MAX_SQLFields=192 then begin
    PInt64Array(@Fields)^[0] := 0;
    PInt64Array(@Fields)^[1] := 0;
    PInt64Array(@Fields)^[2] := 0;
  end else
  if MAX_SQLFields=256 then begin
    PInt64Array(@Fields)^[0] := 0;
    PInt64Array(@Fields)^[1] := 0;
    PInt64Array(@Fields)^[2] := 0;
    PInt64Array(@Fields)^[3] := 0;
  end else
    {$ifdef FPC}FillChar{$else}FillCharFast{$endif}(Fields,SizeOf(Fields),0);
end;

{$ifdef FPC}{$pop}{$else}{$WARNINGS ON}{$endif}

procedure FieldBitsToIndex(const Fields: TSQLFieldBits; var Index: TSQLFieldIndexDynArray;
  MaxLength,IndexStart: integer);
var i,n: integer;
    sets: array[0..MAX_SQLFIELDS-1] of TSQLFieldIndex; // to avoid memory reallocation
begin
  n := 0;
  for i := 0 to MaxLength-1 do
    if i in Fields then begin
      sets[n] := i;
      inc(n);
    end;
  SetLength(Index,IndexStart+n);
  for i := 0 to n-1 do
    Index[IndexStart+i] := sets[i];
end;

function FieldBitsToIndex(const Fields: TSQLFieldBits;
  MaxLength: integer): TSQLFieldIndexDynArray;
begin
  FieldBitsToIndex(Fields,result,MaxLength);
end;

function AddFieldIndex(var Indexes: TSQLFieldIndexDynArray; Field: integer): integer;
begin
  result := length(Indexes);
  SetLength(Indexes,result+1);
  Indexes[result] := Field;
end;

function SearchFieldIndex(var Indexes: TSQLFieldIndexDynArray; Field: integer): integer;
begin
  for result := 0 to length(Indexes)-1 do
    if Indexes[result]=Field then
      exit;
  result := -1;
end;

procedure FieldIndexToBits(const Index: TSQLFieldIndexDynArray; var Fields: TSQLFieldBits);
var i: integer;
begin
  {$ifdef FPC}FillChar{$else}FillCharFast{$endif}(Fields,SizeOf(Fields),0);
  for i := 0 to Length(Index)-1 do
    if Index[i]>=0 then
      include(Fields,Index[i]);
end;

function FieldIndexToBits(const Index: TSQLFieldIndexDynArray): TSQLFieldBits;
begin
  FieldIndexToBits(Index,result);
end;

function DateToSQL(Date: TDateTime): RawUTF8;
begin
  if Date<=0 then
    result := '' else begin
    SetLength(result,13);
    PCardinal(pointer(result))^ := JSON_SQLDATE_MAGIC;
    DateToIso8601PChar(Date,PUTF8Char(pointer(result))+3,True);
  end;
end;

function DateToSQL(Year,Month,Day: Cardinal): RawUTF8;
begin
  if (Year=0) or (Month-1>11) or (Day-1>30) then
    result := '' else begin
    SetLength(result,13);
    PCardinal(pointer(result))^ := JSON_SQLDATE_MAGIC;
    DateToIso8601PChar(PUTF8Char(pointer(result))+3,True,Year,Month,Day);
  end;
end;

var
  JSON_SQLDATE_MAGIC_TEXT: RawUTF8;

function DateTimeToSQL(DT: TDateTime; WithMS: boolean): RawUTF8;
begin
  if DT<=0 then
    result := '' else begin
    if frac(DT)=0 then
      result := JSON_SQLDATE_MAGIC_TEXT+DateToIso8601(DT,true) else
    if trunc(DT)=0 then
      result := JSON_SQLDATE_MAGIC_TEXT+TimeToIso8601(DT,true,'T',WithMS) else
      result := JSON_SQLDATE_MAGIC_TEXT+DateTimeToIso8601(DT,true,'T',WithMS);
  end;
end;

function TimeLogToSQL(const Timestamp: TTimeLog): RawUTF8;
begin
  if Timestamp=0 then
    result := '' else
    result := JSON_SQLDATE_MAGIC_TEXT+PTimeLogBits(@Timestamp)^.Text(true);
end;

function Iso8601ToSQL(const S: RawByteString): RawUTF8;
begin
  if IsIso8601(pointer(S),length(S)) then
    result := JSON_SQLDATE_MAGIC_TEXT+S else
    result := '';
end;

function SQLToDateTime(const ParamValueWithMagic: RawUTF8): TDateTime;
begin
  result := Iso8601ToDateTimePUTF8Char(PUTF8Char(pointer(ParamValueWithMagic))+3,
    length(ParamValueWithMagic)-3);
end;

const
  NULL_LOW  = ord('n')+ord('u')shl 8+ord('l')shl 16+ord('l')shl 24;

function SQLParamContent(P: PUTF8Char; out ParamType: TSQLParamType; out ParamValue: RawUTF8;
  out wasNull: boolean): PUTF8Char;
var PBeg: PAnsiChar;
    L: integer;
    c: cardinal;
begin
  ParamType := sptUnknown;
  wasNull := false;
  result := nil;
  if P=nil then
    exit;
  while (P^<=' ') and (P^<>#0) do inc(P);
  case P^ of
  '''','"': begin
    P := UnQuoteSQLStringVar(P,ParamValue);
    if P=nil then
      exit; // not a valid quoted string (e.g. unexpected end in middle of it)
    ParamType := sptText;
    L := length(ParamValue)-3;
    if L>0 then begin
      c := PInteger(ParamValue)^ and $00ffffff;
      if c=JSON_BASE64_MAGIC then begin
        // ':("\uFFF0base64encodedbinary"):' format -> decode
        Base64MagicDecode(ParamValue); // wrapper function to avoid temp. string
        ParamType := sptBlob;
      end else
      if (c=JSON_SQLDATE_MAGIC) and // handle ':("\uFFF112012-05-04"):' format
         IsIso8601(PUTF8Char(pointer(ParamValue))+3,L) then begin
        Delete(ParamValue,1,3);   // return only ISO-8601 text
        ParamType := sptDateTime; // identified as Date/Time
      end;
    end;
  end;
  '-','+','0'..'9': begin // allow 0 or + in SQL
    // check if P^ is a true numerical value
    PBeg := pointer(P);
    ParamType := sptInteger;
    repeat inc(P) until not (P^ in ['0'..'9']); // check digits
    if P^='.' then begin
      inc(P);
      if P^ in ['0'..'9'] then begin
        ParamType := sptFloat;
        repeat inc(P) until not (P^ in ['0'..'9']); // check fractional digits
      end else begin
        ParamType := sptUnknown; // invalid '23023.' value
        exit;
      end;
    end;
    if byte(P^) and $DF=ord('E') then begin
      ParamType := sptFloat;
      inc(P);
      if P^='+' then inc(P) else
      if P^='-' then inc(P);
      while P^ in ['0'..'9'] do inc(P);
    end;
    FastSetString(ParamValue,PBeg,P-PBeg);
  end;
  'n':
  if PInteger(P)^=NULL_LOW then begin
    inc(P,4);
    wasNull := true;
  end else
    exit; // invalid content (only :(null): expected)
  else
    exit; // invalid content
  end;
  while (P^<=' ') and (P^<>#0) do inc(P);
  if PWord(P)^<>Ord(')')+Ord(':')shl 8 then
    // we expect finishing with P^ pointing at '):'
    ParamType := sptUnknown else
    // result<>nil only if value content in P^
    result := P+2;
end;

function ExtractInlineParameters(const SQL: RawUTF8;
  var Types: TSQLParamTypeDynArray; var Values: TRawUTF8DynArray;
  var maxParam: integer; var Nulls: TSQLFieldBits): RawUTF8;
var ppBeg: integer;
    P, Gen: PUTF8Char;
    wasNull: boolean;
begin
  maxParam := 0;
  {$ifdef FPC}FillChar{$else}FillCharFast{$endif}(Nulls,SizeOf(Nulls),0);
  ppBeg := PosEx(RawUTF8(':('),SQL,1);
  if (ppBeg=0) or (PosEx(RawUTF8('):'),SQL,ppBeg+2)=0) then begin
    // SQL code with no valid :(...): internal parameters -> leave maxParam=0
    result := SQL;
    exit;
  end;
  // compute GenericSQL from SQL, converting :(...): into ?
  FastSetString(result,pointer(SQL),length(SQL)); // private copy for unescape
  P := pointer(result); // in-place string unescape (keep SQL untouched)
  Gen := P+ppBeg-1; // Gen^ just before :(
  inc(P,ppBeg+1);   // P^ just after :(
  repeat
    Gen^ := '?'; // replace :(...): by ?
    inc(Gen);
    if length(Values)<=maxParam then
      SetLength(Values,maxParam+16);
    if length(Types)<=maxParam then
      SetLength(Types,maxParam+64);
    P := SQLParamContent(P,Types[maxParam],Values[maxParam],wasNull);
    if P=nil then begin
      maxParam := 0;
      result := SQL;
      exit; // any invalid parameter -> try direct SQL
    end;
    if wasNull then
      include(Nulls,maxParam);
    while (P^<>#0) and (PWord(P)^<>Ord(':')+Ord('(')shl 8) do begin
      Gen^ := P^;
      inc(Gen);
      inc(P);
    end;
    if P^=#0 then
      Break;
    inc(P,2);
    inc(maxParam);
  until false;
  // return generic SQL statement, with ? place-holders and params in Values[]
  SetLength(result,Gen-pointer(result));
  inc(maxParam);
end;

function InlineParameter(ID: Int64): shortstring;
begin
  FormatShort(':(%):',[ID],result);
end;

function InlineParameter(const value: RawUTF8): RawUTF8;
begin
  QuotedStrJSON(value,result,':(','):');
end;

function SQLVarLength(const Value: TSQLVar): integer;
begin
  case Value.VType of
    ftBlob:
      result := Value.VBlobLen;
    ftUTF8:
      result := StrLen(Value.VText); // fast enough for our purpose
    else
      result := 0; // simple/ordinal values, or ftNull
  end;
end;

{$ifndef NOVARIANTS}

procedure VariantToSQLVar(const Input: variant; var temp: RawByteString;
  var Output: TSQLVar);
var wasString: boolean;
begin
  Output.Options := [];
  with TVarData(Input) do
  if VType=varVariant or varByRef then
    VariantToSQLVar(PVariant(VPointer)^,temp,Output) else
  case VType of
  varEmpty, varNull:
    Output.VType := ftNull;
  varByte: begin
    Output.VType := ftInt64;
    Output.VInt64 := VByte;
  end;
  varInteger: begin
    Output.VType := ftInt64;
    Output.VInt64 := VInteger;
  end;
  {$ifndef DELPHI5OROLDER}
  varLongWord: begin
    Output.VType := ftInt64;
    Output.VInt64 := VLongWord;
  end;
  {$endif}
  varWord64, varInt64: begin
    Output.VType := ftInt64;
    Output.VInt64 := VInt64;
  end;
  varSingle: begin
    Output.VType := ftDouble;
    Output.VDouble := VSingle;
  end;
  varDouble: begin // varDate would be converted into ISO8601 by VariantToUTF8()
    Output.VType := ftDouble;
    Output.VDouble := VDouble;
  end;
  varCurrency: begin
    Output.VType := ftCurrency;
    Output.VInt64 := VInt64;
  end;
  varString: begin // assume RawUTF8
    Output.VType := ftUTF8;
    Output.VText := VPointer;
  end;
  else // handle less current cases
    if VariantToInt64(Input,Output.VInt64) then
      Output.VType := ftInt64 else begin
      VariantToUTF8(Input,RawUTF8(temp),wasString);
      if wasString then begin
        Output.VType := ftUTF8;
        Output.VText := pointer(temp);
      end else
        Output.VType := ftNull;
    end;
  end;
end;

function VariantTypeToSQLDBFieldType(const V: Variant): TSQLDBFieldType;
var tmp: TVarData;
begin
  with TVarData(V) do
  case VType of
  varEmpty:
    result := ftUnknown;
  varNull:
    result := ftNull;
  {$ifndef DELPHI5OROLDER}varShortInt, varWord, varLongWord,{$endif}
  varSmallInt, varByte, varBoolean, varInteger, varInt64, varWord64:
    result := ftInt64;
  varSingle,varDouble:
    result := ftDouble;
  varDate:
    result := ftDate;
  varCurrency:
    result := ftCurrency;
  varString:
    if (VString<>nil) and (PCardinal(VString)^ and $ffffff=JSON_BASE64_MAGIC) then
      result := ftBlob else
      result := ftUTF8;
  else
  if SetVariantUnRefSimpleValue(V,tmp) then
    result := VariantTypeToSQLDBFieldType(variant(tmp)) else
    result := ftUTF8;
  end;
end;

{$endif NOVARIANTS}

{ TJSONWriter }

procedure TJSONWriter.CancelAllVoid;
const VOIDARRAY: PAnsiChar = '[]'#10;
      VOIDFIELD: PAnsiChar = '{"FieldCount":0}';
begin
  CancelAll; // rewind JSON
  if fExpand then // same as sqlite3_get_table()
    inc(fTotalFileSize,fStream.Write(VOIDARRAY^,3)) else
    inc(fTotalFileSize,fStream.Write(VOIDFIELD^,16));
end;

constructor TJSONWriter.Create(aStream: TStream; Expand, withID: boolean;
  const Fields: TSQLFieldBits; aBufSize: integer);
begin
  Create(aStream,Expand,withID,FieldBitsToIndex(Fields),aBufSize);
end;

constructor TJSONWriter.Create(aStream: TStream; Expand, withID: boolean;
  const Fields: TSQLFieldIndexDynArray; aBufSize: integer);
begin
  if aStream=nil then
    CreateOwnedStream else
    inherited Create(aStream,aBufSize);
  fExpand := Expand;
  fWithID := withID;
  fFields := Fields;
end;

procedure TJSONWriter.AddColumns(aKnownRowsCount: integer);
var i: integer;
begin
  if fExpand then begin
    if twoForceJSONExtended in CustomOptions then
      for i := 0 to High(ColNames) do
        ColNames[i] := ColNames[i]+':' else
      for i := 0 to High(ColNames) do
        ColNames[i] := '"'+ColNames[i]+'":';
  end else begin
    AddShort('{"fieldCount":');
    Add(length(ColNames));
    if aKnownRowsCount>0 then begin
      AddShort(',"rowCount":');
      Add(aKnownRowsCount);
    end;
    AddShort(',"values":["');
    // first row is FieldNames
    for i := 0 to High(ColNames) do begin
      AddString(ColNames[i]);
      AddNoJSONEscape(PAnsiChar('","'),3);
    end;
    CancelLastChar('"');
    fStartDataPosition := fStream.Position+(B-fTempBuf);
     // B := buf-1 at startup -> need ',val11' position in
     // "values":["col1","col2",val11,' i.e. current pos without the ','
  end;
end;

procedure TJSONWriter.ChangeExpandedFields(aWithID: boolean;
  const aFields: TSQLFieldIndexDynArray);
begin
  if not Expand then
    raise ESynException.CreateUTF8(
      '%.ChangeExpandedFields() called with Expanded=false',[self]);
  fWithID := aWithID;
  fFields := aFields;
end;

procedure TJSONWriter.EndJSONObject(aKnownRowsCount,aRowsCount: integer;
  aFlushFinal: boolean);
begin
  CancelLastComma; // cancel last ','
  Add(']');
  if not fExpand then begin
    if aKnownRowsCount=0 then begin
      AddShort(',"rowCount":');
      Add(aRowsCount);
    end;
    Add('}');
  end;
  Add(#10);
  if aFlushFinal then
    FlushFinal;
end;

procedure TJSONWriter.TrimFirstRow;
var P, PBegin, PEnd: PUTF8Char;
begin
  if (self=nil) or not fStream.InheritsFrom(TMemoryStream) or
     fExpand or (fStartDataPosition=0) then
    exit;
  // go to begin of first row
  FlushToStream; // we need the data to be in fStream memory
  // PBegin^=val11 in { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
  PBegin := TMemoryStream(fStream).Memory;
  PEnd := PBegin+fStream.Position;
  PEnd^ := #0; // mark end of current values
  inc(PBegin,fStartDataPosition+1); // +1 to include ',' of ',val11'
  // jump to end of first row
  P := GotoNextJSONItem(PBegin,length(ColNames));
  if P=nil then exit; // unexpected end
  // trim first row data
  if P^<>#0 then
    {$ifdef FPC}Move{$else}MoveFast{$endif}(P^,PBegin^,PEnd-P); // erase content
  fStream.Seek(PBegin-P,soCurrent); // adjust current stream position
end;


{ ************ Expression Search Engine ************************** }

function ToText(r: TExprParserResult): PShortString;
begin
  result := GetEnumName(TypeInfo(TExprParserResult), ord(r));
end;

function ToUTF8(r: TExprParserResult): RawUTF8;
begin
  result := UnCamelCase(TrimLeftLowerCaseShort(ToText(r)));
end;


{ TExprNode }

function TExprNode.Append(node: TExprNode): boolean;
begin
  result := node <> nil;
  if result then
    Last.fNext := node;
end;

constructor TExprNode.Create(nodeType: TExprNodeType);
begin
  inherited Create;
  fNodeType := nodeType;
end;

destructor TExprNode.Destroy;
begin
  fNext.Free;
  inherited Destroy;
end;

function TExprNode.Last: TExprNode;
begin
  result := self;
  while result.Next <> nil do
    result := result.Next;
end;


{ TParserAbstract }

constructor TParserAbstract.Create;
begin
  inherited Create;
  Initialize;
end;

destructor TParserAbstract.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TParserAbstract.Clear;
begin
  fWordCount := 0;
  fWords := nil;
  fExpression := '';
  FreeAndNil(fFirstNode);
end;

function TParserAbstract.ParseExpr: TExprNode;
begin
  result := ParseFactor;
  ParseNextCurrentWord;
  if (fCurrentWord = '') or (fCurrentWord = ')') then
    exit;
  if IdemPropNameU(fCurrentWord, fAndWord) then begin // w1 & w2 = w1 AND w2
    ParseNextCurrentWord;
    if result.Append(ParseExpr) then
      result.Append(TExprNode.Create(entAnd));
    exit;
  end
  else if IdemPropNameU(fCurrentWord, fOrWord) then begin // w1 + w2 = w1 OR w2
    ParseNextCurrentWord;
    if result.Append(ParseExpr) then
      result.Append(TExprNode.Create(entOr));
    exit;
  end
  else if fNoWordIsAnd and result.Append(ParseExpr) then // 'w1 w2' = 'w1 & w2'
    result.Append(TExprNode.Create(entAnd));
end;

function TParserAbstract.ParseFactor: TExprNode;
begin
  if fCurrentError <> eprSuccess then
    result := nil
  else if IdemPropNameU(fCurrentWord, fNotWord) then begin
    ParseNextCurrentWord;
    result := ParseFactor;
    if fCurrentError <> eprSuccess then
      exit;
    result.Append(TExprNode.Create(entNot));
  end
  else
    result := ParseTerm;
end;

function TParserAbstract.ParseTerm: TExprNode;
begin
  result := nil;
  if fCurrentError <> eprSuccess then
    exit;
  if fCurrentWord = '(' then begin
    ParseNextCurrentWord;
    result := ParseExpr;
    if fCurrentError <> eprSuccess then
      exit;
    if fCurrentWord <> ')' then begin
      FreeAndNil(result);
      fCurrentError := eprMissingParenthesis;
    end;
  end
  else if fCurrentWord = '' then begin
    result := nil;
    fCurrentError := eprMissingFinalWord;
  end
  else
    try // calls meta-class overriden constructor
      result := fWordClass.Create(self, fCurrentWord);
      fCurrentError := TExprNodeWordAbstract(result).ParseWord;
      if fCurrentError <> eprSuccess then begin
        FreeAndNil(result);
        exit;
      end;
      SetLength(fWords, fWordCount + 1);
      fWords[fWordCount] := TExprNodeWordAbstract(result);
      inc(fWordCount);
    except
      FreeAndNil(result);
      fCurrentError := eprInvalidExpression;
    end;
end;

function TParserAbstract.Parse(const aExpression: RawUTF8): TExprParserResult;
var
  depth: integer;
  n: TExprNode;
begin
  Clear;
  fCurrentError := eprSuccess;
  fCurrent := pointer(aExpression);
  ParseNextCurrentWord;
  if fCurrentWord = '' then begin
    result := eprNoExpression;
    exit;
  end;
  fFirstNode := ParseExpr;
  result := fCurrentError;
  if result = eprSuccess then begin
    depth := 0;
    n := fFirstNode;
    while n <> nil do begin
      case n.NodeType of
        entWord: begin
          inc(depth);
          if depth > high(fFoundStack) then begin
            result := eprTooManyParenthesis;
            break;
          end;
        end;
        entOr, entAnd:
          dec(depth);
      end;
      n := n.Next;
    end;
  end;
  if result = eprSuccess then
    fExpression := aExpression
  else
    Clear;
  fCurrent := nil;
end;

class function TParserAbstract.ParseError(const aExpression: RawUTF8): RawUTF8;
var
  parser: TParserAbstract;
  res: TExprParserResult;
begin
  parser := Create;
  try
    res := parser.Parse(aExpression);
    if res = eprSuccess then
      result := ''
    else
      result := ToUTF8(res);
  finally
    parser.Free;
  end;
end;

function TParserAbstract.Execute: boolean;
var
  n: TExprNode;
  st: PBoolean;
begin // code below compiles very efficiently on FPC/x86-64
  st := @fFoundStack;
  n := fFirstNode;
  repeat
    case n.NodeType of
      entWord: begin
        st^ := TExprNodeWordAbstract(n).fFound;
        inc(st); // see eprTooManyParenthesis above to avoid buffer overflow
      end;
      entNot:
        PAnsiChar(st)[-1] := AnsiChar(ord(PAnsiChar(st)[-1]) xor 1);
      entOr: begin
        dec(st);
        PAnsiChar(st)[-1] := AnsiChar(st^ or boolean(PAnsiChar(st)[-1]));
      end; { TODO : optimize TExprParser OR when left member is already TRUE }
      entAnd: begin
        dec(st);
        PAnsiChar(st)[-1] := AnsiChar(st^ and boolean(PAnsiChar(st)[-1]));
      end;
    end;
    n := n.Next;
  until n = nil;
  result := boolean(PAnsiChar(st)[-1]);
end;


{ TExprParserAbstract }

procedure TExprParserAbstract.Initialize;
begin
  fAndWord := '&';
  fOrWord := '+';
  fNotWord := '-';
  fNoWordIsAnd := true;
end;

procedure TExprParserAbstract.ParseNextCurrentWord;
var
  P: PUTF8Char;
begin
  fCurrentWord := '';
  P := fCurrent;
  if P = nil then
    exit;
  while P^ in [#1..' '] do
    inc(P);
  if P^ = #0 then
    exit;
  if P^ in PARSER_STOPCHAR then begin
    FastSetString(fCurrentWord, P, 1);
    fCurrent := P + 1;
  end
  else begin
    fCurrent := P;
    ParseNextWord;
  end;
end;

procedure TExprParserAbstract.ParseNextWord;
const
  STOPCHAR = PARSER_STOPCHAR + [#0, ' '];
var
  P: PUTF8Char;
begin
  P := fCurrent;
  while not(P^ in STOPCHAR) do
    inc(P);
  FastSetString(fCurrentWord, fCurrent, P - fCurrent);
  fCurrent := P;
end;


{ TExprNodeWordAbstract }

constructor TExprNodeWordAbstract.Create(aOwner: TParserAbstract; const aWord: RawUTF8);
begin
  inherited Create(entWord);
  fWord := aWord;
  fOwner := aOwner;
end;


{ TExprParserMatchNode }

type
  TExprParserMatchNode = class(TExprNodeWordAbstract)
  protected
    fMatch: TMatch;
    function ParseWord: TExprParserResult; override;
  end;
  PExprParserMatchNode = ^TExprParserMatchNode;

function TExprParserMatchNode.ParseWord: TExprParserResult;
begin
  fMatch.Prepare(fWord, (fOwner as TExprParserMatch).fCaseSensitive, {reuse=}true);
  result := eprSuccess;
end;


{ TExprParserMatch }

constructor TExprParserMatch.Create(aCaseSensitive: boolean);
begin
  inherited Create;
  fCaseSensitive := aCaseSensitive;
end;

procedure TExprParserMatch.Initialize;
begin
  inherited Initialize;
  fWordClass := TExprParserMatchNode;
end;

function TExprParserMatch.Search(const aText: RawUTF8): boolean;
begin
  result := Search(pointer(aText), length(aText));
end;

function TExprParserMatch.Search(aText: PUTF8Char; aTextLen: PtrInt): boolean;
const // rough estimation of UTF-8 characters
  IS_UTF8_WORD = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z', #$80 ..#$ff];
var
  P, PEnd: PUTF8Char;
  n: PtrInt;
begin
  P := aText;
  if (P = nil) or (fWords = nil) then begin
    result := false;
    exit;
  end;
  if fMatchedLastSet > 0 then begin
    n := fWordCount;
    repeat
      dec(n);
      fWords[n].fFound := false;
    until n = 0;
    fMatchedLastSet := 0;
  end;
  PEnd := P + aTextLen;
  while (P < PEnd) and (fMatchedLastSet < fWordCount) do begin
    while not(P^ in IS_UTF8_WORD) do begin
      inc(P);
      if P = PEnd then 
        break;
    end;
    if P = PEnd then
      break;
    aText := P;
    repeat
      inc(P);
    until (P = PEnd) or not(P^ in IS_UTF8_WORD);
    aTextLen := P - aText;
    n := fWordCount;
    repeat
      dec(n);
      with TExprParserMatchNode(fWords[n]) do
        if not fFound and fMatch.Match(aText, aTextLen) then begin
          fFound := true;
          inc(fMatchedLastSet);
        end;
    until n = 0;
  end;
  result := Execute;
end;



initialization
  Assert(SizeOf(TSynTableFieldType)=1); // as expected by TSynTableFieldProperties
  Assert(SizeOf(TSynTableFieldOptions)=1);
  {$ifndef NOVARIANTS}
  Assert(SizeOf(TSynTableData)=SizeOf(TVarData));
  {$endif NOVARIANTS}
  Assert(SizeOf(THTab)=$40000*3); // 786,432 bytes
  Assert(SizeOf(TSynUniqueIdentifierBits)=SizeOf(TSynUniqueIdentifier));
  
  SetLength(JSON_SQLDATE_MAGIC_TEXT,3);
  PCardinal(pointer(JSON_SQLDATE_MAGIC_TEXT))^ := JSON_SQLDATE_MAGIC;
end.

