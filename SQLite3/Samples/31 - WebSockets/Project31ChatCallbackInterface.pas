unit Project31ChatCallbackInterface;

interface

uses
  SysUtils,
  SynCommons,
  mORMot;

type
  IChatCallback = interface(IInvokable)
    ['{EA7EFE51-3EBA-4047-A356-253374518D1D}']
    procedure BlaBla(const pseudo, msg: string);
  end;

  IChatService = interface(IInvokable)
    ['{C92DCBEA-C680-40BD-8D9C-3E6F2ED9C9CF}']
    procedure Join(const pseudo: string; const callback: IChatCallback);
    procedure BlaBla(const pseudo,msg: string);
    // match TInterfaceFactory.MethodIndexCallbackReleased method signature
    procedure CallbackReleased(const callback: IInvokable);
  end;

const
  PROJECT31_TRANSMISSION_KEY = 'longwork_privatekey';

  
implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IChatService),TypeInfo(IChatCallback)]);
end.
