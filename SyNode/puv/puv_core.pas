unit puv_core;

interface

{$IFNDEF MSWINDOWS}
function puv_cloexec(fd: Integer; do_set: Boolean): Integer;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
{$ELSE}
uses
  BaseUnix,
  termio;

function puv_cloexec(fd: Integer; do_set: Boolean): Integer;
var
  r: Integer;
  req: TIOCtlRequest;
begin
  if do_set then
    req := FIOCLEX
  else
    req := FIONCLEX;

  repeat
    r := FpIOCtl(fd, req, nil);
  until ((r <> -1) or (errno <> ESysEINTR));

  if (r <> 0) then
    Result := -errno
  else
    Result := 0;
end;
{$ENDIF}
end.

