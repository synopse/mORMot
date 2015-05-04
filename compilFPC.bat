@echo off

cd \dev\lib
mkdir tempbuild\fpc >nul
del tempbuild\fpc\*.exe >nul
del tempbuild\fpc\*.o >nul
del tempbuild\fpc\*.a >nul
del tempbuild\fpc\*.ppu >nul
del tempbuild\fpc\*.map >nul
del tempbuild\fpc\*.db3 >nul
del tempbuild\fpc\*.ini >nul
del tempbuild\fpc\*.data >nul
del tempbuild\fpc\*.mdb >nul
del tempbuild\fpc\TestSQL3.* >nul
@echo.

cd \dev\lib\SQLite3

c:\Development\FreePascal\fpc\bin\i386-win32\fpc.exe -B -MObjFPC -Scgi -CX -O2 -XX -ve -dFPCSQLITE3STATIC -FiD:\Dev\Lib -FuD:\Dev\Lib -FuD:\Dev\Lib\SQLite3 -l -FlD:\Dev\Lib\fpc-win32 -FED:\Dev\Lib\tempbuild\fpc TestSQL3.dpr >D:\Dev\Lib\tempbuild\fpc\compilation

rem C:\development\fpc\bin\i386-win32\fpc.exe -B -MObjFPC -Scgi -CX -O2 -XX -ve -FiD:\Dev\Lib -FuD:\Dev\Lib -FuD:\Dev\Lib\SQLite3 -l -FlD:\Dev\Lib\fpc-win32 -FED:\Dev\Lib\tempbuild\fpc TestSQL3.dpr >D:\Dev\Lib\tempbuild\fpc\compilation

cd \dev\lib\tempbuild\fpc
TestSQL3

pause