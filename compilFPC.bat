@echo off

cd \dev\lib
if not exist tempbuild\fpc mkdir tempbuild\fpc
cd tempbuild\fpc
del *.exe *.o *.a *.ppu *.map *.db3 *.ini *.data *.mdb TestSQL3.* > nul
echo.

cd \dev\lib\SQLite3
echo Compiling...

c:\Development\FreePascal\fpc\bin\i386-win32\fpc.exe -B -MObjFPC -Scgi -CX -O2 -XX -ve -dFPCSQLITE3STATIC -FiD:\Dev\Lib -FuD:\Dev\Lib -FuD:\Dev\Lib\SQLite3 -l -FlD:\Dev\Lib\fpc-win32 -FED:\Dev\Lib\tempbuild\fpc TestSQL3.dpr >D:\Dev\Lib\tempbuild\fpc\compilation

rem C:\development\fpc\bin\i386-win32\fpc.exe -B -MObjFPC -Scgi -CX -O2 -XX -ve -FiD:\Dev\Lib -FuD:\Dev\Lib -FuD:\Dev\Lib\SQLite3 -l -FlD:\Dev\Lib\fpc-win32 -FED:\Dev\Lib\tempbuild\fpc TestSQL3.dpr >D:\Dev\Lib\tempbuild\fpc\compilation

cd \dev\lib\tempbuild\fpc
TestSQL3

pause