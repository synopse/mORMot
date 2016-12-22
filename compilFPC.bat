@echo off

set lib=d:\dev\lib
set bin=z:\tempbuild\fpc

cd \dev\lib
if not exist %bin% mkdir %bin%
del %bin%\*.exe %bin%\*.o %bin%\*.a %bin%\*.ppu %bin%\*.map %bin%\*.db3 %bin%\*.ini %bin%\*.data %bin%\*.mdb %bin%\TestSQL3.* > nul 2> nul
echo.

cd \dev\lib\SQLite3
echo Compiling...

@rem   -O2 makes GPF during DDD tests... :(

c:\NewPascal\fpc\bin\i386-win32\fpc.exe -B -MObjFPC -Scgi -CX -O1 -XX -ve -dFPCSQLITE3STATIC -Fi%lib% -Fu%lib% -Fu%lib%\SQLite3 -Fu%lib%\kylix -l -Fl%lib%\fpc-win32 -FE%bin% TestSQL3.dpr >%bin%\compilMsg

rem C:\development\fpc\bin\i386-win32\fpc.exe -B -MObjFPC -Scgi -CX -O1 -XX -ve -Fi%lib% -Fu%lib% -Fu%lib%\SQLite3 -l -Fl%lib%\fpc-win32 -FE%bin% TestSQL3.dpr >%bin%\compilation

del %bin%\*.o %bin%\*.a %bin%\*.ppu %bin%\*.mdb > nul

%bin%\TestSQL3

pause