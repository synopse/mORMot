@echo FossilRepository=%1
@echo GitRepository=%2
@echo GitExe=%3
@echo DescFile=%4
@echo.

ROBOCOPY %1 %2 /xf _fossil_  >nul
ROBOCOPY %1\SQLite3 %2\SQLite3  >nul
ROBOCOPY %1\SynDBDataSet %2\SynDBDataSet  >nul

%3 commit -a --file=%4

%3 push

@echo.
@pause
