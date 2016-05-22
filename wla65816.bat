@echo off
set path=%PATH%;%cd%;C:\Program Files (x86)\wla-dx-9.6
set /p src=Type the name of SNES source file and press Enter.  
for /F "delims=." %%a in ("%src%") do set name=%%a
REM echo %src%
REM echo %name%
REM @pause
wla-65816 -vo %src%
echo Object file %name%.o created.  Press enter to assemble %name%.smc
@pause
wlalink -vr %name%.linkfile %name%.smc
echo %name%.smc created!  Press enter to close this window.
@pause
