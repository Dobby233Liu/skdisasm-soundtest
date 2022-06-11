@ECHO OFF

pushd "%~dp0\.."

REM // make sure we can write to the file soundtest.gen
REM // also make a backup to soundtest.prev.bin
IF NOT EXIST soundtest.gen goto LABLNOCOPY
IF EXIST soundtest.prev.bin del soundtest.prev.bin
IF EXIST soundtest.prev.bin goto LABLNOCOPY
move /Y soundtest.gen soundtest.prev.bin
IF EXIST soundtest.gen goto LABLERROR2

:LABLNOCOPY
REM // delete some intermediate assembler output just in case
IF EXIST soundtest.p del soundtest.p
IF EXIST soundtest.p goto LABLERROR1

REM // clear the output window
REM cls

REM // run the assembler
REM // -xx shows the most detailed error output
REM // -q makes AS shut up
REM // -A gives us a small speedup
set AS_MSGPATH=AS\Win32
set USEANSI=n

REM // allow the user to choose to output error messages to file by supplying the -logerrors parameter
IF "%1"=="-logerrors" ( "AS\Win32\asw.exe" -xx -q -c -E -A -L -i "%cd%" soundtest.asm ) ELSE "AS\Win32\asw.exe" -xx -q -c -D Sonic3_Complete=1 -A -L -i "%cd%" soundtest.asm

REM // if there were errors, a log file is produced
IF "%1"=="-logerrors" ( IF EXIST soundtest.log goto LABLERROR3 )

REM // combine the assembler output into a rom
IF EXIST soundtest.p "AS\Win32\fdp2bin" soundtest.p soundtest.gen soundtest.h

REM // done -- pause if we seem to have failed, then exit
IF NOT EXIST soundtest.p goto LABLPAUSE
IF EXIST soundtest.gen goto LABLEXIT

:LABLPAUSE
pause
goto LABLEXIT

:LABLERROR1
echo Failed to build because write access to soundtest.p was denied.
pause
goto LABLEXIT

:LABLERROR2
echo Failed to build because write access to soundtest.gen was denied.
pause
goto LABLEXIT

:LABLERROR3
REM // display a noticeable message
echo.
echo ***************************************************************************
echo *                                                                         *
echo *   There were build errors/warnings. See soundtest.log for more details.   *
echo *                                                                         *
echo ***************************************************************************
echo.
pause

:LABLEXIT
popd
exit /b
