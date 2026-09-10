@echo off
cd /d "%~dp0"
title Hermeskim Auswertung

echo ===================================================
echo Hermeskim Auswertung - Application Launcher
echo ===================================================
echo Working Directory: %CD%

:: Set R_PORTABLE directory
set "R_PORTABLE=%~dp0R-Portable"
if not exist "%R_PORTABLE%" set "R_PORTABLE=%~dp0R-Portable\R-Portable"

if exist "%R_PORTABLE%\bin\x64\Rscript.exe" (
    set "RSCRIPT=%R_PORTABLE%\bin\x64\Rscript.exe"
    set "RBIN=%R_PORTABLE%\bin\x64"
) else if exist "%R_PORTABLE%\bin\Rscript.exe" (
    set "RSCRIPT=%R_PORTABLE%\bin\Rscript.exe"
    set "RBIN=%R_PORTABLE%\bin"
) else (
    echo [FEHLER] Rscript.exe wurde nicht gefunden!
    pause
    exit /b 1
)

:: Set explicit environment variables for Portable R
set "R_HOME=%R_PORTABLE%"
set "PATH=%RBIN%;%PATH%"

echo Found Rscript at: "%RSCRIPT%"
echo R_HOME set to: "%R_HOME%"
echo.
echo Starting Shiny App...
echo ===================================================
echo.

:: Run Rscript and log stdout/stderr to r_output.log
"%RSCRIPT%" "scripts\run.R" > r_output.log 2>&1

set EXIT_CODE=%ERRORLEVEL%

if %EXIT_CODE% NEQ 0 (
    echo.
    echo ===================================================
    echo [FEHLER] Rscript wurde mit Fehlercode %EXIT_CODE% beendet.
    echo R-Fehlerausgabe (r_output.log):
    echo ---------------------------------------------------
    type r_output.log
    echo ---------------------------------------------------
    echo ===================================================
) else (
    echo Die Anwendung wurde ordnungsgemaess beendet.
)

pause