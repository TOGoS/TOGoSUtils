@echo off

rem Change directory, but:
rem - Also switch drive
rem - Also set the window title to the last path section

set "ccd_self_name=%~nx0"
rem set "ccd_debug=echo %ccd_self_name%:"
set ccd_debug=rem

set "ccd_target_drive=%~d1"
set "ccd_target_title=%~n1"
set "ccd_target=%~1"

if not exist "%ccd_target%" goto guess


:go_ahead
%ccd_debug% Okay; target_drive="%ccd_target_drive%", target_title="%ccd_target_title%", target="%ccd_target%"
title %ccd_target_title%
%ccd_target_drive%
cd "%ccd_target%"
goto clean_up_and_exit


:guess
%ccd_debug% %ccd_target% does not exist.  Trying some guesses...

set "ccd_guess=%UserProfile%\stuff\%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

set "ccd_guess=%UserProfile%\stuff\docs\%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

set "ccd_guess=%UserProfile%\stuff\proj\%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

set "ccd_guess=D:\stuff\%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

set "ccd_guess=D:\stuff\docs\%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

set "ccd_guess=D:\stuff\proj\%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

if "%ccd_target%" == "timelog" (set "ccd_target=job\EarthIT\timelog" & goto guess)

if "%ccd_target%" == "amn" (set "ccd_target=job\ATR-MCAS\notes" & goto guess)
if "%ccd_target%" == "et" (set "ccd_target=job\EarthIT\timelog" & goto guess)
if "%ccd_target%" == "en" (set "ccd_target=job\ETF\notes" & goto guess)
if "%ccd_target%" == "pn2" (set "ccd_target=docs\ProjectNotes2" & goto guess)
if "%ccd_target%" == "tu" (set "ccd_target=proj\TOGoSUtils" & goto guess)
if "%ccd_target%" == "ln" (set "ccd_target=docs\LoveNotes" & goto guess)
if "%ccd_target%" == "tmm" (set "ccd_target=music\TOGoSMusicMetadata" & goto guess)
if "%ccd_target%" == "n24" (set "ccd_target=sites\nuke24" & goto guess)
if "%ccd_target%" == "nuke" (set "ccd_target=sites\nuke24" & goto guess)
if "%ccd_target%" == "osd" (set "ccd_target=proj\OpenSCADDesigns" & goto guess)
if "%ccd_target%" == "sg" (set "ccd_target=proj\SynthGen2100" & goto guess)
if "%ccd_target%" == "sgdl" (set "ccd_target=proj\SynthGen2100-devlog" & goto guess)
if "%ccd_target%" == "9t" (set "ccd_target=docs\financial\4909" & goto guess)


goto not_found



:guess_okay
set "ccd_target=%ccd_guess%"
set "ccd_target_drive=%ccd_guess:~0,2%"
for /F "delims=" %%i in ("%ccd_target%") do set "ccd_target_title=%%~ni"
%ccd_debug% Found %ccd_target%! 
goto go_ahead


:not_found
echo %ccd_self_name%: Error: '%ccd_target%' not found. >&2
%ccd_debug% Time to unset all these ccd vars!
set ccd_debug=
set ccd_guess=
set ccd_self_name=
set ccd_target=
set ccd_target_drive=
set ccd_target_title=
exit /B 1


:clean_up_and_exit
%ccd_debug% Time to unset all these ccd vars!
set ccd_debug=
set ccd_guess=
set ccd_self_name=
set ccd_target=
set ccd_target_drive=
set ccd_target_title=
