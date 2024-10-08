@echo off

rem Change directory, but:
rem - Also switch drive
rem - Also set the window title to the last path section

set "ccd_self_name=%~nx0"

set ccd_debug=rem

:parseargs
%ccd_debug% parseargs: %%1 = "%1"
if "%~1" == "" goto parseargs_end
if "%~1" == "-v" goto parseargs_v
goto parseargs_target


:parseargs_target
set "ccd_target=%~1"
set "ccd_target_drive=%~d1"
set "ccd_target_title=%~n1"
shift
goto parseargs


:parseargs_v
set "ccd_debug=echo %ccd_self_name%:"
shift
goto parseargs


:parseargs_end
%ccd_debug% Done parsing arguments

if not defined ccd_target goto no_target_specified



%ccd_debug% ccd_target_drive = %ccd_target_drive%
rem Later logic guesses target drive based on the first two characters
rem of some supposedly-absolute path.
rem For the '%1 exists' case, we can't make that assumption,
rem so short-circuit to avoid that logic:
if exist "%ccd_target%" goto go_ahead
goto guess


:guess
%ccd_debug% %ccd_target% does not exist.  Trying some guesses...

set "ccd_guess=%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

set "ccd_guess=%UserProfile%\%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

set "ccd_guess=%UserProfile%\stuff\%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

set "ccd_guess=%UserProfile%\stuff\docs\%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

set "ccd_guess=%UserProfile%\stuff\proj\%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

set "ccd_guess=%UserProfile%\stuff\sites\%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

set "ccd_guess=%UserProfile%\workspace\%ccd_target%"
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

set "ccd_guess=D:\stuff\sites\%ccd_target%"
%ccd_debug% Trying %ccd_guess%
if exist "%ccd_guess%" goto guess_okay

if "%ccd_target%" == "timelog" (set "ccd_target=job\EarthIT\timelog" & goto guess)

if "%ccd_target%" == "amn" (set "ccd_target=job\ATR-MCAS\notes" & goto guess)
if "%ccd_target%" == "et" (set "ccd_target=job\EarthIT\timelog" & goto guess)
if "%ccd_target%" == "en" (set "ccd_target=job\ETF\notes" & goto guess)
if "%ccd_target%" == "h" (set "ccd_target=%UserProfile%" & goto guess)
if "%ccd_target%" == "home" (set "ccd_target=%UserProfile%" & goto guess)
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
if "%ccd_target%" == "taxes" (set "ccd_target=docs\financial\taxes" & goto guess)

goto not_found


:guess_okay
set "ccd_target=%ccd_guess%"
rem This assumes that %ccd_guess% is a full path, which is maybe not a good assumption to make
set "ccd_target_drive=%ccd_guess:~0,2%"
for /F "delims=" %%i in ("%ccd_target%") do set "ccd_target_title=%%~ni"
%ccd_debug% Found %ccd_target%! 
goto go_ahead


:no_target_specified
echo %ccd_self_name%: Error: No target specified!>&2
goto clean_up_and_fail


:not_found
echo %ccd_self_name%: Error: '%ccd_target%' not found. >&2
%ccd_debug% Time to unset all these ccd vars!
goto clean_up_and_fail


:go_ahead
%ccd_debug% Okay; target_drive="%ccd_target_drive%", target_title="%ccd_target_title%", target="%ccd_target%"
title %ccd_target_title%
if defined ccd_target_drive %ccd_target_drive%
cd "%ccd_target%"
goto clean_up_and_exit


:clean_up_and_fail
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
