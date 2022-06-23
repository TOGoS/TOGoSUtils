@echo off

setlocal EnableDelayedExpansion

rem set "cmd_prefix=call" if you want this script to actually do the things
if not defined cmd_prefix set cmd_prefix=echo

for %%f in (%*) do (
	set "input_file=%%f"
	set "output_file=!input_file:.txt=.bulk-inserts.sql!"
	!cmd_prefix! echo Rewriting !output_file!...
	!cmd_prefix! node "C:\Users\stevedxfph\stuff\proj\TOGoSUtils\bin\bulk-insertify.mjs" !input_file! -o !output_file!
	!cmd_prefix! del !input_file!
	!cmd_prefix! ren !output_file! !input_file!
)
