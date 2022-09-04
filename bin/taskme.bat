@echo off
setlocal

cls
cat ^
		%UserProfile%\stuff\job\EarthIT\timelog\project-tasks.tef ^
		%UserProfile%\stuff\music\TOGoSMusicMetadata\tasks.tef ^
	| deno run "https://deno.land/x/listtodo27@v0.1.1/list-todo.ts" -p
