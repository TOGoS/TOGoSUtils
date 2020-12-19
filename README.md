# TOGoSUtils

Personal command-line utilities to automate my life.

- `bin/` - Command-line programs
- `src/` - Source code for commands/libraries
- `ext-bin/` - Binaries imported from elsewhere

## Regarding windows batch scripts

Some of the .bat files under bin/ actually just run a bash script.
I wrote and tested this using bash.exe that's included with Git for Windows
(e.g. ```git version 2.17.0.windows.1``` works).

Other versions might not work so good.
The UnxUtils one is very weird and definitely does not work.
Also the UnxUtils find.exe is old and doesn't support some needed features.
