# TOGoSUtils

Personal command-line utilities to automate my life.

- [bin/](./bin/) - Command-line programs
- [src/](./src/) - Source code for commands/libraries, organized by sub-project and language (Maven-style)
  - [src/lib/](./src/lib/) - TOGoSUtils libs, primarily for use by scripts in `bin/` or `setup-scripts/`
- [ext-bin/](./ext-bin/) - Binaries imported from elsewhere
- [dotfiles/](./dotfiles/) - Common configuration files, especially `.emacs`, but also others.
  Structured to match home directory, so you should be able to `ln -s TOGoSUtils/dotfiles/xyz/123 ~/xyz/123`
- [setup-scripts/](./setup-scripts/) - Scripts to automate installation of...TOGoS modules, for lack of a better term.

## Regarding windows batch scripts

Some of the .bat files under bin/ actually just run a bash script.
I wrote and tested this using bash.exe that's included with Git for Windows
(e.g. ```git version 2.17.0.windows.1``` works).

Other versions might not work so good.
The UnxUtils one is very weird and definitely does not work.
Also the UnxUtils find.exe is old and doesn't support some needed features.
