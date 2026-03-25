# Maybe add this to your actual nushell/env.nu:
#
#   const TOGOSUTILS_DIR = "C:/Users/TOGoS/stuff/proj/TOGoSUtils"
# 
# And this to your actual nushell/config.nu:
# 
#   source ($TOGOSUTILS_DIR)/dotfiles/.config/nushell/togos-win-config.nu

$env.config.datetime_format.table = "%Y-%m-%d %H:%M:%S"

use ($TOGOSUTILS_DIR)/dotfiles/.config/nushell/togos-win-stuff.nu [ 'runcl' 'jcr36' 'ccd' 'ccouch' 'ucm' ]
