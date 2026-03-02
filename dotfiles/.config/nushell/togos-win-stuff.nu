# NuShell stuff assuming Windows environment
# and things in the usual places.
#
# Maybe add this to your actual nushell/env.nu:
#
#   const TOGOSUTILS_DIR = "C:/Users/TOGoS/stuff/proj/TOGoSUtils"
# 
# And this to your actual nushell/config.nu:
# 
#   use ($TOGOSUTILS_DIR)/dotfiles/.config/nushell/togos-win-stuff.nu [ 'runcl' 'jcr36' 'ccd' ]

export alias jcr36 = java -jar $"($env.USERPROFILE)/stuff/proj/JavaCommandRunner36/JCR36.1.30.jar"

export def --wrapped runcl [...rest] {
    let m2_repo_dir = $"($env.USERPROFILE)/.m2/repository"
    let clojure_jars = $"($m2_repo_dir)/org/clojure/clojure/1.12.3/clojure-1.12.3.jar;($m2_repo_dir)/org/clojure/spec.alpha/0.5.238/spec.alpha-0.5.238.jar;($m2_repo_dir)/org/clojure/core.specs.alpha/0.4.74/core.specs.alpha-0.4.74.jar"

    java -cp $clojure_jars clojure.main ...$rest
}

export def --env --wrapped ccd [...rest] {
    let target = (ccd.bat /print ...$rest)
    cd $target
}

export def --wrapped ccouch [...rest] {
	java -jar C:/Users/TOGoS/stuff/proj/ContentCouch/ContentCouch.jar $"-repo:($env.CCOUCH_REPO_NAME)" $env.CCOUCH_REPO_DIR ...$rest
}
