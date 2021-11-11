# Youtube Podcast Downloader

2021-11-11

Scripts in Julia for downloading music and renaming `.opus` files to `.ogg`
so that they can play on some music players that don't recognize `.opus` as an audio filename extension.

1. Pick a scratch directory.  Audio will be downloaded into a `downloaded` sub-directory
   and hardlinked into `for-phone`.
2. Provide your own video-urls.lst in the scratch directory.
   The example one here is from Underdog Music School's channel.
3. Either copy the scripts to a scratch directory and run make.bat, or
   from the scratch directory, call the make.bat here, which should reference the Julia scripts
   relative to itself.

As of [2021-11-11T12:40], this is not tested.
You may have to fix make.bat for the above instructions to work idk.
