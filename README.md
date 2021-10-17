# How to play

Windows:

``stack run | ffmpeg.exe -f s16be -i pipe:0 -f wav pipe:1 | vlc -``

Linux:

``stack run | aplay -f S16_BE -r 8000``
