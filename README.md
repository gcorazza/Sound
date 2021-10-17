# How to play

Windows:

``stack run | ffmpeg.exe -f u8 -i pipe:0 -f wav pipe:1 | vlc -``

Linux:

``stack run | aplay ``
