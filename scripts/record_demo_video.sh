#!/bin/bash

# rm ./topbarheight.temp 2> /dev/null
# rm ./demo.webm 2> /dev/null
rm ./demo.mp4 2> /dev/null
RECORD_DEMO=true electron .

# TOPBAR_HEIGHT=$(cat topbarheight.temp)
# TODO: to crop or not to crop?
# TODO: upload to droplet-website/ folder
# ffmpeg -i demo.webm -filter:v "crop=iw:ih-${TOPBAR_HEIGHT}:0:${TOPBAR_HEIGHT}" demo.mp4
ffmpeg -i demo.webm demo.mp4

# rm ./topbarheight.temp 2> /dev/null
rm demo.webm 2> /dev/null
