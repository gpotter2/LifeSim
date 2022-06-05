#!/bin/bash

mkdir -p res

# On lance la simulation
dune exec Vol testprog.txt
RESULT=$?
if [ $RESULT -eq 0 ]; then
    # Création de la vidéo
    echo
    echo "Conversion en vidéo..."
    ffmpeg \
      -y -hide_banner -loglevel error \
      -framerate 30 -i "res/%03d.png" \
      -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2" -c:v libx264 -pix_fmt yuv420p \
      out.mp4
    readlink -f out.mp4
else
  echo "Une erreur est survenue !"
fi

