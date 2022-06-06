#!/bin/bash

# Exécution de programmes de test

files=( "examples/"*.txt )

PS3='Choisir le programme de test, ou 0 pour quitter: '
select file in "${files[@]}"; do
    if [[ $REPLY == "0" ]]; then
        echo 'Bye!' >&2
        exit
    elif [[ -z $file ]]; then
        echo 'Invalide. Réesayez' >&2
    else
        break
    fi
done

rm -rf res
mkdir -p res
echo
echo

# On lance la simulation
dune exec LifeSim $file
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

