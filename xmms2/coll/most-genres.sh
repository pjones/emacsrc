#!/bin/sh

playlist_name="Auto-Lame-Genres"
collection_name="Random"

lame_genres="\
  genre:Comedy       \
  genre:Folk         \
  genre:Humor        \
  genre:Children*    \
  genre:Holiday      \
  genre:Audio*       \
  genre:Spoken*      \
  genre~Game         \
  genre:Religious    \
  genre:Meditation   \
  genre:Christmas    \
  genre:Halloween    \
  genre:Introduction \
  genre:Radio*"

xmms2 playlist remove $playlist_name
xmms2 playlist create $playlist_name

for pattern in $lame_genres; do
  xmms2 add -p $playlist_name -t "$pattern"
done

xmms2 collection remove $collection_name
xmms2 collection create $collection_name "NOT in:Playlists/$playlist_name"
xmms2 playlist remove $collection_name
xmms2 playlist create $collection_name
xmms2 playlist config -t pshuffle -i $collection_name -u 10 $collection_name
