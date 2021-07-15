library(tidyverse)
library(ggjoy)
library(knitr)
library(genius)
library(spotifyr)
library(lubridate)
Sys.setenv(SPOTIFY_CLIENT_ID = 'd43f9c0c22be4532bdf2f8343fbd2a50')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'd55d4388272d410db47e4d016c800bab')
access_token<-get_spotify_access_token()

#data from spotify API by a single user
aaaa=get_my_saved_tracks(limit=50, offset=0, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
  mutate(artists = map_chr(track.artists, function(x) x$name[1]))

for (i in seq(from = 50, to = 250, by = 50)) {
  aaaa = rbind(aaaa, get_my_saved_tracks(limit=50, offset=i, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
                 mutate(artists = map_chr(track.artists, function(x) x$name[1])))
}

#get id, name, traccia
id=c()
name=c()
track=c()

for(i in 1:nrow(aaaa)){
  id=c(id,aaaa[[2]][[i]][2])
  name=c(name, aaaa[[2]][[i]][3])
  track=c(track, as.character(aaaa[i,10]))
}

#divido i featuring
name1=c()
name2=c()
name3=c()
for(i in 1:nrow(aaaa)){
  name1=c(name1, name[[i]][1])
  name2=c(name2, name[[i]][2])
  name3=c(name3, name[[i]][3])
}

#prendo solo il primo id nei casi di featuring
id1=c()
for(i in 1:nrow(aaaa)){
  id1=c(id1, id[[i]][1])
}
id1=as.data.frame(id1)

#prendo i generi per ciascun id(artista) (ci mette qualche minuto a girare)
genere1=c()
genere2=c()
genere3=c()
genere4=c()
for(i in 1:nrow(id1)){
  genere1=c(genere1, as.character(get_artist(id=as.character((id1)[i,]), authorization = get_spotify_access_token())[[3]][1]))
  genere2=c(genere2, as.character(get_artist(id=as.character((id1)[i,]), authorization = get_spotify_access_token())[[3]][2]))
  genere3=c(genere3, as.character(get_artist(id=as.character((id1)[i,]), authorization = get_spotify_access_token())[[3]][3]))
  genere4=c(genere4, as.character(get_artist(id=as.character((id1)[i,]), authorization = get_spotify_access_token())[[3]][4]))
}
genere1=as.data.frame(genere1)
genere2=as.data.frame(genere2)
genere3=as.data.frame(genere3)
genere4=as.data.frame(genere4)

#salvo tutto in dati 
name1=as.data.frame(name1)
name2=as.data.frame(name2)
name3=as.data.frame(name3)
track=as.data.frame(track)


dati=cbind(id1, name1, name2, name3, track, genere1, genere2, genere3, genere4)

write.csv2(dati,"dati.csv")