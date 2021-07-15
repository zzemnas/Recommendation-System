
library(dplyr)

#Cosine similarity function
f.simab<-function(a,b,rank=rank.opt){
  asqr<-sqrt(sum(a^2))
  bsqr<-sqrt(sum(b^2))
  return(a%*%b/(asqr*bsqr))
}


#trova indici degli artisti in item_em
arti=data.frame(artisti=colnames(item_em),ind=seq(from=1,to=ncol(item_em),by=1))

#artisti simili agli artisti predetti all'utente i-esimo (riga di myPredction) 
myPrediction
art.pred=colnames(total)[myPrediction[1,]]
id=c()
for(i in 1:length(art.pred)){
  id=c(id,arti[which(arti$artisti==art.pred[i]),2])
}

simil.art=c()
simab=c()
for (i in 1:ncol(item_em)){
  if(i != id[1]){
    simil.art=c(simil.art, colnames(item_em)[i])
    simab=c(simab,f.simab(item_em[,id[1]], item_em[,i], rank=rank.opt))
  }
}
sim=as.data.frame(cbind(artists=simil.art,value=simab))
sim=arrange(sim, sim[,2])
simili=sim[c(1746:1750),1]
artisti.simili=data.frame(artista=art.pred[1], sim1=simili[5], sim2=simili[4], sim3=simili[3], sim4=simili[2], sim5=simili[1])

val=as.vector(sim[c(1741:1750),2])
par(mar = c(9.0, 4.2, 4.1, 2.1))
plot(val, type="l", col="2", main=art.pred[1], xlab='', ylab='simab', xaxt='n')
axis(1, at=1:10, labels=as.vector(sim[c(1741:1750),1]), las = 2)

for(j in 2:length(id)){
  simil.art=c()
  simab=c()
  for (i in 1:ncol(item_em)){
    if(i != id[j]){
      simil.art=c(simil.art, colnames(item_em)[i])
      simab=c(simab,f.simab(item_em[,id[j]], item_em[,i], rank=rank.opt))
    }
  }
  sim=as.data.frame(cbind(artists=simil.art,value=simab))
  sim=arrange(sim, sim[,2])
  simili=sim[c(1746:1750),1]
  artisti.simili=rbind(artisti.simili,data.frame(artista=art.pred[j], sim1=simili[5], sim2=simili[4], sim3=simili[3], sim4=simili[2], sim5=simili[1]))
  
  val=as.vector(sim[c(1741:1750),2])
  par(mar = c(9.0, 4.2, 4.1, 2.1))
  plot(val, type="l", col="2", main=art.pred[j], xlab='', ylab='simab', xaxt='n')
  axis(1, at=1:10, labels=as.vector(sim[c(1741:1750),1]), las = 2)
}

View(artisti.simili)






