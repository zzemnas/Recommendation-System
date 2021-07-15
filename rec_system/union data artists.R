#load the artist tables for each user
jac <- read.csv2("artistiJacopo1.csv")
lui <- read.csv2("artistiLuigi1.csv")
fra <- read.csv2("artistiFrancesco1.csv")
lor <- read.csv2("artistiLorenzo1.csv")
gia <- read.csv2("artistiGianluca1.csv")

chi <- read.csv2("artistiChiara1.csv")
ire <- read.csv2("artistiIrene1.csv")
and <- read.csv2("artistiAndrea1.csv")
dan <- read.csv2("artistiDaniele1.csv")
dani <- read.csv2("artistiDani1.csv")
adri <- read.csv2("artistiAdriano1.csv")
sim <- read.csv2("artistiSimona1.csv")

#join all data in a matrix (n x m)  n users, m artists
a<-merge(lui, lor, by="Var1", all=T)
names(a)[names(a) == "Utente.x"] <- "Luigi"
names(a)[names(a) == "Utente.y"] <- "Lorenzo"
names(a)[names(a) == "Freq.x"] <- "Freq.Luigi"
names(a)[names(a) == "Freq.y"] <- "Freq.Lorenzo"

b<-merge(a, jac, by="Var1", all=T)
names(b)[names(b) == "Utente"] <- "Jacopo"
names(b)[names(b) == "Freq"] <- "Freq.Jacopo"

c<-merge(b, fra, by="Var1", all=T)
names(c)[names(c) == "Utente"] <- "Francesco"
names(c)[names(c) == "Freq"] <- "Freq.Francesco"

d<-merge(c, gia, by="Var1", all=T)
names(d)[names(d) == "Utente"] <- "Gianluca"
names(d)[names(d) == "Freq"] <- "Freq.Gianluca"

e<-merge(d, chi, by="Var1", all=T)
names(e)[names(e) == "Utente"] <- "Chiara"
names(e)[names(e) == "Freq"] <- "Freq.Chiara"

f<-merge(e, ire, by="Var1", all=T)
names(f)[names(f) == "Utente"] <- "Irene"
names(f)[names(f) == "Freq"] <- "Freq.Irene"

g<-merge(f, and, by="Var1", all=T)
names(g)[names(g) == "Utente"] <- "Andrea"
names(g)[names(g) == "freq"] <- "Freq.Andrea"

h<-merge(g, dan, by="Var1", all=T)
names(h)[names(h) == "Utente"] <- "Daniele"
names(h)[names(h) == "Freq"] <- "Freq.Daniele"

i<-merge(h, dani, by="Var1", all=T)
names(i)[names(i) == "Utente"] <- "Dani"
names(i)[names(i) == "Freq"] <- "Freq.Dani"

l<-merge(i, adri, by="Var1", all=T)
names(l)[names(l) == "Utente"] <- "Adriano"
names(l)[names(l) == "Freq"] <- "Freq.Adriano"

m<-merge(l, sim, by="Var1", all=T)
names(m)[names(m) == "Utente"] <- "Simona"
names(m)[names(m) == "Freq"] <- "Freq.Simona"

total = matrix(data = c(m$Freq.Luigi, m$Freq.Lorenzo, m$Freq.Jacopo, m$Freq.Francesco, m$Freq.Gianluca, m$Freq.Chiara, m$Freq.Irene, m$Freq.Andrea, m$Freq.Daniele, m$Freq.Dani, m$Freq.Adriano, m$Freq.Simona), ncol = dim(m)[1], byrow = T)
colnames(total) = m$Var1
rownames(total) = c("Luigi", "Lorenzo","Jacopo", "Francesco", "Gianluca", "Chiara", "Irene", "Andrea", "Daniele", "Dani", "Adriano", "Simona")
total[is.na(total)]=0
View(total)

write.csv2(total,"matrice.csv")
