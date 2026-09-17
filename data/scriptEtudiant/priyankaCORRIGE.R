#DM
#lecture d'un fichier
getwd()
dm <- read.csv("data/election2022.csv",fileEncoding = "UTF-8",dec=",")
names(dm)
# ATTENTION AU NOM DE VARIABLE 
str(dm)               
#filtre sur la commune
data <-dm [dm$INSEE_COM==14514,]
#Chercher les inscrits dans data
ind <- grep("Ins",names(data))
#Exprimer les valeurs en pourcentage
pct <- data[,ind]
#Chercher la valeur nom dans la data
ind <- grep("Nom",names(data))
#assigner les valeurs de la colonne 1 aux noms
noms <- data[1,ind]
#pourcentages obtenus par les candidats
names(pct)[c(7:18)]
#Assigner les valeurs aux noms
names(pct)[c(7:18)] <- noms
#Filtre sur les colonnes
pct <- pct[,c(2,7:18)]
#assigner abstention aux noms de la colonne 1
names(pct)[1] <- "abst"
#Transposer les noms des candidats aux titres de lignes
rownames(pct) [c(1:32)]
summary(pct)
cor(pct)
pairs(pct)
res <- prcomp(pct)
#visualiser les donn?es 1 axe
plot(res)
#visualiser donn?es 2 axes
biplot(res)
