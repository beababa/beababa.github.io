#### prep des données
#################


# jointure avec le fichier RPLS
rpls <- read.csv("data/fr2000verif.csv", fileEncoding = "UTF-8")
str(rpls)
data <- read.csv2("data/dataMultivariee2.csv", 
                  encoding = "UTF-8", dec = ".", 
                  na.strings = "N/A", 
                  colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric"), skip = 2)
# jointure entre les 2 fichiers avec le code insee
jointure <- merge(rpls, data, by = "Code")
# on a 302 communes, il en manque 37...
# On sauvegarde la donnée
write.csv(jointure, "data/base.csv", fileEncoding = "UTF-8")


# formatage pour l'analyse
data <- read.csv("data/base.csv", fileEncoding = "UTF-8")
str(data)
names(data)
rownames(data) <- data [,2]
data <- data [,c(4,7:10)]
names(data) <- c( "rpls", "nbEnt", "RP", "nonDipl", "IFS")
pairs(data)

# Etude de la donnée, valeurs manquantes, aberrantes etc...
which(row.names(data) == 31555)
data <- data [-42,]

# ...puis centrer - réduire
data_cr <- scale (data)
pairs(data_cr)
write.csv(data_cr, "data/base_cr", fileEncoding = "UTF-8")

# ACP
#####################
acp <- prcomp(data_cr)
# par défaut direction négative on inverse
acp$rotation <- -1 * acp$rotation
acp$rotation
head(acp$x*-1)
biplot(acp, col = c("white", "red"), scale = 0, xlim = c(-2, 2), ylim = c(-2,4))



# Classification
######################
# Centrage et réduction
data.cr <- scale(data, center = T, scale = T)
# matrice des distances entre les individus
data.d <- dist(data.cr)
# classification
cah <- hclust(data.d)
# dendogramme
plot(cah)
# matérialisation des groupes
rect.hclust(cah, k = 3)
# découpage
groupes.cah <- cutree(cah, k = 3)
liste <- sort(groupes.cah)
# matérialiser les groupes par couleur.
acp <- princomp(data.cr, cor = T, scores = T)
par(bg = "lightgrey", mar = c(1,1,1,1))
plot(acp$scores[,1],acp$scores[,2], type = "p")
text(acp$scores[,1],acp$scores[,2],col=c(topo.colors(3))[groupes.cah],cex
     =1,labels=rownames(data))

