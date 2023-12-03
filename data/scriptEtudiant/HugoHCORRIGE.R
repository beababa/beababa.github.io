#je ne comprends pas pourquoi cela ne fonctionne pas alors que j'ai suivi 
#toutes les étapes de votre cours à la lettre, ainsi que la méthode que vous
#suivi avec un autre groupe, j'ai recommencé des dizaines de fois et je ne
#comprends toujours pas à quel endroit cela coince.

# QUAND CELA NE FONCTIONNE PAS ?


getwd()
data <- read.csv("data/election2022.csv", fileEncoding = "UTF-8", dec = ",")
names(data)
data <- data[data$INSEE_COM == '94022',]
# 22 BV SUR LA COMMUNE CHOISIE
ind <- grep("Ins", names(data))
pct <- data[,ind]
# FILTRAGE COLONNES PCT INSCRITS POUR TABLEAU NEAU
ind <- grep("Nom", names(data))
noms <- data[1,ind]
# LES 12 CANDIDATS
names(pct[c(7:18)])
names(pct)[c(7:18)] <- noms
# ATTRIBUTION DES NOMS AUX PCT DE VOIX
pct
pct <- pct[,c(2,7:18)]
# FILTRE COLONNES ABST ET PCT DE VOIX
names(pct)[1] <- "abst"
# ON RENOMME LES COLONNES EN FONCTION DU NB DE BUREAUX
rownames(pct) <- (1:22)
#je ne comprends pas pourquoi cela ne fonctionne pas ici
# C'EST BIZARRE, CELA FONCTIONNE POURTANT
summary(pct)
# CELA FONCTIONNE
cor(pct)
pairs(pct)
# ATTENTION VALEUR ABERRANTE EN ABSTENTION
hist(pct$abst)
# PEUT PASSER.
#je ne comprends pas non plus ici pourquoi cela ne fonctionne pas
res <- prcomp(pct)
plot(res)
# 60 % VARIATION EXPLIQUE PAR LES 2 PREMIERS FACTEURS
#là non plus
biplot(res)
# MACRON <> MELENCHON + ABSTENTION
d.pct <- dist(pct)
cah <- hclust(d.pct)
plot(cah)
#là non plus
plot(cah)
rect.hclust(cah,k=4)
groupes.cah <- cutree(cah,k=4)
print(sort(groupes.cah))
#BUREAU 2 3 7 ABSTENTION ELEVEE
pct_2_3_7 <- pct [c(2,3,7),]
mat <- as.matrix(pct_2_3_7)
boxplot(t(mat))
# LA REPARTITION DES VOIX PARAIT TRES SEMBLABLE DANS LES 3 BUREAUX !