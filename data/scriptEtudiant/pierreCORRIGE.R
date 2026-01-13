getwd()
data <- read.csv("election2022.csv", fileEncoding = "UTF-8", dec = ",")
names(data)
#l'on a identifier notre emplacement et donner une nomination plus 
#simple au fichier que l'on va lire 
data <- data [data$INSEE_COM == '89024',]
ind <- grep("Ins", names(data))
pct <- data [, ind]
ind <- grep("Nom", names(data))
noms <- data [1,ind]
names(pct) [c(7:18)]
#ici j'ai isoler ma commune suite à cela j'ai donné à divers indicateur comme le 
#pourcentage une appelation dans mon code, ensuite j'ai isoler des bureau de vote (7 18)

names(pct) [c(7:18)] <- noms
pct <- pct [,c(2, 7:18)]
names(pct) [1] <- "abst"
#Ici j'ai donné à la valeur 1 pour les nom les resultat de l'abstention
rownames(pct) <- c(1:32)
#la ligne au-dessus ne marche pas je ne sais pas pourquoi et les lignes ci-dessus servent à 
#renvoyer les résultats de ce qui a été attribué au dessus sous forme de tableau par-exemple
summary(pct)

cor(pct)

pairs(pct)

res <- prcomp(pct)
plot(res)

biplot(res)

# matrice des distances dans les lignes qui suivent j'ai des données que je ne comprend pas 
#pouvez vous m'expliquez la signification du code ci-dessous merci
d.pct <- dist(pct)
cah <- hclust(d.pct)
plot(cah)

plot(cah)
rect.hclust(cah,k=4)

groupes.cah <- cutree(cah,k=4)
#liste des groupes
print(sort(groupes.cah))

