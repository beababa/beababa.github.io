#je ne comprends pas pourquoi cela ne fonctionne pas alors que j'ai suivi 
#toutes les étapes de votre cours à la lettre, ainsi que la méthode que vous
#suivi avec un autre groupe, j'ai recommencé des dizaines de fois et je ne
#comprends toujours pas à quel endroit cela coince.


getwd()
data <- read.csv("election2022.csv", fileEncoding = "UTF-8", dec = ",")
names(data)
data <- data[data$INSEE_COM == '94022',]
ind <- grep("Ins", names(data))
pct <- data[,ind]
ind <- grep("Nom", names(data))
noms <- data[1,ind]
names(pct[c(7:18)])
names(pct)[c(7:18)] <- noms
pct <- pct[,c(2,7:18)]
names(pct)[1] <- "abst"
rownames(pct) <- (1:32)
#je ne comprends pas pourquoi cela ne fonctionne pas ici
summary(pct)
cor(pct)
pairs(pct)
#je ne comprends pas non plus ici pourquoi cela ne fonctionne pas
res <- prcomp(pct)
plot(res)
#là non plus
biplot(res)
d.pct <- dist(pct)
cah <- hclust(d.pct)
plot(cah)
#là non plus
plot(cah)
rect.hclust(cah,k=4)
groupes.cah <- cutree(cah,k=4)
print(sort(groupes.cah))
