getwd ()
head(data)
data <- read.csv("data/election2022.csv", dec = ",")
names(data)
data <- data[data$INSEE_COM=='17299',]
# 20 BUREAUX
ind <- grep("Ins",names(data))
pct <- data [,ind]
ind <- grep("Nom",names(data))
noms <- data [1,ind]
names(pct) [c(7:18)]
names(pct) [c(7:18)]<- noms
pct <- pct [,c(2, 7:18)]
names(pct) [1] <- "abst"
rownames(pct) <- c(1:20)
# CAR IL Y A  BUREAUX
summary(pct)
cor(pct)
pairs(pct)
# ON VOIT valeur aberrante abstention
hist(pct$abst)
pct [pct$abst> 40,]
# bureau 3
res <- prcomp(pct)
plot(res)
# seulement 50 % des variations expliqués par les 2 premiers facteurs
biplot (res)
#  LE PEN MELENCHON et MACRON ABST
# ABSTENTION TRES IMPORTANTE
# MACRON TRES REPRESENTE
# bureau 3 se démarque par rapport à l'abstention
d.pct <- dist(pct)
cah <- hclust(d.pct)
plot(cah)


plot(cah)
rect.hclust(cah,k=4)
groupes.cah <- cutree(cah,k=4)
#liste des groupes
print(sort(groupes.cah))

# orginalité du bureau 3

pct2 <- pct [ c(3, 17),]
mat <- as.matrix(pct2)
barplot(mat, las =2, col =terrain.colors(2), beside = T)
# principale différence l'abstention. Mais le vote Le Pen identique. 