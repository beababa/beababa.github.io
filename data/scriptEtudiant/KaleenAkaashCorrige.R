getwd ()
head(data)
data <- read.csv("data/election2022.csv", dec = ",")
names(data)
data <- data[data$INSEE_COM=='92073',]
# 32 BUREAUX
ind <- grep("Ins",names(data))
pct <- data [,ind]
ind <- grep("Nom",names(data))
noms <- data [1,ind]
names(pct) [c(7:18)]
names(pct) [c(7:18)]<- noms
pct <- pct [,c(2, 7:18)]
names(pct) [1] <- "abst"
rownames(pct) <- c(1:32)
# CAR IL Y A 32 BUREAUX
summary(pct)
cor(pct)
pairs(pct)
# ON VOIT VOTE LE PEN TRES FORT SUR 3 BUREAUX
hist(pct$`LE PEN`)
pct [pct$`LE PEN`> 12,]
# bureau 29 -32
res <- prcomp(pct)
plot(res)
biplot (res)
# pas de relation LE PEN MELENCHON
# LE PEN IDENTIQUE A L'ABSTENTION
# MACRON TRES REPRESENTE
# on retrouve les 4 bureaux le pen mais 29 /30 et 31 32
d.pct <- dist(pct)
cah <- hclust(d.pct)
plot(cah)


plot(cah)
rect.hclust(cah,k=4)
groupes.cah <- cutree(cah,k=4)
#liste des groupes
print(sort(groupes.cah))

# les groupes 31 32 hors 29 30 2 20

pct2 <- pct [ c(29, 2, 31),]
mat <- as.matrix(pct2)
barplot(mat, las =2, col =terrain.colors(3), beside = T)
# principale différence l'abstention.