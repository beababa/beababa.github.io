getwd ()
head(data)
data <- read.csv("data/election2022.csv", dec = ",")
# NE PAS OUBLIER DE METTRE LA DECIMALE
names(data)
sel <- data[data$INSEE_COM=='94430',]
# PAS DE VILLE 94 430, il s'agit du code postal le code insee est 94019
data <- data[data$INSEE_COM== '94019',]
# 12 BUREAUX
ind <- grep("Ins",names(data))
pct <- data [,ind]
ind <- grep("Nom",names(data))
noms <- data [1,ind]
names(pct) [c(7:18)]
names(pct) [c(7:18)]<- noms
pct <- pct [,c(2, 7:18)]
names(pct) [1] <- "abst"
rownames(pct) <- c(1:12)
# CAR IL Y A 12 BUREAUX
summary(pct)
cor(pct)
pairs(pct)
res <- prcomp(pct)
plot(res)
bipolt (res)
biplot(res)
#JE TE LAISSE L'EXPLIQUER