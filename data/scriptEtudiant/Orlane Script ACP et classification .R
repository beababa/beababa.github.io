getwd ()
data <- read.csv("data/election2022 (1).csv")
names(data)
data <- data[data$INSEE_COM=='94430',]
ind <- grep("Ins",names(data))
pct <- data [,ind]
ind <- grep("Nom",names(data))
noms <- data [1,ind]
names(pct) [c(7:18)]
names(pct) [c(7:18)]<- noms
pct <- pct [,c(2, 7:18)]
names(pct) [1] <- "abst"
rownames(pct) <- c(1:32)
summary(pct)
cor(pct)
pairs(pct)
res <- prcomp(pct)
plot(res)
bipolt (res)
