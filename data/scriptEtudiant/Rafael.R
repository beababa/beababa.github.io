getwd()
#On obtient le chemin d'accès
data <- read.csv("election2022.csv", fileEncoding = "UTF-8", dec = ",")
#On lit le csv
names(data)
#On visualise les variables
data <- data [data$INSEE_COM == '95582',]
#On pose le filtre pour notre commune : Sannois
ind <- grep("Ins", names(data))
#On cherche le motif "Ins" dans les variables
pct <- data [, ind]
ind <- grep("Nom", names(data))
#On cherche le motif "Nom" dans les variables
noms <- data [1,ind]
names(pct) [c(7:18)]
#Il ne nous reste que les variables du pourcentage de vote parmi les inscrits
names(pct) [c(7:18)] <- noms
pct <- pct [,c(2, 7:18)]
names(pct) [1] <- "abst"
#On désigne l'abstention comme un candidat
summary(pct)
#On affiche un résumé statistique pour chaque candidat
cor(pct)
#On compare les variances 2 à 2
pairs(pct)
#On fait des graphiques avec ces comparaisons
res <- prcomp(pct)
#On réalise l'analyse en composante principale
plot(res)
#On le montre en graphique
biplot(res)
#On obtient le graphique d'analyse multivariée désirée
#On voit tout de suite le rapport entre l'abstention et Macron 
#Mais aussi entre Mélenchon et Le Pen
#Les petits candidats se confondent
d.pct <- dist(pct)
#Calcul de la distance entre les points
cah <- hclust(d.pct)
#Classement de ces points en cluster
plot(cah)
#Montre le dendrogramme
rect.hclust(cah,k=4)
#Montre 4 groupes sur le dendrogramme pour des bureaux de vote similaires
groupes.cah <- cutree(cah,k=4)
#Fait une liste des groupes
print(sort(groupes.cah))
#Montre les groupes