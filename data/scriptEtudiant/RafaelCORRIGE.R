getwd()
#On obtient le chemin d'accès
data <- read.csv("data/election2022.csv", fileEncoding = "UTF-8", dec = ",")
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
# NUMEROTATION DES BUREAUX DE VOTE
rownames(pct)<- seq(1:17)

#On affiche un résumé statistique pour chaque candidat
cor(pct)
#On compare les variances 2 à 2
pairs(pct)
# UNE VALEUR EXTREME POUR ARTHAUD MAIS ON LA GARDE CAR AMPLITUDE RESTE FAIBLE.
hist(pct$ARTHAUD)
pct
#On fait des graphiques avec ces comparaisons
res <- prcomp(pct)
#On réalise l'analyse en composante principale
plot(res)
# LES DEUX PREMIERS FACTEURS EXPLIQUENT PLUS DE 80 % DES VARIATIONS
#On le montre en graphique
biplot(res)
#On obtient le graphique d'analyse multivariée désirée
#On voit tout de suite le rapport entre l'abstention et Macron 
#Mais aussi entre Mélenchon et Le Pen
# LE PEN PEU REPRESENTE
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
# 9, 3, ET 17 ONT BCP VOTE POUR MELEHCHON
pct9_3_17 <- pct [c(3,9, 17),]
mat <- as.matrix(pct9_3_17)
barplot(mat, las = 2, col = terrain.colors(3))
# LES BUREAUX SE RESSEMBLENT BEAUCOUP 