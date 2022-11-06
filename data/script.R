####SCRIPT R pour cours VARIANCE

# Le fichier total fait plus de 113 M lignes.
# lecture du fichier. Quelle remarque faire ?

data <-  read.csv("data/total.csv", fileEncoding = "UTF-8")

# concaténation zone et nom ville
data$loc <- paste0(data$zone,"_", data$LIBCOM)
str(data)
# dénombrement
tab <- table(data$loc,data$FINANcode)
# on met les marges
tab <- addmargins(tab,2)
# on affiche
knitr::kable(tab)
# calcul
rapport <- tab [,1]/ tab[,4]
# création des 2 vecteurs
zone <- substring(names(rapport), 1,2)
ville <- substring(names(rapport),4,25)
# constitution tableau
data <- data.frame(ville, zone, rapport)
# boites à mpustacjes
boxplot(data$rapport~data$zone)
# avec un peu de présentation
# On multiplie  par 100 le rapport pour avoir un %
data$rapport <- data$rapport * 100
boxplot(data$rapport~data$zone, xlab ="Zone", ylab="% logement intermédiaire",
        col=rainbow(8))

# La variance

tapply(data$rapport, data$zone,var)
# verif pour le cas du 77 par exemple
dataSel <- data [data$zone == 'ee',]
mean((dataSel$rapport - mean(dataSel$sum))^2)

##### calcul de la variance
data$zone <- as.factor(data$zone)
modele <- lm (tot ~ zone, data = data)
anova(modele)









