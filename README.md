# RegressionLineaireMultiple
Validité d’un modèle linéaire multiple

library(readxl)
cig <- read_excel("cigarettes.xlsx")
View(cig)

#Staistiques descriptives simples
print(summary(cig))

#Nuages de points deux à deux
pairs(cig[,2 :4], col="red",main="Le nuage des points deux à deux variables")


#Régression linéaire multiple
modele <- lm(CO~GOUDRON+NICOTINE+POIDS, data = cig)
print(modele)

#Objet summary
sm <- summary(modele)
print(sm)

#Coefficients
print(sm$coefficients)

# Écarts-type des coefficients estimés
print(sm$coefficients[,2])

# Résidus
e <- modele$residuals #ou encore e <- residuals(modele)
print(mean(e))

# Graphique des résidus
plot(cig$CO,e,ylab="Résidu",xlab="CO") abline(h=0)

# Résidus studentisés
res.student <- rstudent(modele)
print(res.student)

# Seuil critique
#risque alpha = 0.1
alpha <- 0.1
#calcul du seuil à partir de la loi de Student à (n-p-2) ddl
seuil.student <- qt(1-alpha/2,19-3-2)
print(seuil.student)

atypiques.rstudent <- (res.student < -seuil.student | res.student > +seuil.student)
ab.student <- cig[atypiques.rstudent,]
print(ab.student)

plot(cig$CO,res.student,cex=0.75)
abline(h=-seuil.student)
abline(h=+seuil.student) abline(h=0) text(cig$CO[atypiques.rstudent],res.student[atypiques.rstudent],
                                          rownames(cig)[atypiques.rstudent])

#Levier
indicateurs <- influence.measures(modele)
# Descripteurs disponibles
attributes(indicateurs)
res.hat <- indicateurs$infmat[,"hat"]
print(res.hat)

#le seuil est défini par 2(p+1)/n.
seuil.hat <-
  print(seuil.hat)
#Les points atypiques au sens du levier
atypiques.levier <- (res.hat > seuil.hat)
ab.hat <- rats[atypiques.levier,]
print(ab.hat)
