###########################################
###########################################
###TP_NOTÉ SCRIPT SENECHAL MORGAN M1 BDML##
###########################################
###########################################


###########################################
###########################################
#######description du jeu de données#######
###########################################
###########################################
# Afficher les premières lignes du jeu de données
head(prostate.dataset)

# Afficher le nombre total d'observations
cat("Nombre d'observations :", nrow(prostate.dataset), "\n")

# Afficher un résumé statistique des variables
cat("Résumé statistique des variables :\n")
print(summary(prostate.dataset))

# Calculer des statistiques descriptives pour la variable cible psa
cat("Statistiques descriptives pour la variable cible psa :\n")
print(summary(prostate.dataset$psa))

# Calculer le coefficient de corrélation entre psa et les autres variables
correlations <- cor(prostate.dataset)[,"psa"]
correlations <- correlations[-length(correlations)]  # Exclure la corrélation de psa avec elle-même

# Afficher les coefficients de corrélation
cat("Coefficients de corrélation entre psa et les autres variables :\n")
print(correlations)

# Identifier la variable la plus corrélée avec psa
max_corr_var <- names(which.max(abs(correlations)))
cat("La variable la plus corrélée avec psa est :", max_corr_var, "avec un coefficient de corrélation de", round(correlations[max_corr_var], 3), "\n")

# Tracer le nuages de points
plot(prostate.dataset,main="Scatter plot of prostate.dataset")

# Appliquer la transformation logarithmique aux variables spécifiées
prostate.dataset$lvol <- log(prostate.dataset$vol)
prostate.dataset$lwht <- log(prostate.dataset$wht)
prostate.dataset$lbh <- log(prostate.dataset$bh)
prostate.dataset$lpc <- log(prostate.dataset$pc)
prostate.dataset$lpsa <- log(prostate.dataset$psa)

# Supprimer les anciennes colonnes
prostate.dataset$vol <- NULL
prostate.dataset$wht <- NULL
prostate.dataset$bh <- NULL
prostate.dataset$pc <- NULL
prostate.dataset$psa <- NULL

# Créer un scatterplot matrix avec les variables transformées
pairs(prostate.dataset, main = "Scatterplot Matrix with Log-transformed Variables")


###########################################
###########################################
####Analyse en composantes principales#####
###########################################
###########################################

# Calculer la variance de chaque variable
variances <- apply(prostate.dataset, 2, var)

# Afficher les variances
print(variances)

# Fonction de normalisation
normalize_data <- function(data){
  return((data - mean(data)) / sd(data))
}
# Normalisation des données
prostate.dataset.normalized <- as.data.frame(lapply(prostate.dataset, normalize_data))
# Afficher le résumé statistique des données normalisées pour vérification
print(summary(prostate.dataset.normalized))
# Créer un scatterplot matrix avec les variables normalisées
pairs(prostate.dataset.normalized, main = "Scatterplot Matrix with Normalized Variables")



#Installer le package FactoMiner
install.packages("FactoMineR")
#Utiliser le Package
library(FactoMineR)

#Application de la fonction PCA
res.pca <- PCA(prostate.dataset, scale.unit=TRUE)
print(res.pca$eig)

#Graphique des individus
plot(res.pca, choix="ind", habillage="cos2")

#Graphique des variable
plot(res.pca, choix="var", habillage="cos2")

#Graphique des valeurs propres
barplot(res.pca$eig[, 2], names.arg=1:nrow(res.pca$eig), main="Scree plot", ylab="Percentage of variance", xlab="Principal components")

#summary(res.pca)

# Calcul du PVE
pve <- res.pca$eig[, 2]
# Créer un graphique en barres pour le PVE
barplot(pve, 
        main="PVE par composant", 
        ylab="Pourcentage de Variance Expliquée", 
        xlab="Composantes Principales", 
        ylim=c(0, max(pve) + 10), 
        col="lightblue", 
        border="blue")

# Ajouter le texte au-dessus des barres pour indiquer le pourcentage exact
text(x=seq_along(pve), y=pve + 2, labels=round(pve, 2), cex=0.8)

# Calcul du PVE cumulé
cumulative_pve <- cumsum(pve)

# Créer un graphique pour le PVE cumulé
plot(cumulative_pve, 
     type="b", 
     pch=19, 
     col="red", 
     lwd=2, 
     xlab="Composantes Principales", 
     ylab="Pourcentage de Variance Expliquée Cumulée", 
     main="PVE Cumulé", 
     ylim=c(0, 100))

# Ajouter le texte au-dessus des points pour indiquer le pourcentage exact cumulé
text(x=seq_along(cumulative_pve), y=cumulative_pve + 2, labels=round(cumulative_pve, 2), cex=0.8)

# Calcul du PVE
pve <- (res.pca$eig[, 1] / sum(res.pca$eig[, 1])) * 100

# Afficher le PVE pour chaque composante
print(pve)

###########################################
###########################################
############Régression linéaire############
###########################################
###########################################

#Correlation lpsa avec tout les autre variables
correlations <- cor(prostate.dataset.normalized)[, "lpsa"]
correlations

#estimation des coeficients avec lpsa et lvol
model <- lm(lpsa ~ lvol, data=prostate.dataset.normalized)
model_summary <- summary(model)
model_summary$coefficients

#Test hypothèse de pente nulle pour le coefficient B1
# Ajuster le modèle de régression linéaire
model <- lm(lpsa ~ lvol, data=prostate.dataset.normalized)

# Obtenir un résumé du modèle
model_summary <- summary(model)

# Afficher le récapitulatif des coefficients
model_summary$coefficients

# Extraire la valeur p pour lvol
p_value <- coef(model_summary)["lvol", "Pr(>|t|)"]

# Tester l'hypothèse
if (p_value < 0.05) {
  cat("Nous rejetons l'hypothèse nulle. Le coefficient pour lvol est significativement différent de 0.\n")
} else {
  cat("Nous ne parvenons pas à rejeter l’hypothèse nulle. Il n'y a pas suffisamment de preuves pour suggérer que le coefficient de lvol est différent de 0.\n")
}

#Coefficient R2
model_summary$r.squared

#Coefficient R2 ajusted
model_summary$adj.r.squared

