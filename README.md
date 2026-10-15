# Production et consommation d'électricité en France (2000-2025) : modélisation univariée et multivariée

Projet de séries temporelles Master 1 EEET, Université Paris-Saclay, 2025-2026. 

## Objectif
Modéliser et prévoir la production mensuelle d'électricité renouvelable en France, puis étudier ses interactions avec la consommation et la production nucléaire. 

## Données
Séries mensuelles RTE, janvier 2000 à novembre 2025 (311 observations) :
- Production d'électricité renouvelable (hydraulique, éolien, solaire, bioénergies)
- Consommation d'électricité corrigée des aléas climatiques
- Production nucléaire

## Méthode
**Partie univariée**
- Tests de stationnarité (Dickey-Fuller augmenté, Phillips-Perron, KPSS)
- Identification par ACF/PACF et estimation d'un SARIMA(1,0,0)(0,1,1)[12] avec tendance déterministe
- Validation des résidus (Ljung-Box, Shapiro-Wilk) et prévisions sur et hors échantillon (horizon 24 mois)

**Partie multivariée**
- Modèle VAR(1) sur les trois séries
- Causalité de Granger, exogénéité, fonctions de réponse impulsionnelle, décomposition de la variance
- Prévisions et comparaison avec le modèle univarié

## Principaux résultats
- Le SARIMA prévoit la production renouvelable avec une erreur moyenne (MAPE) de 3,81 %
- Le VAR prévoit très bien la consommation (MAPE de 0,83 %) mais moins bien la production renouvelable (9,49 %)
- À l'échelle mensuelle, les filières renouvelable et nucléaire évoluent de façon largement autonome : un modèle plus riche n'améliore pas forcément la prévision

## Contenu du dépôt
- `[nom_du_fichier].pdf` : rapport complet
- `[nom_du_fichier].R` : script R (tests, estimation des modèles, validation et prévisions)

## Outils
R (`forecast`, `tseries`, `urca`, `vars`)
