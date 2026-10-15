# PROJET SÉRIE TEMPORELLE - Baptiste BILLAULT - Rémi GERARD - Oliver NDOUNTOUMEBE 

# Nettoyage de l'environnement
rm(list = ls())

# Chargement des packages
# install.packages(c("readxl", "forecast", "tseries", "urca", "vars"))

library(readxl)
library(forecast)
library(tseries)
library(urca)
library(vars)


# CHARGEMENT DES DONNÉES (3 séries dans un seul fichier Excel, onglet DONNÉES)

data <- read_excel("DataST.xlsx", sheet = "DONNÉES")

# Conversion en séries temporelles mensuelles (début : janvier 2000)
enr   <- ts(data$`Total ENR (TWh)`,              start = c(2000, 1), frequency = 12)
nuke  <- ts(data$`Production nucléaire (TWh)`,   start = c(2000, 1), frequency = 12)
conso <- ts(data$`Consommation corrigée (TWh)`,  start = c(2000, 1), frequency = 12)


# PARTIE I : MODÉLISATION UNIVARIÉE (SARIMA sur la production ENR)


#Visualisation de la série brute
plot(enr, main = "Production mensuelle ENR (TWh)", 
     ylab = "TWh", col = "darkgreen", lwd = 1.5)


# Transformation log pour stabiliser la variance
log_enr <- log(enr)
plot(log_enr, main = "log(Production ENR)")


# Analyse graphique de la stationnarité
acf(ts(log_enr, frequency = 1), lag.max = 36)
pacf(ts(log_enr, frequency = 1), lag.max = 36)


# Différenciation saisonnière (s=12) 
log_enr_sai <- diff(log_enr, lag = 12)
acf(ts(log_enr_sai, frequency = 1), lag.max = 36)


# Procédure séquentielle DF (Modèle 3 : constante + tendance)
# Résultat : tau3 = -8.16 -> rejet H0, trend significatif -> série TS
summary(ur.df(log_enr_sai, type = "trend", lags = 0))


# Retrait de la tendance par régression OLS (car série TS)
t <- 1:length(log_enr_sai)
modele_trend <- lm(log_enr_sai ~ t)
serie_detrend <- ts(residuals(modele_trend),
                    start = start(log_enr_sai),
                    frequency = 12)
plot(serie_detrend)


# Tests de stationnarité complémentaires
adf.test(serie_detrend)    # H0 : racine unitaire
pp.test(serie_detrend)     # H0 : racine unitaire
kpss.test(serie_detrend)   # H0 : stationnarité (inversé)


# Identification du modèle SARIMA via ACF/PACF
acf(ts(serie_detrend, frequency = 1), lag.max = 36)
pacf(ts(serie_detrend, frequency = 1), lag.max = 36)
# Modèle retenu : SARIMA(1,0,0)(0,1,1)[12]


# Estimation du modèle avec tendance déterministe (xreg = t)
t <- 1:length(log_enr)
modele <- Arima(log_enr, 
                order = c(1, 0, 0),
                seasonal = list(order = c(0, 1, 1), period = 12),
                xreg = t)
modele


# Validation : analyse des résidus
residus <- residuals(modele)
plot(residus)
acf(ts(residus, frequency = 1), lag.max = 36)

# Autocorrélation (H0 : non-autocorrélés)
Box.test(residus, lag = 20, type = "Ljung-Box")

# Homoscédasticité (H0 : variance constante)
Box.test(residus^2, lag = 12, type = "Ljung-Box")

# Normalité (H0 : résidus normaux)
shapiro.test(residus)
qqnorm(residus); qqline(residus)


# Prévisions sur échantillon (12 derniers mois) 
serie_ent <- window(log_enr, end = c(2024, 10))
serie_sur <- window(log_enr, start = c(2024, 11))

# Ré-estimation sur l'échantillon d'entraînement
t_ent <- 1:length(serie_ent)
modele_ent <- Arima(serie_ent,
                    order = c(1, 0, 0),
                    seasonal = list(order = c(0, 1, 1), period = 12),
                    xreg = t_ent)

# Prévision sur 12 mois
t_sur <- (length(serie_ent) + 1):(length(serie_ent) + length(serie_sur))
prev_in <- forecast(modele_ent, h = length(serie_sur), xreg = t_sur)

# Continuité visuelle : on préfixe la dernière valeur observée
prev_mean_complet  <- ts(c(tail(serie_ent, 1), as.numeric(prev_in$mean)),
                         start = c(2024, 10), frequency = 12)
prev_lower_complet <- ts(c(tail(serie_ent, 1), as.numeric(prev_in$lower[,2])),
                         start = c(2024, 10), frequency = 12)
prev_upper_complet <- ts(c(tail(serie_ent, 1), as.numeric(prev_in$upper[,2])),
                         start = c(2024, 10), frequency = 12)

# Graphique en TWh (retour à l'échelle originale avec exp())
plot(exp(log_enr), ylab = "TWh", main = "Prévisions sur échantillon (TWh)",
     col = "black", lwd = 1, xlim = c(2000, 2026))
lines(exp(prev_mean_complet),  col = "red",  lwd = 2)
lines(exp(serie_sur),          col = "blue", lwd = 1)
lines(exp(prev_lower_complet), col = "gray", lty = 2)
lines(exp(prev_upper_complet), col = "gray", lty = 2)
legend("topleft", 
       legend = c("Historique", "Prévisions", "Vraies valeurs", "IC 95%"),
       col = c("black", "red", "blue", "gray"), 
       lwd = c(1, 2, 2, 1), lty = c(1, 1, 1, 2))

# Zoom sur 2023-2026
plot(window(exp(log_enr), start = c(2023, 1)), ylab = "TWh",
     main = "Prévisions sur échantillon - Zoom (TWh)",
     col = "black", lwd = 1, xlim = c(2023, 2026))
lines(exp(prev_mean_complet),  col = "red",  lwd = 2)
lines(exp(serie_sur),          col = "blue", lwd = 1)
lines(exp(prev_lower_complet), col = "gray", lty = 2)
lines(exp(prev_upper_complet), col = "gray", lty = 2)
legend("topleft", 
       legend = c("Historique", "Prévisions", "Vraies valeurs", "IC 95%"),
       col = c("black", "red", "blue", "gray"), 
       lwd = c(1, 2, 2, 1), lty = c(1, 1, 1, 2), cex = 0.7)

# Qualité des prévisions
accuracy(prev_in, serie_sur)


# Prévisions hors échantillon (24 mois futurs) 
t_futur <- (length(log_enr) + 1):(length(log_enr) + 24)
prev_out <- forecast(modele, h = 24, xreg = t_futur)

prev_mean_out  <- ts(c(tail(log_enr, 1), as.numeric(prev_out$mean)),
                     start = c(2025, 11), frequency = 12)
prev_lower_out <- ts(c(tail(log_enr, 1), as.numeric(prev_out$lower[,2])),
                     start = c(2025, 11), frequency = 12)
prev_upper_out <- ts(c(tail(log_enr, 1), as.numeric(prev_out$upper[,2])),
                     start = c(2025, 11), frequency = 12)

plot(exp(log_enr), ylab = "TWh", main = "Prévisions hors échantillon (TWh)",
     col = "black", lwd = 1, xlim = c(2000, 2028))
lines(exp(prev_mean_out),  col = "red",  lwd = 1)
lines(exp(prev_lower_out), col = "gray", lty = 2)
lines(exp(prev_upper_out), col = "gray", lty = 2)
legend("topleft", legend = c("Historique", "Prévisions", "IC 95%"),
       col = c("black", "red", "gray"), lwd = c(1, 2, 1), lty = c(1, 1, 2))

# Zoom
plot(window(exp(log_enr), start = c(2023, 1)), ylab = "TWh",
     main = "Prévisions hors échantillon - Zoom (TWh)",
     col = "black", lwd = 1, xlim = c(2023, 2028))
lines(exp(prev_mean_out),  col = "red",  lwd = 2)
lines(exp(prev_lower_out), col = "gray", lty = 2)
lines(exp(prev_upper_out), col = "gray", lty = 2)
legend("topleft", legend = c("Historique", "Prévisions", "IC 95%"),
       col = c("black", "red", "gray"), lwd = c(1, 2, 1), lty = c(1, 1, 2), cex = 0.7)

accuracy(prev_out)


# PARTIE II : MODÉLISATION MULTIVARIÉE (VAR sur ENR, Conso, Nucléaire)


# Visualisation des 3 séries
par(mfrow = c(3, 1), mar = c(3, 4.5, 2.5, 1))

plot(enr,   main = "Production ENR (TWh)",             ylab = "TWh", 
     col = "green4", lwd = 1.2); grid(col = "gray90")
plot(conso, main = "Consommation totale corrigée (TWh)", ylab = "TWh", 
     col = "blue3",  lwd = 1.2); grid(col = "gray90")
plot(nuke,  main = "Production nucléaire (TWh)",       ylab = "TWh", 
     col = "red3",   lwd = 1.2); grid(col = "gray90")


# Graphique base 100 pour comparer les dynamiques
par(mfrow = c(1, 1), mar = c(4, 4.5, 3, 1))

enr_norm   <- enr / enr[1] * 100
conso_norm <- conso / conso[1] * 100
nuke_norm  <- nuke / nuke[1] * 100

plot(conso_norm, main = "Évolution comparée (base 100 = janvier 2000)", 
     ylab = "Indice base 100", 
     ylim = c(min(enr_norm, conso_norm, nuke_norm), 
              max(enr_norm, conso_norm, nuke_norm)),
     col = "blue3", lwd = 1.5)
lines(nuke_norm, col = "red3",   lwd = 1.5)
lines(enr_norm,  col = "green4", lwd = 1.5)
abline(h = 100, lty = 2, col = "gray50")
grid(col = "gray90")
legend("topleft", 
       legend = c("Consommation corrigée", "Production nucléaire", "Production ENR"),
       col = c("blue3", "red3", "green4"), 
       lwd = 2, cex = 0.8, bg = "white")


# Transformation log des 3 séries
log_enr   <- log(enr)
log_conso <- log(conso)
log_nuke  <- log(nuke)

par(mfrow = c(3, 1), mar = c(3, 4.5, 2.5, 1))
plot(log_enr,   main = "log(Production ENR)",           col = "green4", lwd = 1.2)
plot(log_conso, main = "log(Consommation corrigée)",    col = "blue3",  lwd = 1.2)
plot(log_nuke,  main = "log(Production nucléaire)",     col = "red3",   lwd = 1.2)


# ACF et PACF des 2 nouvelles séries
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
acf(ts(log_conso, frequency = 1),  lag.max = 36, main = "ACF - log(Conso)")
pacf(ts(log_conso, frequency = 1), lag.max = 36, main = "PACF - log(Conso)")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
acf(ts(log_nuke, frequency = 1),   lag.max = 36, main = "ACF - log(Nucléaire)")
pacf(ts(log_nuke, frequency = 1),  lag.max = 36, main = "PACF - log(Nucléaire)")


# Différenciation saisonnière
log_enr_sai   <- diff(log_enr,   lag = 12)
log_conso_sai <- diff(log_conso, lag = 12)
log_nuke_sai  <- diff(log_nuke,  lag = 12)


# Procédure séquentielle DF 
# Consommation - Modèle 3 : tau3 = -5.48 -> rejet H0, trend significatif -> TS
summary(ur.df(log_conso_sai, type = "trend", lags = 12, selectlags = "AIC"))

# Nucléaire - Modèle 3 : tau3 = -4.84 -> rejet H0 mais trend non significatif
summary(ur.df(log_nuke_sai, type = "trend", lags = 12, selectlags = "AIC"))

# Nucléaire - Modèle 2 : tau2 = -4.87 -> rejet H0 -> série I(0)
summary(ur.df(log_nuke_sai, type = "drift", lags = 12, selectlags = "AIC"))


# Tests complémentaires ADF, PP, KPSS 
# Consommation
adf.test(log_conso_sai)
pp.test(log_conso_sai)
kpss.test(log_conso_sai)
kpss.test(log_conso_sai, null = "Trend")   # KPSS avec trend (série TS)

# Nucléaire
adf.test(log_nuke_sai)
pp.test(log_nuke_sai)
kpss.test(log_nuke_sai)


#Sélection du nombre de retards p
Y <- cbind(log_enr_sai, log_conso_sai, log_nuke_sai)
colnames(Y) <- c("ENR", "Conso", "Nuke")

VARselect(Y, lag.max = 12, type = "both")
# AIC et FPE -> p=12, HQ et SC -> p=1
# Choix retenu : p=1 (parcimonie, BIC)


# Estimation du VAR(1)
# type = "both" : constante + tendance déterministe (séries TS)
var_model <- VAR(Y, p = 1, type = "both")
summary(var_model)


#Validation des résidus

# Autocorrélation (Ljung-Box multivarié)
serial.test(var_model, lags.pt = 16, type = "PT.adjusted")

# Comparaison avec un VAR(12)
var_12 <- VAR(Y, p = 12, type = "both")
serial.test(var_12, lags.pt = 16, type = "PT.adjusted")

# Normalité (Jarque-Bera multivarié)
normality.test(var_model, multivariate.only = TRUE)

# Homoscédasticité (ARCH multivarié)
arch.test(var_model, lags.multi = 5)


#Causalité de Granger (tests par paires)

# ENR comme cause
Y_ec <- cbind(log_enr_sai, log_conso_sai)
colnames(Y_ec) <- c("ENR", "Conso")
var_ec <- VAR(Y_ec, p = 1, type = "both")
causality(var_ec, cause = "ENR")$Granger

Y_en <- cbind(log_enr_sai, log_nuke_sai)
colnames(Y_en) <- c("ENR", "Nuke")
var_en <- VAR(Y_en, p = 1, type = "both")
causality(var_en, cause = "ENR")$Granger

# Conso comme cause
causality(var_ec, cause = "Conso")$Granger

Y_cn <- cbind(log_conso_sai, log_nuke_sai)
colnames(Y_cn) <- c("Conso", "Nuke")
var_cn <- VAR(Y_cn, p = 1, type = "both")
causality(var_cn, cause = "Conso")$Granger

# Nucléaire comme cause
causality(var_en, cause = "Nuke")$Granger
causality(var_cn, cause = "Nuke")$Granger


# Exogénéité instantanée (corrélation contemporaine des résidus)
# Test symétrique : 3 paires = 3 tests
causality(var_ec, cause = "ENR")$Instant    # ENR <-> Conso
causality(var_en, cause = "ENR")$Instant    # ENR <-> Nuke
causality(var_cn, cause = "Conso")$Instant  # Conso <-> Nuke


# Fonctions de réponse impulsionnelle (IRF, 18 mois) 
irf_all <- irf(var_model, n.ahead = 18, boot = TRUE, ci = 0.95, runs = 100)
plot(irf_all)


# Décomposition de la variance (FEVD, 12 mois) 
fevd_result <- fevd(var_model, n.ahead = 12)
plot(fevd_result)
fevd_result


# Prévisions sur échantillon (12 derniers mois, 3 séries) 
h <- 12
n <- nrow(Y)

# Ré-estimation sur l'échantillon d'entraînement
var_train <- VAR(ts(Y[1:(n - h), ], start = start(Y), frequency = 12), 
                 p = 1, type = "both")
pred_in <- predict(var_train, n.ahead = h, ci = 0.95)

# Fonction de reconstruction : log-diff saisonnière -> TWh
reconstruire <- function(log_complete, fcst_logdiff) {
  exp(fcst_logdiff + 
      log_complete[(length(log_complete) - h - 11):(length(log_complete) - h)])
}

fcst_enr   <- reconstruire(log_enr,   pred_in$fcst$ENR[, "fcst"])
fcst_conso <- reconstruire(log_conso, pred_in$fcst$Conso[, "fcst"])
fcst_nuke  <- reconstruire(log_nuke,  pred_in$fcst$Nuke[, "fcst"])

real_enr   <- as.numeric(tail(enr,   h))
real_conso <- as.numeric(tail(conso, h))
real_nuke  <- as.numeric(tail(nuke,  h))

# Métriques MAPE et RMSE
mape <- function(r, p) mean(abs((r - p) / r)) * 100
rmse <- function(r, p) sqrt(mean((r - p)^2))

cat("\n===== Prévisions VAR sur échantillon (12 mois) =====\n")
cat(sprintf("ENR   : MAPE = %.2f%%   RMSE = %.3f TWh\n", 
            mape(real_enr, fcst_enr),     rmse(real_enr, fcst_enr)))
cat(sprintf("Conso : MAPE = %.2f%%   RMSE = %.3f TWh\n", 
            mape(real_conso, fcst_conso), rmse(real_conso, fcst_conso)))
cat(sprintf("Nuke  : MAPE = %.2f%%   RMSE = %.3f TWh\n", 
            mape(real_nuke, fcst_nuke),   rmse(real_nuke, fcst_nuke)))


# Graphiques sur échantillon (observé vs prévision)
plot_prev <- function(serie_obs, prev_TWh, real_TWh, titre, couleur, 
                      show_xlab = FALSE) {
  nb_affichage <- 60
  serie_histo <- as.numeric(tail(serie_obs, nb_affichage))
  
  date_fin    <- 2025 + 11/12
  dates_histo <- seq(date_fin - (nb_affichage - 1)/12, date_fin, 
                     length.out = nb_affichage)
  idx_prev    <- (nb_affichage - h + 1):nb_affichage
  
  prev_plot  <- c(serie_histo[nb_affichage - h], prev_TWh)
  real_plot  <- c(serie_histo[nb_affichage - h], real_TWh)
  dates_plot <- c(dates_histo[nb_affichage - h], dates_histo[idx_prev])
  
  xlab_txt <- if (show_xlab) "Année" else ""
  
  plot(dates_histo, serie_histo, type = "l", col = "black", lwd = 1.5,
       main = titre, xlab = xlab_txt, ylab = "TWh",
       ylim = range(c(serie_histo, prev_TWh, real_TWh)))
  grid(col = "gray90")
  lines(dates_plot, real_plot, col = "black",  lwd = 2)
  lines(dates_plot, prev_plot, col = couleur, lwd = 2, lty = 2)
  legend("topleft", inset = 0.02,
         legend = c("Observé", "Prévision VAR"),
         col = c("black", couleur), lwd = 2, lty = c(1, 2),
         bg = "white", box.col = "black", cex = 0.9)
}

par(mfrow = c(3, 1), mar = c(3.5, 4.5, 2.5, 1))
plot_prev(enr,   fcst_enr,   real_enr,   
          "Production ENR - Prévision VAR vs Réel",        "green4")
plot_prev(conso, fcst_conso, real_conso, 
          "Consommation corrigée - Prévision VAR vs Réel", "purple")
plot_prev(nuke,  fcst_nuke,  real_nuke,  
          "Production nucléaire - Prévision VAR vs Réel",  "red3", 
          show_xlab = TRUE)


# Prévisions hors échantillon (24 mois futurs) avec IC 95%
pred_out <- predict(var_model, n.ahead = 24, ci = 0.95)

# Reconstruction avec IC (les 12 premiers mois utilisent log(t-12) observé,
# les 12 suivants utilisent log(t-12) prévu à l'étape précédente)
reconstruire_out_ic <- function(log_complete, pred_obj_var) {
  n_log <- length(log_complete)
  fcst  <- pred_obj_var[, "fcst"]
  lower <- pred_obj_var[, "lower"]
  upper <- pred_obj_var[, "upper"]
  
  fcst_log  <- numeric(24)
  lower_log <- numeric(24)
  upper_log <- numeric(24)
  
  for (i in 1:24) {
    base <- if (i <= 12) log_complete[n_log - 12 + i] else fcst_log[i - 12]
    fcst_log[i]  <- fcst[i]  + base
    lower_log[i] <- lower[i] + base
    upper_log[i] <- upper[i] + base
  }
  list(fcst = exp(fcst_log), lower = exp(lower_log), upper = exp(upper_log))
}

fcst_enr_out   <- reconstruire_out_ic(log_enr,   pred_out$fcst$ENR)
fcst_conso_out <- reconstruire_out_ic(log_conso, pred_out$fcst$Conso)
fcst_nuke_out  <- reconstruire_out_ic(log_nuke,  pred_out$fcst$Nuke)


# Graphiques hors échantillon avec bandes d'IC 
plot_prev_out_ic <- function(serie_obs, prev_list, titre, couleur, 
                             couleur_ic, show_xlab = FALSE) {
  nb_histo <- 60
  serie_histo <- as.numeric(tail(serie_obs, nb_histo))
  
  date_fin_histo <- 2025 + 11/12
  dates_histo <- seq(date_fin_histo - (nb_histo - 1)/12, date_fin_histo, 
                     length.out = nb_histo)
  dates_prev  <- seq(date_fin_histo + 1/12, date_fin_histo + 24/12, 
                     length.out = 24)
  
  prev_plot  <- c(serie_histo[nb_histo], prev_list$fcst)
  lower_plot <- c(serie_histo[nb_histo], prev_list$lower)
  upper_plot <- c(serie_histo[nb_histo], prev_list$upper)
  dates_plot <- c(dates_histo[nb_histo], dates_prev)
  
  ylim_range <- range(c(serie_histo, prev_list$lower, prev_list$upper))
  xlab_txt   <- if (show_xlab) "Année" else ""
  
  plot(dates_histo, serie_histo, type = "l", col = "black", lwd = 1.5,
       main = titre, xlab = xlab_txt, ylab = "TWh",
       xlim = c(min(dates_histo), max(dates_prev)),
       ylim = ylim_range)
  grid(col = "gray90")
  
  polygon(c(dates_plot, rev(dates_plot)), 
          c(upper_plot, rev(lower_plot)),
          col = couleur_ic, border = NA)
  
  lines(dates_histo, serie_histo,  col = "black",  lwd = 1.5)
  lines(dates_plot,  prev_plot,    col = couleur, lwd = 2, lty = 2)
  abline(v = date_fin_histo, lty = 3, col = "gray50")
  
  legend("topleft", inset = 0.02,
         legend = c("Observé", "Prévision VAR", "IC 95%"),
         col = c("black", couleur, couleur_ic),
         lwd = c(2, 2, 8), lty = c(1, 2, 1),
         bg = "white", box.col = "black", cex = 1.0)
}

par(mfrow = c(3, 1), mar = c(3.5, 4.5, 2.5, 1))

plot_prev_out_ic(enr, fcst_enr_out,   
                 "Production ENR - Prévision VAR (déc. 2025 - nov. 2027)", 
                 "green4", rgb(0.2, 0.7, 0.2, alpha = 0.2))
plot_prev_out_ic(conso, fcst_conso_out, 
                 "Consommation corrigée - Prévision VAR", 
                 "purple", rgb(0.2, 0.3, 0.8, alpha = 0.2))
plot_prev_out_ic(nuke, fcst_nuke_out,  
                 "Production nucléaire - Prévision VAR", 
                 "red3", rgb(0.8, 0.2, 0.2, alpha = 0.2), 
                 show_xlab = TRUE)
