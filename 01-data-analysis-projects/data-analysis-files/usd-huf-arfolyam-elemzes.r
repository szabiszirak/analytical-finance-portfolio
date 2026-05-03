setwd("C:/Users/Lenovo/Desktop/Egyetem/Idősoros ökonometria")

library(readxl)
USDHUF_eredeti <- read_excel("Csoportos_hazi_adat.xlsx")

str(USDHUF_eredeti)

USDHUF_eredeti$Date <- as.Date(USDHUF_eredeti$Date)
USDHUF <- USDHUF_eredeti[, c("Date", "Close")]

str(USDHUF)

USDHUF$t <- 1:nrow(USDHUF)

# Ábra az idősorról
library(ggplot2)
ggplot(data = USDHUF, aes(x = Date)) + 
  geom_line(aes(y = Close)) + 
  labs(
    title = "USD/HUF árfolyamának alakulása", 
    x = "Évek", 
    y = "Árfolyam"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times New Roman", size = 16),
    axis.title = element_text(family = "Times New Roman", size = 14),
    axis.text = element_text(family = "Times New Roman", size = 14),
    plot.background = element_rect(color = "black", fill = NA, linewidth = 1)
  )

### Trendépítés

## Lineráris trend

lin_trend <- lm(Close ~ t, data = USDHUF)
summary(lin_trend)

# Ábra a lineáris trendről
ggplot(USDHUF, aes(x = Date))+
  geom_line(aes(y = Close, color = "Valós"), size=1) +
  geom_line(aes(y = lin_trend$fitted.values, color = "Becsült (lineáris)"), size=1) +
  theme_minimal()

## Exponenciális trend

exp_trend <- lm(log(Close) ~ t, data = USDHUF)
summary(exp_trend)

# Ábra az exponenciális trendről
ggplot(USDHUF, aes(x = Date))+
  geom_line(aes(y = Close, color = "Valós"), size=1) +
  geom_line(aes(y = exp(exp_trend$fitted.values), color = "Becsült (exponenciális)"), size=1) +
  theme_minimal()

## Szakaszos trend
# strucchange csomag segítségével megállapítjuk hány darab töréspont kell
library(strucchange)
breaks <- breakpoints(Close ~ t, data = USDHUF)
summary(breaks)
plot(breaks)

# segmented csomag segítségével megállapítjuk a töréspontokat
#install.packages("segmented")
library(segmented)

mod_lm <- lm(Close ~ t, data = USDHUF)
mod_seg <- segmented(mod_lm, seg.Z = ~ t, npsi = 5)  # 5 töréspont keresése
summary(mod_seg)

# Ábra a szakaszos trendről
plot(USDHUF$Date, USDHUF$Close, type = "l")
lines(USDHUF$Date, fitted(mod_seg), col = "red")

breakpoints <- mod_seg$psi[, "Est."]
breakpoints
USDHUF$Date[378]
USDHUF$Date[553]
USDHUF$Date[724]
USDHUF$Date[823]
USDHUF$Date[1239]

library(lmtest)
library(strucchange)

sctest(USDHUF$Close ~ USDHUF$t, type = "Chow", point = 378)
sctest(USDHUF$Close ~ USDHUF$t, type = "Chow", point = 553)
sctest(USDHUF$Close ~ USDHUF$t, type = "Chow", point = 724)
sctest(USDHUF$Close ~ USDHUF$t, type = "Chow", point = 823)
sctest(USDHUF$Close ~ USDHUF$t, type = "Chow", point = 1239)
# mindhárom Chow-teszt eredménye, hogy az adott pontnál van strukturális törás

USDHUF$szakasz <- "1. szakasz"
USDHUF$szakasz[USDHUF$Date > "2021-06-11"&USDHUF$Date <= "2022-02-11"] <- "2. szakasz"
USDHUF$szakasz[USDHUF$Date > "2022-02-11"&USDHUF$Date <= "2022-10-10"] <- "3. szakasz"
USDHUF$szakasz[USDHUF$Date > "2022-10-10"&USDHUF$Date <= "2023-02-24"] <- "4. szakasz"
USDHUF$szakasz[USDHUF$Date > "2023-02-24"&USDHUF$Date <= "2024-09-30"] <- "5. szakasz"
USDHUF$szakasz[USDHUF$Date > "2024-09-30"] <- "6. szakasz"

szakasz_trend <- lm(Close ~ t + szakasz + t * szakasz, data = USDHUF)
summary(szakasz_trend)

# Ábra a töréses trendről
ggplot(USDHUF, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Árfolyam"), size = 0.5) +  # fekete vonal
  geom_line(aes(y = szakasz_trend$fitted.values, color = "Trend"), size = 0.7) +  # piros trend
  scale_color_manual(
    name = "Jelmagyarázat",
    values = c("Árfolyam" = "black", "Trend" = "red")
  ) +
  labs(
    title = "Töréses trend",
    x = "Dátum",
    y = "Árfolyam értéke (USD/HUF)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14, hjust = 0.5),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 13, hjust = 0.5),
    legend.text = element_text(size = 12, hjust = 0.5),
    plot.background = element_rect(color = "black", fill = NA, linewidth = 1)
  )


## Loess modell

loessmodel <- loess(Close ~ t, data = USDHUF, span = 0.4) # span 0,4, mivel nem szeretnék kisebb spannel túlilleszteni
ggplot(USDHUF, aes(x = Date))+
  geom_line(aes(y = Close, color = "Valós")) +
  geom_line(aes(y = loessmodel$fitted, color = "LOESS")) +
  theme_minimal()




# Modellek összehasonlítása reziduális szórás alapján

resid_lin <- lin_trend$residuals
exp_kalap <- exp(exp_trend$fitted.values)
resid_exp <- USDHUF$Close - exp_kalap
resid_szakasz <- szakasz_trend$residuals
resid_loess <- loessmodel$residuals


# Reziduális szórás:
s_e_lin <- sqrt(sum(resid_lin^2) / length(resid_lin))
s_e_exp <- sqrt(sum(resid_exp^2) / length(resid_exp))
s_e_szakasz <- sqrt(sum(resid_szakasz^2) / length(resid_szakasz))
s_e_loess <- sqrt(sum(resid_loess^2) / length(resid_loess))
s_e_lin
s_e_exp
s_e_szakasz
s_e_loess

# A reziduális szórások alapján a szakaszos trendet választjuk

# Illesztett trend ellenőrzése, rezidumok vizsgálata

library(tseries)
ggplot(USDHUF, aes(x = Date))+
  geom_line(aes(y = resid_szakasz, color = "Szakaszos trend rezidumai"), size=1) +
  theme_minimal()
adf.test(resid_szakasz)
# H0-t elvetjük, a rezidumok stacinoer idősort alkotnak

library(lmtest)
bgtest(resid_szakasz ~ 1, order = 10)
# H0-t elvetjük, van autokorreláció, így a rezidumok biztosan nem fehérzajt alkotnak


### Loghozamok vizsgálata

## Eseményelemzés

USDHUF$LogReturn <- c(NA, diff(log(USDHUF$Close)))
ggplot(data = USDHUF, aes(x = Date))+ 
  geom_line(aes(y = LogReturn)) + 
  theme_minimal()
adf.test(na.omit(USDHUF$LogReturn))
# p-érték minden szokásos alfánál kisebb, H0-t elvetjük, stacioner az adatsorunk

# Eseménynapok a töréses trend alapján
esemeny1 <- as.Date("2021-06-11")
esemeny2 <- as.Date("2022-02-24")
esemeny3 <- as.Date("2022-10-10")
esemeny4 <- as.Date("2023-02-24")
esemeny5 <- as.Date("2024-09-30")

# Paraméterek
ablak <- 10         # eseményablak: -10 ... +10 nap
ablak_becs <- 60    # becslési ablak: előtte 60 nap

# 1. esemény
esemeny1_ablak <- USDHUF[USDHUF$Date >= (esemeny1 - ablak) & USDHUF$Date <= (esemeny1 + ablak), ]
esemeny1_ablak_becs <- USDHUF[USDHUF$Date >= (esemeny1 - ablak - ablak_becs) & USDHUF$Date < (esemeny1 - ablak), ]
atlag1 <- mean(esemeny1_ablak_becs$LogReturn, na.rm = TRUE)

esemeny1_abnorm <- esemeny1_ablak$LogReturn - atlag1
esemeny1_CAR <- sum(esemeny1_abnorm, na.rm = TRUE)

esemeny1_abnorm
esemeny1_CAR
t.test(esemeny1_abnorm, mu = 0)
# p < 0,05, H0-t elvetjük, az eltérés szignifikáns

# 2. esemény
esemeny2_ablak <- USDHUF[USDHUF$Date >= (esemeny2 - ablak) & USDHUF$Date <= (esemeny2 + ablak), ]
esemeny2_ablak_becs <- USDHUF[USDHUF$Date >= (esemeny2 - ablak - ablak_becs) & USDHUF$Date < (esemeny2 - ablak), ]
atlag2 <- mean(esemeny2_ablak_becs$LogReturn, na.rm = TRUE)
esemeny2_abnorm <- esemeny2_ablak$LogReturn - atlag2
esemeny2_CAR <- sum(esemeny2_abnorm, na.rm = TRUE)

esemeny2_abnorm
esemeny2_CAR
t.test(esemeny2_abnorm, mu = 0)
# H0-t nem vetjük el, az eltérés nem szignifikáns

# 3. esemény
esemeny3_ablak <- USDHUF[USDHUF$Date >= (esemeny3 - ablak) & USDHUF$Date <= (esemeny3 + ablak), ]
esemeny3_ablak_becs <- USDHUF[USDHUF$Date >= (esemeny3 - ablak - ablak_becs) & USDHUF$Date < (esemeny3 - ablak), ]
atlag3 <- mean(esemeny3_ablak_becs$LogReturn, na.rm = TRUE)
esemeny3_abnorm <- esemeny3_ablak$LogReturn - atlag3
esemeny3_CAR <- sum(esemeny3_abnorm, na.rm = TRUE)

esemeny3_abnorm
esemeny3_CAR
t.test(esemeny3_abnorm, mu = 0)
# H0-t nem vetjük el, az eltérés nem szignifikáns

# 4. esemény
esemeny4_ablak <- USDHUF[USDHUF$Date >= (esemeny4 - ablak) & USDHUF$Date <= (esemeny4 + ablak), ]
esemeny4_ablak_becs <- USDHUF[USDHUF$Date >= (esemeny4 - ablak - ablak_becs) & USDHUF$Date < (esemeny4 - ablak), ]
atlag4 <- mean(esemeny4_ablak_becs$LogReturn, na.rm = TRUE)
esemeny4_abnorm <- esemeny4_ablak$LogReturn - atlag4
esemeny4_CAR <- sum(esemeny4_abnorm, na.rm = TRUE)

esemeny4_abnorm
esemeny4_CAR
t.test(esemeny4_abnorm, mu = 0)
# H0-t nem vetjük el, az eltérés nem szignifikáns

# 5. esemény
esemeny5_ablak <- USDHUF[USDHUF$Date >= (esemeny5 - ablak) & USDHUF$Date <= (esemeny5 + ablak), ]
esemeny5_ablak_becs <- USDHUF[USDHUF$Date >= (esemeny5 - ablak - ablak_becs) & USDHUF$Date < (esemeny5 - ablak), ]
atlag5 <- mean(esemeny5_ablak_becs$LogReturn, na.rm = TRUE)
esemeny5_abnorm <- esemeny5_ablak$LogReturn - atlag5
esemeny5_CAR <- sum(esemeny5_abnorm, na.rm = TRUE)

esemeny5_abnorm
esemeny5_CAR
t.test(esemeny5_abnorm, mu = 0)
# H0 kb 0,05, elvetjük, igy azt mondjuk, hogy az eltérés szignifikáns




## ARIMA modell építése

library(lmtest)
bgtest(USDHUF$LogReturn ~ 1, order = 10)
# H0-t elvetjük. Az idősorunk biztosan nem fehérzaj.

acf(na.omit(USDHUF$LogReturn))
# 6. lagnál van szignifikáns eltérés
pacf(na.omit(USDHUF$LogReturn))
# 6. lagnál van szignifikáns eltérés

fit1 <- arima(USDHUF$LogReturn, order=c(3,0,0))
fit1

bgtest(resid(fit1)~1, order=10)
# AR(3) modell még nem jó

fit2 <- arima(USDHUF$LogReturn, order=c(0,0,3))
fit2

bgtest(resid(fit2)~1, order=10)
# MA(3) modell még nem jó

fit3 <- arima(USDHUF$LogReturn, order=c(1,0,1))
fit3

bgtest(resid(fit3)~1, order=10)
# ARMA(1,1) még nem jó

fit4 <- arima(USDHUF$LogReturn, order=c(1,0,2))
fit4

bgtest(resid(fit4)~1, order=10)
# ARMA(1,2) nem

fit5 <- arima(USDHUF$LogReturn, order=c(1,0,3))
fit5

bgtest(resid(fit5)~1, order=10)
# ARMA(1,3) nem

fit6 <- arima(USDHUF$LogReturn, order=c(2,0,1))
fit6

bgtest(resid(fit6)~1, order=10)
# ARMA(2,1) nem

fit7 <- arima(USDHUF$LogReturn, order=c(2,0,2))
fit7

bgtest(resid(fit7)~1, order=10)
# ARMA(2,2) fehérzaj

fit8 <- arima(USDHUF$LogReturn, order=c(2,0,3))
fit8

bgtest(resid(fit8)~1, order=10)
# ARMA(2,3) fehérzaj

fit9 <- arima(USDHUF$LogReturn, order=c(3,0,1))
fit9

bgtest(resid(fit9)~1, order=10)
# ARMA(3,1) nem fehérzaj

fit10 <- arima(USDHUF$LogReturn, order=c(3,0,2))
fit10

bgtest(resid(fit10)~1, order=10)
# ARMA(3,2) fehérzaj

fit11 <- arima(USDHUF$LogReturn, order=c(3,0,3))
fit11


bgtest(resid(fit11)~1, order=10)
# ARMA(3,3) fehérzaj

BIC(fit7)
BIC(fit8)
BIC(fit10)
BIC(fit11)

AIC(fit7)
AIC(fit8)
AIC(fit10)
AIC(fit11)

# Az AIC és a BIC mutatók alapján is az ARMA(2,2) modell a legmegfelelőbb

coeftest(fit7)

library(tseries)
jarque.bera.test(na.omit(resid(fit7)))
# modell rezidumai fehérzajnak tekinthetők, viszont nem normális eloszlású fehérzajnak

#Ábra
plot(USDHUF$Date, USDHUF$LogReturn, type = "l", col = "black", lwd = 1,
     main = "ARMA(2,2) modell illesztése a loghozamra",
     xlab = "Dátum", ylab = "Loghozam értéke",
     family = "Times New Roman", cex.main = 1.33, cex.lab = 1.17)

lines(USDHUF$Date, fitted(fit7), col = "red", lwd = 1)

## ARCH, GARCH

# Csomagok betöltése
# install.packages("FinTS")
# install.packages("rugarch")
library(FinTS)
library(rugarch)

resids_negyzet <- (resid(fit7))^2
resids_negyzet

acf(na.omit(resids_negyzet))
pacf(na.omit(resids_negyzet))
# Szemmel láthatóan is jelen van a heteroszkedaszticitás
# ARCH(1), ARCH(2) vagy GARCH(1,1) modellek lehetségesek az ábrák alapján 

#Ábra
library(ggplot2)
library(forecast)
library(patchwork)

#ACF
acf_plot <- ggAcf(na.omit(USDHUF$LogReturn)) +
  labs(
    title = "ACF a loghozamokra",
    x = "Késleltetés",
    y = "Autokorreláció"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.background = element_rect(color = "black", fill = NA, linewidth = 1)
  )

#PACF
pacf_plot <- ggPacf(na.omit(USDHUF$LogReturn)) +
  labs(
    title = "PACF a loghozamokra",
    x = "Késleltetés",
    y = "Parciális autokorreláció"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.background = element_rect(color = "black", fill = NA, linewidth = 1)
  )

acf_plot + pacf_plot

ArchTest(USDHUF$LogReturn)
# H0-t elvetjük, van Arch hatás a modellben, tehát az idősor heteroszkedasztikus

LogReturn_tiszta <- na.omit(USDHUF$LogReturn)

# GARCH(1,1) 
garch <- ugarchspec(
  mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "norm"
)

garch_fit <- ugarchfit(spec = garch, data = LogReturn_tiszta)
show(garch_fit)

resid_garch <- residuals(garch_fit, standardize = TRUE)

jarque.bera.test(na.omit(resid_garch))
# nem kapunk normális eloszlást így sem a reziduumokra

# GARCH(1,1) student-t eloszlást feltételezve
garch_2 <- ugarchspec(
  mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "std"
)

garch_fit2 <- ugarchfit(spec = garch_2, data = LogReturn_tiszta)
show(garch_fit2)


resid_garch2 <- residuals(garch_fit2, standardize = TRUE)

jarque.bera.test(na.omit(resid_garch2))
# nem kapunk normális eloszlást így sem a reziduumokra

# GARCH(1,1) Skewed t-eloszlást feltételezve
garch_3 <- ugarchspec(
  mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "sstd"
)

garch_fit3 <- ugarchfit(spec = garch_3, data = LogReturn_tiszta)
show(garch_fit3)


resid_garch3 <- residuals(garch_fit3, standardize = TRUE)

jarque.bera.test(na.omit(resid_garch3))
# nem kapunk normális eloszlást így sem a reziduumokra



# EGARCH(1,1) specifikáció skewed t-eloszlással
egarch <- ugarchspec(
  mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  distribution.model = "std"
)

# Modell illesztése
egarch_fit <- ugarchfit(spec = egarch, data = LogReturn_tiszta)

# Eredmény megtekintése
show(egarch_fit)

# Reziduumok kinyerése és JB-teszt
resid_egarch <- residuals(egarch_fit, standardize = TRUE)
jarque.bera.test(na.omit(resid_egarch))
# nem kapunk normális eloszlást így sem a reziduumokra



modellek <- data.frame(
  Modell = c("ARMA(2,2)",
             "GARCH(1,1), normál",
             "GARCH(1,1), t-eloszl",
             "GARCH(1,1), skewed t",
             "EGARCH(1,1), t-eloszl"),
  AIC = c(-8862.277, -6.9301, -6.9423, -6.9412, -6.9407),
  BIC = c(-8831.233, -6.8984, -6.9066, -6.9015, -6.9010)
)

# nem megfelelők az AIC, BIC mutatók, mert a GARCH modellen a reziduumok normalizáva vannak

# AIC és BIC GARCH modellre "valódi" (nem normalizált) módon
n <- length(na.omit(LogReturn_tiszta))
n

# GARCH modellek AIC/BIC értékeinek újraszámítása teljes mintára
modellek$AIC[2:5] <- modellek$AIC[2:5] * n
modellek$BIC[2:5] <- modellek$BIC[2:5] * n

# Táblázat megjelenítése ábraként
library(gridExtra)
library(grid)

# Egyedi betűtípus (Times New Roman)
custom_theme <- ttheme_default(
  core = list(fg_params = list(fontfamily = "Times New Roman")),
  colhead = list(fg_params = list(fontfamily = "Times New Roman", fontface = "bold"))
)

# Táblázat rajzolása
grid.newpage()
grid.draw(tableGrob(modellek, rows = NULL, theme = custom_theme))




# A GARCH(1,1) t-eloszlás AIC, BIC és a Loglikelihood mutatók alapján is jobb, ezért azt választjuk.
# A residumok továbbra sem követnek normális eloszlást

#Ábra
plot(USDHUF$Date, USDHUF$LogReturn, type = "l", col = "black", lwd = 1,
     main = "ARMA(2,2) modell illesztése a loghozamra",
     xlab = "Dátum", ylab = "Loghozam értéke",
     family = "Times New Roman", cex.main = 1.33, cex.lab = 1.17)

lines(USDHUF$Date, fitted(fit7), col = "red", lwd = 1)





 





