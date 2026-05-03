setwd("C:/Users/Lenovo/Desktop/Egyetem/Idősoros ökonometria")

library(readxl)
Gyumi <- read_excel("HF_adat.xlsx", sheet = "Munka1")

str(Gyumi)
Gyumi$t <- 1:nrow(Gyumi)

library(ggplot2)
Gyumi$Datum <- as.Date(Gyumi$Datum)
ggplot(data = Gyumi, aes(x = Datum))+ 
  geom_line(aes(y = Gyumolcsjoghurt), size=1) + 
  labs(title = "A gyümölcsjoghurt átlágárának alakulása", 
       x = "Évek", y = "Átlagár (Ft)") +
  theme_minimal()

Gyumi$t <- 1:nrow(Gyumi)

# Lineráris trend

lin_trend <- lm(Gyumolcsjoghurt ~ t, data = Gyumi)
summary(lin_trend)

# Ábra a lineáris trendről
ggplot(Gyumi, aes(x = Datum))+
  geom_line(aes(y = Gyumolcsjoghurt, color = "Valós"), size=1) +
  geom_line(aes(y = lin_trend$fitted.values, color = "Becsült (lineáris)"), size=1) +
  theme_minimal()

# Exponenciális trend

exp_trend <- lm(log(Gyumolcsjoghurt) ~ t, data = Gyumi)
summary(exp_trend)

# Ábra az exponenciális trendről
ggplot(Gyumi, aes(x = Datum))+
  geom_line(aes(y = Gyumolcsjoghurt, color = "Valós"), size=1) +
  geom_line(aes(y = exp(exp_trend$fitted.values), color = "Becsült (exponenciális)"), size=1) +
  theme_minimal()

# Kvadratikus trend

kvad_trend <- lm(Gyumolcsjoghurt ~ t + I(t^2), data = Gyumi)
summary(kvad_trend)

# Ábra a kvadratikus trendről
ggplot(Gyumi, aes(x = Datum))+
  geom_line(aes(y = Gyumolcsjoghurt, color = "Valós"), size=1) +
  geom_line(aes(y = kvad_trend$fitted.values, color = "Becsült (kvadratikus)"), size=1) +
  theme_minimal()

# Törések

### Szakaszok - segmented
#install.packages("segmented")
library(segmented)

mod_lm <- lm(Gyumolcsjoghurt ~ t, data = Gyumi)
mod_seg <- segmented(mod_lm, seg.Z = ~ t, npsi = 3)  # 2 töréspont keresése

summary(mod_seg)
plot(Gyumi$Datum, Gyumi$Gyumolcsjoghurt, type = "l")
lines(Gyumi$Datum, fitted(mod_seg), col = "red")

breakpoints <- mod_seg$psi[, "Est."]
breakpoints
Gyumi$Datum[14]
Gyumi$Datum[26]
Gyumi$Datum[44]

library(strucchange)

sctest(Gyumi$Gyumolcsjoghurt ~ Gyumi$t, type = "Chow", point = 14)
sctest(Gyumi$Gyumolcsjoghurt ~ Gyumi$t, type = "Chow", point = 26)
sctest(Gyumi$Gyumolcsjoghurt ~ Gyumi$t, type = "Chow", point = 44)
# mindhárom Chow-teszt eredménye, hogy az adott pontnál van strukturális törás

Gyumi$szakasz2 <- "1. szakasz2"
Gyumi$szakasz2[Gyumi$Datum > "2022-02-01"&Gyumi$Datum <= "2023-02-01"] <- "2. szakasz2"
Gyumi$szakasz2[Gyumi$Datum > "2023-02-01"&Gyumi$Datum <= "2024-08-01"] <- "3. szakasz2"
Gyumi$szakasz2[Gyumi$Datum > "2024-08-01"] <- "4. szakasz2"

szakasz2_trend <- lm(Gyumolcsjoghurt ~ t + szakasz2 + t * szakasz2, data = Gyumi)
summary(szakasz2_trend)

# Ábra a töréses trendről
ggplot(Gyumi, aes(x = Datum))+
  geom_line(aes(y = Gyumolcsjoghurt, color = "Valós"), size=1) +
  geom_line(aes(y = szakasz2_trend$fitted.values, color = "Becsült (szakaszos)"), size=1) +
  labs(title = "A gyümölcsjoghurt átlágárának alakulása", 
       x = "Évek", y = "Átlagár (Ft)") +
  theme_minimal()


# szakaszok - strucchange
library(strucchange)
toresek <- breakpoints(Gyumi$Gyumolcsjoghurt ~ 1)
toresek
length(toresek$breakpoints) #3 darab törés van




Gyumi$Datum[toresek$breakpoints[1]]

Gyumi$Datum[toresek$breakpoints[2]]

Gyumi$Datum[toresek$breakpoints[3]]


Gyumi$szakasz <- "1. szakasz"
Gyumi$szakasz[Gyumi$Datum > "2022-02-01"&Gyumi$Datum <= "2022-09-01"] <- "2. szakasz"
Gyumi$szakasz[Gyumi$Datum > "2022-09-01"&Gyumi$Datum <= "2022-09-01"] <- "3. szakasz"
Gyumi$szakasz[Gyumi$Datum > "2022-09-01"] <- "4. szakasz"

szakasz_trend <- lm(Gyumolcsjoghurt ~ t + szakasz + t * szakasz, data = Gyumi)
summary(szakasz_trend)

# Ábra a töréses trendről
ggplot(Gyumi, aes(x = Datum))+
  geom_line(aes(y = Gyumolcsjoghurt, color = "Valós"), size=1) +
  geom_line(aes(y = szakasz_trend$fitted.values, color = "Becsült (szakaszos)"), size=1) +
  theme_minimal()

# Mozgóátlag - nem centrális

Gyumi$MA <- rollmean(Gyumi$Gyumolcsjoghurt, k=12, fill=NA, align = "right")

# Mozgóátlag - centrális
Gyumi$CMA <- rollmean(Gyumi$Gyumolcsjoghurt, k=12, fill=NA)

# Mozgóátlag - exponenciális
library(pracma)

Gyumi$EMA <- movavg(Gyumi$Gyumolcsjoghurt, n=9, type="e")
# alfa=0,2

ggplot(Gyumi, aes(x = Datum))+
  geom_line(aes(y = Gyumolcsjoghurt, color = "Valós"), size = 1)+
  geom_line(aes(y = MA, color = "MA"), size = 1)+
  geom_line(aes(y = CMA, color = "CMA"), size = 1)+
  geom_line(aes(y = EMA, color = "EMA"), size = 1)+
  theme_minimal()

# Loess modell

loessmodel <- loess(Gyumolcsjoghurt ~ t, data = Gyumi, span = 0.6)
ggplot(Gyumi, aes(x = Datum))+
  geom_line(aes(y = Gyumolcsjoghurt, color = "Valós"), size = 1) +
  geom_line(aes(y = loessmodel$fitted, color = "LOESS"), size = 1) +
  labs(title = "A gyümölcsjoghurt átlágárának alakulása", 
       x = "Évek", y = "Átlagár (Ft)") +
  theme_minimal()

# Modellek összehasonlítása BIC alapján

resid_lin <- lin_trend$residuals
exp_kalap <- exp(exp_trend$fitted.values)
resid_exp <- Gyumi$Gyumolcsjoghurt - exp_kalap
resid_kvad <- kvad_trend$residuals
resid_szakasz <- szakasz_trend$residuals
resid_szakasz2 <- szakasz2_trend$residuals
resid_loess <- loessmodel$residuals


# Reziduális szórás:
s_e_lin <- sqrt(sum(resid_lin^2) / length(resid_lin))
s_e_exp <- sqrt(sum(resid_exp^2) / length(resid_exp))
s_e_kvad <- sqrt(sum(resid_kvad^2) / length(resid_kvad))
s_e_szakasz <- sqrt(sum(resid_szakasz^2) / length(resid_szakasz))
s_e_szakasz2 <- sqrt(sum(resid_szakasz2^2) / length(resid_szakasz2))
s_e_loess <- sqrt(sum(resid_loess^2) / length(resid_loess))
s_e_lin
s_e_exp
s_e_kvad
s_e_szakasz
s_e_szakasz2
s_e_loess

# A szakaszos 2 regresszió rendelkezik a legkisebb reziduális szórással, ezért azt választjuk




# Szezonalitás vizsgálata

Gyumi_TS <- ts(data = Gyumi$Gyumolcsjoghurt,
               start = c(2021,1),
               frequency = 12)

Dekompadd <- decompose(Gyumi_TS, type = "additive")
plot(Dekompadd)
#ez biztos nem jó, a hibatagban (random) ott maradt a szezonalitás

Dekompmulti <- decompose(Gyumi_TS, type = "multiplicative")
plot(Dekompmulti)
#egy fokkal jobb, de mivel nem állandó a szezonalitás az idősorban, így ez sem megfelelő

#STL lesz a megoldás
#mert ez ilyen gördülő ablak technikával fogja kezelni a szezonhatásokat
STLdekomp <- stl(Gyumi_TS, s.window = 7)
#7 azonos hónap alapján becsül, aztán gördül tovább az ablak
plot(STLdekomp)
#ezzel is lehet szezonálisan kiigazítani
kiigazitott <- Gyumi_TS - STLdekomp$time.series[,"seasonal"] #additív kiigazítás
ggplot(Gyumi, aes(x = Datum))+
  geom_line(aes(y = Gyumolcsjoghurt, color = "Valós"), size=1) +
  geom_line(aes(y = kiigazitott, color = "Kiigazított"), size=1) +
  labs(title = "A gyümölcsjoghurt átlágárának alakulása", 
       x = "Évek", y = "Átlagár (Ft)") +
  theme_minimal()



# Autokorreláció vizsgálata

library(lmtest)
bgtest(Gyumi$Gyumolcsjoghurt ~ 1, order = 10)
# H0-t elvetjük,van autokorreláció

library(aTSA) 

adf.test(Gyumi$Gyumolcsjoghurt)
# ADF tesztben: H0: az idősor nem stacioner
# P-érték minden Type 1 értékre nagyobb mint bármely szokásos p-érték, ezért delta Y-okat kell vennünk

Gyumi$Gyumolcsjoghurt_diff <- c(NA, diff(Gyumi$Gyumolcsjoghurt))
str(Gyumi)

adf.test(Gyumi$Gyumolcsjoghurt_diff)
# A legtöbb esetben elvetjük H0-t 5%-os szignifikanciaszint mellett, ezért az idősort stacionáriusnak tekinthető

acf(Gyumi$Gyumolcsjoghurt_diff[-1], main="ACF")

pacf(Gyumi$Gyumolcsjoghurt_diff[-1], main="PACF")
# Egyik ábrán sincs szignifikáns autokorreláció
# sejtés: ez az idősor fehérzajnak tekinthető

bgtest(Gyumi$Gyumolcsjoghurt_diff ~ 1, order = 10)
# a p-érték minden szokásos alfánál nagyobb, vagyis az idősort valóban fehérzajnak tekinthetjük


fit <- arima(Gyumi$Gyumolcsjoghurt, order=c(0,1,0))
bgtest(resid(fit) ~ 1, order = 10)
# a H0-t nem tudjuk elvetni, vagyis 10 késleltetésig nincs autokorreláció a rezidumok között

library(tseries)
jarque.bera.test(resid(fit))
# A rezidumok normális eloszlást követknek, készen vagyunk
# Random walk modellt kaptunk - ARIMA(0,1,0)

