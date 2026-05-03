setwd("C:/Users/Lenovo/Desktop/Egyetem/Idősoros ökonometria/2/HF/Önálló")
library(readxl)
adat <- read_excel("HF-adatbazis.xlsx")

adat$TIME <- as.Date(adat$TIME)

str(adat)

library(ggplot2)

### Adatok bemutatása ###


ggplot(adat, aes(x = TIME, y = train)) +
  geom_line(color = "#1f78b4", linewidth = 1) +
  labs(
    title = "Vasúti idősor",
    x = "Idő",
    y = "Vasúti utaskilométer (ezer pkm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      hjust = 0.5,       
      face = "bold",
      size = 18
    ),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(linewidth = 0.3, color = "grey80")
  )



ggplot(adat, aes(x = TIME, y = air)) +
  geom_line(color = "#e31a1c", linewidth = 1) +
  labs(
    title = "Légi közlekedés",
    x = "Idő",
    y = "Légi utasok száma (fő)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 18
    ),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3, color = "grey80")
  )

ggplot(adat, aes(x = TIME, y = cons)) +
  geom_line(color = "#33a02c", linewidth = 1) +
  labs(
    title = "Fogyasztás",
    x = "Idő",
    y = "Fogyasztás (millió EUR, reálérték)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 18
    ),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3, color = "grey80")
  )


# mindhárom adatsor látszatra sem stacioner
# erős minhdhárom esetén a trend és a szezonalitás komponense is


### Szezonalitás szűrése ###
# Először a szezonalítástól szűrjük meg a az idősorokat

train_ts <- ts(adat$train, start = c(2004, 1), frequency = 4)
air_ts   <- ts(adat$air,   start = c(2004, 1), frequency = 4)
cons_ts  <- ts(adat$cons,  start = c(2004, 1), frequency = 4)


stl_train <- stl(train_ts, s.window = 7)
train_con <- train_ts-stl_train$time.series[,"seasonal"]
plot(train_con)

stl_air <- stl(air_ts, s.window = 7)
air_con <- air_ts-stl_air$time.series[,"seasonal"]
plot(air_con)

stl_cons <- stl(cons_ts, s.window = 7)
cons_con <- cons_ts-stl_cons$time.series[,"seasonal"]
plot(cons_con)




adat$train_con <- as.numeric(train_con)
adat$air_con   <- as.numeric(air_con)
adat$cons_con  <- as.numeric(cons_con)  
str(adat)

# időpontok az eredeti dátum alapján
# az adatsorokat, viszont stacionerré kell alakítani

### Stacionerré alakítás ###
# Differenciázás


adat$d_train <- c(NA, diff(adat$train_con))
adat$d_air <- c(NA, diff(adat$air_con))
adat$d_cons <- c(NA, diff(adat$cons_con))
str(adat)

ggplot(adat, aes(x=TIME)) +
  geom_line(aes(y=d_train, color="train"), size=1)
# Adf teszt segítségével megvizsgáljuk a stcionaritást a differenciázott idősorok esetén

library(aTSA)
adf.test(adat$d_train)
adf.test(adat$d_air)
adf.test(adat$d_cons)

# az adf teszt alapján kijelenthetjük, ezek az adatsorok Alfa = 5% mellett biztosan stacionerek

# k- lag megválasztása

# install.packages("vars")
library(vars)

VARselect(adat[2:nrow(adat), c("d_train", "d_air", "d_cons")], lag.max = 16)

# A BIC alapján választok, mert az erősen bünteti a paraméter számot is
# tehát k = 4 lagot vizsgálom VAR modellel


var_modell4 <- VAR(adat[2:nrow(adat),
                             c("d_train", "d_air", "d_cons")],
                  p = 4,
                  type = "const")
roots(var_modell4)

# Az egységgyökök terén oké, egyik gyök sincs 1 közelében, a legnagyobb is 0,88-nál kisebb


serial.test(var_modell4, lags.pt = 12, type = "PT.asymptotic")
# Megfelelő a modell, a hibatagok együttesen fehérzajnak tekinthetőek

resid <- as.data.frame(resid(var_modell4))
lapply(resid, function(x){bgtest(x~1, order = 12)})

# az első két esetben, a train és air változóknál azt mondhatjuk hoyg alpha = 10% százalékos szignifikancia szinten is el tdujuk fogadni H0-t
# A harmadik esetében csak alpha = 1%-os szignifikancia szinten tudom elfogadni H0-t és így mondhatom azt, hogy a hibatagok külön-külön is fehérzajnak tekinthetőek


summary(var_modell4)

### Granger-okság ###

# install.packages("aod")
library(aod)
# train - air
wald.test(b=coef(var_modell4$varresult$d_train),
          Sigma=vcov(var_modell4$varresult$d_train), Terms=c(2, 5, 8, 11))

# A p-érték nagyobb, mint minden szoksásos szignifikancia szint, tehát a nullhipotézist elfogadjuk, a wald-teszt alapján a d_air nem gyakorol kimutatható hatást a d_train jelenlegi értékére, így ezen a késleltetési csatornán nem áll fenn Granger-okozás.

# Air - cons
wald.test(b=coef(var_modell4$varresult$d_air),
          Sigma=vcov(var_modell4$varresult$d_air), Terms=c(3, 6, 9, 12))

# A p-érték kisebb, mint 5%, tehát a nullhipotézist elutasítjuk, a wald-teszt alapján a d_cons gyakorol kimutatható hatást a d_air jelenlegi értékére, így ezen a késleltetési csatornán fennáll Granger-okozás.

# Cons - air
wald.test(b=coef(var_modell4$varresult$d_train),
          Sigma=vcov(var_modell4$varresult$d_train), Terms=c(2, 5, 8, 11))

# A p-érték nagyobb, mint minden szoksásos szignifikancia szint, tehát a nullhipotézist elfogadjuk, a wald-teszt alapján a d_air nem gyakorol kimutatható hatást a d_cons jelenlegi értékére, így ezen a késleltetési csatornán nem áll fenn Granger-okozás.

### Koefficiensek ###

coef(var_modell4$varresult$d_train)


library(gt)
library(magrittr)  # EZ adja a %>% operátort

b <- coef(var_modell4$varresult$d_train)

tabla_d_train <- data.frame(
  Valtozo    = names(b),
  Egyutthato = round(as.numeric(b), 4)
)

gt_tab <- tabla_d_train %>%
  gt() %>%
  tab_header(
    title = "A d_train egyenlet becsült együtthatói – VAR(4)"
  ) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", size = px(16)),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", weight = "bold", size = px(18)),
    locations = cells_title("title")
  ) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", size = px(16), weight = "bold"),
    locations = cells_column_labels()
  )

gt_tab

### Impulzus-válaszfüggvények ###

library(vars)


irf_train_air  <- irf(var_modell4, impulse = "d_train", response = "d_air",
                      n.ahead = 15, ortho = TRUE, boot = TRUE)
irf_air_train  <- irf(var_modell4, impulse = "d_air",   response = "d_train",
                      n.ahead = 15, ortho = TRUE, boot = TRUE)
irf_cons_air   <- irf(var_modell4, impulse = "d_cons",  response = "d_air",
                      n.ahead = 15, ortho = TRUE, boot = TRUE)
irf_cons_train <- irf(var_modell4, impulse = "d_cons",  response = "d_train",
                      n.ahead = 15, ortho = TRUE, boot = TRUE)

plot_irf <- function(irf_obj, impulse, response, main_title) {

  h <- 0:(nrow(irf_obj$irf[[impulse]]) - 1)

  mid   <- irf_obj$irf[[impulse]][, response]
  lower <- irf_obj$Lower[[impulse]][, response]
  upper <- irf_obj$Upper[[impulse]][, response]
  
  # alapskála
  plot(h, mid, type = "l",
       ylim = range(lower, upper, na.rm = TRUE),
       xlab = "Horizont (negyedév)",
       ylab = "Impulzus-válasz",
       main = main_title)
  abline(h = 0, lty = 2)
  lines(h, lower, lty = 3)
  lines(h, upper, lty = 3)
}


op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot_irf(irf_air_train,
         impulse = "d_air", response = "d_train",
         main_title = "Légi → vasúti forgalom")

plot_irf(irf_train_air,
         impulse = "d_train", response = "d_air",
         main_title = "Vasúti → légi forgalom")

plot_irf(irf_cons_air,
         impulse = "d_cons", response = "d_air",
         main_title = "Fogyasztás → légi forgalom")

plot_irf(irf_cons_train,
         impulse = "d_cons", response = "d_train",
         main_title = "Fogyasztás → vasúti forgalom")

par(op)


