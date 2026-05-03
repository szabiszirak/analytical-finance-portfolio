setwd("C:/Users/frics/OneDrive/Desktop/GPME/GPME_4/idosor")

library(readxl)
library(tseries)
library(vars)
library(urca)
library(lmtest)
library(zoo)
library(ggplot2)
library(aTSA)
library(aod)
library(FinTS)


### Adatok előkészítése ###

#beolvasás
adat<-read_excel("Csoportos.xlsx")
#View(adat)
str(adat)

adat$Date <- as.Date(adat$Date)

#loghozamokká alakítás
adat$logqcom <- c(NA, diff(log(adat$qcom)))
adat$lognvda <- c(NA, diff(log(adat$nvda)))
adat$logamd <- c(NA, diff(log(adat$amd)))

ggplot(adat, aes(x=Date)) +
  geom_line(aes(y=lognvda, color="NVIDIA")) +
  geom_line(aes(y=logamd, color="AMD")) +
  geom_line(aes(y=logqcom, color="QUALCOMM")) +
  labs(title="Loghozamok", x="Dátum", y="Loghozam") +
  scale_color_manual(values=c("NVIDIA"="skyblue", "AMD"="violet", "QUALCOMM"="orange")) +
  theme_minimal()

### GO-GARCH modell építése ###

## stacionaritás ADF tesztek

#ADF-teszt, H0: nem stacioner
adf.test(adat$lognvda)
adf.test(adat$logamd)
adf.test(adat$logqcom)
#mind3 stacioner


## volatilitás klasztereződése

# adatok abszolút értéke új oszlopokban
adat$qcom_vol_proxy <- abs(adat$logqcom)
adat$nvda_vol_proxy <- abs(adat$lognvda)
adat$amd_vol_proxy <- abs(adat$logamd)

#20 napos gördülő szórás
adat$qcom_roll_sd20 <- rollapply(adat$logqcom,
                                 width = 20,
                                 FUN = sd,
                                 fill = NA,
                                 align = "right")
adat$nvda_roll_sd20 <- rollapply(adat$lognvda,
                                 width = 20,
                                 FUN = sd,
                                 fill = NA,
                                 align = "right")
adat$amd_roll_sd20 <- rollapply(adat$logamd,
                                width = 20,
                                FUN = sd,
                                fill = NA,
                                align = "right")

#időben változó volatilitás ábrázolása
ggplot(adat, aes(x = Date, y = qcom_roll_sd20)) +
  geom_line(color = "darkred") +
  labs(title = "20 napos gördülő szórás – volatilitás klasztereződése QCOM",
       x = "Dátum", y = "Szórás (volatilitás)")
ggplot(adat, aes(x = Date, y = nvda_roll_sd20)) +
  geom_line(color = "darkgreen") +
  labs(title = "20 napos gördülő szórás – volatilitás klasztereződése, NVDA",
       x = "Dátum", y = "Szórás (volatilitás)")
ggplot(adat, aes(x = Date, y = amd_roll_sd20)) +
  geom_line(color = "darkblue") +
  labs(title = "20 napos gördülő szórás – volatilitás klasztereződése AMD",
       x = "Dátum", y = "Szórás (volatilitás)")

# szemmel jól látható a szórások ingaddozása, vagyis a szórás időben nem állandó, ami arch hatást sejtet


# ACF az abszolút loghozamokra (volatilitás-proxy)
acf(adat$qcom_vol_proxy, na.action = na.omit,
    main = "ACF – abszolút loghozam QCOM (volatilitás proxy)")
pacf(adat$qcom_vol_proxy, na.action = na.omit,
     main = "ACF – abszolút loghozam QCOM (volatilitás proxy)")

acf(adat$nvda_vol_proxy, na.action = na.omit,
    main = "ACF – abszolút loghozam NVDA (volatilitás proxy)")
pacf(adat$nvda_vol_proxy, na.action = na.omit,
     main = "ACF – abszolút loghozam NVDA (volatilitás proxy)")

acf(adat$amd_vol_proxy, na.action = na.omit,
    main = "ACF – abszolút loghozam AMD (volatilitás proxy)")
pacf(adat$amd_vol_proxy, na.action = na.omit,
     main = "ACF – abszolút loghozam AMD (volatilitás proxy)")
# mindhárom esetben ACF és PACF ábrák esetén is megfigyelhetünk autókorrelációt



#ARCH LM teszt
#H0:nincs ARCH-hatás, a hozamok varianciája nem autokorrelált, nincs volatilitás-klasztereződés
ArchTest(adat$logqcom[-1], lags = 12)
ArchTest(adat$lognvda[-1], lags = 12)
ArchTest(adat$logamd[-1], lags = 12)
#H0-t erősen elutasítjuk mindhárom esetben, tehát van Arch-hatás

#outlierek bemutatása

library(dplyr)
mad_fun <- function(x) {
  m <- median(x, na.rm=TRUE)
  mad_val <- mad(x, constant = 1.4826, na.rm=TRUE)
  (x - m) / mad_val
}

adat$out_mad_qcom <- mad_fun(adat$logqcom)
adat$out_mad_nvda <- mad_fun(adat$lognvda)
adat$out_mad_amd <- mad_fun(adat$logamd)

out_qcom_table <- adat %>%
  filter(abs(out_mad_qcom) > 3.5) %>%
  dplyr::select(Date, logqcom, out_mad_qcom)
out_nvda_table <- adat %>%
  filter(abs(out_mad_nvda) > 3.5) %>%
  dplyr::select(Date, lognvda, out_mad_nvda)
out_amd_table <- adat %>%
  filter(abs(out_mad_amd) > 3.5) %>%
  dplyr::select(Date, logamd, out_mad_amd)

out_qcom_table
out_nvda_table
out_amd_table
#ezek az outlierek


#standardizált hozamok készítése
X <- cbind(
  logqcom = adat$logqcom,
  lognvda = adat$lognvda,
  logamd  = adat$logamd
)

X <- na.omit(X)
X_std <- scale(X, center = TRUE, scale = TRUE)

adat$logqcom_std <- NA
adat$lognvda_std <- NA
adat$logamd_std  <- NA

adat$logqcom_std[!is.na(adat$logqcom) &
                   !is.na(adat$lognvda) &
                   !is.na(adat$logamd)] <- X_std[,"logqcom"]

adat$lognvda_std[!is.na(adat$logqcom) &
                   !is.na(adat$lognvda) &
                   !is.na(adat$logamd)] <- X_std[,"lognvda"]

adat$logamd_std[!is.na(adat$logqcom) &
                  !is.na(adat$lognvda) &
                  !is.na(adat$logamd)]  <- X_std[,"logamd"]
#kis ellenőrzés
max(adat$logqcom_std, na.rm = TRUE)
max(adat$logqcom, na.rm = TRUE)

max(adat$lognvda_std, na.rm = TRUE)
max(adat$lognvda, na.rm = TRUE)

max(adat$logamd_std, na.rm = TRUE)
max(adat$logamd, na.rm = TRUE)


#szétszedem 80%-20%-ra az adatokat a train-testhez
N <- nrow(adat)
train_size <- floor(0.8 * N)

train <- adat[1:train_size, ]
test  <- adat[(train_size+1):N, ]


## GO-garch modell építése

library(rmgarch)

spec_go <- gogarchspec(
  mean.model     = list(model = "constant"),               
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "mvnorm",                           
  ica            = "fastica",
  factors        = 3                                       
)

Y_train <- na.omit(cbind(
  qcom = train$logqcom_std,
  nvda = train$lognvda_std,
  amd  = train$logamd_std
))

fit_go <- gogarchfit(
  spec = spec_go,
  data = Y_train,
  gfun = "tanh"   # fastICA-hoz tipikus
)

summary(fit_go)
show(fit_go)
plot(fit_go, which = 1)   # Független komponensek vizsgálata
plot(fit_go, which = 2)   # Komponensek volatilitása


# ha fut, oké; ha nem volt betöltve, legfeljebb kapsz egy warningot, nem baj
if ("package:nlme" %in% search()) {
  detach("package:nlme", unload = TRUE)
}
methods(class = "goGARCHfit")
methods("sigma")


library(rugarch)
library(rmgarch)


# feltételes szórások a 3 részvényre (T x 3 mátrix)
Sigma_mat <- sigma(fit_go)

# feltételes kovariancia-mátrixok (3 x 3 x T tömb)
H_array <- rcov(fit_go)

# feltételes korreláció-mátrixok (3 x 3 x T tömb)
R_array <- rcor(fit_go)

rho_qcom_nvda <- R_array[1, 2, ]   # 1–2 elem minden t-re
dates_train   <- train$Date        # a train rész dátumai

plot(dates_train, rho_qcom_nvda, type = "l",
     xlab = "Dátum", ylab = "Korreláció",
     main = "Időben változó korreláció – QCOM vs NVDA (GO-GARCH)")

str(adat)

library(gogarch)

# Új adatsor azoknak az adatoknak, amivel dollgozni szeretnénk

sectors <- adat[, c("logqcom_std", "lognvda_std", "logamd_std")]
sectors <- na.omit(sectors)
sectors <- apply(sectors, 2, scale, scale = FALSE)
str(sectors)


library(gogarch)

# GO-GARCH modell
library(gogarch)
gogmm <- gogarch(sectors,
                 formula = ~ garch(1,1),
                 estby   = "ml",
                 lag.max = 100)

gogmm

mods <- gogmm@models

mod1 <- mods[[1]]
mod2 <- mods[[2]]
mod3 <- mods[[3]]

# standardizált maradékok az egyes komponensekre
res1_std <- residuals(mod1, standardize = TRUE)
res2_std <- residuals(mod2, standardize = TRUE)
res3_std <- residuals(mod3, standardize = TRUE)

library(FinTS)
ArchTest(res1_std, lags = 10)
ArchTest(res2_std, lags = 10)
ArchTest(res3_std, lags = 10)

# az arch-tesztek alapján egyik komponens rezidumaiban sem maradt arch hatás vagyis a go garch modellünk jó

Sigma <- cov(sectors)     # a standardizált loghozamok kovariancia-mátrixa
Sigma
eigen(Sigma)$values        # sajátértékek (mind > 0?)
det(Sigma)                 # determináns (nem lehet 0 vagy túl kicsi)


# Feltételes varianciák – időben változó volatilitás
var_ts <- cvar(gogmm) 


rho_qcom_nvda <- var_ts[, 1]

library(ggplot2)
library(dplyr)

df_var <- data.frame(
  Var_QCOM = var_ts[,"V.logqcom_std"],
  Var_NVDA = var_ts[,"V.lognvda_std"],
  Var_AMD = var_ts[,"V.logamd_std"])

df_var$Date <- adat$Date[which(complete.cases(adat[, c("logqcom_std", "lognvda_std", "logamd_std")]))]


# Ábra: QCOM variancia
ggplot(df_var, aes(x = Date, y = Var_QCOM)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  labs(
    title = "Időben változó variancia – QCOM (GO-GARCH)",
    x = "Dátum",
    y = expression(sigma[t]^2)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank()
  )


# Ábra: NVDA variancia
ggplot(df_var, aes(x = Date, y = Var_NVDA)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  labs(
    title = "Időben változó variancia – NVDA (GO-GARCH)",
    x = "Dátum",
    y = expression(sigma[t]^2)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank()
  )

# Ábra: AMD variancia
ggplot(df_var, aes(x = Date, y = Var_AMD)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  labs(
    title = "Időben változó variancia – AMD (GO-GARCH)",
    x = "Dátum",
    y = expression(sigma[t]^2)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank()
  )




# Feltételes korrelációk
cor_ts <- ccor(gogmm)
head(cor_ts)
colnames(cor_ts)

df_cor <- data.frame(
  cor_1 = cor_ts[,"lognvda_std & logqcom_std"],
  cor_2 = cor_ts[,"logamd_std & logqcom_std"],
  cor_3 = cor_ts[,"logamd_std & lognvda_std"])

df_cor$Date <- adat$Date[which(complete.cases(adat[, c("logqcom_std", "lognvda_std", "logamd_std")]))]


# Ábra: korreláció – QCOM vs NVDA
ggplot(df_cor, aes(x = Date, y = cor_1)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  labs(
    title = "Időben változó korreláció – QCOM vs NVDA",
    x = "Dátum",
    y = expression(rho[t])
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank()
  )


# Ábra:korreláció – QCOM vs AMD
ggplot(df_cor, aes(x = Date, y = cor_2)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  labs(
    title = "Időben változó korreláció – QCOM vs AMD",
    x = "Dátum",
    y = expression(rho[t])
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank()
  )

# Ábra: korreláció – AMD vs NVDA
ggplot(df_cor, aes(x = Date, y = cor_3)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  labs(
    title = "Időben változó korreláció – AMD vs NVDA",
    x = "Dátum",
    y = expression(rho[t])
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank()
  )


# Portfólió VaR

alpha <- 0.01
w <- c(1/3, 1/3, 1/3)   # QCOM, NVDA, AMD súlyok

# Varianciák
var_ts <- cvar(gogmm)

var_q <- as.numeric(var_ts[,"V.logqcom_std"])
var_n <- as.numeric(var_ts[,"V.lognvda_std"])
var_a <- as.numeric(var_ts[,"V.logamd_std"])

# Korrelációk
cor_ts <- ccor(gogmm)

rho_nq <- as.numeric(cor_ts[,"lognvda_std & logqcom_std"])
rho_aq <- as.numeric(cor_ts[,"logamd_std & logqcom_std"])
rho_an <- as.numeric(cor_ts[,"logamd_std & lognvda_std"])

# Kovarianciák
cov_nq <- rho_nq * sqrt(var_n * var_q)
cov_aq <- rho_aq * sqrt(var_a * var_q)
cov_an <- rho_an * sqrt(var_a * var_n)

# Portfólió variancia és szórás
wq <- w[1]; wn <- w[2]; wa <- w[3]

var_p <- (wq^2)*var_q + (wn^2)*var_n + (wa^2)*var_a +
  2*wq*wn*cov_nq + 2*wq*wa*cov_aq + 2*wn*wa*cov_an

sig_p <- sqrt(var_p)

## VaR idősor
VaR_p <- sig_p * qnorm(alpha)

# Realizált portfólióhozam
rp <- as.numeric(sectors[,"logqcom_std"])*wq +
  as.numeric(sectors[,"lognvda_std"])*wn +
  as.numeric(sectors[,"logamd_std"])*wa

# Sértések aránya
exc <- as.integer(rp < VaR_p)
exc_rate <- mean(exc)

exc_rate


## KUPIEC backtest

Tn <- length(exc)
N  <- sum(exc)

p0 <- alpha
pH <- N / Tn


LR_uc <- -2 * ( (Tn-N)*log((1-p0)/(1-pH)) + N*log(p0/pH) )
p_uc  <- 1 - pchisq(LR_uc, df = 1)

c(
  Observations = Tn,
  Exceptions   = N,
  ExceptionRate = N / Tn,
  LR_uc = LR_uc,
  p_value = p_uc
)

####

var_ts <- cvar(gogmm)
vol_data <- var_ts
colnames(vol_data) <- c("qcom_var", "nvda_var", "amd_var")
vol_data <- na.omit(vol_data)
library(vars)

lagselect <- VARselect(vol_data, lag.max = 10, type = "const")
lagselect$selection 

p <- lagselect$selection[1]  # mondjuk AIC szerint

var_model <- VAR(vol_data, p = p, type = "const")


#spillover becsles
#install.packages("frequencyConnectedness")
library(frequencyConnectedness)

# FEVD
Theta <- genFEVD(var_model, n.ahead = 10)

# Teljes spillover index
TSI <- (sum(Theta) - sum(diag(Theta))) / 3 * 100

# Directional
TO   <- colSums(Theta) - diag(Theta)
FROM <- rowSums(Theta) - diag(Theta)
NET  <- TO - FROM

Theta
TSI
TO
FROM
NET

#pontos spillover
res <- spilloverDY12(var_model, n.ahead = 10, no.corr = TRUE)
res

#rolling window kezdete
#nézzük meg a sima gogarchroll fv-t

library(rmgarch)

adatb<- adat[2:nrow(adat),c("logqcom","lognvda","logamd")]

str(adatb)

roll <- gogarchroll(
  spec = spec_go,
  data = adatb,
  n.ahead = 1,
  forecast.length = 500,     
  refit.every = 50,          
  refit.window = "moving"
)

show(roll)
summary(roll)

fc <- gogarchforecast(fit, n.ahead = 1)

fits <- roll@model$fit
length(fits)
#0 length az hibát jelent a függvény lefutásában -> próbáljuk meg manuálisan csinálni


XY <- scale(adatb, center = TRUE, scale = FALSE)
#mátrix-á alakítás


class(XY)   
mode(XY)    
dim(XY)
#az adatok jónak tűnnek

n_roll <- floor((nrow(XY) - window)/step) + 1
mu_roll <- array(NA, dim=c(n_roll, ncol(XY)))
sigma_roll <- array(NA, dim=c(n_roll, ncol(XY)))
cov_roll <- vector("list", n_roll)

manual_gogarch_roll <- function(XY, spec,
                                window = 2013,
                                step = 50,
                                n.ahead = 1,
                                solver = "solnp",
                                verbose = TRUE) {
  if (!is.matrix(X)) stop("XY must be a numeric matrix.")
  Tn <- nrow(X)
  p <- ncol(X)
  if (window >= Tn) stop("túl kicsi ablak")
  if (step <= 0) stop("poz kell hogy legyen")
  
  end_points <- seq(window, Tn, by = step)
  n_roll <- length(end_points)
  if (n_roll < 1) stop("kisebb ablak vagy lépés kell")
  if (verbose) message("rolling fits: ", n_roll)
  
  mu_roll <- matrix(NA_real_, nrow = n_roll, ncol = p)
  sigma_roll <- matrix(NA_real_, nrow = n_roll, ncol = p)
  cov_roll <- vector("list", n_roll)
  failures <- vector("list", n_roll)  # tároljuk az esetleges hibákat
  
  for (i in seq_len(n_roll)) {
    end_idx <- end_points[i]
    data_window <- X[1:end_idx, , drop = FALSE]  # soha nem out-of-bounds
    
    
    fit <- tryCatch({
      gogarchfit(spec, data_window, solver = solver)
    }, error = function(e) {
      failures[[i]] <<- paste0("fit_error: ", conditionMessage(e))
      return(NULL)
    })
    
    if (is.null(fit)) {
      mu_roll[i, ] <- NA
      sigma_roll[i, ] <- NA
      cov_roll[[i]] <- NA
      if (verbose) message("Iteration ", i, ": fit failed.")
      next
    }
    
    fc <- tryCatch({
      gogarchforecast(fit, n.ahead = n.ahead)
    }, error = function(e) {
      failures[[i]] <<- paste0("forecast_error: ", conditionMessage(e))
      return(NULL)
    })
    
    if (is.null(fc)) {
      mu_roll[i, ] <- NA
      sigma_roll[i, ] <- NA
      cov_roll[[i]] <- NA
      if (verbose) message("Iteration ", i, ": forecast failed.")
      next
    }
    
    
    mu_val <- NULL
    if (!is.null(fc@mforecast$mu)) {
      # fc@mforecast$mu lehet vektor vagy mátrix; harmonizáljuk
      mu_val <- as.numeric(fc@mforecast$mu)
      if (length(mu_val) != p) mu_val <- NA_real_
    } else {
      mu_val <- NA_real_
    }
    mu_roll[i, ] <- mu_val
    
    
    sigma_val <- NA_real_
    if (!is.null(fc@mforecast$sigma)) {
      tmp <- as.numeric(fc@mforecast$sigma)
      if (length(tmp) == p) sigma_val <- tmp
      else sigma_val <- NA_real_
    }
    sigma_roll[i, ] <- sigma_val
    
    
    if (!is.null(fc@mforecast$cov)) {
      cov_mat <- fc@mforecast$cov
      # ellenőrizzük, hogy kvadratikus mátrix-e p x p
      if (is.matrix(cov_mat) && all(dim(cov_mat) == c(p, p))) {
        cov_roll[[i]] <- cov_mat
      } else {
        cov_roll[[i]] <- NA
      }
    } else {
      cov_roll[[i]] <- NA
    }
    
    if (verbose) {
      message(sprintf("Iter %d/%d done (end_idx=%d): mu ok=%s, sigma ok=%s, cov ok=%s",
                      i, n_roll, end_idx,
                      !any(is.na(mu_roll[i, ])),
                      !any(is.na(sigma_roll[i, ])),
                      !is.na(cov_roll[[i]]) ))
    }
  } 
  
  return(list(
    end_points = end_points,
    mu = mu_roll,
    sigma = sigma_roll,
    cov = cov_roll,
    failures = failures
  ))
}
#lefut gond nélkül látszólag a rolling window gogarchra

n <- nrow(X)

res <- manual_gogarch_roll(
  X = X,
  window = 2013,
  step = 50,
  n.ahead = 1,
  verbose = TRUE
)
#eredmények kiszedése

corr_list <- lapply(res$cov, function(C) {
  if (is.matrix(C)) {
    out <- cov2cor(C)
    return(out)
  } else {
    return(NA)
  }
})

corr_list

i <- 5

res$mu[i, ]           # adott pont mu-je
res$sigma[i, ]        # adott pont volatilitása
res$cov[[i]] 
#a volatility, a cov és a corr is NA

flatten_cov <- function(cov_list) {
  p <- ncol(cov_list[[which(!sapply(cov_list, is.null))[1]]])
  
  out <- lapply(seq_along(cov_list), function(i) {
    if (is.matrix(cov_list[[i]])) {
      C <- cov_list[[i]]
      data.frame(
        iter = i,
        as.list(as.vector(C))   # p*p hosszú vektor
      )
    } else {
      data.frame(
        iter = i,
        as.list(rep(NA, p*p))
      )
    }
  })
  do.call(rbind, out)
}

which(sapply(res$cov, is.matrix))
cov_df <- flatten_cov(res$cov)
#innen már nem fut le rendesen, és ez látszik is az eredményekből, viszont megmutatjuk hogyan kéne ennek kinéznie
colSums(is.na(XY))
is.numeric(XY)

spec0 <- gogarchspec()
#nem fog lefutni mivel nem működik rendesen a gogarchspec() függvény


plot(res$mu[,1], type="l", main="mu – 1. változó")

matplot(res$mu, type="l", lty=1, main="Rolling mu")


fit_test <- gogarchfit(spec, X[1:2013, ])
fc_test <- gogarchforecast(fit_test, n.ahead = 1)

slotNames(fc_test@mforecast)
fc_test@mforecast$mu
fc_test@mforecast$sigma
fc_test@mforecast$cov

library(rugarch)  
library(fastICA)  
#esetleg ha univariáns GARCH-ot illesztünk a változókra?
manual_gogarch_roll <- function(X, window = 2013, step = 50, n.ahead = 1, verbose = TRUE) {
  if (!is.matrix(X)) X <- as.matrix(X)
  Tn <- nrow(X)
  p <- ncol(X)
  end_points <- seq(window, Tn, by = step)
  n_roll <- length(end_points)
  
  mu_roll <- matrix(NA_real_, nrow = n_roll, ncol = p)
  sigma_roll <- matrix(NA_real_, nrow = n_roll, ncol = p)
  cov_roll <- vector("list", n_roll)
  
  for (i in seq_len(n_roll)) {
    end_idx <- end_points[i]
    data_window <- X[1:end_idx, , drop = FALSE]
    
    garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                             distribution.model = "norm")
    
    garch_fits <- lapply(1:p, function(j) {
      tryCatch(ugarchfit(garch_spec, data_window[, j]), error = function(e) NULL)
    })
    
    mu_vals <- numeric(p)
    sigma_vals <- numeric(p)
    for (j in 1:p) {
      fit <- garch_fits[[j]]
      if (!is.null(fit)) {
        fc <- ugarchforecast(fit, n.ahead = n.ahead)
        mu_vals[j] <- as.numeric(fc@forecast$seriesFor)
        sigma_vals[j] <- as.numeric(fc@forecast$sigmaFor)
      } else {
        mu_vals[j] <- NA
        sigma_vals[j] <- NA
      }
    }
    mu_roll[i, ] <- mu_vals
    sigma_roll[i, ] <- sigma_vals
    
    ica_res <- tryCatch(fastICA(data_window, n.comp = p, alg.typ = "parallel", fun = "logcosh"),
                        error = function(e) NULL)
    
    if (!is.null(ica_res)) {
      S <- ica_res$S  
      A <- ica_res$A  
      
      D <- diag(sigma_vals^2)
      cov_roll[[i]] <- tryCatch(A %*% D %*% t(A), error = function(e) NA)
    } else {
      cov_roll[[i]] <- NA
    }
    
    if (verbose) {
      message(sprintf("Iteration %d/%d done (end_idx=%d)", i, n_roll, end_idx))
    }
  }
  
  return(list(
    end_points = end_points,
    mu = mu_roll,
    sigma = sigma_roll,
    cov = cov_roll
  ))
}

res <- manual_gogarch_roll(
  X = X,
  window = 2013,
  step = 50,
  n.ahead = 1,
  verbose = TRUE
)

res$mu

res$sigma 

res$cov
#cov és sigma továbbra is NA -> ez R hiba, nem lehet kóddal megoldani

#Rolling spillover

library(vars)
library(frequencyConnectedness)

rolling_spillover_DY <- function(X,
                                 window = 1000,
                                 step = 10,
                                 p = 1,
                                 n.ahead = 10,
                                 verbose = TRUE) {
  
  X <- as.matrix(X)
  Tn <- nrow(X)
  k  <- ncol(X)
  
  end_points <- seq(window, Tn, by = step)
  n_roll <- length(end_points)
  
  TSI  <- numeric(n_roll)
  TO   <- matrix(NA, n_roll, k)
  FROM <- matrix(NA, n_roll, k)
  NET  <- matrix(NA, n_roll, k)
  Theta_list <- vector("list", n_roll)
  
  for (i in seq_along(end_points)) {
    end_i   <- end_points[i]
    start_i <- end_i - window + 1
    
    X_win <- X[start_i:end_i, , drop = FALSE]
    
    # VAR
    var_model <- tryCatch(
      VAR(X_win, p = p, type = "const"),
      error = function(e) NULL
    )
    
    if (is.null(var_model)) {
      TSI[i] <- NA
      next
    }
    
    # FEVD
    Theta <- tryCatch(
      genFEVD(var_model, n.ahead = n.ahead),
      error = function(e) NULL
    )
    
    if (is.null(Theta)) {
      TSI[i] <- NA
      next
    }
    
    Theta_list[[i]] <- Theta
    
    # Spillover mutatók (ugyanaz, mint a kódod!)
    TSI[i]  <- (sum(Theta) - sum(diag(Theta))) / k * 100
    TO[i, ] <- colSums(Theta) - diag(Theta)
    FROM[i, ] <- rowSums(Theta) - diag(Theta)
    NET[i, ] <- TO[i, ] - FROM[i, ]
    
    if (verbose)
      message("Rolling window ", i, "/", n_roll,
              " [", start_i, ":", end_i, "]")
  }
  
  colnames(TO)   <- colnames(X)
  colnames(FROM) <- colnames(X)
  colnames(NET)  <- colnames(X)
  
  return(list(
    end_points = end_points,
    TSI = TSI,
    TO = TO,
    FROM = FROM,
    NET = NET,
    Theta = Theta_list
  ))
}

res_roll <- rolling_spillover_DY(
  X = XY,          # a log-diff hozam mátrixod
  window = 2013,   # rolling ablak
  step = 50,
  p = 1,
  n.ahead = 10
)

plot(res_roll$end_points, res_roll$TSI, type = "l",
     main = "Rolling Total Spillover Index",
     ylab = "TSI (%)", xlab = "Idő")


