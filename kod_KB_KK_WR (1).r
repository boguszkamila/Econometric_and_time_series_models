library("forecast")
dane = read.table("C:/Users/ktoxi/OneDrive/Pulpit/bioinf/zaawansowane elementy Mucha/daneMucha_updated.csv", sep = ',', header = TRUE)
head(dane)
br = dane$stopa
br = ts(br, start = c(2015,1), frequency = 12)
br
#odcinamy rok danych zeby sprawdzic czy nasza prognoza jest dobra
br.ts = window(br, end = c(2022,12))
br.ts
br.test = window(br, start = c(2022,11))
length(br.test)
plot(br.ts)
monthplot(br.ts)
par(mar=c(4, 2, 2, 1) + 0.1) 
seasonplot(br.ts, year.labels = T, col = rainbow(3), main = "")
lag.plot(br.ts, lags = 12, do.lines = F)
Acf(br.ts)
Pacf(br.ts)

# roznicowanie z opoznieniem 12

br.diff12 = diff(br.ts, lag = 12)
plot(br.diff12)
lag.plot(br.diff12, lags = 12, do.lines = F)
Acf(br.diff12)
Pacf(br.diff12)

# roznicowanie z opoznieniem 1

br.diff12.diff = diff(br.diff12, lag = 12)
plot(br.diff12.diff)
lag.plot(br.diff12.diff, lags = 12, do.lines = F)
Acf(br.diff12.diff) #MA(1)
Pacf(br.diff12.diff) #AR(1)

# dekompozycja klasyczna

br.decomp = decompose(br.ts)
plot(br.decomp)
br.decomp.res = br.decomp$random
Acf(br.decomp.res) #MA(1)
Pacf(br.decomp.res) #AR(1)

# dekompozycja na podstawie modelu regresji

br.tslm = tslm(br.ts ~ trend + season)
br.tslm.res = br.tslm$residuals
lag.plot(br.tslm.res, lag = 12, do.lines = F)
Acf(br.tslm.res) #MA(1)
Pacf(br.tslm.res)

# model MA(1)

model.MA1 = Arima(br.ts, order = c(0,1,1), seasonal = c(0,1,0))
summary(model.MA1)

# MA(1) - istotnosc wspolczynnikow

coefs = model.MA1$coef
coefs
coefs.sd = sqrt(diag(model.MA1$var.coef))
coefs  
coefs.sd
ind = abs(coefs/(1.96*coefs.sd)) 
ind
signif = which(ind >= 1)
signif 
temp.fixed = numeric(1)
temp.fixed[signif] = NA

model.MA1.fixed = Arima(br.ts, order = c(0,1,1), seasonal = c(0,1,0), fixed = temp.fixed)
summary(model.MA1.fixed)

# konstruowanie prognozy dla modelu MA(1)

model.MA1.prognoza = forecast(model.MA1.fixed, h =12)
model.MA1.prognoza$mean #prognoza punktowa

ts.plot(model.MA1.prognoza$mean)
plot(model.MA1.prognoza)
lines(br.test, col = "red")
accuracy(model.MA1.prognoza, br.test)[,c("ME","MAE","MPE",
                                        "MAPE","RMSE", "Theil's U")]

# model AR(3)

model.AR3 = Arima(br.ts, order = c(3,1,0), seasonal = c(0,1,0))
summary(model.AR3)
# nie oplaca się

# AR(3) - istotnosc wspolczynnikow
model.AR3 = Arima(br.ts, order = c(3,1,0), seasonal = c(0,1,0))
summary(model.AR3)

coefs = model.AR3$coef
coefs
coefs.sd = sqrt(diag(model.AR3$var.coef))
coefs  
coefs.sd
ind = abs(coefs/(1.96*coefs.sd)) 
ind
signif = which(ind >= 1)
signif 
temp.fixed = numeric(3)
temp.fixed[signif] = NA

model.AR3.fixed = Arima(br.ts, order = c(3,1,0), seasonal = c(0,1,0), fixed = temp.fixed)
summary(model.AR3.fixed)
# tez slabo
# model AR(1)

model.AR1 = Arima(br.ts, order = c(1,1,0), seasonal = c(0,1,0))
summary(model.AR1)

# AR(1) - istotnosc wspolczynnikow

coefs = model.AR1$coef
coefs
coefs.sd = sqrt(diag(model.AR1$var.coef))
coefs  
coefs.sd
ind = abs(coefs/(1.96*coefs.sd)) 
ind
signif = which(ind >= 1)
signif 
temp.fixed = numeric(1)
temp.fixed[signif] = NA

model.AR1.fixed = Arima(br.ts, order = c(1,1,0), seasonal = c(0,1,0), fixed = temp.fixed)
summary(model.AR1.fixed)

# konstruowanie prognozy dla modelu AR(1)

model.AR1.prognoza = forecast(model.AR1.fixed, h =12)
model.AR1.prognoza$mean #prognoza punktowa

ts.plot(model.AR1.prognoza$mean)
plot(model.AR1.prognoza)
lines(br.test, col = "red")
accuracy(model.AR1.prognoza, br.test)[,c("ME","MAE","MPE",
                                         "MAPE","RMSE", "Theil's U")]

# model MA(2)

model.MA2 = Arima(br.ts, order = c(0,1,2), seasonal = c(0,1,0))
summary(model.MA2)

# MA(2) - istotnosc wspolczynnikow

coefs = model.MA2$coef
coefs
coefs.sd = sqrt(diag(model.MA2$var.coef))
coefs  
coefs.sd
ind = abs(coefs/(1.96*coefs.sd)) 
ind
signif = which(ind >= 1)
signif 
temp.fixed = numeric(2)
temp.fixed[signif] = NA

model.MA2.fixed = Arima(br.ts, order = c(0,1,2), seasonal = c(0,1,0), fixed = temp.fixed)
summary(model.MA2.fixed)
# nope zostaje MA(1) 

# model AR(2)

model.AR2 = Arima(br.ts, order = c(2,1,0), seasonal = c(0,1,0))
summary(model.AR2)

# AR(2) - istotnosc wspolczynnikow

coefs = model.AR2$coef
coefs
coefs.sd = sqrt(diag(model.AR2$var.coef))
coefs  
coefs.sd
ind = abs(coefs/(1.96*coefs.sd)) 
ind
signif = which(ind >= 1)
signif 
temp.fixed = numeric(2)
temp.fixed[signif] = NA

model.AR2.fixed = Arima(br.ts, order = c(2,1,0), seasonal = c(0,1,0), fixed = temp.fixed)
summary(model.AR2.fixed)

model.AR2.prognoza = forecast(model.AR2.fixed, h =12)
model.AR2.prognoza$mean #prognoza punktowa

ts.plot(model.AR2.prognoza$mean)
plot(model.AR2.prognoza)
lines(br.test, col = "red")
accuracy(model.AR2.prognoza, br.test)[,c("ME","MAE","MPE",
                                         "MAPE","RMSE", "Theil's U")]




# auto model
model.auto = auto.arima(br.ts)
summary(model.auto)
model.auto.prognoza = forecast(model.auto, h =12)
model.auto.prognoza$mean

ts.plot(model.auto.prognoza$mean)
plot(model.auto.prognoza)
lines(br.test,col = "red")
accuracy(model.auto.prognoza, br.test)[,c("ME","MAE","MPE",
                                          "MAPE","RMSE","Theil's U")]

# automatyczny dobór modelu dla reszt po tslm()

tslm.prognoza = forecast(br.tslm, h = 12)
tslm.prognoza$mean
model.res.auto = auto.arima(br.tslm.res)
summary(model.res.auto)
model.res.auto.prognoza = forecast(model.res.auto, h = 12)
model.res.auto.prognoza$mean
model.tslm.res.auto.prognoza = tslm.prognoza$mean + model.res.auto.prognoza$mean
model.tslm.res.auto.prognoza
ts.plot(model.tslm.res.auto.prognoza)
lines(br.test,col = "red")
accuracy(model.tslm.res.auto.prognoza, br.test)[,c("ME","MAE","MPE",
                                     "MAPE","RMSE","Theil's U")]

### MODEL EKONOMETRYCZNY
library('caret')
library('dplyr')
set.seed(2137)
dane_test = dane%>%filter(dane$rok == c(2023))
dane_ucz = dane%>%filter(dane$rok != c(2023))
tail(dane_ucz)
Y = dane_ucz$stopa
X1 = dane_ucz$sr_zarob
X2 = dane_ucz$sr.temp
X3 = dane_ucz$stopa_oprocentowania
X4 = as.numeric(dane_ucz$saldo_obrotow_tow)
X5 = dane_ucz$pzws

par(mfrow = c(2,3))
plot(Y~X1)
plot(Y~X2)
plot(Y~X3)
plot(Y~X4)
plot(Y~X5)

#im większe zarobki tym mniejsza stopa oprocentowania
#X2 im więcej ludzi tym większa stopa bezrobocia
corelation_XY <- rbind(c(cor.test(Y,X1)$p.value,cor.test(Y,X1)$estimate),
                    c(cor.test(Y,X2)$p.value,cor.test(Y,X2)$estimate),
                    c(cor.test(Y,X3)$p.value,cor.test(Y,X3)$estimate),
                    c(cor.test(Y,X4)$p.value,cor.test(Y,X4)$estimate),
                    c(cor.test(Y,X5)$p.value,cor.test(Y,X5)$estimate))
rownames(corelation_XY) <- c("Y i X1:", "Y i X2:", "Y i X3:","Y i X4:", "Y i X5:")
options(scipen = 999)
round(corelation_XY, digits = 2)
corelation_XX <- rbind(c(cor.test(X1,X2)$p.value,cor.test(X1,X2)$estimate),
                      c(cor.test(X1,X3)$p.value,cor.test(X1,X3)$estimate),
                      c(cor.test(X1,X4)$p.value,cor.test(X1,X4)$estimate),
                      c(cor.test(X1,X5)$p.value,cor.test(X1,X5)$estimate),
                      c(cor.test(X2,X3)$p.value,cor.test(X2,X3)$estimate),
                      c(cor.test(X2,X4)$p.value,cor.test(X2,X4)$estimate),
                      c(cor.test(X2,X5)$p.value,cor.test(X2,X5)$estimate),
                      c(cor.test(X3,X4)$p.value,cor.test(X3,X4)$estimate),
                      c(cor.test(X3,X5)$p.value,cor.test(X3,X5)$estimate),
                      c(cor.test(X4,X5)$p.value,cor.test(X4,X5)$estimate))

rownames(corelation_XX) <- c("X1 i X2:", "X1 i X3:", "X1 i X4:", "X1 i X5", "X2 i X3:", "X2 i X4:", "X2 i X5:", "X3 i X4:", "X3 i X5", "X4 i X5")
options(scipen = 999)
round(corelation_XX, digits = 2)
#WSPÓŁCZYNNIK ZMIENNOSCI
zm_X1 = sd(X1)/mean(X1)
cat("Zmienna X1: ", zm_X1)
zm_X2 = sd(X2)/mean(X2)
cat("Zmienna X2: ", zm_X2)
zm_X3 = sd(X3)/mean(X3)
cat("Zmienna X3: ", zm_X3)
zm_X4 = sd(X4)/mean(X4)
cat("Zmienna X4: ", zm_X4)
zm_X5 = sd(X5)/mean(X5)
cat("Zmienna X5: ", zm_X5)
#Tworzeni modeli - dobor zmiennych
R0 <- matrix(c(-0.79, -0.15, -0.24, 0.34), (4:1))
R0
matrix_R = rbind(c(1, 0.02, 0.49,-0.49), c(0.02, 1, 0.02,0.03), c(0.49, 0.02, 1,-0.67),c(-0.49, 0.03, -0.67,1))
matrix_R
#pi12 = lm(Y ~ X1 + X2, data = dane_ucz)
#pi13 = lm(Y ~ X1 + X3, data = dane_ucz)
#pi14 = lm(Y ~ X1 + X4, data = dane_ucz)
#pi23 = lm(Y ~ X2 + X3, data = dane_ucz)
#pi24 = lm(Y ~ X2 + X4, data = dane_ucz)
#pi34 = lm(Y ~ X3 + X4, data = dane_ucz)
#pi123 = lm(Y ~ X1 + X2 + X3, data = dane_ucz)
#pi124 = lm(Y ~ X1 + X2 + X4, data = dane_ucz)
#pi134 = lm(Y ~ X1 + X3 + X4, data = dane_ucz)
#pi234 = lm(Y ~ X2 + X3 + X4, data = dane_ucz)
#pi1234 = lm(Y ~ X1 +X2 + X3 + X4, data = dane_ucz)
#BIC(pi12, pi13, pi14, pi23, pi24, pi34, pi123, pi124, pi134, pi234, pi1234)

(h1 = (0.79**2/1)) #0.6241
(h2 = (0.15**2/1)) #0.0225
(h3 = (0.24**2/1)) #0.0576
(h4 = (0.34**2/1)) #0.1156
(h12 = (0.79**2/(1+0.02))+(0.15**2/(1+0.02))) #0.6339216
(h13 = (0.79**2/(1+0.49))+(0.24**2/(1+0.49))) #0.4575168
(h14 = (0.79**2/(1+0.49))+(0.34**2/(1+0.49))) #0.496443
(h23 = (0.15**2/(1+0.02))+(0.24**2/(1+0.02))) #0.07852941
(h24 = (0.15**2/(1+0.03))+(0.34**2/(1+0.03))) #0.134
(h34 = (0.24**2/(1+0.67))+(0.34**2/(1+0.67))) #0.1037
(h123 = (0.79**2/(1+0.02+0.49))+(0.15**2/(1+0.02+0.49))+(0.24**2/(1+0.02+0.49))) #0.4664
(h124 = (0.79**2/(1+0.02+0.49))+(0.15**2/(1+0.02+0.03))+(0.34**2/(1+0.03+0.49))) #0.51
(h234 = (0.15**2/(1+0.02+0.03))+(0.24**2/(0.02+1+0.67))+(0.34**2/(0.03+0.67+1))) #0.1235
(h1234 = (0.79**2/(1+0.02+0.49+0.49))+(0.15**2/(0.02+1+0.02+0.03))+(0.24**2/(0.49+0.02+1+0.67))+(0.34**2/(0.49+0.03+0.67+1))) #0.412

tab_h <- rbind(h1, h2, h3, h4, h12, h13, h14, h23, h24, h34, h123, h124, h234, h1234)
rownames(tab_h) <- c("h1", "h2", "h3", "h4", "h12", "h13", "h14", "h23", "h24", "h34", "h123", "h124", "h234", "h1234")
tab_h
# MODEL: y = a0+a1X1+a2X2+e
summary(lm(Y ~ X1+X2, data = dane_ucz))
model1 = lm(Y ~ X1+X2, data = dane_ucz)
  # y = 2593.21-0,277X1-5.2023X2+e

### METODA GRAFOWA
(rstar = sqrt((1.985**2)/(1.985**2+96-2))) #0.2005765 mniejsze lub równe to odrzucamy
matrix_Rstar = rbind(c(1, 0, 0.49, -0.49), c(0, 1, 0, 0), c(0.49, 0, 1, -0.67), c(-0.49, 0, -0.67, 1))
matrix_Rstar
summary(lm(Y ~ X1, data = dane_ucz))
model2 = lm(Y ~ X1, data = dane_ucz)
  #MODEL y = a0+a1X1+e
  # y = 2549.1476 - 0.2782X1+e
### METODA AUTOMATYCZNEJ FUNKCJI DOBORU ZMIENNYCH
step(lm(Y ~ X1 + X2 + X3 +X4, data=dane_ucz))
summary(lm(Y ~ X1 + X2 + X3, data=dane_ucz)) 
model3 = lm(Y ~ X1  + X2 + X3, data=dane_ucz)
  # y = 2706.73 - 0,311X1 - 5.287X2 +35.181X3+e
  
### METODA ANALIZY WSPÓŁCZYNNIKÓW KORELACJI
  #na kartce 
summary(lm(Y ~ X1, data=dane_ucz))
model4 = lm(Y ~ X1, data=dane_ucz)

#regresja krokowa
model5 = lm(Y ~ X1 + X2 + X3 + X4 + X5, dane_ucz)
summary(model5)
model5 = lm(Y ~ X1 + X2 + X4 + X5, dane_ucz)
summary(model5)

### PROGNOZOWANIE
# model 2 i 4 to te same modele
anova(model1, model2)
anova(model1, model3)
anova(model1, model5)
anova(model2, model3)
anova(model2, model5)
anova(model3, model5)


### BADANIE WŁAŚCIWOŚCI SKŁADNIKÓW LOSOWYCH
  #Normlaność shapiro-wilk
    #H0 = dane pochodzą z rozkładu normalnego
    #H1 = dane nie pochodzą z rozkładu normalnego
shapiro.test(model1$residuals)
shapiro.test(model2$residuals)
shapiro.test(model3$residuals)
shapiro.test(model5$residuals)
    #dane nie pochodzą z rozkładu normalnego
  #Autokorelacja - test Durbina-Watsona
    #H0 = Brak autokorelacji zmiennych
    #H1 = Występuje autokorelacja zmiennych
library(lmtest)
dwtest(model1, alternative = 'two.sided')
dwtest(model2, alternative = 'two.sided')
dwtest(model3, alternative = 'two.sided')
dwtest(model5, alternative = 'two.sided')
    #występuje autokorelacja zmiennych
    #przekształcenie Cochrana-Orcutta
library('orcutt')
model1_co = cochrane.orcutt(model1)
model2_co = cochrane.orcutt(model2)
model3_co = cochrane.orcutt(model3)
model5_co = cochrane.orcutt(model5)

dwtest(model1_co, alternative = 'two.sided')
dwtest(model2_co, alternative = 'two.sided')
dwtest(model3_co, alternative = 'two.sided')
dwtest(model5_co, alternative = 'two.sided')

  #homoskedastyczność - test Breush'a-Pagan'a
    #H0 jednorodność wiariancji składników losowych
    #H1 Brak jednorodności wariancji składników losowych
bptest(model1_co)
bptest(model2_co)
bptest(model3_co)
bptest(model5_co)
  #losowść - test serii
resid1 = model1_co$residuals
resid2 = model2_co$residuals
resid3 = model3_co$residuals
resid5 = model5_co$residuals

signals1 = ifelse(resid1>0,1,0)
signals2 = ifelse(resid2>0,1,0)
signals3 = ifelse(resid3>0,1,0)
signals5 = ifelse(resid5>0,1,0)

runs1 = sum(diff(signals1) != 0)+1
runs2 = sum(diff(signals2) != 0)+1
runs3 = sum(diff(signals3) != 0)+1
runs5 = sum(diff(signals5) != 0)+1

m1n1 = sum(signals1 == 1)
m1n2 = sum(signals1 == 0)
m2n1 = sum(signals2 == 1)
m2n2 = sum(signals2 == 0)
m3n1 = sum(signals3 == 1)
m3n2 = sum(signals3 == 0)
m5n1 = sum(signals5 == 1)
m5n2 = sum(signals5 == 0)

(exp_runs1 = 2*m1n1*m1n2/(m1n1+m1n2)+1)
(exp_runs2 = 2*m2n1*m2n2/(m2n1+m2n2)+1)
(exp_runs3 = 2*m3n1*m3n2/(m3n1+m3n2)+1)
(exp_runs5 = 2*m5n1*m5n2/(m5n1+m5n2)+1)

(var_runs1 = (exp_runs1 - 1)*(exp_runs1 - 2)/(m1n1+m1n2-1))
(var_runs2 = (exp_runs2 - 1)*(exp_runs2 - 2)/(m2n1+m2n2-1))
(var_runs3 = (exp_runs3 - 1)*(exp_runs3 - 2)/(m3n1+m3n2-1))
(var_runs5 = (exp_runs5 - 1)*(exp_runs5 - 2)/(m5n1+m5n2-1))

(z1 = (runs1 - exp_runs1)/sqrt(var_runs1))
(z2 = (runs2 - exp_runs2)/sqrt(var_runs2))
(z3 = (runs3 - exp_runs3)/sqrt(var_runs3))
(z5 = (runs5 - exp_runs5)/sqrt(var_runs5))

(p1 = 2*(1-pnorm(abs(z1))))
(p2 = 2*(1-pnorm(abs(z2))))
(p3 = 2*(1-pnorm(abs(z3))))
(p5 = 2*(1-pnorm(abs(z5))))
    #H0 = reszty są losowe
    #H1 = reszty nie są losowe
  #test symetrii
    #H0 p=0.5
    #H1 p=/=0.5
n = 96
m1 = length(which(model1_co$residuals>0))
m2 = length(which(model2_co$residuals>0))
m3 = length(which(model3_co$residuals>0))
m5 = length(which(model5_co$residuals>0))
(t1 = (m1/n-0.5)/sqrt(((m1/n)*(1-m1/n))/(n-1)))
(t2 = (m2/n-0.5)/sqrt(((m2/n)*(1-m2/n))/(n-1)))
(t3 = (m3/n-0.5)/sqrt(((m3/n)*(1-m3/n))/(n-1)))
(t5 = (m5/n-0.5)/sqrt(((m5/n)*(1-m5/n))/(n-1)))
qt(0.975,96)#1.984884
    #odrzucamy h0 przyjmujemy h1 t1,t2 i t3 jest większe znacznie od tkryt
  #model1 = lm(Y ~ X1+X2, data = dane_ucz)
  #model2 = lm(Y ~ X1, data = dane_ucz)
  #model3 = lm(Y ~ X1  + X2 + X3, data=dane_ucz)
  #model5 = lm(Y ~ X1 + X2 + X4 + X5, dane_ucz)
model_final1 = lm(model1_co$model$YB ~ model1_co$model$XB[,2]+model1_co$model$XB[,3])
model_final2 = lm(model2_co$model$YB ~ model2_co$model$XB[,2])
model_final3 = lm(model3_co$model$YB ~ model3_co$model$XB[,2]+model3_co$model$XB[,3]+model3_co$model$XB[,4])
model_final5 = lm(model5_co$model$YB ~ model5_co$model$XB[,2]+model5_co$model$XB[,3]+model5_co$model$XB[,4]+model5_co$model$XB[,5])
model5_co$model$XB
par(mfrow = c(3,3))
plot(model_final1)
plot(model_final2)
plot(model_final3)
plot(model_final5)

y_1 = predict.orcutt(model1_co) 
y_2 = predict.orcutt(model2_co)
y_3 = predict.orcutt(model3_co)
y_5 = predict.orcutt(model5_co)

blad_prognozy1 = dane_test$stopa - y_1[1:12]
abs_blad1 = abs(dane_test$stopa - y_1[1:12])
kwadrat_blad1 = (dane_test$stopa - y_1[1:12])^2
wzgl_blad1 = ((dane_test$stopa - y_1[1:12])/dane_test$stopa) * 100
wzgl_absolutny_blad1 = (abs(dane_test$stopa - y_1[1:12])/dane_test$stopa) * 100
wzgl_kwadrat_blad1 = ((dane_test$stopa - y_1[1:12])^2/dane_test$stopa) * 100

ME_1 = mean(blad_prognozy1)
MAE_1 = mean(abs_blad1)
MSE_1 = mean(kwadrat_blad1)
MPE_1 = mean(wzgl_blad1)
MAPE_1 = mean(wzgl_absolutny_blad1)

RMSE_1 = sqrt(MSE_1) 
RMSE_1
VRMSE_1 = RMSE_1/mean(dane_test$stopa)

blad_prognozy2 = dane_test$stopa - y_2[1:12]
abs_blad2 = abs(dane_test$stopa - y_2[1:12])
kwadrat_blad2 = (dane_test$stopa - y_2[1:12])^2
wzgl_blad2 = ((dane_test$stopa - y_2[1:12])/dane_test$stopa) * 100
wzgl_absolutny_blad2 = (abs(dane_test$stopa - y_2[1:12])/dane_test$stopa) * 100
wzgl_kwadrat_blad1 = ((dane_test$stopa - y_1[1:12])^2/dane_test$stopa) * 100

ME_2 = mean(blad_prognozy2)
MAE_2 = mean(abs_blad2)
MSE_2 = mean(kwadrat_blad2)
MPE_2 = mean(wzgl_blad2)
MAPE_2 = mean(wzgl_absolutny_blad2)

RMSE_2 = sqrt(MSE_2) 
RMSE_2
VRMSE_2 = RMSE_2/mean(dane_test$stopa)

blad_prognozy3 = dane_test$stopa - y_3[1:12]
abs_blad3 = abs(dane_test$stopa - y_3[1:12])
kwadrat_blad3 = (dane_test$stopa - y_3[1:12])^2
wzgl_blad3 = ((dane_test$stopa - y_3[1:12])/dane_test$stopa) * 100
wzgl_absolutny_blad3 = (abs(dane_test$stopa - y_3[1:12])/dane_test$stopa) * 100
wzgl_kwadrat_blad3 = ((dane_test$stopa - y_3[1:12])^2/dane_test$stopa) * 100

ME_3 = mean(blad_prognozy3)
MAE_3 = mean(abs_blad3)
MSE_3 = mean(kwadrat_blad3)
MPE_3 = mean(wzgl_blad3)
MAPE_3 = mean(wzgl_absolutny_blad3)

RMSE_3 = sqrt(MSE_3) 
RMSE_3
VRMSE_3 = RMSE_3/mean(dane_test$stopa)

blad_prognozy5 = dane_test$stopa - y_5[1:12]
abs_blad5 = abs(dane_test$stopa - y_5[1:12])
kwadrat_blad5 = (dane_test$stopa - y_5[1:12])^2
wzgl_blad5 = ((dane_test$stopa - y_5[1:12])/dane_test$stopa) * 100
wzgl_absolutny_blad5 = (abs(dane_test$stopa - y_5[1:12])/dane_test$stopa) * 100
wzgl_kwadrat_blad5 = ((dane_test$stopa - y_5[1:12])^2/dane_test$stopa) * 100

ME_5 = mean(blad_prognozy5)
MAE_5 = mean(abs_blad5)
MSE_5 = mean(kwadrat_blad5)
MPE_5 = mean(wzgl_blad5)
MAPE_5 = mean(wzgl_absolutny_blad5)

RMSE_5 = sqrt(MSE_5) 
RMSE_5
VRMSE_5 = RMSE_3/mean(dane_test$stopa)

I2_1 = (sum((dane_test$stopa - y_1[1:12])^2))/(sum((dane_test$stopa)^2))
I2_2 = (sum((dane_test$stopa - y_2[1:12])^2))/(sum((dane_test$stopa)^2))
I2_3 = (sum((dane_test$stopa - y_3[1:12])^2))/(sum((dane_test$stopa)^2))
I2_5 = (sum((dane_test$stopa - y_5[1:12])^2))/(sum((dane_test$stopa)^2))

tab_1 <- rbind(I2_1, ME_1, MAE_1, MSE_1, MPE_1, MAPE_1, RMSE_1, VRMSE_1)
rownames(tab_1) <- c("I2", "ME", "MAE", "MSE", "MPE", "MAPE", "RMSE", "VRMSE")

tab_2 <- rbind(I2_2, ME_2, MAE_2, MSE_2, MPE_2, MAPE_2, RMSE_2, VRMSE_2)
rownames(tab_2) <- c("I2", "ME", "MAE", "MSE", "MPE", "MAPE", "RMSE", "VRMSE")

tab_3 <- rbind(I2_3, ME_3, MAE_3, MSE_3, MPE_3, MAPE_3, RMSE_3, VRMSE_3)
rownames(tab_3) <- c("I2", "ME", "MAE", "MSE", "MPE", "MAPE", "RMSE", "VRMSE")

tab_5 <- rbind(I2_5, ME_5, MAE_5, MSE_5, MPE_5, MAPE_5, RMSE_5, VRMSE_5)
rownames(tab_5) <- c("I2", "ME", "MAE", "MSE", "MPE", "MAPE", "RMSE", "VRMSE")

tab <- cbind(tab_1, tab_2,tab_3,tab_5)
colnames(tab) <- c("Model 1", "Model 2","Model 3","Model 5")
options(scipen = 999)
round(tab, digits = 4)

par(mfrow = c(1,1))
y_range <- range(c(y_1, y_2, y_3, y_5, dane_test$stopa), na.rm = TRUE)

# Rysowanie pierwszego wykresu (y_1), ustawienie typu na "l" dla linii, oraz koloru na niebieski
plot(y_1[1:12], type = "p", col = "blue", ylim = y_range, xlab = "Miesiąc", ylab = "Bezrobotni", main = "Wizualizacja modeli")

# Dodawanie kolejnych linii do wykresu
points(y_2[1:12], col = "purple") # y_2 na zielono
points(y_3[1:12], col = "brown") # y_3 na fioletowo
points(y_5[1:12], col = "orange")
points(dane_test$stopa, col = "red") # stopa na czerwono


# Dodanie legendy
legend("bottomleft", legend = c("model_1", "model_2", "model_3","model_5", "faktyczne dane"), col = c("blue", "purple", "brown","orange", "red"),cex =0.5, lty = 1)
