install.packages("xlsx", dep = T)
library("xlsx")
install.packages("foreign")
library("foreign")
install.packages("urca")
library("urca")
install.packages("ggplot2")
library("ggplot2")
install.packages("vars")
library("vars")
install.packages("mFilter")
library("mFilter")
install.packages("tseries")
library("tseries")
install.packages("tidyverse")
library("tidyverse")
install.packages("forecast")
library("forecast")

df  <- readxl::read_xlsx("/Users/irina/Desktop/курсовая/дата.xlsx")

##############################ВЛИЯНИЕ ВЕНЧУРНЫХ ИНВЕСТИЦИЙ И ЭКСПОРТА##########################################

ggplot(data = df) + geom_point(mapping = aes(x = VC, y = exp))

VC <- ts(df$VC, start = c(1997), frequency = 1) 
exp <- ts(df$exp, start = c(1997), frequency = 1) 

plot(cbind(VC, exp))

#OLS

OLS1 <- lm(exp ~ VC)
summary(OLS1)
#взаимосвязь очень мала и статистически почти не значима (влияние венчурных инвест и экспорта)
#VC           0.06664    0.05506   1.210  0.23845  (ни одной звездочки или точки и 0.23845 больше 0.05)


##############################ВЛИЯНИЕ ВЕНЧУРНЫХ ИНВЕСТИЦИЙ И НИОКР##############################################

ggplot(data = df) + geom_point(mapping = aes(x = VC, y = NIOKR))

VC <- ts(df$VC, start = c(1997), frequency = 1) 
NIOKR <- ts(df$NIOKR, start = c(1997), frequency = 1) 

plot(cbind(VC, NIOKR))

#OLS
OLS2 <- lm(NIOKR ~ VC)
summary(OLS2) 
#взаимосвязь статистически значима
#VC           0.38351    0.06579   5.829 6.12e-06 *** (6.12e-06 близко к нулю)

#определение постоянства модели
acf(NIOKR, main = "ACF for NIOKR") #два лага
pacf(NIOKR, main = "PACF for NIOKR") #получаем частичную автокорреляционную функцию от расходов на ниокр

acf(VC, main = "ACF for VC") #два лага
pacf(VC, main = "PACF for VC")

#нахождение оптимальных лагов
df.bv <- cbind(NIOKR, VC)
colnames(df.bv) <- cbind("NIOKR", "VC")

lagselect <- VARselect(df.bv, lag.max = 10, type = "const")
lagselect$selection

#построение VAR 
ModelDf1 <- VAR(df.bv, p = 7, type = "const", season = NULL, exogen = NULL)
summary(ModelDf1)
 
#диагностика VAR

#тест на автокорреляцию
Serial1 <- serial.test(ModelDf1, lags.pt = 16, type = "PT.asymptotic")
Serial1
# p-value = 0.08766 то есть больше 0.05, значит автокорреляция отсутствует

#тест на гетероскедантичность
Arch1 <- arch.test(ModelDf1, lags.multi = 12, multivariate.only = TRUE)
Arch1
#p-value = 1 то есть больше 0.05, значит гетероскедантичность отсутствует

#тест на нормальное распределение остатков 
Norm1 <- normality.test(ModelDf1, multivariate.only = TRUE)
Norm1
#p-value = 0.754, p-value = 0.7936, p-value = 0.4872

#тест на структурные разрывы в остатках
Stability1 <- stability(ModelDf1, type = "OLS-CUSUM")
plot(Stability1)
#графики не пересекают доверительные интервалы значит все окей

#причинно-следственная часть по Грейнджеру
GrangerNIOKR <- causality(ModelDf1, cause = "NIOKR")
GrangerNIOKR
#p-value = 0.0455 что меньше 0,05 и следовательно гипотеза об отсутствии влиянии НИОКР на Венчурные ивестиции отвергается (ниокр влияет на венч инвест)  

GrangerVC <- causality(ModelDf1, cause = "VC")
GrangerVC
#p-value = 0.08432 что больше 0,05, следовательно гипотеза об отсутствии влияния Венчурных инвестиций на НИОКР не отвергается (венч инвест не влияют на ниокр)

ccf(NIOKR, VC, lag.max = 12, type = c("correlation"), plot = TRUE) 
# 2 значимых лага 
grangertest(VC, NIOKR, order = 2) 
# Вероятность что VC влияет на NIOKR 8.5% 

ccf(VC, NIOKR, lag.max = 12, type = c("correlation"), plot = TRUE) 
# 2 значимых лага 
grangertest(NIOKR, VC, order = 2) 
# Вероятность что NIOKR влияет на VC 18.1% 

#функция импульсного отклика (как Венчурные инвестиции среагируют на изменения НИОКР на 20 единиц)
VCirf <- irf(ModelDf1, impulse = "NIOKR", response = "VC", n.ahead = 20, boot = TRUE)
plot(VCirf, ylab = "VC", main = "Shock from NIOKR")
#красная линия сверху это увеличение ниокр в большую сторону, красная линия снизу это доверительный интервал. Соотвественно если НИОКР увел то и Венччурные увел и наоборот, прямопропорциональная зависимость или это значит что зависимости нет как таковой

NIOKRirf <- irf(ModelDf1, impulse = "VC", response = "NIOKR", n.ahead = 20, boot = TRUE)
plot(NIOKRirf, ylab = "NIOKR", main = "Shock from VC")
#изменения венчурных инвестиций влияет на НИОКР отрицательно

#декомпозиция дисперсии (уровень влияния из предыдущих графиков)
FEVD1 <- fevd(ModelDf1, n.ahead = 10)
plot(FEVD1)


#VAR прогноз
forecast <- predict(ModelDf1, n.ahead = 7, ci = 0.95)
plot(forecast, names = "VC")
fanchart(forecast, names = "VC")
plot(forecast, name = "NIOKR")
fanchart(forecast, names = "NIOKR")

##############################ВЛИЯНИЕ ВЕНЧУРНЫХ ИНВЕСТИЦИЙ И КОЭФФ ИЗОБРЕТ АКТИВНОСТИ##############################################

ggplot(data = df) + geom_point(mapping = aes(x = VC, y = koef_act))

VC <- ts(df$VC, start = c(1997), frequency = 1) 
koef_act <- ts(df$koef_act, start = c(1997), frequency = 1) 

plot(cbind(VC, koef_act))

#OLS
OLS3 <- lm(koef_act ~ VC)
summary(OLS3) 
#взаимосвязь статистически значима
#VC           0.02504    0.01010   2.479   0.0209 * (0.0209 близко к 0)

#определение постоянства модели
acf(koef_act, main = "ACF for koef_act") #один лаг
pacf(koef_act, main = "PACF for koef_act")

acf(VC, main = "ACF for VC") #два лага
pacf(VC, main = "PACF for VC")

#нахождение оптимальных лагов
df.bv1 <- cbind(koef_act, VC)
colnames(df.bv1) <- cbind("koef_act", "VC")

lagselect1 <- VARselect(df.bv1, lag.max = 10, type = "const")
lagselect1$selection

#построение VAR 
ModelDf2 <- VAR(df.bv1, p = 7, type = "const", season = NULL, exogen = NULL)
summary(ModelDf2)

#диагностика VAR

#тест на автокорреляцию
Serial2 <- serial.test(ModelDf2, lags.pt = 12, type = "PT.asymptotic")
Serial2
# p-value = 0.09059 то есть больше 0.05, значит автокорреляция отсутствует

#тест на гетероскедантичность
Arch2 <- arch.test(ModelDf2, lags.multi = 12, multivariate.only = TRUE)
Arch2
#p-value = 1 то есть больше 0.05, значит гетероскедантичность отсутствует

#тест на нормальное распределение остатков 
Norm2 <- normality.test(ModelDf2, multivariate.only = TRUE)
Norm2
#p-value = 0.9985, p-value = 0.9636, p-value = 0.9818

#тест на структурные разрывы в остатках
Stability2 <- stability(ModelDf2, type = "OLS-CUSUM")
plot(Stability2)
#графики не пересекают доверительные интервалы значит все окей

#причинно-следственная часть по Грейнджеру
Granger_koef_act <- causality(ModelDf2, cause = "koef_act")
Granger_koef_act
#p-value = 0.04388 что меньше 0,05 и следовательно гипотеза об отсутствии влиянии коеф изоб акт на Венчурные ивестиции отвергается (изоб акт влияет на венч инвест)  

Granger_VC <- causality(ModelDf2, cause = "VC")
Granger_VC
#p-value = 0.7435 что больше 0,05, следовательно гипотеза об отсутствии влияния Венчурных инвестиций на коеф изоб акт не отвергается (венч инвест не влияют на коеф изоб акт)

ccf(koef_act, VC, lag.max = 12, type = c("correlation"), plot = TRUE) 
# 2 значимых лага 
grangertest(VC, koef_act, order = 2) 
# Вероятность что VC влияет на koef_act 33.9% 

ccf(VC, koef_act, lag.max = 12, type = c("correlation"), plot = TRUE) 
# 2 значимых лага 
grangertest(koef_act, VC, order = 2) 
# Вероятность что koef_act влияет на VC 0.0087% 

#функция импульсного отклика (как Венчурные инвестиции среагируют на изменения koef_act на 20 единиц)
VCirf <- irf(ModelDf2, impulse = "koef_act", response = "VC", n.ahead = 20, boot = TRUE)
plot(VCirf, ylab = "VC", main = "Shock from koef_act")
#увеличение коэф акт влечет позитивное изменение венчурных инвест

koef_actirf <- irf(ModelDf2, impulse = "VC", response = "koef_act", n.ahead = 20, boot = TRUE)
plot(koef_actirf, ylab = "koef_act", main = "Shock from VC")
#изменения венчурных инвестиций влияет на коэф акт несильно, но скорее отрицательно чем положительно

#декомпозиция дисперсии (уровень влияния из предыдущих графиков)
FEVD2 <- fevd(ModelDf2, n.ahead = 10)
plot(FEVD2)
#непонятная блин интерпретация

#VAR прогноз
forecast1 <- predict(ModelDf2, n.ahead = 7, ci = 0.95)
plot(forecast1, names = "VC")
fanchart(forecast1, names = "VC")
plot(forecast1, name = "koef_act")
fanchart(forecast1, names = "koef_act")


##############################ВЛИЯНИЕ ВЕНЧУРНЫХ ИНВЕСТИЦИЙ И ЧЕЛОВЕЧЕСКОГО КАПИТАЛА##############################################

ggplot(data = df) + geom_point(mapping = aes(x = VC, y = hum_cap))

VC <- ts(df$VC, start = c(1997), frequency = 1) 
hum_cap <- ts(df$hum_cap, start = c(1997), frequency = 1) 

plot(cbind(VC, hum_cap))

#OLS
OLS4 <- lm(hum_cap ~ VC)
summary(OLS4) 
#взаимосвязь статистически значима
#VC           0.20069    0.07659    2.62   0.0153 *  (0.0153 близко к 0)

#определение постоянства модели
acf(hum_cap, main = "ACF for hum_cap") #5 лагов
pacf(hum_cap, main = "PACF for hum_cap") #незначительная автокорр (1 лаг)

acf(VC, main = "ACF for VC") #два лага
pacf(VC, main = "PACF for VC") #незначительная автокорр (1 лаг)

#нахождение оптимальных лагов
df.bv2 <- cbind(hum_cap, VC)
colnames(df.bv2) <- cbind("hum_cap", "VC")

lagselect2 <- VARselect(df.bv2, lag.max = 10, type = "const")
lagselect2$selection

#построение VAR 
ModelDf3 <- VAR(df.bv2, p = 7, type = "const", season = NULL, exogen = NULL)
summary(ModelDf3)

#диагностика VAR

#тест на автокорреляцию
Serial3 <- serial.test(ModelDf3, lags.pt = 16, type = "PT.asymptotic")
Serial3
# p-value = 0.06097 то есть больше 0.05, значит автокорреляция отсутствует

#тест на гетероскедантичность
Arch3 <- arch.test(ModelDf3, lags.multi = 12, multivariate.only = TRUE)
Arch3
#p-value = 1 то есть больше 0.05, значит гетероскедантичность отсутствует

#тест на нормальное распределение остатков 
Norm3 <- normality.test(ModelDf3, multivariate.only = TRUE)
Norm3
#p-value = p-value = 0.97, p-value = 0.9892, p-value = 0.7735

#тест на структурные разрывы в остатках
Stability3 <- stability(ModelDf3, type = "OLS-CUSUM")
plot(Stability3)
#графики не пересекают доверительные интервалы значит все окей

#причинно-следственная часть по Грейнджеру
Granger_hum_cap <- causality(ModelDf3, cause = "hum_cap")
Granger_hum_cap
#p-value = 0.4926 что больше 0,05 и следовательно гипотеза об отсутствии влиянии человеч капит на Венчурные ивестиции не отвергается (чел кап влияет на венч инвест)  

Granger_VC <- causality(ModelDf3, cause = "VC")
Granger_VC
#p-value = 0.1805 что больше 0,05, следовательно гипотеза об отсутствии влияния Венчурных инвестиций на человеч капит не отвергается (венч инвест влияют на чел кап)

ccf(hum_cap, VC, lag.max = 12, type = c("correlation"), plot = TRUE) 
# 1 значимый лаг 
grangertest(VC, hum_cap, order = 1) 
# Вероятность что VC влияет на hum_cap 50.17% 

ccf(VC, hum_cap, lag.max = 12, type = c("correlation"), plot = TRUE) 
# 1 значимых лага 
grangertest(hum_cap, VC, order = 1) 
# Вероятность что hum_cap влияет на VC 86.95% 

#функция импульсного отклика (как Венчурные инвестиции среагируют на изменения hum_cap на 20 единиц)
VCirf <- irf(ModelDf3, impulse = "hum_cap", response = "VC", n.ahead = 20, boot = TRUE)
plot(VCirf, ylab = "VC", main = "Shock from hum_cap")
#увеличение человеч кап влечет позитивное изменение венчурных инвест

hum_capirf <- irf(ModelDf3, impulse = "VC", response = "hum_cap", n.ahead = 20, boot = TRUE)
plot(hum_capirf, ylab = "hum_cap", main = "Shock from VC")
#увеличение венчурных инвестиций  влечет позитивное изменение человеч кап

#декомпозиция дисперсии (уровень влияния из предыдущих графиков)
FEVD3 <- fevd(ModelDf3, n.ahead = 10)
plot(FEVD3)
#непонятная блин интерпретация

#VAR прогноз
forecast2 <- predict(ModelDf3, n.ahead = 7, ci = 0.95)
plot(forecast2, names = "VC")
fanchart(forecast2, names = "VC")
plot(forecast2, name = "hum_cap")
fanchart(forecast2, names = "hum_cap")


