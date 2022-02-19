###################################################################################################################################################
# Tecnicas Estatisticas de Predição
# Professor: Otaviano Francisco Neves
###################################################################################################################################################

#####################        Aula Series Temporais    #############################################################################################

###################################################################################################################################################
#####################  Modelos Automáticos ########################################################################################################
###################################################################################################################################################

#Series temporais e analises preditivas

#########################################################################################

#pacotes essencias utilizados durante o curso
install.packages("forecast")
install.packages("fpp2")
install.packages("ggplot2")


library(forecast)
library(fpp2)
library(ggplot2)


#########################################################################################
#cria uma serie temporal normalmente distribuida

serie1 = rnorm(60)
serie1 = ts(serie1,start = c(2012,1), end=c(2016,12), frequency=12)
plot(serie1)

#########################################################################################
#Importa e cria  uma serie temporal

setwd("C:\\Users\\Elaine\\Documents\\Pedro\\Pós\\modelagem\\series temporais")# direciona o R para pasta
getwd() # visualiza a pasta de trabalho do R
dados1 <- read.csv2("Dados de Emprego_IA.csv") # Leitura do arquivo csv no R

serie2 = ts(dados1$Comercio,start = c(2003,1), end=c(2007,12), frequency=12)
class(serie2)
plot(serie2)

serie3 = ts(dados1$Alimento,start = c(2003,1), end=c(2007,12), frequency=12)
class(serie3)
plot(serie3)

serie4 = ts(dados1$Metalurgia,start = c(2003,1), end=c(2007,12), frequency=12)
class(serie4)
plot(serie4)

########################################################################################
#explorando parte da serie

serie2a = window(serie2,2003,c(2004,12))
plot(serie2a)
hist(serie2a)
boxplot(serie2)
sd(serie2)
min(serie2)
max(serie2)
summary(serie2)
length(serie2)
start(serie2)
head(serie2)
end(serie2)
tail(serie2)
sum(is.null(serie2))

#######################################################################################
#Modelo de média móvel

#calculando a media movel com ordem 3
seriemm1 = ma(serie2, order = 3 )
autoplot(seriemm1)

#calculando a media movel com ordem 5
seriemm2 = ma(serie2, order = 5 )
autoplot(seriemm2)

#novamente, ordem 12
seriemm3 = ma(serie2, order = 12 )
autoplot(seriemm3)


#comparando 
plot(serie2)
lines(seriemm1, col="red")
lines(seriemm2, col="blue")
lines(seriemm3, col="green")

#legenda
legend("topleft",legend=c("Orig.","MM3", "MM5","MM12"), 
       col = c("black","red","blue","green"), lty=1:2, cex=0.6,)



###########################Decomposição de uma série temporal#########################

autoplot(serie2)

dec1 <- decompose(serie2)
dec2 <- decompose(serie2, type = c("multiplicative"))

autoplot(dec1)
autoplot(dec2)


########################################################################################
#alisamento exponencial Simples - sem tendência e sem sazonalidade

autoplot(serie2)
modelo1 = ses(serie2, h = 16) # Previsao para os próximos 16 meses
summary(modelo1)

########################################################################################
#alisamento exponencial duplo - com tendência - holt

autoplot(serie2)
modelo2 = holt(serie2, h=16)
autoplot(modelo2)
modelo2$model
summary(modelo2)

########################################################################################
#alisamento exponencial holt Winter - com tendência e sazonalidade aditiva

autoplot(serie2)
modelo3 = hw(serie2, seasonal = "additive", h=16)
autoplot(modelo2)
modelo3$model
summary(modelo3)

########################################################################################
#alisamento exponencial holt Winter - com tendência e sazonalidade multiplicativa

autoplot(serie2)
modelo4 = hw(serie2, seasonal = "multiplicative", h=16)
plot(modelo4)
modelo4$model
summary(modelo4)

################################ utilizando modelos genéricos ##########################

ETS - ERRO , TENDÊNCIA E SAZONALIDADE

########################################################################################
#alisamento exponencial Simples

modelo1 <- ets(serie2, "ANN")
modelo1

autoplot(modelo1$residuals)
autoplot(modelo1$fitted)

prev = forecast(modelo1, h=16,levels=c(85,90))
print(prev$mean)
autoplot(prev)


#######################################################################################
#alisamento exponencial Duplo

modelo2 <- ets(serie2, "AAN")
modelo2

autoplot(modelo2$residuals)
autoplot(modelo2$fitted)

prev = forecast(modelo2, h=16,levels=c(85,90))
print(prev$mean)
autoplot(prev)


########################################################################################
#alisamento exponencial  - holt - Winters - Aditivo

modelo3 <- ets(serie2, "AAA")
modelo3

autoplot(modelo3$residuals)
autoplot(modelo3$fitted)

prev = forecast(modelo3, h=16,levels=c(85,90))
print(prev$mean)
autoplot(prev)


########################################################################################
#alisamento exponencial  - holt - Winters - Multiplicativo

modelo4 <- ets(serie2, "MAM")
modelo4

autoplot(modelo4$residuals)
autoplot(modelo4$fitted)

prev = forecast(modelo4, h=16,levels=c(85,90))
print(prev$mean)
autoplot(prev)


########################################################################################
#alisamento exponencial  - holt - Winters - Aditivo - Incompleto

modelo5 <- ets(serie2, "ANA")
modelo5

autoplot(modelo5$residuals)
autoplot(modelo5$fitted)

prev = forecast(modelo5, h=16,levels=c(85,90))
print(prev$mean)
autoplot(prev)


########################################################################################
#alisamento exponencial  - Automático

modelo6 <- ets(serie2 ,"ZZZ")
modelo6

autoplot(modelo6$residuals)
autoplot(modelo6$fitted)

prev = forecast(modelo6, h=16,levels=c(85,90))
print(prev$mean)
autoplot(prev)


########################################################################################
#comparando modelos


treino = window(serie2,2003,c(2005,12))
teste  = window(serie2,2006,c(2007,12))


#alisamento exponencial  - holt - Winters - Aditivo

modelo7 <- ets(treino, "AAA")
modelo7

autoplot(modelo7$residuals)
autoplot(modelo7$fitted)

prev1 = forecast(modelo7, h=24)
print(prev1$mean)
autoplot(prev1)

#alisamento exponencial  - holt - Winters - Multiplicativo

modelo8 <- ets(treino, "MAM")
modelo8

autoplot(modelo8$residuals)
autoplot(modelo8$fitted)

prev2 = forecast(modelo8, h=24)
print(prev2$mean)
autoplot(prev2)


#################################################################################

plot(serie2)
lines(prev1$mean, col="blue")
lines(prev2$mean, col="red")
lines(teste, col="green")
legend("topright",legend=c("AAA","MAM","Teste"), col = c("blue","red","green"),
       lty=1:2, cex=0.8)

accuracy(prev1,teste )
accuracy(prev2,teste )


########################################################################################

