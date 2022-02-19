##############################################################################
# Métodos Estatístico de Previsão
# Professor: Otaviano Francisco Neves
# Alunos: Pedro Henrique de Souza Martins e Clinton Julio
##############################################################################


#####################  Regressão Linear  Múltipla #####################
##############################################################################


getwd() # visualiza a pasta de trabalho do R

setwd("C:\\Users\\Elaine\\Documents\\Pedro\\Pós\\modelagem\\Exercicio airBNB") # direciona o R para pasta

getwd() # visualiza a pasta de trabalho do R

dados1<- read.csv2("Dados airbnb - IA.csv") # Leitura do arquivo csv no R


names(dados1)
head(dados1)
tail(dados1)
summary(dados1)




dados2 <- dados1[, c("Grupo.de.vizinhança","Tipo.de.hospedagem",
                     "Preço","locação.mínima","Número.de.avaliações",
                     "Taxa.mensal.de.ocupação","Número.máximo.de.hóspedes",
                     "Disponibilidade.anual")]

dados3 <- dados1[, c("Preço","locação.mínima","Número.de.avaliações",
                     "Taxa.mensal.de.ocupação","Número.máximo.de.hóspedes",
                     "Disponibilidade.anual")]


dados2$Grupo.de.vizinhança<- as.factor(dados2$Grupo.de.vizinhança)
dados2$Tipo.de.hospedagem<- as.factor(dados2$Tipo.de.hospedagem)


summary(dados2)

attach(dados2)


tapply(Taxa.mensal.de.ocupação, Grupo.de.vizinhança, summary)
tapply(Taxa.mensal.de.ocupação, Grupo.de.vizinhança, t.test)

tapply(Taxa.mensal.de.ocupação, Tipo.de.hospedagem, summary)
tapply(Taxa.mensal.de.ocupação, Tipo.de.hospedagem, t.test)

attach(dados3)


summary(dados3) # Estatistica descritiva no R

summary(Taxa.mensal.de.ocupação) # Estatistica descritiva R

hist(Taxa.mensal.de.ocupação) # histograma no R

boxplot(Taxa.mensal.de.ocupação)

plot(Taxa.mensal.de.ocupação) # gráfico de dispersão no R
boxplot(Preço) # gráfico de dispersão no R

cor(dados3) #correlação de Pearson no R

View(dados3)

############################# Modelo #######################################

attach(dados2)


modelo1 <- lm(Taxa.mensal.de.ocupação~. ,dados2)# ajuste do modelo de regressão no R

summary(modelo1)#Resultado do modelo de regressão no R


modelo2 <- lm(Taxa.mensal.de.ocupação~Preço + Número.de.avaliações ,dados2)

summary(modelo2)#Resultado do modelo de regressão no R

plot(modelo2)

modelo3 <- lm(Taxa.mensal.de.ocupação~Preço ,dados2)
summary(modelo3)

modelo4 <- lm(Taxa.mensal.de.ocupação~Número.de.avaliações ,dados2)
summary(modelo4)


predict(modelo2, data.frame(Preço=c(0) , Número.de.avaliações = c(200)), interval = "prediction")
predict(modelo2, data.frame(Preço=c(90) , Número.de.avaliações = c(35)), interval = "prediction")
predict(modelo2, data.frame(Preço=c(125) , Número.de.avaliações = c(20)), interval = "confidence")



##############################################################################
##############################################################################