library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)
library(forecast)
library(tseries)
nnetar()

myts = read.csv(file.choose(), sep = ',', header = T)
myts = ts(myts, start = c(1,1), frequency = 24)


mytsTratada = function(x){
  
  s = 120
  f = 48
  lim=1
  #cont=1 #se a serie começar pelo inicio de semana
  cont= 121 #se a serie começar pelo final de semana
  
  nv = c()
  newSerie = c()
  newSerieClean = c()
  nv1 = c()
  nv2 = c()
  while (lim <= length(x)) {
    #nv = append(nv,x[cont])
    
    if(cont <= s){
      #print("add uma semana")  
      nv = append(nv,x[lim])
    }else{
      
      lim = lim+(f-1)
      cont = 0
    }
    cont = cont+1
    lim = lim+1
  }          
  newSerie = ts(nv, start = c(1,1), frequency = 24)
  newSerieClean = tsclean(newSerie)
  nv1 = newSerie
  nv2 = newSerieClean
  plot(newSerie)
  plot(newSerieClean)
  return(nv2)
}



#mytsClean = tsclean(myts)
#plot(mytsClean)



x = ur.kpss(mytsArima18$residuals)
plot(x)
ggAcf(mytsArima3$residuals)
checkresiduals(mytsArima3$residuals, main="Residuals PRA+Arima")
checkresiduals(mytsNeural3$residuals, main="Residuals PRA+NNAR")
shapiro.test(mytsArima3$residuals)
shapiro.test(mytsNeural12$residuals)

shapiro.test(mytsArima3$residuals)
shapiro.test(mytsNeural3$residuals)

autoplot(prevArima)
autoplot(prevNeural3)





########### MÉDIA MÓVEL  ################

mediaMovel = ma(mytsOriginal[,('value_avg')], order = 24)
plot(mediaMovel)
split.screen(figs = c(2,1))
screen(1)
plot(mediaMovel)
screen(2)
plot(mytsOriginal[,('value_avg')])
close.screen(all.screens = T)
print(mediaMovel)

accuracy(mediaMovel)
print(mytsNeural24)
teste = auto.arima(mytsTrain6, order = c(0,0,24))
print(teste)
print(mytsArima6)
############### TESTES ESTATÍSTICOS ###############
hist(myts)
autoplot(myts)
boxplot(myts)
dec = decompose(mytsClean)
dec1 = decompose(mytsOriginal[,('value_avg')])
autoplot(dec1)
autoplot(dec$trend, main = 'Teste', col= 'blue')
ggseasonplot(myts[,('value_avg')])
mytsEst = ur.kpss(mytsTrain15)
plot(mytsEst)
print(mytsEst)
ndiffs(mytsTrain15)
###TESTES ESTACIONARIEDADE #DICKEY-FULLER
mytsEst = adf.test(mytsTrain3)
print(mytsEst)
plot(mytsEst)
um =(acf(mytsTrain3, main='1'))
dois =(acf(mytsTrain6, main='2'))
tres=(acf(mytsTrain9, main='3'))
quatro = acf(mytsTrain12, main='4')
cinco = acf(mytsTrain15, main='5')
seis=acf(mytsTrain18, main='6')
sete= acf(mytsTrain21, main='7')
oito= acf(mytsTrain24, main='8')
nove= acf(mytsTrain27, main='9')
dez=acf(mytsTrain30, main='10')
onze=acf(mytsTrain33, main='11')
doze=acf(mytsTrain36, main='12')
treze=acf(mytsTrain39, main='13')
quatorze=acf(mytsTrain42, main='14')
quinze=acf(mytsTrain45, main='15')
dezesseis=acf(mytsTrain48, main='16')
dezessete=acf(mytsTrain51, main='17')
dezoito=acf(mytsTrain54, main='18')
dezenove=acf(mytsTrain57, main='19')
vinte=acf(mytsTrain60, main='20')
vinteUm=acf(mytsTrain63, main='21')

#######################
mytsClean = tsclean(myts)
autoplot(mytsClean)
autoplot(myts)
mytsEst1 = ur.kpss(mytsClean)
dec1 = decompose(mytsClean)
autoplot(dec1)

print(mytsClean)
mytsDif = diff(mytsClean)
plot(mytsDif)
ndiffs(mytsDif)
autoplot(mytsDif)


#######################
x = ur.kpss(mytsTrain3)
print(x)
z = diff(mytsTrain3)
x = ur.kpss(z)
print(x)
ndiffs(mytsTrain3)

split.screen(figs = c(2,1))
screen(1)
plot(mytsClean)
screen(2)
plot(z)
##########


mytsOriginal = read.csv(file.choose(), sep = ',', header = T)
mytsOriginal = ts(mytsOriginal, start = c(1,1), frequency = 24)

autoplot(mytsOriginal[,('value_avg')])

plot(mytsOriginal)

mytsOriginalClean = tsclean(mytsOriginal[,('value_avg')])
autoplot(mytsOriginalClean)

#####################
mytsArima3 = auto.arima(mytsTrain3, stepwise = F, approximation = F)
mytsNeural3 = nnetar(mytsTrain3)
mytsTest = window(mytsClean, start = c(4,1), end = c(4,24))
#######################

####DIVISÃO CONJUNTOS TREINAMENTO com PRA e Sem PRA#####
#ex com PRA mytsTrain3 = window(mytsClean, start = c(1,1), end = c(3,24))
#ex sem PRA mytsTrain3 = window(mytsOriginal[,('value_avg')], start = c(1,1), end = c(3,24))

mytsTrain3 = window(mytsClean, start = c(1,1), end = c(3,24))
mytsTrain6 = window(mytsClean, start = c(1,1), end = c(6,24))
mytsTrain9 = window(mytsClean, start = c(1,1), end = c(9,24))
mytsTrain12 = window(mytsClean, start = c(1,1), end = c(12,24))
mytsTrain15 = window(mytsClean, start = c(1,1), end = c(15,24))
mytsTrain18 = window(mytsClean, start = c(1,1), end = c(18,24))
mytsTrain21 = window(mytsClean, start = c(1,1), end = c(21,24))
mytsTrain24 = window(mytsClean, start = c(1,1), end = c(24,24))
mytsTrain27 = window(mytsClean, start = c(1,1), end = c(27,24))
mytsTrain30 = window(mytsClean, start = c(1,1), end = c(30,24))
mytsTrain33 = window(mytsClean, start = c(1,1), end = c(33,24))
mytsTrain36 = window(mytsClean, start = c(1,1), end = c(36,24))
mytsTrain39 = window(mytsClean, start = c(1,1), end = c(39,24))
mytsTrain42 = window(mytsClean, start = c(1,1), end = c(42,24))
mytsTrain45 = window(mytsClean, start = c(1,1), end = c(45,24))
mytsTrain48 = window(mytsClean, start = c(1,1), end = c(48,24))
mytsTrain51 = window(mytsClean, start = c(1,1), end = c(51,24))
mytsTrain54 = window(mytsClean, start = c(1,1), end = c(54,24))
mytsTrain57 = window(mytsClean, start = c(1,1), end = c(57,24))
mytsTrain60 = window(mytsClean, start = c(1,1), end = c(60,24))
mytsTrain63 = window(mytsClean, start = c(1,1), end = c(63,24))
plot(mytsClean)
#####TREINAMENTO ARIMA############
Sys.time()
mytsArima3 = auto.arima(mytsTrain3, stepwise = F, approximation = F)
mytsArima6 = auto.arima(mytsTrain6, stepwise = F, approximation = F)
mytsArima9 = auto.arima(mytsTrain9, stepwise = F, approximation = F)
mytsArima12 = auto.arima(mytsTrain12, stepwise = F, approximation = F)
mytsArima15 = auto.arima(mytsTrain15, stepwise = F, approximation = F)
mytsArima18 = auto.arima(mytsTrain18, stepwise = F, approximation = F)
mytsArima21 = auto.arima(mytsTrain21, stepwise = F, approximation = F)
mytsArima24 = auto.arima(mytsTrain24, stepwise = F, approximation = F)
mytsArima27 = auto.arima(mytsTrain27, stepwise = F, approximation = F)
mytsArima30 = auto.arima(mytsTrain30, stepwise = F, approximation = F)
mytsArima33 = auto.arima(mytsTrain33, stepwise = F, approximation = F)
mytsArima36 = auto.arima(mytsTrain36, stepwise = F, approximation = F)
mytsArima39 = auto.arima(mytsTrain39, stepwise = F, approximation = F)
mytsArima42 = auto.arima(mytsTrain42, stepwise = F, approximation = F)
mytsArima45 = auto.arima(mytsTrain45, stepwise = F, approximation = F)
mytsArima48 = auto.arima(mytsTrain48, stepwise = F, approximation = F)
mytsArima51 = auto.arima(mytsTrain51, stepwise = F, approximation = F)
mytsArima54 = auto.arima(mytsTrain54, stepwise = F, approximation = F)
mytsArima57 = auto.arima(mytsTrain57, stepwise = F, approximation = F)
mytsArima60 = auto.arima(mytsTrain60, stepwise = F, approximation = F)
mytsArima63 = auto.arima(mytsTrain63, stepwise = F, approximation = F)
Sys.time()


########TREINAMENTO ARIMA SEM STEP AND APPROX...#############

Sys.time()
mytsArima3 = auto.arima(mytsTrain3)
mytsArima6 = auto.arima(mytsTrain6)
mytsArima9 = auto.arima(mytsTrain9)
mytsArima12 = auto.arima(mytsTrain12)
mytsArima15 = auto.arima(mytsTrain15)
mytsArima18 = auto.arima(mytsTrain18)
mytsArima21 = auto.arima(mytsTrain21)
mytsArima24 = auto.arima(mytsTrain24)
mytsArima27 = auto.arima(mytsTrain27)
mytsArima30 = auto.arima(mytsTrain30)
mytsArima33 = auto.arima(mytsTrain33)
mytsArima36 = auto.arima(mytsTrain36)
mytsArima39 = auto.arima(mytsTrain39)
mytsArima42 = auto.arima(mytsTrain42)
mytsArima45 = auto.arima(mytsTrain45)
mytsArima48 = auto.arima(mytsTrain48)
mytsArima51 = auto.arima(mytsTrain51)
mytsArima54 = auto.arima(mytsTrain54)
mytsArima57 = auto.arima(mytsTrain57)
mytsArima60 = auto.arima(mytsTrain60)
mytsArima63 = auto.arima(mytsTrain63)
Sys.time()

#TREINAMENTO NEURAL###########
Sys.time()

mytsNeural3 = nnetar(mytsTrain3)
mytsNeural6 = nnetar(mytsTrain6)
mytsNeural9 = nnetar(mytsTrain9)
mytsNeural12 = nnetar(mytsTrain12)
mytsNeural15 = nnetar(mytsTrain15)
mytsNeural18 = nnetar(mytsTrain18)
mytsNeural21 = nnetar(mytsTrain21)
mytsNeural24 = nnetar(mytsTrain24)
mytsNeural27 = nnetar(mytsTrain27)
mytsNeural30 = nnetar(mytsTrain30)
mytsNeural33 = nnetar(mytsTrain33)
mytsNeural36 = nnetar(mytsTrain36)
mytsNeural39 = nnetar(mytsTrain39)
mytsNeural42 = nnetar(mytsTrain42)
mytsNeural45 = nnetar(mytsTrain45)
mytsNeural48 = nnetar(mytsTrain48)
mytsNeural51 = nnetar(mytsTrain51)
mytsNeural54 = nnetar(mytsTrain54)
mytsNeural57 = nnetar(mytsTrain57)
mytsNeural60 = nnetar(mytsTrain60)
mytsNeural63 = nnetar(mytsTrain63)
Sys.time()

########## TREINAMENTO HOLT WINTERS ###############
Sys.time()
mytsHoltETS3 = ets(mytsTrain3, model = 'ZAA', damped = T)
mytsHoltETS6 = ets(mytsTrain6, model = 'ZAA', damped = T)
mytsHoltETS9 = ets(mytsTrain9, model = 'ZAA', damped = T)
mytsHoltETS12 = ets(mytsTrain12, model = 'ZAA', damped = T)
mytsHoltETS15 = ets(mytsTrain15, model = 'ZAA', damped = T)
mytsHoltETS18 = ets(mytsTrain18, model = 'ZAA', damped = T)
mytsHoltETS21 = ets(mytsTrain21, model = 'ZAA', damped = T)
mytsHoltETS24 = ets(mytsTrain24, model = 'ZAA', damped = T)
mytsHoltETS27 = ets(mytsTrain27, model = 'ZAA', damped = T)
mytsHoltETS30 = ets(mytsTrain30, model = 'ZAA', damped = T)
mytsHoltETS33 = ets(mytsTrain33, model = 'ZAA', damped = T)
mytsHoltETS36 = ets(mytsTrain36, model = 'ZAA', damped = T)
mytsHoltETS39 = ets(mytsTrain39, model = 'ZAA', damped = T)
mytsHoltETS42 = ets(mytsTrain42, model = 'ZAA', damped = T)
mytsHoltETS45 = ets(mytsTrain45, model = 'ZAA', damped = T)
mytsHoltETS48 = ets(mytsTrain48, model = 'ZAA', damped = T)
mytsHoltETS51 = ets(mytsTrain51, model = 'ZAA', damped = T)
mytsHoltETS54 = ets(mytsTrain54, model = 'ZAA', damped = T)
mytsHoltETS57 = ets(mytsTrain57, model = 'ZAA', damped = T)
mytsHoltETS60 = ets(mytsTrain60, model = 'ZAA', damped = T)
mytsHoltETS63 = ets(mytsTrain63, model = 'ZAA', damped = T)
Sys.time()

###### TREINAMENTO HOLT WINTERS AUTOMÁTICO ########
Sys.time()
mytsHolt3 = ets(mytsTrain3)
mytsHolt6 = ets(mytsTrain6)
mytsHolt9 = ets(mytsTrain9)
mytsHolt12 = ets(mytsTrain12)
mytsHolt15 = ets(mytsTrain15)
mytsHolt18 = ets(mytsTrain18)
mytsHolt21 = ets(mytsTrain21)
mytsHolt24 = ets(mytsTrain24)
mytsHolt27 = ets(mytsTrain27)
mytsHolt30 = ets(mytsTrain30)
mytsHolt33 = ets(mytsTrain33)
mytsHolt36 = ets(mytsTrain36)
mytsHolt39 = ets(mytsTrain39)
mytsHolt42 = ets(mytsTrain42)
mytsHolt45 = ets(mytsTrain45)
mytsHolt48 = ets(mytsTrain48)
mytsHolt51 = ets(mytsTrain51)
mytsHolt54 = ets(mytsTrain54)
mytsHolt57 = ets(mytsTrain57)
mytsHolt60 = ets(mytsTrain60)
mytsHolt63 = ets(mytsTrain63)
Sys.time()
######################

###### TREINAMENTO HOLT WINTERS AUTOMÁTICO série original ########
Sys.time()
mytsHoltOrig3 = ets(mytsTrainHoltOrig3)
mytsHoltOrig6 = ets(mytsTrainHoltOrig6)
mytsHoltOrig9 = ets(mytsTrainHoltOrig9)
mytsHoltOrig12 = ets(mytsTrainHoltOrig12)
mytsHoltOrig15 = ets(mytsTrainHoltOrig15)
mytsHoltOrig18 = ets(mytsTrainHoltOrig18)
mytsHoltOrig21 = ets(mytsTrainHoltOrig21)
mytsHoltOrig24 = ets(mytsTrainHoltOrig24)
mytsHoltOrig27 = ets(mytsTrainHoltOrig27)
mytsHoltOrig30 = ets(mytsTrainHoltOrig30)
mytsHoltOrig33 = ets(mytsTrainHoltOrig33)
mytsHoltOrig36 = ets(mytsTrainHoltOrig36)
mytsHoltOrig39 = ets(mytsTrainHoltOrig39)
mytsHoltOrig42 = ets(mytsTrainHoltOrig42)
mytsHoltOrig45 = ets(mytsTrainHoltOrig45)
mytsHoltOrig48 = ets(mytsTrainHoltOrig48)
mytsHoltOrig51 = ets(mytsTrainHoltOrig51)
mytsHoltOrig54 = ets(mytsTrainHoltOrig54)
mytsHoltOrig57 = ets(mytsTrainHoltOrig57)
mytsHoltOrig60 = ets(mytsTrainHoltOrig60)
mytsHoltOrig63 = ets(mytsTrainHoltOrig63)
Sys.time()
######################

######Conjunto Teste#######
mytsTest1 = window(mytsClean, start = c(4,1), end = c(4,24))
mytsTest2 = window(mytsClean, start = c(7,1), end = c(7,24)) 
mytsTest3 = window(mytsClean, start = c(10,1), end = c(10,24)) 
mytsTest4 = window(mytsClean, start = c(13,1), end = c(13,24)) 
mytsTest5 = window(mytsClean, start = c(16,1), end = c(16,24)) 
mytsTest6 = window(mytsClean, start = c(19,1), end = c(19,24)) 
mytsTest7 = window(mytsClean, start = c(22,1), end = c(22,24)) 
mytsTest8 = window(mytsClean, start = c(25,1), end = c(25,24))
mytsTest9 = window(mytsClean, start = c(28,1), end = c(28,24)) 
mytsTest10 = window(mytsClean, start = c(31,1), end = c(31,24)) 
mytsTest11 = window(mytsClean, start = c(34,1), end = c(34,24)) 
mytsTest12 = window(mytsClean, start = c(37,1), end = c(37,24))
mytsTest13 = window(mytsClean, start = c(40,1), end = c(40,24)) 
mytsTest14 = window(mytsClean, start = c(43,1), end = c(43,24)) 
mytsTest15 = window(mytsClean, start = c(46,1), end = c(46,24)) 
mytsTest16 = window(mytsClean, start = c(49,1), end = c(49,24))
mytsTest17 = window(mytsClean, start = c(52,1), end = c(52,24)) 
mytsTest18 = window(mytsClean, start = c(55,1), end = c(55,24)) 
mytsTest19 = window(mytsClean, start = c(58,1), end = c(58,24)) 
mytsTest20 = window(mytsClean, start = c(61,1), end = c(61,24))
mytsTest21 = window(mytsClean, start = c(64,1), end = c(64,24))

###################################################

###TESTES ESTACIONARIEDADE #DICKEY-FULLER

while (TRUE) {
    
    mytsEst = adf.test(mytsTrain3)
    print(mytsEst)
    plot(mytsEst)
    um =(acf(mytsTrain3, main='1'))
    dois =(acf(mytsTrain6, main='2'))
    tres=(acf(mytsTrain9, main='3'))
    quatro = acf(mytsTrain12, main='4')
    cinco = acf(mytsTrain15, main='5')
    seis=acf(mytsTrain18, main='6')
    sete= acf(mytsTrain21, main='7')
    oito= acf(mytsTrain24, main='8')
    nove= acf(mytsTrain27, main='9')
    dez=acf(mytsTrain30, main='10')
    onze=acf(mytsTrain33, main='11')
    doze=acf(mytsTrain36, main='12')
    treze=acf(mytsTrain39, main='13')
    quatorze=acf(mytsTrain42, main='14')
    quinze=acf(mytsTrain45, main='15')
    dezesseis=acf(mytsTrain48, main='16')
    dezessete=acf(mytsTrain51, main='17')
    dezoito=acf(mytsTrain54, main='18')
    dezenove=acf(mytsTrain57, main='19')
    vinte=acf(mytsTrain60, main='20')
    vinteUm=acf(mytsTrain63, main='21')

}
#previsões########################
mytsTest4
prevArima12 = forecast(mytsArima12, h = 24)    
prevNeural12 = forecast(mytsNeural12, h = 24)
prevHoltParametros12 = forecast(mytsHoltETS12, h =24)

mytsTest9
prevArima27 = forecast(mytsArima27, h = 24)    
prevNeural27 = forecast(mytsNeural27, h = 24)
prevHoltParametros36 = forecast(mytsHoltETS36, h =24)


mytsTest15
prevArima45 = forecast(mytsArima45, h = 24)  
prevNeural45 = forecast(mytsNeural45, h = 24)
prevHoltParametros45 = forecast(mytsHoltETS45, h =24)

mytsTest19
prevArima57 = forecast(mytsArima57, h = 24)    
prevNeural57 = forecast(mytsNeural57, h = 24)
prevHoltParametros63 = forecast(mytsHoltETS63, h =24)
###########

while (TRUE) {
  
    print("---Iniciando previsão Arima---")
  
    prevArima3 = forecast(mytsArima3, h = 24)
    prevArima6 = forecast(mytsArima6, h = 24)
    prevArima9 = forecast(mytsArima9, h = 24)
    prevArima12 = forecast(mytsArima12, h = 24)
    prevArima15 = forecast(mytsArima15, h = 24)
    prevArima18 = forecast(mytsArima18, h = 24)
    prevArima21 = forecast(mytsArima21, h = 24)
    prevArima24 = forecast(mytsArima24, h = 24)
    prevArima27 = forecast(mytsArima27, h = 24)
    prevArima30 = forecast(mytsArima30, h = 24)
    prevArima33 = forecast(mytsArima33, h = 24)
    prevArima36 = forecast(mytsArima36, h = 24)
    prevArima39 = forecast(mytsArima39, h = 24)
    prevArima42 = forecast(mytsArima42, h = 24)
    prevArima45 = forecast(mytsArima45, h = 24)
    prevArima48 = forecast(mytsArima48, h = 24)
    prevArima51 = forecast(mytsArima51, h = 24)
    prevArima54 = forecast(mytsArima54, h = 24)
    prevArima57 = forecast(mytsArima57, h = 24)
    prevArima60 = forecast(mytsArima60, h = 24)
    prevArima63 = forecast(mytsArima63, h = 24)

}
###NNAR#################

while (TRUE) {
  
  print("---Iniciando previsão NEURAL---")
  
      prevNeural3 = forecast(mytsNeural3, h = 24)
      prevNeural6 = forecast(mytsNeural6, h = 24)
      prevNeural9 = forecast(mytsNeural9, h = 24)
      prevNeural12 = forecast(mytsNeural12, h = 24)
      prevNeural15 = forecast(mytsNeural15, h = 24)
      prevNeural18 = forecast(mytsNeural18, h = 24)
      prevNeural21 = forecast(mytsNeural21, h = 24)
      prevNeural24 = forecast(mytsNeural24, h = 24)
      prevNeural27 = forecast(mytsNeural27, h = 24)
      prevNeural30 = forecast(mytsNeural30, h = 24)
      prevNeural33 = forecast(mytsNeural33, h = 24)
      prevNeural36 = forecast(mytsNeural36, h = 24)
      prevNeural39 = forecast(mytsNeural39, h = 24)
      prevNeural42 = forecast(mytsNeural42, h = 24)
      prevNeural45 = forecast(mytsNeural45, h = 24)
      prevNeural48 = forecast(mytsNeural48, h = 24)
      prevNeural51 = forecast(mytsNeural51, h = 24)
      prevNeural54 = forecast(mytsNeural54, h = 24)
      prevNeural57 = forecast(mytsNeural57, h = 24)
      prevNeural60 = forecast(mytsNeural60, h = 24)
      prevNeural63 = forecast(mytsNeural63, h = 24)

}
########### TESTES ADICIONAIS ###############
plot(mytsTest1,ylab = 'bps', xlab='Ciclo')
lines(prevArima3$mean, col='red')
lines(prevNeural3$mean, col='blue')
lines(prevHoltParametros12$mean, col='green')

plot(prevArima3)
plot(prevNeural3)

write.csv(prevArima, file = '21dataArima.csv')
write.csv(prevNeural, file = '20dataNeural.csv')
write.csv(prevHoltParametros, file = '21dataHolt.csv')

########### Conjuntos de Treinamento dados originais ###########
write.csv(mytsTest1, file = '4Dia.csv')
write.csv(mytsTest2, file = '7Dia.csv')
write.csv(mytsTest3, file = '10Dia.csv')
write.csv(mytsTest4, file = '13Dia.csv')
write.csv(mytsTest5, file = '16Dia.csv')
write.csv(mytsTest6, file = '19Dia.csv')
write.csv(mytsTest7, file = '22Dia.csv')
write.csv(mytsTest8, file = '25Dia.csv')
write.csv(mytsTest9, file = '28Dia.csv')
write.csv(mytsTest10, file = '31Dia.csv')
write.csv(mytsTest11, file = '34Dia.csv')
write.csv(mytsTest12, file = '37Dia.csv')
write.csv(mytsTest13, file = '40Dia.csv')
write.csv(mytsTest14, file = '43Dia.csv')
write.csv(mytsTest15, file = '46Dia.csv')
write.csv(mytsTest16, file = '49Dia.csv')
write.csv(mytsTest17, file = '52Dia.csv')
write.csv(mytsTest18, file = '55Dia.csv')
write.csv(mytsTest19, file = '58Dia.csv')
write.csv(mytsTest20, file = '61Dia.csv')
write.csv(mytsTest21, file = '64Dia.csv')
#########################################

lines(prevHoltSerieOriginal$mean, col = 'blue')
lines(prevHoltSemParam$mean, col= 'red')
legend("topright",legend=c("Teste","Arima","NNAR"), col = c("black","blue","red"), lty=1:2, cex=0.8)


### TESTES DE MÉTRICAS DESEMPENHO E ERRO #######

#accuracy(prevHoltParametros, mytsTest12)
listaTreinamentos = list()
a=1
for (i in 1:length(listaPrevisoes)) {
  
  Ar = accuracy(ListPrevArima[i], ListMytsTest[i])
  NN = accuracy(ListPrevNeural[i], ListMytTest[i])
    
    cat("Erros RSME do conjunto", ListPrevArima[i])
    
    while (a <= length(i)) {
      
      write.csv(Ar, file = "error.csv")
      write.csv(NN, file = "error.csv")
      a = a+1
    }
    
  
}

accuracy(prevArima, mytsTest[12])
accuracy(prevNeural, mytsTest[12])

###
prevHoltSerieOriginal = forecast(mytsHoltOrig63, h=24)
prevHoltParametros = forecast(mytsHoltETS24, h =24)
print(prevHoltSerieOriginal)
###

###### Lista de FIGURAS #########

#####Figura 1, 2 e 3 seção 6#####

plot(mytsTest1,ylab = 'bps', xlab='Ciclo')
lines(prevArima3$mean, col='blue')
lines(prevNeural3$mean, col='red')
legend("topright", legend = c("Teste", "PRA+ARIMA","PRA+Neural"), col = c("black","blue","red"), lty=1:2, cex=1)

autoplot(prevArima3, main="Forecasts from ARIMA", ylab="bps", xlab="Ciclos")
autoplot(prevNeural3, main="Forecasts from NNAR", ylab="bps", xlab="Ciclos")

split.screen(figs = c(2,1))
screen(1)
plot(prevArima3, main="Forecasts from ARIMA", ylab="bps", xlab="Ciclos")
screen(2)
plot(prevNeural3, main="Forecasts from NNAR", ylab="bps", xlab="Ciclos")
close.screen(all.screens = T)
######Figura 2########################
mytsValueAvg = (myts1[,-c(1)])
mytsValueAvg = (myts1[,('value_avg')])
plot(mytsValueAvg, xlab='Ciclos', main='', cex.lab = 1.2 )
#####################################
######Figura 3#######################
dec = (decompose(mytsClean))
split.screen(figs = c(2,1))
par(mfrow=c(2,1), cex.lab=1.3)
screen(1)
plot(dec$seasonal, xlab='Tempo', ylab='sazonalidade regular')
screen(2)
plot(dec$trend, xlab='Tempo', ylab='tendência estacionária')
close.screen(all.screens = T)
###########figure 3 iscc ###########
par(mfrow=c(4,1), cex.lab=1.5)
screen(1)
plot(mytsOriginal[,3],xlab= '(a) Cycles', ylab='bandwidth (bit/s)')
screen(2)
plot(myts1, xlab= '(b) Cycles', ylab='bandwidth (bit/s)')
screen(3)
plot(mytsClean, xlab='(c) Cycles', ylab='bandwidth (bit/s)')
screen(4)
plot(mytsClean, xlab='(d) Cycles', ylab='bandwidth (bit/s)')
lines(myts1, col='red')
close.screen(all.screens = T)
##########Figure 2 iscc######
plot(mytsOriginal)
dec1 = (decompose(mytsOriginal[,3]))
plot(dec1)
split.screen(figs = c(2,1))
par(mfrow=c(2,1), cex.lab=1.3)
screen(1)
plot(dec1$seasonal, xlab='Tempo', ylab='sazonalidade')
screen(2)
plot(dec1$trend, xlab='Tempo', ylab='tendência')
close.screen(all.screens = T)


#####FIGURA DISS ACF CONJUNTOS TREINAMENTO########
par(mfrow=c(4,6), cex.lab=1.3)
split.screen(figs = c(2,2))
screen(1)
plot(um, main='1')
screen(2)
plot(dois, main='2')
screen(3)
plot(tres, main='3')
screen(4)
plot(quatro, main='4')
screen(5)
plot(cinco, main='5')
screen(6)
plot(seis, main='6')
screen(7)
plot(sete, main='7')
screen(8)
plot(oito, main='8')
screen(9)
plot(nove, main='9')
screen(10)
plot(dez, main='10')
screen(11)
plot(onze, main='11')
screen(12)
plot(doze, main='12')
screen(13)
plot(treze, main='13')
screen(14)
plot(quatorze, main='14')
screen(15)
plot(quinze, main='15')
screen(16)
plot(dezesseis, main='16')
screen(17)
plot(dezessete, main='17')
screen(18)
plot(dezoito, main='18')
screen(19)
plot(dezenove, main='19')
screen(20)
plot(vinte, main='20')
screen(21)
plot(vinteUm, main='21')


close.screen(all.screens =T)
#######Figura 5 journal########################
split.screen(figs = c(1,4))
par(mfrow=c(4,1), cex.lab=1.7)
split.screen(figs = c(1,4))
screen(1)
plot(mytsTest4,ylab = 'bps', xlab='Cycle')
lines(prevArima12$mean, col = 'blue')
lines(prevNeural12$mean, col= 'red')
lines(prevHoltParametros12$mean, col='green')
legend("topright",legend=c("Test","ADF+Arima","ADF+NNAR","ADF+Holt"), col = c("black","blue","red","green"), lty=1.2, cex=1)
screen(2)
plot(mytsTest12,ylab = 'bps', xlab='Cycle')
lines(prevArima36$mean, col = 'blue')
lines(prevNeural36$mean, col= 'red')
lines(prevHoltParametros36$mean, col='green')
legend("bottomright",legend=c("Test","ADF+Arima","ADF+NNAR","ADF+Holt"), col = c("black","blue","red","green"), lty=1.2, cex=1)
screen(3)
plot(mytsTest15,ylab = 'bps', xlab='Cycle')
lines(prevArima45$mean, col = 'blue')
lines(prevNeural45$mean, col= 'red')
lines(prevHoltParametros45$mean, col='green')
legend("bottomright",legend=c("Test","ADF+Arima","ADF+NNAR","ADF+Holt"), col = c("black","blue","red","green"), lty=1.2, cex=1)
screen(4)
plot(mytsTest21,ylab = 'bps', xlab='Cycle')
lines(prevArima63$mean, col = 'blue')
lines(prevNeural63$mean, col= 'red')
lines(prevHoltParametros63$mean, col='green')
legend("bottomright",legend=c("Test","ADF+Arima","ADF+NNAR", "ADF+Holt"), col = c("black","blue","red", "green"), lty=1.2, cex=1)
close.screen(all.screens = T)

#######Figura 5.1 ISCC e Diss########################
#par(mfrow=c(4,1), cex.lab=1.4)
par(mfrow=c(2,2), cex.lab=1.3)

screen(1)
plot(mytsTest4,ylab = 'bps', xlab='Ciclo')
lines(prevArima12$mean, col = 'blue')
lines(prevNeural12$mean, col= 'red')
legend("topright",inset = .02,legend=c("Test","PRA+Arima","PRA+NNAR"), col = c("black","blue","red"), lty=1.2, cex=1)
screen(2)
plot(mytsTest9,ylab = 'bps', xlab='Ciclo')
lines(prevArima27$mean, col = 'blue')
lines(prevNeural27$mean, col= 'red')
legend("top",inset = .02,legend=c("Test","PRA+Arima","PRA+NNAR"), col = c("black","blue","red"), lty=1.2, cex=1)
screen(3)
plot(mytsTest15,ylab = 'bps', xlab='Ciclo')
lines(prevArima45$mean, col = 'blue')
lines(prevNeural45$mean, col= 'red')
legend("top",inset=.02,legend=c("Test","PRA+Arima","PRA+NNAR"), col = c("black","blue","red"), lty=1.2, cex=1)
screen(4)
plot(mytsTest19,ylab = 'bps', xlab='Ciclo')
lines(prevArima57$mean, col = 'blue')
lines(prevNeural57$mean, col= 'red')
legend("top",inset=.02,legend=c("Test","PRA+Arima","PRA+NNAR"), col = c("black","blue","red"), lty=1.2, cex=1)
close.screen(all.screens = T)
#######Figura 5.2 ######################
split.screen(figs = c(4,1))
par(mfrow=c(2,2), cex.lab=1.3)
screen(1)
plot(mytsTest,ylab = 'bps', xlab='Ciclo')
lines(prevArima$mean, col = 'blue')
lines(prevNeural$mean, col= 'red')
legend("topright",legend=c("Teste","PRA+Arima","PRA+NNAR"), col = c("black","blue","red"), lty=1.2, cex=0.8)
screen(2)
plot(mytsTest,ylab = 'bps', xlab='Ciclo')
lines(prevArima$mean, col = 'blue')
lines(prevNeural$mean, col= 'red')
legend("bottomright",legend=c("Teste","PRA+Arima","PRA+NNAR"), col = c("black","blue","red"), lty=1.2, cex=0.85)
screen(3)
plot(mytsTest,ylab = 'bps', xlab='Ciclo')
lines(prevArima$mean, col = 'blue')
lines(prevNeural$mean, col= 'red')
legend("bottomright",legend=c("Teste","PRA+Arima","PRA+NNAR"), col = c("black","blue","red"), lty=1.2, cex=0.77)
screen(4)
plot(mytsTest,ylab = 'bps', xlab='Ciclo')
lines(prevArima$mean, col = 'blue')
lines(prevNeural$mean, col= 'red')
legend("bottomright",legend=c("Teste","PRA+Arima","PRA+NNAR"), col = c("black","blue","red"), lty=1.2, cex=0.77)
close.screen(all.screens = T)
##########################Uma abaixo da outr################
#################################

mytsOriginal = read.csv(file.choose(), sep = ',',header = T)
mytsOriginal = ts(mytsOriginal, start = c(1,1), frequency = 24)
plot(mytsClean)

mytsOriginalTrain = window(mytsOriginal[,('value_avg')], start = c(1,1), end = c(63,24))
mytsOriginalTest = window(mytsOriginal[,('value_avg')], start = c(64,1), end = c(64,24))


plot(mytsOriginalTrain)
lines(mytsOriginalTest, col='red')

mytsArima1512 = auto.arima(mytsOriginalTrain, stepwise = F, approximation = F)
mytsNNAR1512 = nnetar(mytsOriginalTrain)

prevArima = forecast(mytsArima1512, h = 24)
prevNNAR = forecast(mytsNNAR1512, h = 24)

plot(mytsOriginalTest)
lines(prevArima$mean, col='red')
lines(prevNNAR$mean, col='blue')

accuracy(prevArima, mytsOriginalTest)
accuracy(prevNNAR, mytsOriginalTest)


#######FIGURA TODAS OS VALORES PREVISTOS######


par(mfrow=c(5,5), cex.lab=1)

screen(1)
plot(mytsTest1,ylab = 'bps', xlab='Ciclos')
lines(prevArima3$mean, col = 'blue')
lines(prevNeural3$mean, col= 'red')
screen(2)
plot(mytsTest2,ylab = 'bps', xlab='Ciclos')
lines(prevArima6$mean, col = 'blue')
lines(prevNeural6$mean, col= 'red')
screen(3)
plot(mytsTest3,ylab = 'bps', xlab='Ciclos')
lines(prevArima9$mean, col = 'blue')
lines(prevNeural9$mean, col= 'red')
screen(4)
plot(mytsTest4,ylab = 'bps', xlab='Ciclos')
lines(prevArima12$mean, col = 'blue')
lines(prevNeural12$mean, col= 'red')
screen(5)
plot(mytsTest5,ylab = 'bps', xlab='Ciclos')
lines(prevArima15$mean, col = 'blue')
lines(prevNeural15$mean, col= 'red')
screen(6)
plot(mytsTest6,ylab = 'bps', xlab='Ciclos')
lines(prevArima18$mean, col = 'blue')
lines(prevNeural18$mean, col= 'red')
screen(7)
plot(mytsTest7,ylab = 'bps', xlab='Ciclos')
lines(prevArima21$mean, col = 'blue')
lines(prevNeural21$mean, col= 'red')
screen(8)
plot(mytsTest8,ylab = 'bps', xlab='Ciclos')
lines(prevArima24$mean, col = 'blue')
lines(prevNeural24$mean, col= 'red')
screen(9)
plot(mytsTest9, ylab='bps', xlab='Ciclos')
lines(prevArima27$mean, col='blue')
lines(prevNeural27$mean, col='red')


close.screen(all.screens = T)

mytsFig = read.csv(file.choose(), header = T, sep = ',')
mytsFig = ts(mytsFig, start = c(1,1), frequency = 24)
plot(mytsFig, main='', xlab='Cycles')


###### TESTES ADICIONAIS #############

checkresiduals(mytsArima3)
checkresiduals(mytsNeural3)
checkresiduals(mytsNeural6)
checkresiduals(mytsArima6)
checkresiduals(mytsNeural9)
checkresiduals(mytsArima9)
checkresiduals(mytsNeural12)
checkresiduals(mytsArima12)
checkresiduals(mytsArima15)
checkresiduals(mytsArima42)
hist(mytsArima42$residuals)
