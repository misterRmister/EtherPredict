##################################################################
################## Loading packages ##############################
##################################################################

require(jsonlite)
library(RJSONIO)
require(quantmod)
library(xts)
library(forecast)
library(ggplot2)
library("TTR")
library(stats)
require(Quandl)


##################################################################
################## Feature creation ##############################
##################################################################

################# loading data into R ############################

jsonfile <- "https://etherchain.org/api/statistics/price"

globaldata <- fromJSON(jsonfile)

ether <- globaldata[['data']]

#ether[1]

ether_mat <- do.call("rbind",ether)

#ether_df <- as.data.frame(ether_mat)
ether_df <- as.data.frame(cbind(ether_mat[,1],ether_mat[,2]))
head(ether_df)

colnames(ether_df) <- c("time", "usd")

names(ether_df)
ether_df$usd <- as.numeric(ether_df$usd)
class(ether_df$usd)

ether_df$time <- gsub("T"," ",ether_df$time)
ether_df$time <- gsub(".000Z"," ",ether_df$time)
#as.Date(ether_df$time, format = c('Y-m-d h:m:s'))
ether_df$time  <- as.POSIXlt(ether_df$time)

#lags:
ETHdiff1 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=1) /lag(xts(ether_df$usd, order.by = ether_df$time),1) 
ETHdiff2 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=2) /lag(xts(ether_df$usd, order.by = ether_df$time),2)
ETHdiff3 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=3) /lag(xts(ether_df$usd, order.by = ether_df$time),3)
ETHdiff4 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=4) /lag(xts(ether_df$usd, order.by = ether_df$time),4)
ETHdiff5 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=5) /lag(xts(ether_df$usd, order.by = ether_df$time),5) 
ETHdiff10 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=10) /lag(xts(ether_df$usd, order.by = ether_df$time),10)
ETHdiff20 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=20) /lag(xts(ether_df$usd, order.by = ether_df$time),20)
ETHdiff24 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=24) /lag(xts(ether_df$usd, order.by = ether_df$time),24)
ETHdiff40 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=40) /lag(xts(ether_df$usd, order.by = ether_df$time),40)
ETHdiff60 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=60) /lag(xts(ether_df$usd, order.by = ether_df$time),60)
ETHdiff80 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=80) /lag(xts(ether_df$usd, order.by = ether_df$time),80)
ETHdiff120 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=120) /lag(xts(ether_df$usd, order.by = ether_df$time),120)

ETHdiff600 <- diff(xts(ether_df$usd, order.by = ether_df$time), lag=600) /lag(xts(ether_df$usd, order.by = ether_df$time),600)
plot(ETHdiff600)
plot(ETHdiff1)






#Bollinger bands
BTCbb <- as.data.frame(BBands(ether_df[,2], n=480, SMA, sd = 2))
tail(BTCbb)

BTCbb40 <- as.data.frame(BBands(ether_df[,2], n=960, SMA, sd = 2))
BTCbb60 <- as.data.frame(BBands(ether_df[,2], n=1440, SMA, sd = 2))
BTCbb100 <- as.data.frame(BBands(ether_df[,2], n=2400, SMA, sd = 2))

ether_df$lowerBB20 <- BTCbb$dn
ether_df$upperBB20 <- BTCbb$up
ether_df$sma20 <- BTCbb$mavg

ether_df$lowerBB40 <- BTCbb40$dn
ether_df$upperBB40 <- BTCbb40$up
ether_df$sma40 <- BTCbb40$mavg


ether_df$lowerBB60 <- BTCbb60$dn
ether_df$upperBB60 <- BTCbb60$up
ether_df$sma60 <- BTCbb60$mavg


ether_df$lowerBB100 <- BTCbb100$dn
ether_df$upperBB100 <- BTCbb100$up
ether_df$sma100 <- BTCbb100$mavg

ether_df$BTCvsBB20up <- ether_df$usd/BTCbb$up
ether_df$BTCvsBB20low <- ether_df$usd/BTCbb$dn
ether_df$BTCvsBB40up <- ether_df$usd/BTCbb40$up
ether_df$BTCvsBB40low <- ether_df$usd/BTCbb40$dn

ether_df$BTCvsBB60up <- ether_df$usd/BTCbb60$up
ether_df$BTCvsBB60low <- ether_df$usd/BTCbb60$dn

ether_df$BTCvsBB100up <- ether_df$usd/BTCbb100$up
ether_df$BTCvsBB100low <- ether_df$usd/BTCbb100$dn



ether_df$diff1 <- ETHdiff1
ether_df$diff1 <- ETHdiff2
ether_df$diff3 <- ETHdiff3
ether_df$diff4 <- ETHdiff4
ether_df$diff5 <- ETHdiff5
ether_df$diff10 <- ETHdiff10
ether_df$diff20 <- ETHdiff20
ether_df$diff24 <- ETHdiff24
ether_df$diff40 <- ETHdiff40
ether_df$diff60 <- ETHdiff60
ether_df$diff80 <- ETHdiff80
ether_df$diff120 <- ETHdiff120
ether_df$diff600 <- ETHdiff600


dim(ether_df)

extreme <- ifelse(ETHdiff24 > abs(0.02), 1, 0)

more_extreme <- ifelse(ETHdiff24> abs(0.05), 1, 0)

colnames(extreme) <- "extreme"  

colnames(more_extreme) <- "more_extreme"

ether_df$extreme <- extreme
ether_df$more_extreme <- more_extreme



#RSI:
BTCrsi <- as.data.frame(RSI(ether_df$usd))

ether_df<- as.data.frame(cbind(ether_df,BTCrsi))



#MACD
macd <- as.data.frame(MACD(ether_df$usd, 12, 26, 9, maType="EMA"))
head(macd)

ether_df <- as.data.frame(cbind(ether_df,macd))

plot(ether_df$macd,ether_df$diff24)

#Vertical Horizontal Filter


vhf10 <- as.data.frame(VHF(ether_df[,2],10))
vhf20 <- as.data.frame(VHF(ether_df[,2],20))
vhf40 <- as.data.frame(VHF(ether_df[,2],40))
vhf60 <- as.data.frame(VHF(ether_df[,2],60))
vhf100 <- as.data.frame(VHF(ether_df[,2],100))
vhf200 <- as.data.frame(VHF(ether_df[,2],200))
vhf400 <- as.data.frame(VHF(ether_df[,2],400))
vhf600 <- as.data.frame(VHF(ether_df[,2],600))
vhf800 <- as.data.frame(VHF(ether_df[,2],800))


colnames(vhf10)[1] <- "vhf10"
colnames(vhf20)[1] <- "vhf20"
colnames(vhf40)[1] <- "vhf40"
colnames(vhf60)[1] <- "vhf60"
colnames(vhf100)[1] <- "vhf100"
colnames(vhf200)[1] <- "vhf200"
colnames(vhf400)[1] <- "vhf400"
colnames(vhf600)[1] <- "vhf600"
colnames(vhf800)[1] <- "vhf800"

ether_df <- as.data.frame(cbind(ether_df, vhf10,vhf20,vhf40,vhf60,vhf100,vhf200,vhf400,vhf600,vhf800))




#Commodity Channel Index

cci10 <- as.data.frame(CCI(ether_df[,2], n=10))
cci20 <- as.data.frame(CCI(ether_df[,2], n=20))
cci40 <- as.data.frame(CCI(ether_df[,2], n=40))
cci60 <- as.data.frame(CCI(ether_df[,2], n=60))
cci100 <- as.data.frame(CCI(ether_df[,2], n=100))
cci200 <- as.data.frame(CCI(ether_df[,2], n=200))
cci400 <- as.data.frame(CCI(ether_df[,2], n=400))
cci600 <- as.data.frame(CCI(ether_df[,2], n=600))
cci800 <- as.data.frame(CCI(ether_df[,2], n=800))

names(cci10)

colnames(cci10) <- "cci10"
colnames(cci20) <- "cci20"
colnames(cci40) <- "cci40"
colnames(cci60) <- "cci60"
colnames(cci100) <- "cci100"
colnames(cci200) <- "cci200"
colnames(cci400) <- "cci400"
colnames(cci600) <- "cci600"
colnames(cci800) <- "cci800"


ether_df <- as.data.frame(cbind(ether_df,cci10,cci20,cci40,cci60,cci100,cci200,cci400,cci600,cci800))




#Aroon

aroon10 <- as.data.frame(aroon(ether_df[,2],10))
aroon20 <- as.data.frame(aroon(ether_df[,2],20))
aroon40 <- as.data.frame(aroon(ether_df[,2],40))
aroon60 <- as.data.frame(aroon(ether_df[,2],60))
aroon100 <- as.data.frame(aroon(ether_df[,2],100))
aroon200 <- as.data.frame(aroon(ether_df[,2],200))
aroon400 <- as.data.frame(aroon(ether_df[,2],400))
aroon600 <- as.data.frame(aroon(ether_df[,2],600))
aroon800 <- as.data.frame(aroon(ether_df[,2],800))

names(aroon100)

colnames(aroon10) <- c("aroonup10", "arroondn10", "aroonoscillator10")
colnames(aroon20) <- c("aroonup20", "arroondn20", "aroonoscillator20")
colnames(aroon40) <- c("aroonup40", "arroondn40", "aroonoscillator40")
colnames(aroon60) <- c("aroonup60", "arroondn60", "aroonoscillator60")
colnames(aroon100) <- c("aroonup100", "arroondn100", "aroonoscillator100")
colnames(aroon200) <- c("aroonup200", "arroondn200", "aroonoscillator200")
colnames(aroon400) <- c("aroonup400", "arroondn400", "aroonoscillator400")
colnames(aroon600) <- c("aroonup600", "arroondn600", "aroonoscillator600")
colnames(aroon800) <- c("aroonup800", "arroondn800", "aroonoscillator800")

ether_df <- as.data.frame(cbind(ether_df, aroon10,aroon20,aroon40,aroon60,aroon100,aroon200,aroon400,aroon600,aroon800))
dim(ether_df)




#Chande Momentum Oscillator

cmo10 <- as.data.frame(CMO(ether_df[,2],n=10))
cmo20 <- as.data.frame(CMO(ether_df[,2],n=20))
cmo40 <- as.data.frame(CMO(ether_df[,2],n=40))
cmo60 <- as.data.frame(CMO(ether_df[,2],n=60))
cmo100 <- as.data.frame(CMO(ether_df[,2],n=100))
cmo200 <- as.data.frame(CMO(ether_df[,2],n=200))
cmo400 <- as.data.frame(CMO(ether_df[,2],n=400))
cmo600 <- as.data.frame(CMO(ether_df[,2],n=600))
cmo800 <- as.data.frame(CMO(ether_df[,2],n=800))


names(cmo20)

colnames(cmo10) <- "cm10"
colnames(cmo20) <- "cm20"
colnames(cmo40) <- "cm40"
colnames(cmo60) <- "cm60"
colnames(cmo100) <- "cm100"
colnames(cmo200) <- "cm200"
colnames(cmo400) <- "cm400"
colnames(cmo600) <- "cm600"
colnames(cmo800) <- "cm800"


ether_df <- as.data.frame(cbind(ether_df,cmo10,cmo20,cmo40,cmo60,cmo100,cmo200,cmo400,cmo600,cmo800))
dim(ether_df)




#Know Sure Thing
kst <- as.data.frame(KST(ether_df[,2]))
names(kst)

colnames(kst) <- c("kst", "kst_signal")
ether_df <- as.data.frame(cbind(ether_df,kst))


dim(ether_df)



# Running Percent Rank
PR5 <- as.data.frame(runPercentRank(ether_df[,2], n = 5))
PR10 <- as.data.frame(runPercentRank(ether_df[,2], n = 10))
PR20 <- as.data.frame(runPercentRank(ether_df[,2], n = 20))
PR40 <- as.data.frame(runPercentRank(ether_df[,2], n = 40))
PR60 <- as.data.frame(runPercentRank(ether_df[,2], n = 60))
PR100 <- as.data.frame(runPercentRank(ether_df[,2], n = 100))
PR200 <- as.data.frame(runPercentRank(ether_df[,2], n = 200))
PR400 <- as.data.frame(runPercentRank(ether_df[,2], n = 400))
PR600 <- as.data.frame(runPercentRank(ether_df[,2], n = 600))
PR800 <- as.data.frame(runPercentRank(ether_df[,2], n = 800))
PR1000 <- as.data.frame(runPercentRank(ether_df[,2], n = 1000))




colnames(PR5) <- "PR5"
colnames(PR10) <- "PR10"
colnames(PR20) <- "PR20"
colnames(PR40) <- "PR40"
colnames(PR60) <- "PR60"
colnames(PR100) <- "PR100"
colnames(PR100) <- "PR100"
colnames(PR200) <- "PR200"
colnames(PR400) <- "PR400"
colnames(PR600) <- "PR600"
colnames(PR800) <- "PR800"
colnames(PR1000) <- "PR1000"

ether_df <- as.data.frame(cbind(ether_df,PR5,PR10,PR20,PR40,PR60,PR100,PR200,PR400,PR600,PR800,PR1000))
dim(ether_df)






# Running Max
RM5 <- as.data.frame(runMax(ether_df[,2],5))
RM10 <- as.data.frame(runMax(ether_df[,2],10))
RM20 <- as.data.frame(runMax(ether_df[,2],20))
RM40 <- as.data.frame(runMax(ether_df[,2],40))
RM60 <- as.data.frame(runMax(ether_df[,2],60))
RM100 <- as.data.frame(runMax(ether_df[,2],100))
RM200 <- as.data.frame(runMax(ether_df[,2],200))
RM400 <- as.data.frame(runMax(ether_df[,2],400))
RM600 <- as.data.frame(runMax(ether_df[,2],600))
RM800 <- as.data.frame(runMax(ether_df[,2],800))
RM1000 <- as.data.frame(runMax(ether_df[,2],1000))



colnames(RM5) <- "RM5"
colnames(RM10) <- "RM10"
colnames(RM20) <- "RM20"
colnames(RM40) <- "RM40"
colnames(RM60) <- "RM60"
colnames(RM100) <- "RM100"
colnames(RM200) <- "RM200"
colnames(RM400) <- "RM400"
colnames(RM600) <- "RM600"
colnames(RM800) <- "RM800"
colnames(RM1000) <- "RM1000"


ether_df <- as.data.frame(cbind(ether_df,RM5, RM10,RM20, RM40, RM60, RM100,RM200,RM400,RM600,RM800,RM1000))


dim(ether_df)



# Running Min
RMin5 <- as.data.frame(runMin(ether_df[,2],5))
RMin10 <- as.data.frame(runMin(ether_df[,2],10))
RMin20 <- as.data.frame(runMin(ether_df[,2],20))
RMin40 <- as.data.frame(runMin(ether_df[,2],40))
RMin60 <- as.data.frame(runMin(ether_df[,2],60))
RMin100 <- as.data.frame(runMin(ether_df[,2],100))
RMin200 <- as.data.frame(runMin(ether_df[,2],200))
RMin400 <- as.data.frame(runMin(ether_df[,2],400))
RMin600 <- as.data.frame(runMin(ether_df[,2],600))
RMin800 <- as.data.frame(runMin(ether_df[,2],800))
RMin1000 <- as.data.frame(runMin(ether_df[,2],1000))



colnames(RMin5) <- "RMin5"
colnames(RMin10) <- "RMin10"
colnames(RMin20) <- "RMin20"
colnames(RMin40) <- "RMin40"
colnames(RMin60) <- "RMin60"
colnames(RMin100) <- "RMin100"
colnames(RMin200) <- "RMin200"
colnames(RMin400) <- "RMin400"
colnames(RMin600) <- "RMin600"
colnames(RMin800) <- "RMin800"
colnames(RMin1000) <- "RMin1000"


ether_df <- as.data.frame(cbind(ether_df, RMin5, RMin10, RMin20, RMin40, RMin60, RMin100,RMin200,RMin400,RMin600,RMin800,RMin1000))

dim(ether_df)






# Running SD of lags

SDlag15 <- as.data.frame(runSD(as.data.frame(ETHdiff1)[,1], 5))  
SDlag110 <- as.data.frame(runSD(as.data.frame(ETHdiff1)[,1], 10))  
SDlag120 <- as.data.frame(runSD(as.data.frame(ETHdiff1)[,1], 20))  
SDlag140 <- as.data.frame(runSD(as.data.frame(ETHdiff1)[,1], 40))  
SDlag160 <- as.data.frame(runSD(as.data.frame(ETHdiff1)[,1], 60))  
SDlag1100 <- as.data.frame(runSD(as.data.frame(ETHdiff1)[,1], 100))  
SDlag1200 <- as.data.frame(runSD(as.data.frame(ETHdiff1)[,1], 200))  
SDlag1400 <- as.data.frame(runSD(as.data.frame(ETHdiff1)[,1], 400))  
SDlag1600 <- as.data.frame(runSD(as.data.frame(ETHdiff1)[,1], 600))  
SDlag1800 <- as.data.frame(runSD(as.data.frame(ETHdiff1)[,1], 800))  


colnames(SDlag15) <- "sdlag15"
colnames(SDlag110) <- "sdlag110"
colnames(SDlag120) <- "sdlag120"
colnames(SDlag140) <- "sdlag140"
colnames(SDlag160) <- "sdlag160"
colnames(SDlag1100) <- "sdlag1100"
colnames(SDlag1200) <- "sdlag1200"
colnames(SDlag1400) <- "sdlag1400"
colnames(SDlag1600) <- "sdlag1600"
colnames(SDlag1800) <- "sdlag1800"


ether_df <- as.data.frame(cbind(ether_df, SDlag15, SDlag110, SDlag120, SDlag140, SDlag160, SDlag1100,SDlag1200,SDlag1400,SDlag1600,SDlag1800))

dim(ether_df)








SDlag245 <- as.data.frame(runSD(as.data.frame(ETHdiff24)[,1], 5))  
SDlag2410 <- as.data.frame(runSD(as.data.frame(ETHdiff24)[,1], 10))  
SDlag2420 <- as.data.frame(runSD(as.data.frame(ETHdiff24)[,1], 20))  
SDlag2440 <- as.data.frame(runSD(as.data.frame(ETHdiff24)[,1], 40))  
SDlag2460 <- as.data.frame(runSD(as.data.frame(ETHdiff24)[,1], 60))  
SDlag24100 <- as.data.frame(runSD(as.data.frame(ETHdiff24)[,1], 100))  
SDlag24200 <- as.data.frame(runSD(as.data.frame(ETHdiff24)[,1], 200))  
SDlag24400 <- as.data.frame(runSD(as.data.frame(ETHdiff24)[,1], 400))  
SDlag24600 <- as.data.frame(runSD(as.data.frame(ETHdiff24)[,1], 600))  
SDlag24800 <- as.data.frame(runSD(as.data.frame(ETHdiff24)[,1], 800))  


colnames(SDlag245) <- "sdlag245"
colnames(SDlag2410) <- "sdlag2410"
colnames(SDlag2420) <- "sdlag2420"
colnames(SDlag2440) <- "sdlag2440"
colnames(SDlag2460) <- "sdlag2460"
colnames(SDlag24100) <- "sdlag24100"
colnames(SDlag24200) <- "sdlag24200"
colnames(SDlag24400) <- "sdlag24400"
colnames(SDlag24600) <- "sdlag24600"
colnames(SDlag24800) <- "sdlag24800"


ether_df <- as.data.frame(cbind(ether_df, SDlag245, SDlag2410, SDlag2420, SDlag2440, SDlag2460, SDlag24100,SDlag24200,SDlag24400,SDlag24600,SDlag24800))

dim(ether_df)






SDlag605 <- as.data.frame(runSD(as.data.frame(ETHdiff60)[,1], 5))  
SDlag6010 <- as.data.frame(runSD(as.data.frame(ETHdiff60)[,1], 10))  
SDlag6020 <- as.data.frame(runSD(as.data.frame(ETHdiff60)[,1], 20))  
SDlag6040 <- as.data.frame(runSD(as.data.frame(ETHdiff60)[,1], 40))  
SDlag6060 <- as.data.frame(runSD(as.data.frame(ETHdiff60)[,1], 60))  
SDlag60100 <- as.data.frame(runSD(as.data.frame(ETHdiff60)[,1], 100))  
SDlag60200 <- as.data.frame(runSD(as.data.frame(ETHdiff60)[,1], 200))  
SDlag60400 <- as.data.frame(runSD(as.data.frame(ETHdiff60)[,1], 400))  
SDlag60600 <- as.data.frame(runSD(as.data.frame(ETHdiff60)[,1], 600))  
SDlag60800 <- as.data.frame(runSD(as.data.frame(ETHdiff60)[,1], 800))  


colnames(SDlag605) <- "sdlag605"
colnames(SDlag6010) <- "sdlag6010"
colnames(SDlag6020) <- "sdlag6020"
colnames(SDlag6040) <- "sdlag6040"
colnames(SDlag6060) <- "sdlag6060"
colnames(SDlag60100) <- "sdlag60100"
colnames(SDlag60200) <- "sdlag60200"
colnames(SDlag60400) <- "sdlag60400"
colnames(SDlag60600) <- "sdlag60600"
colnames(SDlag60800) <- "sdlag60800"


ether_df <- as.data.frame(cbind(ether_df, SDlag605, SDlag6010, SDlag6020, SDlag6040, SDlag6060, SDlag60100,SDlag60200,SDlag60400,SDlag60600,SDlag60800))

dim(ether_df)




SDlag6005 <- as.data.frame(runSD(as.data.frame(ETHdiff600)[,1], 5))  
SDlag60010 <- as.data.frame(runSD(as.data.frame(ETHdiff600)[,1], 10))  
SDlag60020 <- as.data.frame(runSD(as.data.frame(ETHdiff600)[,1], 20))  
SDlag60040 <- as.data.frame(runSD(as.data.frame(ETHdiff600)[,1], 40))  
SDlag60060 <- as.data.frame(runSD(as.data.frame(ETHdiff600)[,1], 60))  
SDlag600100 <- as.data.frame(runSD(as.data.frame(ETHdiff600)[,1], 100))  
SDlag600200 <- as.data.frame(runSD(as.data.frame(ETHdiff600)[,1], 200))  
SDlag600400 <- as.data.frame(runSD(as.data.frame(ETHdiff600)[,1], 400))  
SDlag600600 <- as.data.frame(runSD(as.data.frame(ETHdiff600)[,1], 600))  
SDlag600800 <- as.data.frame(runSD(as.data.frame(ETHdiff600)[,1], 800))  


colnames(SDlag6005) <- "sdlag6005"
colnames(SDlag60010) <- "sdlag60010"
colnames(SDlag60020) <- "sdlag60020"
colnames(SDlag60040) <- "sdlag60040"
colnames(SDlag60060) <- "sdlag60060"
colnames(SDlag600100) <- "sdlag600100"
colnames(SDlag600200) <- "sdlag600200"
colnames(SDlag600400) <- "sdlag600400"
colnames(SDlag600600) <- "sdlag600600"
colnames(SDlag600800) <- "sdlag600800"


ether_df <- as.data.frame(cbind(ether_df, SDlag6005, SDlag60010, SDlag60020, SDlag60040, SDlag60060, SDlag600100,SDlag600200,SDlag600400,SDlag600600,SDlag600800))

dim(ether_df)








# Running Median of lags (differences = returns)
Mdlag15 <- as.data.frame(runMedian(as.data.frame(ETHdiff24)[,1], 5))  
Mdlag110 <- as.data.frame(runMedian(as.data.frame(ETHdiff24)[,1], 10))  
Mdlag120 <- as.data.frame(runMedian(as.data.frame(ETHdiff24)[,1], 20))  
Mdlag140 <- as.data.frame(runMedian(as.data.frame(ETHdiff24)[,1], 40))  
Mdlag160 <- as.data.frame(runMedian(as.data.frame(ETHdiff24)[,1], 60))  
Mdlag1100 <- as.data.frame(runMedian(as.data.frame(ETHdiff24)[,1], 100))  




colnames(Mdlag15) <- "Mdlag15"
colnames(Mdlag110) <- "Mdlag110"
colnames(Mdlag120) <- "Mdlag120"
colnames(Mdlag140) <- "Mdlag140"
colnames(Mdlag160) <- "Mdlag160"
colnames(Mdlag1100) <- "Mdlag1100"


ether_df <- as.data.frame(cbind(ether_df, as.data.frame(cbind( Mdlag15, Mdlag110, Mdlag120, Mdlag140, Mdlag160, Mdlag1100))))

dim(ether_df)

hist(Mdlag140$Mdlag140,1000)







# Running Median of lags (differences = returns)
Mdlag25 <- as.data.frame(runMedian(as.data.frame(ETHdiff40)[,1], 5))  
Mdlag210 <- as.data.frame(runMedian(as.data.frame(ETHdiff40)[,1], 10))  
Mdlag220 <- as.data.frame(runMedian(as.data.frame(ETHdiff40)[,1], 20))  
Mdlag240 <- as.data.frame(runMedian(as.data.frame(ETHdiff40)[,1], 40))  
Mdlag260 <- as.data.frame(runMedian(as.data.frame(ETHdiff40)[,1], 60))  
Mdlag2100 <- as.data.frame(runMedian(as.data.frame(ETHdiff40)[,1], 100))  




colnames(Mdlag25) <- "Mdlag25"
colnames(Mdlag210) <- "Mdlag210"
colnames(Mdlag220) <- "Mdlag220"
colnames(Mdlag240) <- "Mdlag240"
colnames(Mdlag260) <- "Mdlag260"
colnames(Mdlag2100) <- "Mdlag2100"


ether_df <- as.data.frame(cbind(ether_df, as.data.frame(cbind( Mdlag25, Mdlag210, Mdlag220, Mdlag240, Mdlag260, Mdlag2100))))

dim(ether_df)

hist(Mdlag2100$Mdlag2100,1000)







# Running Median of lags (differences = returns)
Mdlag65 <- as.data.frame(runMedian(as.data.frame(ETHdiff600)[,1], 5))  
Mdlag610 <- as.data.frame(runMedian(as.data.frame(ETHdiff600)[,1], 10))  
Mdlag620 <- as.data.frame(runMedian(as.data.frame(ETHdiff600)[,1], 20))  
Mdlag640 <- as.data.frame(runMedian(as.data.frame(ETHdiff600)[,1], 40))  
Mdlag660 <- as.data.frame(runMedian(as.data.frame(ETHdiff600)[,1], 60))  
Mdlag6100 <- as.data.frame(runMedian(as.data.frame(ETHdiff600)[,1], 100))  




colnames(Mdlag65) <- "Mdlag65"
colnames(Mdlag610) <- "Mdlag610"
colnames(Mdlag620) <- "Mdlag620"
colnames(Mdlag640) <- "Mdlag640"
colnames(Mdlag660) <- "Mdlag660"
colnames(Mdlag6100) <- "Mdlag6100"


ether_df <- as.data.frame(cbind(ether_df, as.data.frame(cbind( Mdlag65, Mdlag610, Mdlag620, Mdlag640, Mdlag660, Mdlag6100))))

dim(ether_df)

hist(Mdlag6100$Mdlag6100,1000)







# Moving averages of differences

sma15 <- as.data.frame(SMA(as.data.frame(ETHdiff24)[,1], 5))  
sma110 <- as.data.frame(SMA(as.data.frame(ETHdiff24)[,1], 10))  
sma120 <- as.data.frame(SMA(as.data.frame(ETHdiff24)[,1], 20))  
sma140 <- as.data.frame(SMA(as.data.frame(ETHdiff24)[,1], 40))  
sma160 <- as.data.frame(SMA(as.data.frame(ETHdiff24)[,1], 60))  
sma1100 <- as.data.frame(SMA(as.data.frame(ETHdiff24)[,1], 100))  




colnames(sma15) <- "sma15"
colnames(sma110) <- "sma110"
colnames(sma120) <- "sma120"
colnames(sma140) <- "sma140"
colnames(sma160) <- "sma160"
colnames(sma1100) <- "sma1100"


str(ether_df)

ether_df <- as.data.frame(cbind(ether_df, as.data.frame(cbind( sma15, sma110, sma120, sma140, sma160, sma1100))))

dim(ether_df)


hist(sma160$sma160,1000)


require(stats)
shapiro.test(sma1100$sma1100)
qqnorm(sma15$sma15)
qqline(sma15$sma15)








# Moving averages of differences

sma25 <- as.data.frame(SMA(as.data.frame(ETHdiff40)[,1], 5))  
sma210 <- as.data.frame(SMA(as.data.frame(ETHdiff40)[,1], 10))  
sma220 <- as.data.frame(SMA(as.data.frame(ETHdiff40)[,1], 20))  
sma240 <- as.data.frame(SMA(as.data.frame(ETHdiff40)[,1], 40))  
sma260 <- as.data.frame(SMA(as.data.frame(ETHdiff40)[,1], 60))  
sma2100 <- as.data.frame(SMA(as.data.frame(ETHdiff40)[,1], 100))  


colnames(sma25) <- "sma25"
colnames(sma210) <- "sma210"
colnames(sma220) <- "sma220"
colnames(sma240) <- "sma240"
colnames(sma260) <- "sma260"
colnames(sma2100) <- "sma2100"


str(ether_df)

ether_df <- as.data.frame(cbind(ether_df, as.data.frame(cbind( sma25, sma210, sma220, sma240, sma260, sma2100))))

dim(ether_df)


hist(sma260$sma260,1000)


require(stats)
qqnorm(sma15$sma15)
qqline(sma15$sma15)







# Moving averages of differences

sma6005 <- as.data.frame(SMA(as.data.frame(ETHdiff600)[,1], 5))  
sma60010 <- as.data.frame(SMA(as.data.frame(ETHdiff600)[,1], 10))  
sma60020 <- as.data.frame(SMA(as.data.frame(ETHdiff600)[,1], 20))  
sma60040 <- as.data.frame(SMA(as.data.frame(ETHdiff600)[,1], 40))  
sma60060 <- as.data.frame(SMA(as.data.frame(ETHdiff600)[,1], 60))  
sma600100 <- as.data.frame(SMA(as.data.frame(ETHdiff600)[,1], 100))  


colnames(sma6005) <- "sma6005"
colnames(sma60010) <- "sma60010"
colnames(sma60020) <- "sma60020"
colnames(sma60040) <- "sma60040"
colnames(sma60060) <- "sma60060"
colnames(sma600100) <- "sma600100"


str(ether_df)

ether_df <- as.data.frame(cbind(ether_df, as.data.frame(cbind( sma6005, sma60010, sma60020, sma60040, sma60060, sma600100))))

dim(ether_df)


hist(sma600100$sma600100,1000)


require(stats)
qqnorm(sma15$sma15)
qqline(sma15$sma15)




##################################################################
################## Model building # ##############################
##################################################################



### offsetting by 24 hours

predict_value <- as.data.frame(ether_df[25:nrow(ether_df),2])

colnames(predict_value) <- "ETH_next_day"

total_data <- cbind( ether_df[1:(nrow(ether_df)-24),], predict_value)





set.seed(3)

#drops <- c("BTC.USD", "aroonup60", "arroondn60", "aroonoscillator60")

drops <- c( "aroonup60", "arroondn60", "aroonoscillator60", "time")

load_data <- total_data[1500:nrow(total_data),]


matrix_data <- load_data[,!(names(load_data) %in% drops)]






#################### Random Forest #####################

require(quantregForest)





matrix_data [is.na(matrix_data )] <- 9999

qf1 <- quantregForest(y = matrix_data $ETH_next_day 
                      , x = matrix_data [,!(names(matrix_data) %in% c("ETH_next_day"))]
                      , ntree = 1000
                      #, mtry = 40
                      , corr.bias = T
                      , do.trace = T
                      , keep.inbag = TRUE
)





### offsetting 48 24 hours

predict_value <- as.data.frame(ether_df[49:nrow(ether_df),2])

colnames(predict_value) <- "ETH_next_day"

total_data <- cbind( ether_df[1:(nrow(ether_df)-48),], predict_value)





set.seed(3)

#drops <- c("BTC.USD", "aroonup60", "arroondn60", "aroonoscillator60")

drops <- c( "aroonup60", "arroondn60", "aroonoscillator60", "time")

load_data <- total_data[1500:nrow(total_data),]


matrix_data <- load_data[,!(names(load_data) %in% drops)]






#################### Random Forest #####################

require(quantregForest)





matrix_data [is.na(matrix_data )] <- 9999

qf2 <- quantregForest(y = matrix_data $ETH_next_day 
                      , x = matrix_data [,!(names(matrix_data) %in% c("ETH_next_day"))]
                      , ntree = 1000
                      #, mtry = 40
                      , corr.bias = T
                      , do.trace = T
                      , keep.inbag = TRUE
)








#prediction for tomorrow:
modelv1 <- lm( ETH_next_day ~.  , data = matrix_data )

summary(modelv1)

tomorrow_quant <- predict (qf1, newdata= ether_df[nrow(ether_df)-1,], what=c(0.01, 0.05,0.25, 0.5,0.75, 0.95,0.99))
tomorrow_mean <- predict(qf1, newdata = ether_df[nrow(ether_df)-1,], what = mean)

tomorrow_cdf <- predict(qf1, newdata = ether_df[nrow(ether_df)-1,], what = seq(0.01, 1, 0.001))


plot(density(tomorrow_cdf))
plot(ecdf(tomorrow_cdf ))
tomorrow_quant
tomorrow_mean
predict(modelv1, ether_df[nrow(ether_df)-1,], interval = "confidence")





