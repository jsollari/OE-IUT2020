#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     23.01.2020
#modificado: 18.02.2020

setwd("2020/2020.01.23_escolas_OE-IUT2020/bin/")

#0. INDEX
{
#1. DATA WRANGLING
##1.1. READ RAW DATA
##1.2. FORMAT RAW DATA
##1.3. AUTOMATIC CHECKS
##1.4. MANUALL CHECKS
##1.5. WRITE DATA
#2. EXPLORE DATA
#2. EXPLORE DATA
## 2.1. VISUALIZATION
## 2.2. SUMMARY STATISTICS
#3. MODEL DATA
## 3.1. PRICE_NEW ~ PRICE
## 3.2. PRICE_NEW ~ PRICE + PLAY_PHONE
## 3.3. PLAY_PHONE ~ PRICE
## 3.4. PLAY_PHONE ~ PRICE + SOCIALNET

}
# 1. DATA WRANGLING
{
## 1.1. READ RAW DATA
f1 <- "../data/data_raw.csv"
d1 <- read.table(
  file=f1,
  header=FALSE,
  sep=",",
  dec=".",
  na.string=c(""),
  skip=1,
  stringsAsFactors=FALSE,
  fileEncoding="UTF-8")

colnames(d1) <- c(
  "TIME",          #Time stamp of interview
  "PLACE",         #Name of education establishment
  "GRADE",         #Grade level
  "SMARTPHONE_YN", #Having Smartphone
  "ACQUIRED",      #Acquisition of Smartphone
  "AGE_FIRST_YN",  #Remember age when first Smartphone acquired
  "AGE_FIRST",     #Age when first smartphone acquired
  "COLOUR",        #Smartphone colour
  "PRICE_YN",      #Remember price of Smartphone
  "PRICE",         #Price of Smartphone
  "PRICE_NEW",     #Price willing to pay for new Smartphone
  "SOCIALNET",     #Social network used more frequently
  "PLAY_PHONE",    #Play on Smartphone
  "PLAY_OTHER",    #Play on other devices
  "SATISFACTION"   #Degree of satisfaction
)

## 1.2. REFORMAT RAW DATA
d2 <- d1

### 1.2.1. Reformat field TIME
d2$TIME <- as.POSIXlt(d2$TIME)

### 1.2.2. Reformat field PLACE
a1 <- "(AL\\sBERTO|AL-BERTO)"
s1 <- grepl(a1,toupper(d2$PLACE))
d2$PLACE[s1] <- "Escola Secundária Poeta Al Berto"
a1 <- "QUINTA.*(MARQUÊS|MARQUES)"
s1 <- grepl(a1,toupper(d2$PLACE))
d2$PLACE[s1] <- "Escola Secundária Quinta do Marquês"
a1 <- "INSTITUTO.*DESENVOLVIMENTO.*SOCIAL"
a2 <- "(IDE|I\\.D\\.E\\.|I\\.\\sD\\.\\sE\\.)"
s1 <- grepl(a1,toupper(d2$PLACE)) | grepl(a2,toupper(d2$PLACE))
d2$PLACE[s1] <- "Instituto para o Desenvolvimento Social"
a1 <- "FREI.*(LUIS|LUÍS).*SOUSA"
s1 <- grepl(a1,toupper(d2$PLACE))
d2$PLACE[s1] <- "Externato Frei Luís de Sousa"
a1 <- "GUADALUPE"
s1 <- grepl(a1,toupper(d2$PLACE))
d2$PLACE[s1] <- "Colégio Guadalupe"
a1 <- "(FUNDAO|FUNDÃO)"
s1 <- grepl(a1,toupper(d2$PLACE))
d2$PLACE[s1] <- "Agrupamento de Escolas do Fundão"
a1 <- "(ANTÓNIO|ANTÔNIO|ANTONIO).*(SÉRGIO|SERGIO)"
s1 <- grepl(a1,toupper(d2$PLACE))
d2$PLACE[s1] <- "Escola Secundária António Sérgio"
a1 <- "PASSOS.*MANUEL"
s1 <- grepl(a1,toupper(d2$PLACE))
d2$PLACE[s1] <- "Agrupamento de Escolas Passos Manuel"
a1 <- "INSTITUTO.*(CIÊNCIAS|CIENCIAS).*EDUCATIVAS"
a2 <- "(ICE|I\\.C\\.E\\.|I\\.\\sC\\.\\sE\\.)"
s1 <- grepl(a1,toupper(d2$PLACE))
d2$PLACE[s1] <- "Instituto de Ciências Educativas"
a1 <- "ESCOLA.*SUPERIOR.*(EDUCAÇÃO|EDUCACÃO|EDUCAÇAO|EDUCACAO).*(SANTARÉM|SANTAREM)"
a2 <- "INSTITUTO.*(POLITÉCNICO|POLITECNICO).*(SANTARÉM|SANTAREM)"
s1 <- grepl(a1,toupper(d2$PLACE)) | grepl(a2,toupper(d2$PLACE))
d2$PLACE[s1] <- "Escola Superior de Educação de Santarém"
d2$PLACE <- factor(d2$PLACE)
a1 <- "SINES"
a2 <- "VASCO.*GAMA"
s1 <- grepl(a1,toupper(d2$PLACE)) | grepl(a2,toupper(d2$PLACE))
d2$PLACE[s1] <- "Escola Básica Vasco da Gama de Sines"
d2$PLACE <- factor(d2$PLACE)
a1 <- "FERREIRA.*DIAS"
s1 <- grepl(a1,toupper(d2$PLACE))
d2$PLACE[s1] <- "Escola Secundária Ferreira Dias"
d2$PLACE <- factor(d2$PLACE)

### 1.2.3. Reformat field GRADE
a1 <- "(12\\D|DOZE|DÉCIMO-SEGUNDO|DECIMO-SEGUNDO|DÉCIMO\\sSEGUNDO|DECIMO\\sSEGUNDO)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- 12
a1 <- "(11\\D|ONZE|DÉCIMO-PRIMEIRO|DECIMO-PRIMEIRO|DÉCIMO\\sPRIMEIRO|DECIMO\\sPRIMEIRO)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- 11
a1 <- "(10\\D|DEZ|DÉCIMO|DECIMO)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- 10
a1 <- "(9\\D|NOVE|NONO)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- 9
a1 <- "(8\\D|OITO|OITAVO)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- 8
a1 <- "(7\\D|SETE|SÉTIMO|SETIMO)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- 7
a1 <- "(6\\D|SEIS|SEXTO)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- 6
a1 <- "(5\\D|CINCO|QUINTO)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- 5
a1 <- "(4\\D|QUATRO|QUARTO)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- 4
a1 <- "(3\\D|TRÊS|TERCEIRO)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- 3
a1 <- "(2\\D|DOIS|SEGUNDO)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- 2
a1 <- "(1\\D|UM|PRIMEIRO)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- 1
a1 <- "(PROFESSOR|PROFESSORA|PROF)"
s1 <- grepl(a1,toupper(d2$GRADE))
d2$GRADE[s1] <- "Professor"
d2$GRADE <- ifelse(d2$GRADE %in% c(1:12,"Professor"),d2$GRADE,
            ifelse(!is.na(d2$GRADE),"Outro",NA))
d2$GRADE <- factor(d2$GRADE,levels=c(1:12,"Professor","Outro"))

### 1.2.4. Reformat field SMARTPHONE_YN
d2$SMARTPHONE_YN <- ifelse(d2$SMARTPHONE_YN=="Sim",TRUE,
                    ifelse(d2$SMARTPHONE_YN=="Não",FALSE,NA))

### 1.2.5. Reformat field ACQUIRED
d2$ACQUIRED <- ifelse(d2$ACQUIRED %in% c("Oferecido","Comprado"),d2$ACQUIRED,
               ifelse(!is.na(d2$ACQUIRED),"Outro",NA))
d2$ACQUIRED <- factor(d2$ACQUIRED,levels=c("Oferecido","Comprado","Outro"))

### 1.2.6. Reformat field AGE_FIRST_YN
d2$AGE_FIRST_YN <- ifelse(d2$AGE_FIRST_YN=="Sim",TRUE,
                   ifelse(d2$AGE_FIRST_YN=="Não",FALSE,NA))

### 1.2.7. Reformat field AGE_FIRST
a1 <- "(ANOS|ANO|ANOS DE IDADE)"
d2$AGE_FIRST <- as.numeric(gsub(a1,"",toupper(d2$AGE_FIRST)))

### 1.2.8. Reformat field COLOUR
a1 <- "(PRATEADO|PRATEADA|SILVER)"
s1 <- grepl(a1,toupper(d2$COLOUR))
d2$COLOUR[s1] <- "Prateado"
a1 <- "(DOURADO|DOURADA|GOLD)"
s1 <- grepl(a1,toupper(d2$COLOUR))
d2$COLOUR[s1] <- "Dourado"
a1 <- "(BRANCO|BRANCA|WHITE)"
s1 <- grepl(a1,toupper(d2$COLOUR))
d2$COLOUR[s1] <- "Branco"
a1 <- "(PRETO|PRETA|BLACK)"
s1 <- grepl(a1,toupper(d2$COLOUR))
d2$COLOUR[s1] <- "Preto"
a1 <- "(CINZENTO|CINZENTA|CINZA|GREY|GRAY)"
s1 <- grepl(a1,toupper(d2$COLOUR))
d2$COLOUR[s1] <- "Cinzento"
a1 <- "(AZUL|BLUE)"
s1 <- grepl(a1,toupper(d2$COLOUR))
d2$COLOUR[s1] <- "Azul"
a1 <- "(VERMELHO|VERMALHO|VERMELHA|VERMALHA|ENCARNADO|ENCARNADA|RED)"
s1 <- grepl(a1,toupper(d2$COLOUR))
d2$COLOUR[s1] <- "Vermelho"
a1 <- "(ROSA|ROSE)"
s1 <- grepl(a1,toupper(d2$COLOUR))
d2$COLOUR[s1] <- "Rosa"
d2$COLOUR <- ifelse(d2$COLOUR %in% c("Prateado","Dourado","Branco",
  "Preto","Cinzento","Azul","Vermelho","Rosa"),d2$COLOUR,
             ifelse(!is.na(d2$COLOUR),"Outra",NA))
d2$COLOUR <- factor(d2$COLOUR,levels=c("Prateado","Dourado","Branco",
  "Preto","Cinzento","Azul","Vermelho","Rosa","Outra"))

### 1.2.9. Reformat field PRICE_YN
d2$PRICE_YN <- ifelse(d2$PRICE_YN=="Sim",TRUE,
               ifelse(d2$PRICE_YN=="Não",FALSE,NA))

### 1.2.10. Reformat field PRICE
d2$PRICE <- gsub(",",".",d2$PRICE)
a1 <- "\\s|€|EUROS"
d2$PRICE <- as.numeric(gsub(a1,"",toupper(d2$PRICE)))

### 1.2.11. Reformat field PRICE_NEW
a1 <- "(NADA|NAO|NÃO|ZERO)"
s1 <- grepl(a1,toupper(d2$PRICE_NEW))
d2$PRICE_NEW[s1] <- 0
d2$PRICE_NEW <- gsub(",",".",d2$PRICE_NEW)
a1 <- "(\\s|CERCA DE|NO MAXIMO|NO MÁXIMO|MAXIMO|MÁXIMO|ATÉ|ATE|MENOS DE|OU MENOS|MENOS|€|EUROS)"
d2$PRICE_NEW <- as.numeric(gsub(a1,"",toupper(d2$PRICE_NEW)))

### 1.2.12. Reformat field SOCIALNET
a1 <- "(WHATSAPP|WPP|WATSAPP|WHATZAP|WATS APP|WHATSHAPP)"
s1 <- grepl(a1,toupper(d2$SOCIALNET))
d2$SOCIALNET[s1] <- "Whatsapp"
a1 <- "YOUTUBE"
s1 <- grepl(a1,toupper(d2$SOCIALNET))
d2$SOCIALNET[s1] <- "Youtube"
a1 <- "(REDDIT|REDIT)"
s1 <- grepl(a1,toupper(d2$SOCIALNET))
d2$SOCIALNET[s1] <- "Reddit"
d2$SOCIALNET <- ifelse(d2$SOCIALNET %in% c("Instagram","Snapchat","Facebook",
  "Twitter","Pinterest","Whatsapp","Youtube","Reddit","Nenhuma"),d2$SOCIALNET,
                ifelse(!is.na(d2$SOCIALNET),"Outra",NA))
d2$SOCIALNET <- factor(d2$SOCIALNET,levels=c("Instagram","Snapchat","Facebook",
  "Twitter","Pinterest","Whatsapp","Youtube","Reddit","Nenhuma","Outra"))

### 1.2.13. Reformat field PLAY_PHONE
d2$PLAY_PHONE <- ifelse(d2$PLAY_PHONE=="Sim",TRUE,
                 ifelse(d2$PLAY_PHONE=="Não",FALSE,NA))

### 1.2.14. Reformat field PLAY_OTHER
d2$PLAY_OTHER <- ifelse(d2$PLAY_OTHER=="Sim",TRUE,
                 ifelse(d2$PLAY_OTHER=="Não",FALSE,NA))

### 1.2.15. Reformat field SATISFACTION
d2$SATISFACTION <- ifelse(d2$SATISFACTION %in% 1:5,d2$SATISFACTION,NA)
d2$SATISFACTION <- factor(d2$SATISFACTION,levels=1:5)

## 1.3. AUTOMATIC CHECKS
s1 <- is.na(d2$TIME)
if(sum(s1) > 0){
  print(d1[s1,])                        #1. TIME cannot have NAs
}
s1 <- is.na(d2$PLACE)
if(sum(s1) > 0){
  print(d1[s1,])                        #2. PLACE cannot have NAs
}
s1 <- is.na(d2$SMARTPHONE_YN)
if(sum(s1) > 0){
  print(d1[s1,])                        #3. SMARTPHONE_YN cannot have NAs
}
s1 <- apply(d2[!d2$SMARTPHONE_YN,-(1:4)],1,function(x){sum(!is.na(x))>0})
if(sum(s1) > 0){
  print(d1[s1,])                        #4. if SMARTPHONE_YN is FALSE then all is NA
}
s1 <- !is.na(d2$AGE_FIRST[!d2$AGE_FIRST_YN])
if(sum(s1) > 0){
  print(d1[s1,])                        #5. if AGE_FIRST_YN is FALSE then AGE_FIRST is NA
}
s1 <- !is.numeric(d2$AGE_FIRST)
if(sum(s1) > 0){
  print(d1[s1,])                        #6. AGE_FIRST has to be numeric
}
s1 <- !is.na(d2$AGE_FIRST) & (d2$AGE_FIRST < 0 | d2$AGE_FIRST > 100)
if(sum(s1) > 0){
  print(d1[s1,])                        #7. AGE_FIRST has to be between 0 and 100
}
s1 <- !is.na(d2$PRICE[!d2$PRICE_YN])
if(sum(s1) > 0){
  print(d1[s1,])                        #8. if PRICE_YN is FALSE then PRICE is NA
}
s1 <- !is.numeric(d2$PRICE)
if(sum(s1) > 0){
  print(d1[s1,])                        #9. PRICE has to be numeric
}
s1 <- !is.na(d2$PRICE) & (d2$PRICE < 0 | d2$PRICE > 5000)
if(sum(s1) > 0){
  print(d1[s1,])                        #10. PRICE has to be between 0 and 5000
}
s1 <- !is.numeric(d2$PRICE_NEW)
if(sum(s1) > 0){
  print(d1[s1,])                        #11. PRICE_NEW has to be numeric
}
s1 <- !is.na(d2$PRICE_NEW) & (d2$PRICE_NEW < 0 | d2$PRICE_NEW > 10000)
if(sum(s1) > 0){
  print(d1[s1,])                        #12. PRICE_NEW has to be less than 10000
}

## 1.4. MANUAL CHECKS
summary(d2,maxsum=15)

s1 <- d2$GRADE == "Outro"
cbind(d1$GRADE,as.character(d2$GRADE))[s1,]         #1. GRADE = "Outro"
s1 <- d2$ACQUIRED == "Outro"
cbind(d1$ACQUIRED,as.character(d2$ACQUIRED))[s1,]   #1. ACQUIRED = "Outro"
s1 <- is.na(d2$AGE_FIRST)
cbind(d1$AGE_FIRST,d2$AGE_FIRST)[s1,]               #2. AGE_FIRST = NA
s1 <- d2$COLOUR == "Outra"
cbind(d1$COLOUR,as.character(d2$COLOUR))[s1,]       #3. COLOUR = "Outra"
s1 <- is.na(d2$PRICE)
cbind(d1$PRICE,d2$PRICE)[s1,]                       #4. PRICE = NA
s1 <- is.na(d2$PRICE_NEW)
cbind(d1$PRICE_NEW,d2$PRICE_NEW)[s1,]               #5. PRICE_NEW = NA
s1 <- d2$SOCIALNET == "Outra"
cbind(d1$SOCIALNET,as.character(d2$SOCIALNET))[s1,] #6. SOCIALNET = "Outra"

## 1.4. WRITE DATA
f2 <- "../results/data_ckd.csv"
write.table(
  x=d2,
  file=f2,
  sep=",",
  row.names=FALSE,
  col.names=TRUE,
  fileEncoding="UTF-8")

}
# 2. EXPLORE DATA
{
d3 <- d2[d2$GRADE!="Professor",]
d4 <- d2[d2$GRADE!="Professor" & d2$SMARTPHONE_YN,]

## 2.1. VISUALIZATION
library("ggplot2")

### 2.1.1. SMARTPHONE_YN (Pie chart)
tlab <- "Tem Smartphone?"
ggplot(d3) +
  geom_bar(aes(x=factor(1),fill=SMARTPHONE_YN)) +
  coord_polar("y",start=0) +
  labs(x="",y="",title=tlab) +
  scale_fill_discrete(name="",labels=c("Não","Sim")) +
  scale_x_discrete(breaks=NULL,labels=NULL)
  
### 2.1.2. ACQUIRED (Barplot)
ylab <- "Frequência"
tlab <- "Aquisição do Smartphone"
ggplot(d4) +
  geom_bar(aes(x=ACQUIRED),show.legend=FALSE) +
  labs(x="",y=ylab,title=tlab)

### 2.1.3. AGE_FIRST (Frequency table)
d5 <- d2[d2$GRADE!="Professor" & d2$SMARTPHONE_YN & d2$AGE_FIRST_YN,]
t1 <- table(d5$AGE_FIRST)
t2 <- prop.table(t1)
cbind(Freq=t1,Cumul=cumsum(t1),Rel=round(t2,2),RelCumul=round(cumsum(t2),2))

### 2.1.4. COLOUR (Colored barplot)
ylab <- "Frequência"
tlab <- "Cor do Smartphone"
ggplot(d4) +
  geom_bar(aes(x=COLOUR,fill=COLOUR),show.legend=FALSE) +
  labs(x="",y=ylab,title=tlab) +
  scale_fill_manual("legend",values=c(
    "Prateado"="lightgrey",
    "Dourado"="gold",
    "Branco"="white",
    "Preto"="black",
    "Cinzento"="grey",
    "Azul"="blue",
    "Vermelho"="red",
    "Rosa"="pink",
    "Outra"="darkgrey"),
    na.value="darkgrey")

### 2.1.5. PRICE (Boxplot)
d5 <- d2[d2$GRADE!="Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]
ylab <- "Preço (€)"
tlab <- "Preço do Smartphone"
ggplot(d5) +
  geom_boxplot(aes(x=factor(1),y=PRICE)) +
  labs(x="",y=ylab,title=tlab) + 
  scale_x_discrete(breaks=NULL,labels=NULL)

### 2.1.6. PRICE_NEW (Histogram)
xlab <- "Preço (€)"
ylab <- "Frequência"
tlab <-  "Preço disposto a pagar por Smartphone"
ggplot(d4) +
  geom_histogram(aes(x=PRICE_NEW),binwidth=50) +
  labs(x=xlab,y=ylab,title=tlab)

### 2.1.7. SOCIALNET vs PLAY_PHONE (Clustered barplot)
xlab <- "Rede Social"
ylab <- "Frequência"
tlab <- "Rede social mais usada vs. Jogar no Smartphone"
llab <- "Jogar"
ggplot(d4) +
  geom_bar(aes(x=SOCIALNET,fill=PLAY_PHONE),position="dodge") +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_fill_discrete(name=llab,labels=c("Não","Sim")) 

### 2.1.8. PLAY_OTHER vs PLAY_PHONE (Counts plot)
xlab <- "Jogar no Smartphone"
ylab <- "Jogar noutro dispositivo"
tlab <- "Jogar no Smartphone vs. Jogar noutro dispositivo"
ggplot(d4) +
  geom_count(mapping=aes(x=PLAY_PHONE,y=PLAY_OTHER)) + 
  labs(x=xlab,y=ylab,title=tlab) +
  scale_x_discrete(labels=c("Não","Sim")) +
  scale_y_discrete(labels=c("Não","Sim"))

### 2.1.9. SATISFACTION vs PRICE (Stacked barplot)
d5 <- d2[d2$GRADE!="Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]
d5$PRICE_CAT <- cut(d5$PRICE,breaks=5,dig.lab=4)

xlab <- "Satisfação [(Muito baixo) 1 - 5 (Muito alto)]"
ylab <- "Frequência"
tlab <- "Grau de satisfação com o Smartphone"
llab <- "Preço (€)"
ggplot(d5) +
  geom_bar(aes(x=SATISFACTION,fill=PRICE_CAT),position="stack") +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_fill_discrete(name=llab) 

## 2.2. SUMMARY STATISTICS

### 2.2.1. AGE_FIRST
d5 <- d2[d2$GRADE!="Professor" & d2$SMARTPHONE_YN & d2$AGE_FIRST_YN,]
d5 <- d5[!is.na(d5$AGE_FIRST),]

rbind(
  Mean=round(mean(d5$AGE_FIRST),2),     #mean
  Median=median(d5$AGE_FIRST),          #median
  Mode=as.numeric(names(which.max(table(d5$AGE_FIRST))))) #mode

rbind(
  StDev=round(sd(d5$AGE_FIRST),2),      #standard deviation
  IQR=IQR(d5$AGE_FIRST),                #interquantil range
  Range=max(d5$AGE_FIRST)-min(d5$AGE_FIRST)) #range

d5$AGE_FIRST_STD <- (d5$AGE_FIRST - mean(d5$AGE_FIRST))/sd(d5$AGE_FIRST)
xlab <- "Idade quando obteve primeiro Smartphone"
ylab <- "Densidade"
tlab <- "Comparação com distribuição Gaussiana"
ggplot(d5) +
  geom_density(aes(x=AGE_FIRST_STD)) +
  stat_function(fun=dnorm,color="red",args=list(mean=0,sd=1)) +
  labs(x=xlab,y=ylab,title=tlab)  

### 2.2.1. PRICE
d5 <- d2[d2$GRADE!="Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]
d5 <- d5[!is.na(d5$PRICE),]

rbind(
  Mean=round(mean(d5$PRICE),2),         #mean
  Median=median(d5$PRICE),              #median
  Mode=as.numeric(names(which.max(table(d5$PRICE))))) #mode

rbind(
  StDev=round(sd(d5$PRICE),2),          #standard deviation
  IQR=IQR(d5$PRICE),                    #interquantil range
  Range=max(d5$PRICE)-min(d5$PRICE))    #range

d5$PRICE_STD <- (d5$PRICE - mean(d5$PRICE))/sd(d5$PRICE)
xlab <- "Preço do Smartphone (€)"
ylab <- "Densidade"
tlab <- "Comparação com distribuição Gaussiana"
ggplot(d5) +
  geom_density(aes(x=PRICE_STD)) +
  stat_function(fun=dnorm,color="red",args=list(mean=0,sd=1)) +
  labs(x=xlab,y=ylab,title=tlab)  

### 2.2.1. PRICE_NEW
d5 <- d4[!is.na(d4$PRICE_NEW),]

rbind(
  Mean=round(mean(d5$PRICE_NEW),2),     #mean
  Median=median(d5$PRICE_NEW),          #median
  Mode=as.numeric(names(which.max(table(d5$PRICE_NEW))))) #mode

rbind(
  StDev=round(sd(d5$PRICE_NEW),2),      #standard deviation
  IQR=IQR(d5$PRICE_NEW),                #interquantil range
  Range=max(d5$PRICE_NEW)-min(d5$PRICE_NEW)) #range

d5$PRICE_NEW_STD <- (d5$PRICE_NEW - mean(d5$PRICE_NEW))/sd(d5$PRICE_NEW)
xlab <- "Preço disposto a pagar por Smartphone (€)"
ylab <- "Densidade"
tlab <- "Comparação com distribuição Gaussiana"
ggplot(d5) +
  geom_density(aes(x=PRICE_NEW_STD)) +
  stat_function(fun=dnorm,color="red",args=list(mean=0,sd=1)) +
  labs(x=xlab,y=ylab,title=tlab)  

}
# 3. MODEL DATA
{
## 3.1. PRICE_NEW ~ PRICE [Simple linear regression]
d5 <- d2[d2$GRADE!="Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]

m1 <- lm(PRICE_NEW ~ PRICE,data=d5)
summary(m1)

xlab <- "Preço (€)"
ylab <- "Preço disposto a pagar (€)"
tlab <- "Preço vs. Preço disposto a pagar"
ggplot(d5,aes(x=PRICE,y=PRICE_NEW)) +
  geom_point() +
  geom_abline(aes(intercept=0,slope=1),linetype="dashed") +
  geom_smooth(method="lm",se=FALSE) +
  labs(x=xlab,y=ylab,title=tlab)

## 3.2. PRICE_NEW ~ PRICE + PLAY_PHONE [Multiple linear regression]
d5 <- d2[d2$GRADE!="Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]

m1 <- lm(PRICE_NEW ~ PRICE + PLAY_PHONE,data=d5)
summary(m1)

xlab <- "Preço Smartphone (€)"
ylab <- "Preço disposto a pagar (€)"
tlab <- "Preço vs. Preço disposto a pagar (by Jogar)"
llab <- "Jogar"
ggplot(d5,aes(x=PRICE,y=PRICE_NEW,color=PLAY_PHONE)) +
  geom_point() +
  geom_abline(aes(intercept=0,slope=1),linetype="dashed") +
  geom_smooth(method="lm",se=FALSE) +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_color_discrete(name=llab,labels=c("Não","Sim")) 

## 3.3. PLAY_PHONE ~ PRICE [Simple logistic regression]
d5 <- d2[d2$GRADE!="Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]

m1 <- glm(PLAY_PHONE ~ PRICE,family="binomial",data=d5)
summary(m1)

m1_val <- m1$null.deviance - m1$deviance
m1_df <- m1$df.null - m1$df.residual
pchisq(m1_val,m1_df,lower.tail=FALSE)

round(exp(cbind(OR=coef(m1),confint.default(m1))),2)[-1,,drop=FALSE]

xlab <- "Jogar no Smartphone"
ylab <- "Preço (€)"
tlab <- "Jogar no Smartphone vs. Preço do Smartphone"
ggplot(d5) +
  geom_boxplot(aes(x=PLAY_PHONE,y=PRICE)) +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_x_discrete(labels=c("Não","Sim"))

xlab <- "Preço (€)"
ylab <- "Probabilidade de Jogar no Smartphone"
tlab <- "Jogar no Smartphone vs. Preço do Smartphone"
ggplot(d5,aes(x=PRICE,y=as.numeric(PLAY_PHONE))) +
  geom_point() +
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE) +
  labs(x=xlab,y=ylab,title=tlab)

## 3.4. PLAY_PHONE ~ PRICE + SOCIALNET [Multiple logistic regression]
d5 <- d2[d2$GRADE!="Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]
t1 <- table(d5$SOCIALNET,d5$PLAY_PHONE)
s1 <- d5$SOCIALNET %in% rownames(t1[apply(t1,1,function(x)all(x!=0)),])
d5 <- d5[s1,]

m1 <- glm(PLAY_PHONE ~ PRICE + SOCIALNET,family="binomial",data=d5)
summary(m1)

m1_val <- m1$null.deviance - m1$deviance
m1_df <- m1$df.null - m1$df.residual
pchisq(m1_val,m1_df,lower.tail=FALSE)

round(exp(cbind(OR=coef(m1),confint.default(m1))),2)[-1,,drop=FALSE]

xlab <- "Jogar no Smartphone"
ylab <- "Preço (€)"
tlab <- "Jogar no Smartphone vs. Preço do Smartphone (by Rede Social)"
ggplot(d5) +
  geom_boxplot(aes(x=PLAY_PHONE,y=PRICE)) +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_x_discrete(labels=c("Não","Sim")) +
  facet_grid(~SOCIALNET)

xlab <- "Preço (€)"
ylab <- "Probabilidade de Jogar no Smartphone"
tlab <- "Jogar no Smartphone vs. Preço do Smartphone (by Rede Social)"
llab <- "Rede Social"
ggplot(d5,aes(x=PRICE,y=as.numeric(PLAY_PHONE),color=SOCIALNET)) +
  geom_point() +
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE) +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_color_discrete(name=llab)

}
