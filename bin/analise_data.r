#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.1.1
#criado:     23.01.2020
#modificado: 02.12.2024

setwd("2024/2024.12.02_escolas_OE-IUT2020/bin/")

library("ggplot2")
library("gridExtra")

#0. INDEX
{
# 1. DATA WRANGLING
# 1.1. READ RAW DATA
# 1.2. FORMAT RAW DATA
# 1.3. AUTOMATIC CHECKS
# 1.4. MANUAL CHECKS
# 1.5. WRITE DATA
# 2. EXPLORE DATA
# 2.1. VISUALIZATION
# 2.2. SUMMARY STATISTICS
# 3. MODEL DATA
# 3.1. PRICE_NEW ~ PRICE
# 3.2. PRICE_NEW ~ PRICE + PLAY_PHONE
# 3.3. PLAY_PHONE ~ PRICE
# 3.4. PLAY_PHONE ~ PRICE + SOCIALNET

}
# 1. DATA WRANGLING
{
## 1.1. READ RAW DATA
f1 <- "../data/datamod_20241202.csv"
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
  "TIME",          #Time stamp of interview             <automatic> <POSIXt>
  "PLACE",         #Name of education establishment     <open>      <factor>
  "GRADE",         #Grade level                         <closed>    <factor>
  "SMARTPHONE_YN", #Having Smartphone                   <yes/no>    <logical>
  "ACQUIRED",      #Acquisition of Smartphone           <mixed>     <factor>
  "AGE_FIRST_YN",  #Remember age of first acquisition   <yes/no>    <logical>
  "AGE_FIRST",     #Age when first smartphone acquired  <open>      <numeric>
  "COLOUR",        #Smartphone colour                   <open>      <factor>
  "PRICE_YN",      #Remember price of Smartphone        <yes/no>    <logical>
  "PRICE",         #Price of Smartphone                 <open>      <numeric>
  "PRICE_NEW",     #Price to pay for new Smartphone     <open>      <numeric>
  "SOCIALNET",     #Social network used more frequently <mixed>     <factor>
  "PLAY_PHONE",    #Play on Smartphone                  <yes/no>    <logical>
  "PLAY_OTHER",    #Play on other devices               <yes/no>    <logical>
  "SATISFACTION"   #Degree of satisfaction              <closed>    <factor>
)

## 1.2. REFORMAT RAW DATA
d2 <- d1

### 1.2.1. Reformat field TIME <automatic>
d2$TIME <- as.POSIXlt(d2$TIME)

### 1.2.2. Reformat field PLACE <open field>
d2$PLACE <- toupper(iconv(enc2utf8(as.character(d2$PLACE)),"UTF-8","ASCII//TRANSLIT"))
a1    <- "(AL\\sBERTO|AL-BERTO)"
a2    <- "QUINTA.*MARQUES"
a3_1  <- "INSTITUTO.*DESENVOLVIMENTO.*SOCIAL"
a3_2  <- "(IDE|I\\.D\\.E\\.|I\\.\\sD\\.\\sE\\.)"
a3_3  <- "(IDS|I\\.D\\.S\\.|I\\.\\sD\\.\\sS\\.)"
a4    <- "FREI.*LUIS.*SOUSA"
a5    <- "GUADALUPE"
a6    <- "FUNDAO"
a7    <- "ANTONIO.*SERGIO"
a8    <- "PASSOS.*MANUEL"
a9_1  <- "INSTITUTO.*CIENCIAS.*EDUCATIVAS"
a9_2  <- "(^ICE$|I\\.C\\.E\\.|I\\.\\sC\\.\\sE\\.)"
a10_1 <- "ESCOLA.*SUPERIOR.*EDUCACAO.*SANTAREM"
a10_2 <- "INSTITUTO.*POLITECNICO.*SANTAREM"
a11_1 <- "SINES"
a11_2 <- "VASCO.*GAMA"
a12   <- "FERREIRA.*DIAS"
a13_1 <- "(LUIS|LUIZ).*FREITAS.*BRANCO"
a13_2 <- "(ESLFB|E\\.S\\.L\\.F\\.B\\.|E\\.\\sS\\.\\sL\\.\\sF\\.\\sB\\.)"
a14_1 <- "INSTITUTO.*EDUCACAO.*DESENVOLVIMENTO.*PROFISSIONAL"
a14_2 <- "(IEDP|I\\.E\\.D\\.P\\.|I\\.\\sE\\.\\sD\\.\\sP\\.)"
a15   <- "BAIXO.*BARROSO"
a16   <- "BATALHA"
a17   <- "PALMELA"
a18   <- "CEFAD"
a19   <- "SA.*BANDEIRA"
a20   <- "GIL.*VICENTE"
a21   <- "COLEGIO.*MIRAMAR"
a22   <- "FRANCISCO.*HOLANDA"
a23   <- "MONTIJO"
a24_1 <- "ALBUFEIRA"
a24_2 <- "(EBSA|E\\.B\\.S\\.A\\.|E\\.\\sB\\.\\sS\\.\\sA\\.)"
a25   <- "JOSE.*FALCAO"
a26_1 <- "CAMILO.*CASTELO.*BRANCO"
a26_2 <- "(AECCB|A\\.E\\.C\\.C\\.B\\.|A\\.\\sE\\.\\sC\\.\\sC\\.\\sB\\.)"
a27_1 <- "ESCOLA.*PROFISSIONAL.*DE.*ESTUDOS.*TECNICOS"
a27_2 <- "(EPET|E\\.P\\.E\\.T|E\\.\\sP\\.\\sE\\.\\sT\\.)"
a28   <- "HENRIQUES.*NOGUEIRA"
a29   <- "MOURA"
a30_1 <- "MANUEL.*(LARANJEIRA|LARANGEIRA)"
a30_2 <- "(AEML|A\\.E\\.M\\.L|A\\.\\sE\\.\\sM\\.\\sL\\.)"
a31   <- "(SENHOR|SR).*(MILAGRES|MILAGRE)"
a32   <- "(DANIEL|DANIEAL).*SAMPAIO"
a33   <- "MANUEL.*CARGALEIRO"
a34   <- "(GUALDIM|GUALDI).*PAIS"
a35   <- "AL.*BERTO"
a36   <- "PEDRO.*ALEXANDRINO"
a37   <- "(D\\.|DONA).*INES"
s1  <- grepl(a1,d2$PLACE)
s2  <- grepl(a2,d2$PLACE)
s3  <- grepl(a3_1,d2$PLACE) | grepl(a3_2,d2$PLACE) | grepl(a3_3,d2$PLACE)
s4  <- grepl(a4,d2$PLACE)
s5  <- grepl(a5,d2$PLACE)
s6  <- grepl(a6,d2$PLACE)
s7  <- grepl(a7,d2$PLACE)
s8  <- grepl(a8,d2$PLACE)
s9  <- grepl(a9_1,d2$PLACE) | grepl(a9_2,d2$PLACE)
s10 <- grepl(a10_1,d2$PLACE) | grepl(a10_2,d2$PLACE)
s11 <- grepl(a11_1,d2$PLACE) | grepl(a11_2,d2$PLACE)
s12 <- grepl(a12,d2$PLACE)
s13 <- grepl(a13_1,d2$PLACE) | grepl(a13_2,d2$PLACE)
s14 <- grepl(a14_1,d2$PLACE) | grepl(a14_2,d2$PLACE)
s15 <- grepl(a15,d2$PLACE)
s16 <- grepl(a16,d2$PLACE)
s17 <- grepl(a17,d2$PLACE)
s18 <- grepl(a18,d2$PLACE)
s19 <- grepl(a19,d2$PLACE)
s20 <- grepl(a20,d2$PLACE)
s21 <- grepl(a21,d2$PLACE)
s22 <- grepl(a22,d2$PLACE)
s23 <- grepl(a23,d2$PLACE)
s24 <- grepl(a24_1,d2$PLACE) | grepl(a24_2,d2$PLACE)
s25 <- grepl(a25,d2$PLACE)
s26 <- grepl(a26_1,d2$PLACE) | grepl(a26_2,d2$PLACE)
s27 <- grepl(a27_1,d2$PLACE) | grepl(a27_2,d2$PLACE)
s28 <- grepl(a28,d2$PLACE)
s29 <- grepl(a29,d2$PLACE)
s30 <- grepl(a30_1,d2$PLACE) | grepl(a30_2,d2$PLACE)
s31 <- grepl(a31,d2$PLACE)
s32 <- grepl(a32,d2$PLACE)
s33 <- grepl(a33,d2$PLACE)
s34 <- grepl(a34,d2$PLACE)
s35 <- grepl(a35,d2$PLACE)
s36 <- grepl(a36,d2$PLACE)
s37 <- grepl(a37,d2$PLACE)
d2$PLACE <- NA
d2$PLACE[s1]  <- "Escola Secundária Poeta Al Berto"
d2$PLACE[s2]  <- "Escola Secundária Quinta do Marquês"
d2$PLACE[s3]  <- "Instituto de Desenvolvimento Social"
d2$PLACE[s4]  <- "Externato Frei Luís de Sousa"
d2$PLACE[s5]  <- "Colégio Guadalupe"
d2$PLACE[s6]  <- "Agrupamento de Escolas do Fundão"
d2$PLACE[s7]  <- "Escola Secundária António Sérgio"
d2$PLACE[s8]  <- "Agrupamento de Escolas Passos Manuel"
d2$PLACE[s9]  <- "Instituto de Ciências Educativas"
d2$PLACE[s10] <- "Escola Superior de Educação de Santarém"
d2$PLACE[s11] <- "Escola Básica Vasco da Gama de Sines"
d2$PLACE[s12] <- "Escola Secundária Ferreira Dias"
d2$PLACE[s13] <- "Escola Secundária Luís Freitas Branco"
d2$PLACE[s14] <- "Instituto de Educação e Desenvolvimento Profissional"
d2$PLACE[s15] <- "Escola Básica e Secundária Baixo Barroso"
d2$PLACE[s16] <- "Agrupamento de Escolas da Batalha"
d2$PLACE[s17] <- "Escola Secundária de Palmela"
d2$PLACE[s18] <- "Escola Profissional CEFAD"
d2$PLACE[s19] <- "Agrupamento de Escolas Sá da Bandeira"
d2$PLACE[s20] <- "Agrupamento de Escolas Gil Vicente"
d2$PLACE[s21] <- "Colégio Miramar"
d2$PLACE[s22] <- "Escola Secundária Francisco de Holanda"
d2$PLACE[s23] <- "Escola Profissional do Montijo"
d2$PLACE[s24] <- "Escola Básica e Secundária de Albufeira"
d2$PLACE[s25] <- "Escola Secundária José Falcão"
d2$PLACE[s26] <- "Agrupamento de Escolas Camilo Castelo Branco"
d2$PLACE[s27] <- "Escola profissional de estudos técnicos"
d2$PLACE[s28] <- "Escola Secundária Henriques Nogueira"
d2$PLACE[s29] <- "Escola Secundária de Moura"
d2$PLACE[s30] <- "Agrupamento de Escolas Dr. Manuel Laranjeira"
d2$PLACE[s31] <- "Colégio Senhor dos Milagres"
d2$PLACE[s32] <- "Escola Secundária Daniel Sampaio"
d2$PLACE[s33] <- "Escola Secundária Manuel Cargaleiro"
d2$PLACE[s34] <- "Escola Básica Gualdim Pais"
d2$PLACE[s35] <- "Escola Secundária Poeta Al Berto"
d2$PLACE[s36] <- "Escola Secundária Pedro Alexandrino"
d2$PLACE[s37] <- "Escola Secundária Dona Inês de Castro de Alcobaça"
d2$PLACE <- factor(d2$PLACE)

### 1.2.3. Reformat field GRADE <closed field>
d2$GRADE <- factor(gsub("ºAno","",d2$GRADE),levels=c(1:12,"Professor"))

### 1.2.4. Reformat field SMARTPHONE_YN <yes/no field>
d2$SMARTPHONE_YN <- ifelse(d2$SMARTPHONE_YN=="Sim",TRUE,
                    ifelse(d2$SMARTPHONE_YN=="Não",FALSE,NA))

### 1.2.5. Reformat field ACQUIRED <mixed field>
d2$ACQUIRED <- ifelse(d2$ACQUIRED %in% c("Novo","Usado"),d2$ACQUIRED,
               ifelse(!is.na(d2$ACQUIRED),"Outro",NA))
d2$ACQUIRED <- factor(d2$ACQUIRED,levels=c("Novo","Usado","Outro"))

### 1.2.6. Reformat field AGE_FIRST_YN <yes/no field>
d2$AGE_FIRST_YN <- ifelse(d2$AGE_FIRST_YN=="Sim",TRUE,
                   ifelse(d2$AGE_FIRST_YN=="Não",FALSE,NA))

### 1.2.7. Reformat field AGE_FIRST <open field>
d2$AGE_FIRST <- toupper(iconv(enc2utf8(as.character(d2$AGE_FIRST)),"UTF-8","ASCII//TRANSLIT"))
a1 <- "(\\s|AOS|TINHA|ANOS|ANO|ANOS DE IDADE)"
d2$AGE_FIRST <- as.numeric(gsub(a1,"",d2$AGE_FIRST))

### 1.2.8. Reformat field COLOUR <open field>
d2$COLOUR <- toupper(iconv(enc2utf8(as.character(d2$COLOUR)),"UTF-8","ASCII//TRANSLIT"))
a1  <- "(PRATEADO|PRATEADA|SILVER|PRATA)"
a2  <- "(DOURADO|DOURADA|DORADO|DORADA|GOLD)"
a3  <- "(BRANCO|BRANCA|WHITE)"
a4  <- "(PRETO|PRETA|PERTO|BLACK)"
a5  <- "(CINZENTO|CINZENTA|CINZA|GREY|GRAY|SINZENTO)"
a6  <- "(AZUL|BLUE|CIANO|CYAN)"
a7  <- "(VERMELHO|VERMALHO|VERMELHA|VERMALHA|ENCARNADO|ENCARNADA|RED)"
a8  <- "(ROSA|ROSINHA|ROSE)"
a9  <- "(VERDE|GREEN)"
a10 <- "(ROXO|ROXA|ROCHO|ROCHA|LILAS|PURPLE)"
s1  <- grepl(a1,d2$COLOUR)
s2  <- grepl(a2,d2$COLOUR)
s3  <- grepl(a3,d2$COLOUR)
s4  <- grepl(a4,d2$COLOUR)
s5  <- grepl(a5,d2$COLOUR)
s6  <- grepl(a6,d2$COLOUR)
s7  <- grepl(a7,d2$COLOUR)
s8  <- grepl(a8,d2$COLOUR)
s9  <- grepl(a9,d2$COLOUR)
s10 <- grepl(a10,d2$COLOUR)
d2$COLOUR[s1] <- "Prateado"
d2$COLOUR[s2] <- "Dourado"
d2$COLOUR[s3] <- "Branco"
d2$COLOUR[s4] <- "Preto"
d2$COLOUR[s5] <- "Cinzento"
d2$COLOUR[s6] <- "Azul"
d2$COLOUR[s7] <- "Vermelho"
d2$COLOUR[s8] <- "Rosa"
d2$COLOUR[s9] <- "Verde"
d2$COLOUR[s10] <- "Roxo"
d2$COLOUR <- ifelse(d2$COLOUR %in% c("Prateado","Dourado","Branco","Preto",
  "Cinzento","Azul","Vermelho","Rosa","Verde","Roxo"),d2$COLOUR,
             ifelse(!is.na(d2$COLOUR),"Outra",NA))
d2$COLOUR <- factor(d2$COLOUR,levels=c("Prateado","Dourado","Branco","Preto",
  "Cinzento","Azul","Vermelho","Rosa","Verde","Roxo","Outra"))

### 1.2.9. Reformat field PRICE_YN <yes/no field>
d2$PRICE_YN <- ifelse(d2$PRICE_YN=="Sim",TRUE,
               ifelse(d2$PRICE_YN=="Não",FALSE,NA))

### 1.2.10. Reformat field PRICE <open field>
d2$PRICE <- gsub("€|%|£|$","",gsub(",",".",d2$PRICE))
d2$PRICE <- toupper(iconv(enc2utf8(as.character(d2$PRICE)),"UTF-8","ASCII//TRANSLIT"))
a1 <- c("\\s","\\+(\\/|\\s)?\\-","E TAL","E POUCOS","APROXIMADAMENTE",
  "APROX\\.","\\~","EUROS","EURO","ACHO","CERCA DE","NO MAXIMO","MAXIMO","ATE",
  "MAIS DE","MENOS DE","OU MENOS","MENOS","NAO MAIS DO QUE", "MAIS OU MENOS",
  "(A|POR) VOLTA (DOS|DE)","QUASE","POR CONTA DE")
a1 <- paste(a1,collapse="|")
d2$PRICE <- as.numeric(gsub(a1,"",d2$PRICE))

### 1.2.11. Reformat field PRICE_NEW <open field>
d2$PRICE_NEW <- gsub("€|%|£|$","",gsub(",",".",d2$PRICE_NEW))
d2$PRICE_NEW <- toupper(iconv(enc2utf8(as.character(d2$PRICE_NEW)),"UTF-8","ASCII//TRANSLIT"))
s1 <- grepl("(^NADA$|^NAO$|^ZERO$|^0$)",d2$PRICE_NEW)
d2$PRICE_NEW[s1] <- NA
d2$PRICE_NEW <- gsub(",",".",d2$PRICE_NEW)
d2$PRICE_NEW <- as.numeric(gsub(a1,"",d2$PRICE_NEW))

### 1.2.12. Reformat field SOCIALNET <mixed field>
d2$SOCIALNET <- toupper(iconv(enc2utf8(as.character(d2$SOCIALNET)),"UTF-8","ASCII//TRANSLIT"))
a1  <- "WHATSAPP"
a2  <- "INSTAGRAM"
a3  <- "SNAPCHAT"
a4  <- "(TWITTER|X)"
a5  <- "FACEBOOK"
a6  <- "NENHUMA"
a7  <- "YOUTUBE"
a8  <- "(REDDIT|REDIT)"
a9  <- "(TIKTOK|TIK\\sTOK|TIKOK)"
a10 <- "TELEGRAM"
a11 <- "DISCORD"
s1  <- grepl(a1,d2$SOCIALNET)
s2  <- grepl(a2,d2$SOCIALNET)
s3  <- grepl(a3,d2$SOCIALNET)
s4  <- grepl(a4,d2$SOCIALNET)
s5  <- grepl(a5,d2$SOCIALNET)
s6  <- grepl(a6,d2$SOCIALNET)
s7  <- grepl(a7,d2$SOCIALNET)
s8  <- grepl(a8,d2$SOCIALNET)
s9  <- grepl(a9,d2$SOCIALNET)
s10 <- grepl(a10,d2$SOCIALNET)
s11 <- grepl(a11,d2$SOCIALNET)
d2$SOCIALNET[s1]  <- "WhatsApp"
d2$SOCIALNET[s2]  <- "Instagram"
d2$SOCIALNET[s3]  <- "Snapchat"
d2$SOCIALNET[s4]  <- "Twitter"
d2$SOCIALNET[s5]  <- "Facebook"
d2$SOCIALNET[s6]  <- "Nenhuma"
d2$SOCIALNET[s7]  <- "Youtube"
d2$SOCIALNET[s8]  <- "Reddit"
d2$SOCIALNET[s9]  <- "TikTok"
d2$SOCIALNET[s10] <- "Telegram"
d2$SOCIALNET[s11] <- "Discord"
d2$SOCIALNET <- ifelse(d2$SOCIALNET %in% c("WhatsApp","Instagram","Snapchat",
  "Twitter","Facebook","Nenhuma","Youtube","Reddit","TikTok","Telegram","Discord"),d2$SOCIALNET,
                ifelse(!is.na(d2$SOCIALNET),"Outra",NA))
d2$SOCIALNET <- factor(d2$SOCIALNET,levels=c("WhatsApp","Instagram","Snapchat",
  "Twitter","Facebook","Youtube","Reddit","TikTok","Telegram","Discord","Outra","Nenhuma"))

### 1.2.13. Reformat field PLAY_PHONE <yes/no field>
d2$PLAY_PHONE <- ifelse(d2$PLAY_PHONE=="Sim",TRUE,
                 ifelse(d2$PLAY_PHONE=="Não",FALSE,NA))

### 1.2.14. Reformat field PLAY_OTHER <yes/no field>
d2$PLAY_OTHER <- ifelse(d2$PLAY_OTHER=="Sim",TRUE,
                 ifelse(d2$PLAY_OTHER=="Não",FALSE,NA))

### 1.2.15. Reformat field SATISFACTION <closed field>
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
s1 <- d2$SMARTPHONE_YN & d2$AGE_FIRST_YN & is.na(d2$AGE_FIRST)
if(sum(s1) > 0){
  print(d1[s1,])                        #6. if AGE_FIRST_YN is TRUE then AGE_FIRST is not NA
}
s1 <- !is.numeric(d2$AGE_FIRST)
if(sum(s1) > 0){
  print(d1[s1,])                        #7. AGE_FIRST has to be numeric
}
s1 <- !is.na(d2$AGE_FIRST) & (d2$AGE_FIRST < 0 | d2$AGE_FIRST > 100)
if(sum(s1) > 0){
  print(d1[s1,])                        #8. AGE_FIRST has to be between 0 and 100
}
s1 <- d2$SMARTPHONE_YN & !d2$PRICE_YN & !is.na(d2$PRICE)
if(sum(s1) > 0){
  print(d1[s1,])                        #9. if PRICE_YN is FALSE then PRICE is NA
}
s1 <- d2$SMARTPHONE_YN & d2$PRICE_YN & is.na(d2$PRICE)
if(sum(s1) > 0){
  print(d1[s1,])                        #10. if PRICE_YN is TRUE then PRICE is not NA
}
s1 <- !is.na(d2$PRICE) & (d2$PRICE < 0 | d2$PRICE > 5000)
if(sum(s1) > 0){
  print(d1[s1,])                        #11. PRICE has to be between 0 and 5000
}
s1 <- !is.na(d2$PRICE_NEW) & (d2$PRICE_NEW < 0 | d2$PRICE_NEW > 5000)
if(sum(s1) > 0){
  print(d1[s1,])                        #12. PRICE_NEW has to be less than 5000
}

## 1.4. MANUAL CHECKS
summary(d2,maxsum=15)

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
f1 <- "../results/data_20241202.csv"
write.table(
  x=d2,
  file=f1,
  sep=",",
  row.names=FALSE,
  col.names=TRUE,
  fileEncoding="UTF-8")

}
# 2. EXPLORE DATA
{
d3 <- d2[!d2$GRADE %in% "Professor",]
d4 <- d2[!d2$GRADE %in% "Professor" & d2$SMARTPHONE_YN,]

## 2.1. VISUALIZATION

### 2.1.1. SMARTPHONE_YN (Pie chart)
f1 <- "../media/1.piechart.tif"
tlab <- "Tem Smartphone?"
slab <- gsub("TRUE","Sim",gsub("FALSE","Não",sort(unique(d3$SMARTPHONE))))
p1 <- ggplot(d3) +
  geom_bar(aes(x=factor(1),fill=SMARTPHONE_YN)) +
  coord_polar("y",start=0) +
  labs(x="",y="",title=tlab) +
  scale_fill_discrete(name="",labels=slab) +
  scale_x_discrete(breaks=NULL,labels=NULL)
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")
  
### 2.1.2. ACQUIRED (Barplot)
f1 <- "../media/2.barplot_simple.tif"
ylab <- "Frequência"
tlab <- "Aquisição do Smartphone"
p1 <- ggplot(d4) +
  geom_bar(aes(x=ACQUIRED),show.legend=FALSE) +
  labs(x="",y=ylab,title=tlab)
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

### 2.1.3. AGE_FIRST (Frequency table)
f1 <- "../media/3.cumtab.csv"
d5 <- d2[!d2$GRADE %in% "Professor" & d2$SMARTPHONE_YN & d2$AGE_FIRST_YN,]
t1 <- table(d5$AGE_FIRST)
t2 <- prop.table(t1)
t3 <- cbind(Freq=t1,Cumul=cumsum(t1),Rel=round(t2,2),RelCumul=round(cumsum(t2),2))
write.table(x=t3,file=f1,sep=",",col.names=NA,fileEncoding="UTF-8")

### 2.1.4. COLOUR (Colored barplot)
f1 <- "../media/4.barplot_colored.tif"
ylab <- "Frequência"
tlab <- "Cor do Smartphone"
p1 <- ggplot(d4) +
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
    "Verde"="green",
    "Roxo"="purple",
    "Outra"="darkgrey"),
    na.value="darkgrey")
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

### 2.1.5. PRICE (Boxplot)
d5 <- d2[!d2$GRADE %in% "Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]

f1 <- "../media/5.boxplot_simple.tif"
ylab <- "Preço (€)"
tlab <- "Preço do Smartphone"
p1 <- ggplot(d5) +
  geom_boxplot(aes(x=factor(1),y=PRICE)) +
  labs(x="",y=ylab,title=tlab) + 
  scale_y_continuous(expand=c(0, 0), limits=c(0, NA)) +
  scale_x_discrete(breaks=NULL,labels=NULL)
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

### 2.1.6. PRICE_NEW (Histogram)
f1 <- "../media/6.histogram.tif"
xlab <- "Preço (€)"
ylab <- "Frequência"
tlab <-  "Preço disposto a pagar por Smartphone"
p1 <- ggplot(d4) +
  geom_histogram(aes(x=PRICE_NEW),binwidth=200) +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_x_continuous(expand=c(0, 0), limits=c(0, NA))
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

### 2.1.7. SOCIALNET vs PLAY_PHONE (Clustered barplot)
f1 <- "../media/7.barplot_clustered.tif"
xlab <- "Rede Social"
ylab <- "Frequência"
tlab <- "Rede social mais usada vs. Jogar no Smartphone"
llab <- "Jogar"
p1 <- ggplot(d4) +
  geom_bar(aes(x=SOCIALNET,fill=PLAY_PHONE),position="dodge") +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_fill_discrete(name=llab,labels=c("Não","Sim")) 
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

### 2.1.8. PLAY_OTHER vs PLAY_PHONE (Counts plot)
f1 <- "../media/8.countplot.tif"
xlab <- "Jogar no Smartphone"
ylab <- "Jogar noutro dispositivo"
tlab <- "Jogar no Smartphone vs. Jogar noutro dispositivo"
p1 <- ggplot(d4) +
  geom_count(mapping=aes(x=PLAY_PHONE,y=PLAY_OTHER)) + 
  labs(x=xlab,y=ylab,title=tlab) +
  scale_size_area() +
  scale_x_discrete(labels=c("Não","Sim")) +
  scale_y_discrete(labels=c("Não","Sim"))
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

### 2.1.9. SATISFACTION vs PRICE (Stacked barplot)
d5 <- d2[!d2$GRADE %in% "Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]
d5$PRICE_CAT <- cut(d5$PRICE,breaks=5,dig.lab=4)

f1 <- "../media/9.barplot_stacked.tif"
xlab <- "Satisfação [(Muito baixo) 1 - 5 (Muito alto)]"
ylab <- "Frequência"
tlab <- "Grau de satisfação com o Smartphone"
llab <- "Preço (€)"
p1 <- ggplot(d5) +
  geom_bar(aes(x=SATISFACTION,fill=PRICE_CAT),position="stack") +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_fill_discrete(name=llab) 
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

## 2.2. SUMMARY STATISTICS

### 2.2.1. AGE_FIRST
d5 <- d2[!d2$GRADE %in% "Professor" & d2$SMARTPHONE_YN & d2$AGE_FIRST_YN,]
d5 <- d5[!is.na(d5$AGE_FIRST),]
v1 <- c(
  Mean=round(mean(d5$AGE_FIRST),2),                       #mean
  Median=median(d5$AGE_FIRST),                            #median
  Mode=as.numeric(names(which.max(table(d5$AGE_FIRST)))), #mode
  StDev=round(sd(d5$AGE_FIRST),2),                        #standard deviation
  IQR=IQR(d5$AGE_FIRST),                                  #interquantil range
  Range=max(d5$AGE_FIRST) - min(d5$AGE_FIRST)             #range
)
d5$AGE_FIRST_STD <- (d5$AGE_FIRST - mean(d5$AGE_FIRST))/sd(d5$AGE_FIRST)
xlab <- "Idade quando obteve primeiro Smartphone"
ylab <- "Densidade"
tlab <- "Comparação com distribuição Gaussiana"
p1 <- ggplot(d5) +
  geom_density(aes(x=AGE_FIRST_STD)) +
  stat_function(fun=dnorm,color="red",args=list(mean=0,sd=1)) +
  labs(x=xlab,y=ylab,title=tlab)  

### 2.2.1. PRICE
d5 <- d2[!d2$GRADE %in% "Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]
d5 <- d5[!is.na(d5$PRICE),]
v2 <- c(
  Mean=round(mean(d5$PRICE),2),                           #mean
  Median=median(d5$PRICE),                                #median
  Mode=as.numeric(names(which.max(table(d5$PRICE)))),     #mode
  StDev=round(sd(d5$PRICE),2),                            #standard deviation
  IQR=IQR(d5$PRICE),                                      #interquantil range
  Range=max(d5$PRICE) - min(d5$PRICE)                     #range
)
d5$PRICE_STD <- (d5$PRICE - mean(d5$PRICE))/sd(d5$PRICE)
xlab <- "Preço do Smartphone (€)"
ylab <- "Densidade"
tlab <- "Comparação com distribuição Gaussiana"
p2 <- ggplot(d5) +
  geom_density(aes(x=PRICE_STD)) +
  stat_function(fun=dnorm,color="red",args=list(mean=0,sd=1)) +
  labs(x=xlab,y=ylab,title=tlab)  

### 2.2.1. PRICE_NEW
d5 <- d4[!is.na(d4$PRICE_NEW),]
v3 <- c(
  Mean=round(mean(d5$PRICE_NEW),2),                       #mean
  Median=median(d5$PRICE_NEW),                            #median
  Mode=as.numeric(names(which.max(table(d5$PRICE_NEW)))), #mode
  StDev=round(sd(d5$PRICE_NEW),2),                        #standard deviation
  IQR=IQR(d5$PRICE_NEW),                                  #interquantil range
  Range=max(d5$PRICE_NEW)-min(d5$PRICE_NEW)               #range
)
d5$PRICE_NEW_STD <- (d5$PRICE_NEW - mean(d5$PRICE_NEW))/sd(d5$PRICE_NEW)
xlab <- "Preço disposto a pagar por Smartphone (€)"
ylab <- "Densidade"
tlab <- "Comparação com distribuição Gaussiana"
p3 <- ggplot(d5) +
  geom_density(aes(x=PRICE_NEW_STD)) +
  stat_function(fun=dnorm,color="red",args=list(mean=0,sd=1)) +
  labs(x=xlab,y=ylab,title=tlab)  

f1 <- "../media/10.summtab.csv"
t1 <- rbind(AGE_FIRST=v1,PRICE=v2,PRICE_NEW=v3)
write.table(x=t1,file=f1,sep=",",col.names=NA,fileEncoding="UTF-8")

f1 <- "../media/11.densityplot.tif"
p4 <- grid.arrange(p1,p2,p3,nrow=1)
ggsave(f1,p4,"tiff",width=21,height=7,units="in",dpi=300,compression="lzw")

}
# 3. MODEL DATA
{
## 3.1. PRICE_NEW ~ PRICE [Simple linear regression]
d5 <- d2[!d2$GRADE %in% "Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]

f1 <- "../media/12.lmtab_simple.txt"
m1 <- lm(PRICE_NEW ~ PRICE,data=d5)
capture.output(summary(m1),file=f1)

f1 <- "../media/13.linear_regression_simple.tif"
xlab <- "Preço (€)"
ylab <- "Preço disposto a pagar (€)"
tlab <- "Preço vs. Preço disposto a pagar"
set.seed(12345)
p1 <- ggplot(d5,aes(x=PRICE,y=PRICE_NEW)) +
  geom_jitter(width=10,height=10) +
  geom_abline(aes(intercept=0,slope=1),linetype="dashed") +
  geom_smooth(method="lm",se=FALSE) +
  labs(x=xlab,y=ylab,title=tlab)
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

## 3.2. PRICE_NEW ~ PRICE + PLAY_PHONE [Multiple linear regression]
d5 <- d2[!d2$GRADE %in% "Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]

f1 <- "../media/14.lmtab_multiple.txt"
m1 <- lm(PRICE_NEW ~ PRICE + PLAY_PHONE,data=d5)
capture.output(summary(m1),file=f1)

f1 <- "../media/15.linear_regression_multiple.tif"
xlab <- "Preço Smartphone (€)"
ylab <- "Preço disposto a pagar (€)"
tlab <- "Preço vs. Preço disposto a pagar (by Jogar)"
llab <- "Jogar"
set.seed(12345)
p1 <- ggplot(d5,aes(x=PRICE,y=PRICE_NEW,color=PLAY_PHONE)) +
  geom_jitter(shape=1,size=2,width=10,height=10) +
  geom_abline(aes(intercept=0,slope=1),linetype="dashed") +
  geom_smooth(method="lm",se=FALSE) +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_color_discrete(name=llab,labels=c("Não","Sim")) 
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

## 3.3. PLAY_PHONE ~ PRICE [Simple logistic regression]
d5 <- d2[!d2$GRADE %in% "Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]

f1 <- "../media/16.glmtab_simple.txt"
m1 <- glm(PLAY_PHONE ~ PRICE,family="binomial",data=d5)
m1_val <- m1$null.deviance - m1$deviance
m1_df <- m1$df.null - m1$df.residual
m1_pc <- pchisq(m1_val,m1_df,lower.tail=FALSE)
m1_or <- round(exp(cbind(OR=coef(m1),confint.default(m1))),2)[-1,,drop=FALSE]
capture.output(summary(m1),file=f1)
capture.output(m1_pc,file=f1,append=TRUE)
capture.output(m1_or,file=f1,append=TRUE)

f1 <- "../media/17.boxplot_simple.tif"
xlab <- "Jogar"
ylab <- "Preço (€)"
tlab <- "Jogar vs. Preço do Smartphone"
p1 <- ggplot(d5) +
  geom_boxplot(aes(x=PLAY_PHONE,y=PRICE)) +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_x_discrete(labels=c("Não","Sim"))
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

f1 <- "../media/18.logistic_regression_simple.tif"
xlab <- "Preço (€)"
ylab <- "Probabilidade de Jogar"
tlab <- "Jogar vs. Preço do Smartphone"
p1 <- ggplot(d5,aes(x=PRICE,y=as.numeric(PLAY_PHONE))) +
  geom_point() +
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE) +
  labs(x=xlab,y=ylab,title=tlab)
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

## 3.4. PLAY_PHONE ~ PRICE + SOCIALNET [Multiple logistic regression]
d5 <- d2[!d2$GRADE %in%"Professor" & d2$SMARTPHONE_YN & d2$PRICE_YN,]
t1 <- table(d5$SOCIALNET,d5$PLAY_PHONE)
s1 <- d5$SOCIALNET %in% rownames(t1[apply(t1,1,function(x)all(x!=0)),])
d5 <- d5[s1,]

f1 <- "../media/19.glmtab_multiple.txt"
m1 <- glm(PLAY_PHONE ~ PRICE + SOCIALNET,family="binomial",data=d5)
m1_val <- m1$null.deviance - m1$deviance
m1_df <- m1$df.null - m1$df.residual
m1_pc <- pchisq(m1_val,m1_df,lower.tail=FALSE)
m1_or <- round(exp(cbind(OR=coef(m1),confint.default(m1))),2)[-1,,drop=FALSE]
capture.output(summary(m1),file=f1)
capture.output(m1_pc,file=f1,append=TRUE)
capture.output(m1_or,file=f1,append=TRUE)

f1 <- "../media/20.boxplot_multiple.tif"
xlab <- "Jogar"
ylab <- "Preço (€)"
tlab <- "Jogar vs. Preço do Smartphone (by Rede Social)"
p1 <- ggplot(d5) +
  geom_boxplot(aes(x=PLAY_PHONE,y=PRICE)) +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_x_discrete(labels=c("Não","Sim")) +
  facet_grid(~SOCIALNET)
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

f1 <- "../media/21.logistic_regression_multiple.tif"
xlab <- "Preço (€)"
ylab <- "Probabilidade de Jogar"
tlab <- "Jogar vs. Preço do Smartphone (by Rede Social)"
llab <- "Rede Social"
set.seed(12345)
p1 <- ggplot(d5,aes(x=PRICE,y=as.numeric(PLAY_PHONE),color=SOCIALNET)) +
  geom_jitter(shape=1,size=2,width=20,height=0) +
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE) +
  labs(x=xlab,y=ylab,title=tlab) +
  scale_color_discrete(name=llab)
ggsave(f1,p1,"tiff",width=5.25,height=5.25,units="in",dpi=300,compression="lzw")

}
