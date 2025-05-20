#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     23.01.2020
#modificado: 20.05.22025

if(interactive()) setwd("2025/2025.05.20_escolas_OE-IUT2020/bin/")

suppressWarnings(suppressMessages(library("tidyverse")))
suppressWarnings(suppressMessages(library("janitor")))
suppressWarnings(suppressMessages(library("gridExtra")))

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
f1 <- "../data/datamod_20250520.csv"
tb1 <- read_csv(f1, col_names = FALSE, skip = 1, col_types = cols(.default = col_character())) |>
  rename(
    TIME = X1,           #Time stamp of interview             <auto>   <POSIXt>
    PLACE = X2,          #Name of education establishment     <open>   <factor>
    GRADE = X3,          #Grade level                         <closed> <factor>
    SMARTPHONE_YN = X4,  #Having Smartphone                   <yes/no> <logical>
    ACQUIRED = X5,       #Acquisition of Smartphone           <mixed>  <factor>
    AGE_FIRST_YN = X6,   #Remember age of first acquisition   <yes/no> <logical>
    AGE_FIRST = X7,      #Age when first smartphone acquired  <open>   <numeric>
    COLOUR = X8,         #Smartphone colour                   <open>   <factor>
    PRICE_YN = X9,       #Remember price of Smartphone        <yes/no> <logical>
    PRICE = X10,         #Price of Smartphone                 <open>   <numeric>
    PRICE_NEW = X11,     #Price to pay for new Smartphone     <open>   <numeric>
    SOCIALNET = X12,     #Most frequently used social network <mixed>  <factor>
    PLAY_PHONE = X13,    #Play on Smartphone                  <yes/no> <logical>
    PLAY_OTHER = X14,    #Play on other devices               <yes/no> <logical>
    SATISFACTION = X15   #Degree of satisfaction              <closed> <factor>
  )

## 1.2. REFORMAT RAW DATA
tb2 <- tb1 

### 1.2.1. Reformat field TIME <automatic>
tb2 <- tb2 |> mutate(
  TIME = as.POSIXlt(TIME)
)

### 1.2.2. Reformat field PLACE <open field>
tb_escolas <- read_csv("../data/escolas.csv", col_types = cols(.default = col_character()))
tb2 <- tb2 |> mutate(
  PLACE = toupper(iconv(enc2utf8(tb2$PLACE), "UTF-8", "ASCII//TRANSLIT"))
)
PLACE_TMP = NA
for(i in 1:nrow(tb_escolas)){
  isel <- grepl(tb_escolas[i,]$patterns, tb2$PLACE)
  PLACE_TMP[isel] <- tb_escolas[i,]$name
}
tb2 <- tb2 |> mutate(
  PLACE = factor(PLACE_TMP)
)
rm(tb_escolas, PLACE_TMP)

### 1.2.3. Reformat field GRADE <closed field>
tb2 <- tb2 |> mutate(
  GRADE = gsub("ºAno", "", GRADE),
  GRADE = factor(GRADE, levels = c(1:12, "Professor"))
)

### 1.2.4. Reformat field SMARTPHONE_YN <yes/no field>
tb2 <- tb2 |> mutate(
  SMARTPHONE_YN = case_when(
    SMARTPHONE_YN == "Sim" ~ TRUE,
    SMARTPHONE_YN == "Não" ~ FALSE,
    .default = NA
  )
)

### 1.2.5. Reformat field ACQUIRED <mixed field>
tb2 <- tb2 |> mutate(
  ACQUIRED = case_when(
    ACQUIRED %in% c("Novo", "Usado") ~ ACQUIRED,
    !is.na(ACQUIRED)                 ~ "Outro",
    .default = NA
  ),
  ACQUIRED = factor(ACQUIRED, levels = c("Novo", "Usado", "Outro"))
)

### 1.2.6. Reformat field AGE_FIRST_YN <yes/no field>
tb2 <- tb2 |> mutate(
  AGE_FIRST_YN = case_when(
    AGE_FIRST_YN == "Sim" ~ TRUE,
    AGE_FIRST_YN == "Não" ~ FALSE,
    .default = NA
  )
)

### 1.2.7. Reformat field AGE_FIRST <open field>
a1 <- "(\\s|AOS|QUANDO|TINHA|ANOS|ANO|ANOS DE IDADE)"
tb2 <- tb2 |> mutate(
  AGE_FIRST = toupper(iconv(enc2utf8(AGE_FIRST), "UTF-8", "ASCII//TRANSLIT")),
  AGE_FIRST = gsub(a1, "", AGE_FIRST),
  AGE_FIRST = suppressWarnings(as.numeric(AGE_FIRST))
)

### 1.2.8. Reformat field COLOUR <open field>
tb_cores <- read_csv("../data/cores.csv", col_types = cols(.default = col_character()))
tb2 <- tb2 |> mutate(
  COLOUR = toupper(iconv(enc2utf8(COLOUR), "UTF-8", "ASCII//TRANSLIT"))
)
COLOUR_TMP <- NA
for(i in 1:nrow(tb_cores)){
  isel <- grepl(tb_cores[i, ]$patterns, tb2$COLOUR)
  COLOUR_TMP[isel] <- tb_cores[i,]$name
}
tb2 <- tb2 |> mutate(
  COLOUR = case_when(
    COLOUR_TMP %in% tb_cores$name ~ COLOUR_TMP,
    !is.na(COLOUR)                ~ "Outra",
    .default = NA
  ),
  COLOUR = factor(COLOUR, levels = c(tb_cores$name, "Outra"))
)
rm(tb_cores, COLOUR_TMP)

### 1.2.9. Reformat field PRICE_YN <yes/no field>
tb2 <- tb2 |> mutate(
  PRICE_YN = case_when(
    PRICE_YN == "Sim" ~ TRUE,
    PRICE_YN == "Não" ~ FALSE,
    .default = NA
  )
)

### 1.2.10. Reformat field PRICE <open field>
a1 <- c("\\s", "\\+(\\/|\\s)?\\-", "E TAL", "E POUCOS", "APROXIMADAMENTE",
  "APROX\\.", "\\~", "EUROS", "EURO", "ACHO", "CERCA DE", "NO MAXIMO", "MAXIMO",
  "ATE", "MAIS DE", "MENOS DE", "OU MENOS", "MENOS", "NAO MAIS DO QUE",
  "MAIS OU MENOS", "(A|POR) VOLTA (DOS|DE)", "QUASE", "POR CONTA DE",
  "ATUALMENTE")
a1 <- paste(a1, collapse = "|")
tb2 <- tb2 |> mutate(
  PRICE = gsub("€|%|£|\\$", "", PRICE),
  PRICE = toupper(iconv(enc2utf8(PRICE), "UTF-8", "ASCII//TRANSLIT")),
  PRICE = gsub(a1, "", PRICE),
  PRICE = suppressWarnings(as.numeric(gsub(",", ".", PRICE)))
)

### 1.2.11. Reformat field PRICE_NEW <open field>
tb2 <- tb2 |> mutate(
  PRICE_NEW = gsub("€|%|£|\\$", "", PRICE_NEW),
  PRICE_NEW = toupper(iconv(enc2utf8(PRICE_NEW), "UTF-8", "ASCII//TRANSLIT")),
  PRICE_NEW = if_else(grepl("(^NADA$|^NAO$|^ZERO$|^0$)", PRICE_NEW), NA, PRICE_NEW),
  PRICE_NEW = gsub(a1, "", PRICE_NEW),
  PRICE_NEW = suppressWarnings(as.numeric(gsub(",", ".", PRICE_NEW)))
)
rm(a1)

### 1.2.12. Reformat field SOCIALNET <mixed field>
tb_redes <- read_csv("../data/redes_sociais.csv", col_types = cols(.default = col_character()))
tb2 <- tb2 |> mutate(
  SOCIALNET = toupper(iconv(enc2utf8(SOCIALNET), "UTF-8", "ASCII//TRANSLIT"))
)
SOCIALNET_TMP <- NA
for(i in 1:nrow(tb_redes)){
  isel <- grepl(tb_redes[i,]$patterns, tb2$SOCIALNET)
  SOCIALNET_TMP[isel] <- tb_redes[i,]$name
}
tb2 <- tb2 |> mutate(
  SOCIALNET = case_when(
    SOCIALNET_TMP %in% tb_redes$name ~ SOCIALNET_TMP,
    !is.na(SOCIALNET)                ~ "Outra",
    .default = NA
  ),
  SOCIALNET = factor(SOCIALNET, levels = c(tb_redes$name, "Outra"))
)
rm(tb_redes, SOCIALNET_TMP)

### 1.2.13. Reformat field PLAY_PHONE <yes/no field>
tb2 <- tb2 |> mutate(
  PLAY_PHONE = case_when(
    PLAY_PHONE == "Sim" ~ TRUE,
    PLAY_PHONE == "Não" ~ FALSE,
    .default = NA
  )
)

### 1.2.14. Reformat field PLAY_OTHER <yes/no field>
tb2 <- tb2 |> mutate(
  PLAY_OTHER = case_when(
    PLAY_OTHER == "Sim" ~ TRUE,
    PLAY_OTHER == "Não" ~ FALSE,
    .default = NA
  )
)

### 1.2.15. Reformat field SATISFACTION <closed field>
tb2 <- tb2 |> mutate(
  SATISFACTION = if_else(SATISFACTION %in% 1:5, SATISFACTION, NA),
  SATISFACTION = factor(SATISFACTION,levels=1:5)
)

## 1.3. AUTOMATIC CHECKS
tb2 |>                      #1. TIME cannot have NAs
  filter(is.na(TIME)) |>
  select(TIME)
tb2 |>                      #2. PLACE cannot have NAs
  filter(is.na(PLACE)) |>
  select(TIME, PLACE)                 
tb2 |>                      #3. SMARTPHONE_YN cannot have NAs
  filter(is.na(SMARTPHONE_YN)) |>
  select(TIME, SMARTPHONE_YN) 
tb2 |>                      #4. if SMARTPHONE_YN is FALSE then all is NA
  filter(!SMARTPHONE_YN, if_any(ACQUIRED:SATISFACTION, ~ !is.na(.))) |>
  select(TIME, SMARTPHONE_YN, ACQUIRED:SATISFACTION)
tb2 |>                      #5. if AGE_FIRST_YN is FALSE then AGE_FIRST is NA
  filter(!AGE_FIRST_YN, !is.na(AGE_FIRST)) |>
  select(TIME, AGE_FIRST_YN, AGE_FIRST)
tb2 |>                      #6. if AGE_FIRST_YN is TRUE then AGE_FIRST is not NA
  filter(SMARTPHONE_YN, AGE_FIRST_YN, is.na(AGE_FIRST)) |>
  select(TIME, SMARTPHONE_YN, AGE_FIRST_YN, AGE_FIRST)
tb2 |>                      #7. AGE_FIRST has to be numeric
  filter(!is.numeric(AGE_FIRST)) |>
  select(TIME, AGE_FIRST)
tb2 |>                      #8. AGE_FIRST has to be between 0 and 100
  filter(!is.na(AGE_FIRST), (AGE_FIRST < 0 | AGE_FIRST > 100)) |>
  select(TIME, AGE_FIRST)
tb2 |>                      #9. if PRICE_YN is FALSE then PRICE is NA
  filter(SMARTPHONE_YN, !PRICE_YN, !is.na(PRICE)) |>
  select(TIME, SMARTPHONE_YN, PRICE_YN, PRICE)
tb2 |>                      #10. if PRICE_YN is TRUE then PRICE is not NA
  filter(SMARTPHONE_YN, PRICE_YN, is.na(PRICE)) |>
  select(TIME, SMARTPHONE_YN, PRICE_YN, PRICE)
tb2 |>                      #11. PRICE has to be between 0 and 5000
  filter(!is.na(PRICE), (PRICE < 0 | PRICE > 5000)) |>
  select(TIME, PRICE)
tb2 |>                      #12. PRICE_NEW has to be less than 5000
  filter(!is.na(PRICE_NEW), (PRICE_NEW < 0 | PRICE_NEW > 5000)) |>
  select(PRICE_NEW)

## 1.4. MANUAL CHECKS
tb2 |> summary(maxsum = 15)

bind_cols(                              #1. ACQUIRED %in% c(NA, "Outra")
  tb1 |> select(TIME, ACQUIRED) |> rename(ACQUIRED_OLD = ACQUIRED),
  tb2 |> select(ACQUIRED)
) |>
  filter(!is.na(ACQUIRED_OLD), is.na(ACQUIRED) | ACQUIRED == "Outro") |>
  print(n = Inf)
bind_cols(                              #2. AGE_FIRST = NA
  tb1 |> select(TIME, AGE_FIRST) |> rename(AGE_FIRST_OLD = AGE_FIRST),
  tb2 |> select(AGE_FIRST)
) |>
  filter(!is.na(AGE_FIRST_OLD), is.na(AGE_FIRST)) |>
  print(n = Inf)
bind_cols(                              #3. COLOUR %in% c(NA, "Outra")
  tb1 |> select(TIME, COLOUR) |> rename(COLOUR_OLD = COLOUR),
  tb2 |> select(COLOUR)
) |>
  filter(!is.na(COLOUR_OLD), is.na(COLOUR) | COLOUR == "Outra") |>
  print(n = Inf)
bind_cols(                              #4. PRICE = NA
  tb1 |> select(TIME, PRICE) |> rename(PRICE_OLD = PRICE),
  tb2 |> select(PRICE)
) |>
  filter(!is.na(PRICE_OLD), is.na(PRICE)) |>
  print(n = Inf)
bind_cols(                              #5. PRICE_NEW = NA
  tb1 |> select(TIME, PRICE_NEW) |> rename(PRICE_NEW_OLD = PRICE_NEW),
  tb2 |> select(PRICE_NEW)
) |>
  filter(!is.na(PRICE_NEW_OLD), is.na(PRICE_NEW)) |>
  print(n = Inf)
bind_cols(                              #6. SOCIALNET %in% c(NA, "Outra")
  tb1 |> select(TIME, SOCIALNET) |> rename(SOCIALNET_OLD = SOCIALNET),
  tb2 |> select(SOCIALNET)
) |>
  filter(!is.na(SOCIALNET_OLD), is.na(SOCIALNET) | SOCIALNET == "Outra") |>
  print(n = Inf)

## 1.4. WRITE DATA
f1 <- "../results/data_20250520.csv"
tb2 |> write_csv(f1)

}
# 2. EXPLORE DATA
{
tb3 <- tb2 |> filter(!GRADE %in% "Professor")
tb4 <- tb3 |> filter(SMARTPHONE_YN)

## 2.1. VISUALIZATION

### 2.1.1. SMARTPHONE_YN (Pie chart)
f1 <- "../media/1.piechart.tif"
tlab <- "Tem Smartphone?"
p1 <- tb3 |>
  mutate(
    SMARTPHONE_YN = factor(SMARTPHONE_YN, levels = c(FALSE, TRUE), labels = c("Não", "Sim"))
  ) |>
  ggplot(aes(x = factor(1), fill = SMARTPHONE_YN)) +
  geom_bar() +
  coord_polar("y", start = 0) +
  labs(x = "", y = "", fill = "", title = tlab) +
  scale_x_discrete(breaks = NULL, labels = NULL)
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")
  
### 2.1.2. ACQUIRED (Barplot)
f1 <- "../media/2.barplot_simple.tif"
ylab <- "Frequência"
tlab <- "Aquisição do Smartphone"
p1 <- tb4 |>
  ggplot(aes(x = ACQUIRED)) +
  geom_bar(show.legend = FALSE) +
  labs(x = "", y = ylab, title = tlab)
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")

### 2.1.3. AGE_FIRST (Frequency table)
f1 <- "../media/3.cumtab.csv"
t1 <- tb4 |>
  filter(AGE_FIRST_YN) |>
  tabyl(AGE_FIRST) |>
  rename(Freq = n, Rel = percent) |>
  mutate(Cumul = cumsum(Freq), .after = Freq) |>
  mutate(RelCumul = cumsum(Rel), .after = Rel) |>
  adorn_pct_formatting(,,,Rel:RelCumul)
t1 |> write_csv(f1)

### 2.1.4. COLOUR (Colored barplot)
f1 <- "../media/4.barplot_colored.tif"
ylab <- "Frequência"
tlab <- "Cor do Smartphone"
icol <- c(
  "Prateado" = "lightgrey",
  "Dourado" = "gold",
  "Branco" = "white",
  "Preto" = "black",
  "Cinzento" = "grey",
  "Azul" = "blue",
  "Vermelho" = "red",
  "Rosa" = "pink",
  "Verde" = "green",
  "Roxo" = "purple",
  "Outra" = "darkgrey"
)
p1 <- tb4 |>
  ggplot(aes(x = COLOUR, fill = COLOUR)) +
  geom_bar(show.legend = FALSE) +
  labs(x = "", y = ylab, title = tlab) +
  scale_fill_manual("legend", values = icol, na.value = "darkgrey")
ggsave(f1 , p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")

### 2.1.5. PRICE (Boxplot)
f1 <- "../media/5.boxplot_simple.tif"
ylab <- "Preço (€)"
tlab <- "Preço do Smartphone"
p1 <- tb4 |>
  filter(PRICE_YN) |>
  ggplot(aes(x = factor(1), y = PRICE)) +
  geom_boxplot(na.rm = TRUE) +
  labs(x = "", y = ylab, title = tlab) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_x_discrete(breaks = NULL, labels = NULL)
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")

### 2.1.6. PRICE_NEW (Histogram)
f1 <- "../media/6.histogram.tif"
xlab <- "Preço (€)"
ylab <- "Frequência"
tlab <- "Preço disposto a pagar por Smartphone"
p1 <- tb4 |>
  ggplot(aes(x = PRICE_NEW)) +
  geom_histogram(binwidth = 200, na.rm = TRUE) +
  labs(x = xlab, y = ylab, title = tlab) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA))
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")

### 2.1.7. SOCIALNET vs PLAY_PHONE (Clustered barplot)
f1 <- "../media/7.barplot_clustered.tif"
xlab <- "Rede Social"
ylab <- "Frequência"
tlab <- "Rede social mais usada vs. Jogar no Smartphone"
llab <- "Jogar"
p1 <- tb4 |>
  mutate(
    PLAY_PHONE = factor(PLAY_PHONE, levels = c(FALSE, TRUE), labels = c("Não", "Sim"))
  ) |>
  ggplot(aes(x = SOCIALNET, fill = PLAY_PHONE)) +
  geom_bar(position = "dodge") +
  labs(x = xlab, y = ylab, fill = llab, title = tlab)
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")

### 2.1.8. PLAY_OTHER vs PLAY_PHONE (Counts plot)
f1 <- "../media/8.countplot.tif"
xlab <- "Jogar no Smartphone"
ylab <- "Jogar noutro dispositivo"
tlab <- "Jogar no Smartphone vs. Jogar noutro dispositivo"
p1 <- tb4 |>
  mutate(
    PLAY_PHONE = factor(PLAY_PHONE, levels = c(FALSE, TRUE), labels = c("Não", "Sim")),
    PLAY_OTHER = factor(PLAY_OTHER, levels = c(FALSE, TRUE), labels = c("Não", "Sim"))
  ) |>
  ggplot(aes(x = PLAY_PHONE, y = PLAY_OTHER)) +
  geom_count() + 
  labs(x = xlab, y = ylab, title = tlab) +
  scale_size_area()
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")

### 2.1.9. SATISFACTION vs PRICE (Stacked barplot)
f1 <- "../media/9.barplot_stacked.tif"
xlab <- "Satisfação [(Muito baixo) 1 - 5 (Muito alto)]"
ylab <- "Frequência"
tlab <- "Grau de satisfação com o Smartphone"
llab <- "Preço (€)"
p1 <- tb4 |>
  filter(PRICE_YN) |>
  mutate(PRICE_CAT = cut(PRICE, breaks = 5, dig.lab = 4)) |>
  ggplot(aes(x = SATISFACTION, fill = PRICE_CAT)) +
  geom_bar(position = "stack") +
  labs(x = xlab, y = ylab, fill = llab, title = tlab)
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")

## 2.2. SUMMARY STATISTICS

### 2.2.1. AGE_FIRST
t1 <- tb4 |>
  filter(AGE_FIRST_YN, !is.na(AGE_FIRST)) |>
  summarize(
    Name = "AGE_FIRST",
    Mean = round(mean(AGE_FIRST), 2),                      #mean
    Median = median(AGE_FIRST),                            #median
    Mode = as.numeric(names(which.max(table(AGE_FIRST)))), #mode
    StDev = round(sd(AGE_FIRST), 2),                       #standard deviation
    IQR = IQR(AGE_FIRST),                                  #interquantil range
    Range = max(AGE_FIRST) - min(AGE_FIRST)                #range
  )
xlab <- "Idade quando obteve primeiro Smartphone"
ylab <- "Densidade"
tlab <- "Comparação com distribuição Gaussiana"
p1 <- tb4 |>
  filter(AGE_FIRST_YN, !is.na(AGE_FIRST)) |>
  mutate(AGE_FIRST_STD = (AGE_FIRST - mean(AGE_FIRST))/sd(AGE_FIRST)) |>
  ggplot(aes(x = AGE_FIRST_STD)) +
  geom_density() +
  stat_function(fun = dnorm, color = "red", args = list(mean = 0, sd = 1)) +
  labs(x = xlab, y = ylab, title = tlab)  

### 2.2.2. PRICE
t2 <- tb4 |>
  filter(PRICE_YN, !is.na(PRICE)) |>
  summarize(
    Name = "PRICE",
    Mean = round(mean(PRICE), 2),                      #mean
    Median = median(PRICE),                            #median
    Mode = as.numeric(names(which.max(table(PRICE)))), #mode
    StDev = round(sd(PRICE), 2),                       #standard deviation
    IQR = IQR(PRICE),                                  #interquantil range
    Range = max(PRICE) - min(PRICE)                    #range
  )
xlab <- "Preço do Smartphone (€)"
ylab <- "Densidade"
tlab <- "Comparação com distribuição Gaussiana"
p2 <- tb4 |>
  filter(PRICE_YN, !is.na(PRICE)) |>
  mutate(PRICE_STD = (PRICE - mean(PRICE))/sd(PRICE)) |>
  ggplot(aes(x = PRICE_STD)) +
  geom_density() +
  stat_function(fun = dnorm, color = "red", args = list(mean = 0, sd = 1)) +
  labs(x = xlab, y = ylab, title = tlab)  

### 2.2.3. PRICE_NEW
t3 <- tb4 |>
  filter(!is.na(PRICE_NEW)) |>
  summarize(
    Name = "PRICE_NEW",
    Mean = round(mean(PRICE_NEW), 2),                      #mean
    Median = median(PRICE_NEW),                            #median
    Mode = as.numeric(names(which.max(table(PRICE_NEW)))), #mode
    StDev = round(sd(PRICE_NEW), 2),                       #standard deviation
    IQR = IQR(PRICE_NEW),                                  #interquantil range
    Range = max(PRICE_NEW) - min(PRICE_NEW)                #range
  )
xlab <- "Preço disposto a pagar por Smartphone (€)"
ylab <- "Densidade"
tlab <- "Comparação com distribuição Gaussiana"
p3 <- tb4 |>
  filter(!is.na(PRICE_NEW)) |>
  mutate(PRICE_NEW_STD = (PRICE_NEW - mean(PRICE_NEW))/sd(PRICE_NEW)) |>
  ggplot(aes(x = PRICE_NEW_STD)) +
  geom_density() +
  stat_function(fun = dnorm, color = "red", args = list(mean = 0, sd = 1)) +
  labs(x = xlab, y = ylab, title = tlab)  

f1 <- "../media/10.summtab.csv"
t4 <- bind_rows(t1, t2, t3)
t4 |> write_csv(f1)

f1 <- "../media/11.densityplot.tif"
p4 <- grid.arrange(p1, p2, p3, nrow = 1)
ggsave(f1, p4, "tiff", width = 21, height = 7, compression = "lzw")

}
# 3. MODEL DATA
{
tb5 <- tb4 |> filter(PRICE_YN)

## 3.1. PRICE_NEW ~ PRICE [Simple linear regression]
f1 <- "../media/12.lmtab_simple.txt"
m1 <- lm(PRICE_NEW ~ PRICE, data = tb5)
capture.output("Simple linear regression:", file = f1)
capture.output(summary(m1), file = f1, append = TRUE)

f1 <- "../media/13.linear_regression_simple.tif"
xlab <- "Preço (€)"
ylab <- "Preço disposto a pagar (€)"
tlab <- "Preço vs. Preço disposto a pagar"
set.seed(12345)
p1 <- tb5 |>
  ggplot(aes(x = PRICE, y = PRICE_NEW)) +
  geom_jitter(width = 10, height = 10, na.rm = TRUE) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = "dashed") +
  geom_smooth(method = "lm", formula = 'y ~ x', se = FALSE, na.rm = TRUE) +
  labs(x = xlab, y = ylab, title = tlab)
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")

## 3.2. PRICE_NEW ~ PRICE + PLAY_PHONE [Multiple linear regression]
f1 <- "../media/14.lmtab_multiple.txt"
m2 <- lm(PRICE_NEW ~ PRICE + PLAY_PHONE, data = tb5)
capture.output("Multiple linear regression:", file = f1)
capture.output(summary(m2), file = f1, append = TRUE)

f1 <- "../media/15.linear_regression_multiple.tif"
xlab <- "Preço Smartphone (€)"
ylab <- "Preço disposto a pagar (€)"
tlab <- "Preço vs. Preço disposto a pagar (by Jogar)"
llab <- "Jogar"
set.seed(12345)
p1 <- tb5 |>
  mutate(
    PLAY_PHONE = factor(PLAY_PHONE, levels = c(FALSE, TRUE), labels = c("Não", "Sim")),
  ) |>
  ggplot(aes(x = PRICE, y = PRICE_NEW, color = PLAY_PHONE)) +
  geom_jitter(shape = 1, size = 2, width = 10, height = 10, na.rm = TRUE) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = "dashed") +
  geom_smooth(method = "lm", formula = 'y ~ x', se = FALSE, na.rm = TRUE) +
  labs(x = xlab, y = ylab, color = llab, title = tlab) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")

## 3.3. PLAY_PHONE ~ PRICE [Simple logistic regression]
f1 <- "../media/16.glmtab_simple.txt"
m3 <- glm(PLAY_PHONE ~ PRICE, family = "binomial", data = tb5)
m3_val <- m3$null.deviance - m3$deviance
m3_df  <- m3$df.null - m3$df.residual
m3_pc  <- pchisq(m3_val, m3_df, lower.tail = FALSE)
m3_or  <- round(exp(cbind(OR = coef(m3), confint.default(m3))), 2)[-1, , drop = FALSE]
capture.output("Simple logistic regression:", file = f1)
capture.output(summary(m3), file = f1, append = TRUE)
capture.output(m3_pc, file = f1, append = TRUE)
capture.output(m3_or, file = f1, append = TRUE)

f1 <- "../media/17.boxplot_simple.tif"
xlab <- "Jogar"
ylab <- "Preço (€)"
tlab <- "Jogar vs. Preço do Smartphone"
p1 <- tb5 |>
  mutate(
    PLAY_PHONE = factor(PLAY_PHONE, levels = c(FALSE, TRUE), labels = c("Não", "Sim")),
  ) |>
  ggplot(aes(x = PLAY_PHONE, y = PRICE)) +
  geom_boxplot(na.rm = TRUE) +
  labs(x = xlab, y = ylab, title = tlab)
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")

f1 <- "../media/18.logistic_regression_simple.tif"
xlab <- "Preço (€)"
ylab <- "Probabilidade de Jogar"
tlab <- "Jogar vs. Preço do Smartphone"
p1 <- tb5 |>
  ggplot(aes(x = PRICE, y = as.numeric(PLAY_PHONE))) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
    formula = 'y ~ x', se = FALSE, na.rm = TRUE) +
  labs(x = xlab, y = ylab, title = tlab)
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")

## 3.4. PLAY_PHONE ~ PRICE + SOCIALNET [Multiple logistic regression]
tb6 <- tb5 |>
  mutate(
    PLAY_PHONE_TRUE = sum(PLAY_PHONE),
    PLAY_PHONE_FALSE = sum(!PLAY_PHONE),
    .by = SOCIALNET
  ) |>
  filter(PLAY_PHONE_TRUE != 0,  PLAY_PHONE_FALSE != 0)
  
f1 <- "../media/19.glmtab_multiple.txt"
m4 <- glm(PLAY_PHONE ~ PRICE + SOCIALNET, family = "binomial", data = tb6)
m4_val <- m4$null.deviance - m4$deviance
m4_df <- m4$df.null - m4$df.residual
m4_pc <- pchisq(m4_val, m4_df, lower.tail = FALSE)
m4_or <- round(exp(cbind(OR = coef(m4), confint.default(m4))), 2)[-1, , drop = FALSE]
capture.output("Multiple logistic regression:", file = f1)
capture.output(summary(m4), file = f1, append = TRUE)
capture.output(m4_pc, file = f1, append = TRUE)
capture.output(m4_or, file = f1, append = TRUE)

f1 <- "../media/20.boxplot_multiple.tif"
xlab <- "Jogar"
ylab <- "Preço (€)"
tlab <- "Jogar vs. Preço do Smartphone (by Rede Social)"
p1 <- tb6 |>
  mutate(
    PLAY_PHONE = factor(PLAY_PHONE, levels = c(FALSE, TRUE), labels = c("Não", "Sim")),
  ) |>
  ggplot(aes(x = PLAY_PHONE, y = PRICE)) +
  geom_boxplot(na.rm = TRUE) +
  labs(x = xlab, y = ylab, title = tlab) +
  facet_grid(~SOCIALNET)
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression = "lzw")

f1 <- "../media/21.logistic_regression_multiple.tif"
xlab <- "Preço (€)"
ylab <- "Probabilidade de Jogar"
tlab <- "Jogar vs. Preço do Smartphone (by Rede Social)"
llab <- "Rede Social"
set.seed(12345)
p1 <- tb6 |>
  ggplot(aes(x = PRICE, y = as.numeric(PLAY_PHONE), color = SOCIALNET)) +
  geom_jitter(shape = 1, size = 2, width = 20, height = 0, na.rm = TRUE) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
    formula = 'y ~ x', se = FALSE, na.rm = TRUE) +
  labs(x = xlab, y = ylab, color = llab, title = tlab) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))
ggsave(f1, p1, "tiff", width = 5.25, height = 5.25, compression="lzw")

}
