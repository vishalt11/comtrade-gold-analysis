library(tidyverse)

options(scipen = 999)

# Yearly Data Pre-process

path <- "import"

df <- read.csv(file = paste("../data/yearly_raw_", path, ".csv", sep = ""), row.names = NULL)

colnames(df) <- colnames(df)[2:ncol(df)]
df <- df[1:(ncol(df)-1)]

df <- df[c("refYear", "reporterDesc", "partnerDesc", "cmdCode", 
           "qtyUnitAbbr", "qty", "altQtyUnitAbbr", "altQty", "primaryValue")]

if(path == "import"){
  colnames(df) <- c("refYear", "partnerDesc", "reporterDesc", "cmdCode", 
                    "qtyUnitAbbr", "qty", "altQtyUnitAbbr", "altQty", "primaryValue")
}

colnames(df)[1] <- "date"

df[df$partnerDesc == "T\xfcrkiye",]$partnerDesc <- 'Turkey'

df[df$qtyUnitAbbr == "N/A",]$qtyUnitAbbr <- NA
df[df$altQtyUnitAbbr == "N/A",]$altQtyUnitAbbr <- NA
df[df$qty == 0,]$qty <- NA
df[df$altQty == 0,]$altQty <- NA

brics <- c("China, Hong Kong SAR", "China", "South Africa", "India", "Russia", "Brazil")

west <- c("USA", "United Kingdom", "Spain", "France", "Germany", "Turkey", "Italy", "Japan",
          "Rep. of Korea", "Switzerland", "Canada", "Cyprus", "Slovenia", "Poland", "Ireland",
          "Norway", "Austria", "New Zealand", "Luxembourg", "Slovakia", "Estonia", "Denmark",
          "Netherlands", "Belgium", "Sweden", "Czechia", "Australia")

df$bricsflag <- NA
df[df$partnerDesc %in% brics,]$bricsflag <- 'b'
df[df$partnerDesc %in% west,]$bricsflag <- 'w'

df$date <- lubridate::ymd(df$date, truncated = 2L)

df <- df[df$partnerDesc != 'World',]

write.csv(df, paste("../data/yearly_cleaned_", path, ".csv", sep = ""), row.names = FALSE)
