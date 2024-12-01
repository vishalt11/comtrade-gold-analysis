library(tidyverse)

options(scipen = 999)

# Monthly Data set pre-process

gold_df <- read.csv('../data/gold_prices_monthly.csv')
gold_df$date <- as.Date(gold_df$date, format = '%m/%d/%Y')
gold_df$date <- format(gold_df$date,"%Y-%m-01")
gold_df$priceUSD_troyounce <- as.numeric(gsub(",", "", gold_df$priceUSD_troyounce))*32.1507

file_list <- list.files(path="../data/export_data/", full.names = TRUE)
ldf <- lapply(file_list, read.csv, row.names = NULL)
df <- do.call("rbind", ldf)

rm(ldf, file_list)

colnames(df) <- colnames(df)[2:ncol(df)]
df <- df[1:(ncol(df)-1)]

# Select the required columns

df <- df[c("refYear", "refMonth", "reporterDesc", "partnerDesc", "cmdCode", 
           "qtyUnitAbbr", "qty", "altQtyUnitAbbr", "altQty", "primaryValue")]


df[df$partnerDesc == "T\xfcrkiye",]$partnerDesc <- 'Turkey'

df[df$qtyUnitAbbr == "N/A",]$qtyUnitAbbr <- NA
df[df$altQtyUnitAbbr == "N/A",]$altQtyUnitAbbr <- NA
df[df$qty == 0,]$qty <- NA
df[df$altQty == 0,]$altQty <- NA


brics <- c("China, Hong Kong SAR", "China", "South Africa", "India", "Russia", "Brazil")

west <- c("USA", "United Kingdom", "Spain", "France", "Germany", "Turkey", "Italy", "Japan",
          "Rep. of Korea", "Switzerland", "Canada", "Cyprus", "Slovenia")

df$bricsflag <- NA
df[df$partnerDesc %in% brics,]$bricsflag <- 'b'
df[df$partnerDesc %in% west,]$bricsflag <- 'w'

df$date <- as.Date(paste('01', df$refMonth, df$refYear, sep = '/'), format = "%d/%m/%Y")
gold_df <- gold_df[gold_df$date >= min(df$date) & gold_df$date <= max(df$date),]

df$refYear <- NULL
df$refMonth <- NULL

df$gold_price <- sapply(df$date, function(x) gold_df[gold_df$date == x,]$priceUSD_troyounce)

df <- df[df$partnerDesc != 'World',]

df$cmdCode <- as.factor(df$cmdCode)

str(df)

df %>%
  filter(cmdCode == 7108, reporterDesc == "Kenya") %>%
  group_by(date) %>% 
  summarise(sqty = sum(qty, na.rm = TRUE), spval = sum(primaryValue),
            saltqty = sum(altQty, na.rm = TRUE), gval = mean(gold_price)) %>%
  mutate(cval_qty = spval/sqty, cval_altqty = spval/saltqty) %>%
  select(sqty, saltqty, spval, cval_qty, cval_altqty, gval) %>%
  print(n = 100)

write.csv(df, "../data/cleaned_monthly_export.csv", row.names = FALSE)
