library(ggplot2)
library(comtradr)
library(tidyverse)
library(dplyr)
library(dbplyr)


library(readr) # for guess_encoding

library(DBI)
library(RPostgres)

options(scipen = 999)

# connect to postgres DB, and get the table
pw <- "muix7pcj"

con_try <- dbCanConnect(RPostgres::Postgres(), dbname="test_db", 
                        port = 5432,user = "postgres", password = pw)
con_try


con <- dbConnect(RPostgres::Postgres(), dbname = "test_db",
                 port = 5432, user = "postgres", password = pw)

df_postgres <- dbGetQuery(con, 'SELECT * FROM Public."rsa_ken_GoldExport"')

# You can also disconnect from a database using:
dbDisconnect(con)



# read csv
guess_encoding('./data/TradeData.csv')
df <- read.csv("./data/TradeData.csv", sep = ",", row.names=NULL)

colnames(df)

sum(is.na(df))
str(df)

df <- df[c("refYear", "reporterDesc", "partnerDesc", "cmdCode",  
           "qty", "altQty", "primaryValue")]

df[df$partnerDesc == "T\xfcrkiye",]$partnerDesc <- 'Turkey'

# You might want to check out pgfutter for importing csv / JSON files into pgadmin / postgreSQL
write.csv(df, './data/cleaned_data.csv', row.names = FALSE)

df$commodity_price <- df$primaryValue/df$altQty


#BRICS and West Analysis

brics <- c("China, Hong Kong SAR", "China", "South Africa", "India", "Russia", "Brazil")
west <- c("USA", "United Kingdom", "Spain", "France", "Germany", "Turkey")

sum(df[df$partnerISO %in% brics,]$fobvalue)
df[df$partnerISO %in% brics,]

#aggregated fob sum by year for brics and west
aggregate(df[df$partnerISO %in% brics,]$fobvalue, by=list(Category=df[df$partnerISO %in% brics,]$refMonth), FUN=sum)

aggregate(df[df$partnerISO %in% west,]$fobvalue, by=list(Category=df[df$partnerISO %in% west,]$refMonth), FUN=sum)


range(df[df$isOriginalClassification == 710812,]$commodity_price, na.rm = TRUE)

ggplot(df[df$isOriginalClassification == 710812,], aes(x = commodity_price)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(title = "Kernel Density Plot of commodity_price", x = "commodity_price", y = "Density")

range(df[df$isOriginalClassification == 710813,]$commodity_price, na.rm = TRUE)

ggplot(df[df$isOriginalClassification == 710813,], aes(x = commodity_price)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(title = "Kernel Density Plot of commodity_price", x = "commodity_price", y = "Density")

















