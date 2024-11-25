library(ggplot2)
library(comtradr)
library(dplyr)
library(tidyverse)

#Price of gold in Jan 4th 2023 is 59,638 USD.
#Price of gold in June 12th 2023 is 63,018 USD.
#Price of gold in Dec 26th 2023 is 66,429 USD.

options(scipen = 999)

#set_primary_comtrade_key(key = "xxxxxc678ca4dbxxxxxxxx8285r3")
#HS_codes <- comtradr::ct_get_ref_table('HS')
#cc <- country_codes
#ct_get_data(commodity_code = 7108, reporter = "GHA", partner = "ARE", start_date = "2023-01", 
#            end_date = "2023-12", flow_direction = "export")

ghana_brics <- read.csv("./TradeData_10_17_2024_18_55_36_Ghana_Brics.csv", sep = ",", row.names=NULL)

colnames(ghana_brics)
ghana_brics <- ghana_brics[c("refMonth", "reporterISO", "flowCode", "partnerISO", 
                             "isOriginalClassification", "qtyUnitAbbr", "isAltQtyEstimated", "cifvalue", "fobvalue")]


ghana_brics$commodity_price <- ghana_brics$fobvalue/ghana_brics$isAltQtyEstimated


ghana_uae <- read.csv("./TradeData_10_20_2024_16_2_27_GhanaRep_UAE_export.csv", sep = ",", row.names=NULL)
ghana_uae <- ghana_uae[c("refMonth", "reporterISO", "flowCode", "partnerISO", 
                             "isOriginalClassification", "qtyUnitAbbr", "isAltQtyEstimated", "cifvalue", "fobvalue")]
ghana_uae$commodity_price <- ghana_uae$fobvalue/ghana_uae$isAltQtyEstimated
sum(ghana_uae$isAltQtyEstimated)

uae_import <- read.csv("./TradeData_10_20_2024_16_25_50_UAERep_import.csv", sep = ",", row.names=NULL)

sum(ghana_uae$fobvalue)

uae_import[uae_import$partnerISO == "Ghana",]$fobvalue


# China and Africa export and import differences


# UAE and Africa export and import differences

uae_world <- read.csv("./UAERep_world_import.csv", sep = ",", row.names=NULL)
world_uae <- read.csv("./WorldRep_uae_export.csv", sep = ",", row.names=NULL)

uae_world <- uae_world[c("refMonth", "reporterISO", "flowCode", "partnerISO", "isAltQtyEstimated", "fobvalue")]
world_uae <- world_uae[c("refMonth", "reporterISO", "flowCode", "partnerISO", "isAltQtyEstimated", "fobvalue")]

uae_world$commodity_price <- uae_world$fobvalue/uae_world$isAltQtyEstimated
world_uae$commodity_price <- world_uae$fobvalue/world_uae$isAltQtyEstimated

#remove row with world
uae_world <- uae_world[which(!uae_world$partnerISO %in% "World"),]

sapply(uae_world$partnerISO, function(x) world_uae[world_uae$reporterISO == x,]$fobvalue)

#ggplot(uae_world, aes(partnerISO, fobvalue)) + geom_point() + coord_flip()

uae_world$partner_reported_value <- NA

for(i in uae_world$partnerISO){
  try({uae_world[uae_world$partnerISO == i,]$partner_reported_value <- world_uae[world_uae$reporterISO == i,]$fobvalue}, silent=TRUE)
}



uae_world <- uae_world[complete.cases(uae_world),]

#Comparison Dot Plot
#https://uc-r.github.io/cleveland-dot-plots
uae_world <- uae_world[sample(nrow(uae_world),20),]

df <- data.frame()

for (i in uae_world$partnerISO) {
  row1 <- c(i, uae_world[uae_world$partnerISO == i,]$fobvalue, "uae_r")
  row2 <- c(i, uae_world[uae_world$partnerISO == i,]$partner_reported_value, "part_r")
  df <- rbind(df, row1)
  df <- rbind(df, row2)
}

colnames(df) <- c("Partner", "Value", "Reporter")

df$Value <- as.numeric(df$Value)
df$Value <- round(df$Value/1000000,2)

df <- df[which(!df$Partner %in% "Switzerland"),]

max(df$Value)


right_label <- df %>%
  group_by(Partner) %>%
  arrange(desc(Value)) %>%
  top_n(1)

left_label <- df %>%
  group_by(Partner) %>%
  arrange(desc(Value)) %>%
  slice(2)


# create data frame that identifies revenue differences over 20%
big_diff <- df %>% 
  spread(Reporter, Value) %>% 
  group_by(Partner) %>% 
  mutate(Max = max(part_r, uae_r),
         Min = min(part_r, uae_r),
         Diff = Max / Min - 1) %>% 
  arrange(desc(Diff)) %>%
  filter(Diff > .2)

# filter the label data frames to only include those cities where the
# difference exceeds 20%
right_label <- filter(right_label, Partner %in% big_diff$Partner)
left_label <- filter(left_label, Partner %in% big_diff$Partner)

highlight <- filter(df, Partner %in% big_diff$Partner)

plot_label <- big_diff %>%
  select(Partner, Value = Max, Diff) %>%
  right_join(right_label)

ggplot(df, aes(Value, Partner)) + 
  geom_point(aes(color = Reporter)) +
  geom_line(aes(group = Partner)) +
  #geom_text(data = right_label, aes(color = Reporter, label = round(Value, 0)), size = 3, hjust = -.5) +
  #geom_text(data = left_label, aes(color = Reporter, label = round(Value, 0)), size = 3, hjust = 1.5) +  
  geom_text(data = plot_label, aes(color = Reporter, label = paste0("+", scales::percent(round(Diff, 2)))),size = 3, hjust = -.5) +
  #scale_x_continuous(limits = c(-500, 15000)) +
  scale_x_continuous(labels = scales::dollar, expand = c(0.02, 0), limits = c(0, 4000),breaks = seq(0, 4000, by = 1000)) +
  theme_minimal()

#-------------------------------------------------------------------------------


uae <- read.csv("./uae_imports_wrong_codes.csv", sep = ",", row.names=NULL)

uae <- uae[c("refMonth", "reporterISO", "flowCode", "partnerISO", 
             "isOriginalClassification", "qtyUnitAbbr", "isAltQtyEstimated", "cifvalue", "fobvalue")]
str(uae)


uae$commodity_price <- uae$fobvalue/uae$isAltQtyEstimated

uae <- uae[uae$isOriginalClassification != 710812,]
