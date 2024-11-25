library(ggplot2)
library(comtradr)
library(tidyverse)
library(lubridate)

library(RColorBrewer)
library(wesanderson)

options(scipen = 999)

# Read gold prices data
gold_df <- read.csv('./gold_prices_monthly.csv')
gold_df$date <- as.Date(gold_df$date, format = '%m/%d/%Y')
gold_df$date <- format(gold_df$date,"%Y-%m-01")
gold_df$priceUSD_troyounce <- as.numeric(gsub(",", "", gold_df$priceUSD_troyounce))*32.1507

# Read multiple data files and combine
file_list <- list.files(path="./data/", full.names = TRUE)
ldf <- lapply(file_list, read.csv, row.names = NULL)
df <- do.call("rbind", ldf)

rm(ldf, file_list)

colnames(df) <- colnames(df)[2:ncol(df)]
df <- df[1:(ncol(df)-1)]

df <- df[c("refYear", "refMonth", "reporterDesc", "partnerDesc", "cmdCode", "qtyUnitAbbr", 
           "qty", "altQtyUnitAbbr", "altQty", "primaryValue")]

df[df$partnerDesc == "T\xfcrkiye",]$partnerDesc <- 'Turkey'

#-------------------------------------------------------------------------------


# Additional task 4
# the sum of the HS6 values matches with HS4 values ie; there is no instance of
# traded reported in HS4 that is not reported in HS6
singleC <- df[df$reporterDesc == 'South Africa',]

sum(singleC[singleC$partnerDesc != 'World' & singleC$cmdCode %in% c(710811, 710812, 710813, 710820),]$primaryValue)
sum(singleC[singleC$partnerDesc == 'World' & singleC$cmdCode == 7108,]$primaryValue)

#-------------------------------------------------------------------------------

# Compare monthly and yearly data
# they don't match
tdf <- read.csv('./cleaned_data.csv')

r1 <- df[df$cmdCode == 7108 & df$partnerDesc != 'World',] %>% 
  group_by(reporterDesc, partnerDesc) %>% summarise(total_pvalue = sum(primaryValue), total_qty = sum(altQty))

r2 <- tdf[tdf$cmdCode == 7108 & tdf$partnerDesc != 'World',] %>% 
  group_by(reporterDesc, partnerDesc) %>% summarise(total_pvalue = sum(primaryValue), total_qty = sum(altQty))

length(unique(r2$total_pvalue[! r2$total_pvalue %in% r1$total_pvalue]))
length(unique(r2$total_qty[! r2$total_qty %in% r1$total_qty]))
unique(r2$partnerDesc[! r2$partnerDesc %in% r1$partnerDesc])

#-------------------------------------------------------------------------------

# brics west
brics <- c("China, Hong Kong SAR", "China", "South Africa", "India", "Russia", "Brazil")

west <- c("USA", "United Kingdom", "Spain", "France", "Germany", "Turkey", "Italy", "Japan",
          "Rep. of Korea", "Switzerland", "Canada", "Cyprus", "Slovenia")

df$bricsflag <- NA
df[df$partnerDesc %in% brics,]$bricsflag <- 'b'
df[df$partnerDesc %in% west,]$bricsflag <- 'w'

# handle date column and subset the gold prices df for the years 2017-2023
df$date <- as.Date(paste('01', df$refMonth, df$refYear, sep = '/'), format = "%d/%m/%Y")
gold_df <- gold_df[gold_df$date >= min(df$date) & gold_df$date <= max(df$date),]

df$refYear <- NULL
df$refMonth <- NULL

df$gold_price <- sapply(df$date, function(x) gold_df[gold_df$date == x,]$priceUSD_troyounce)

#**
df <- df[df$partnerDesc != 'World',]
#**

#-------------------------------------------------------------------------------
# 1st attempt at transformation

df %>% select(qty, altQty, primaryValue)

df$qtyTransform <-df$altQty/df$qty

df[!(df$qtyTransform %in% c(1, NA, Inf, NaN, 1000)),] %>% select(qty, altQty, qtyTransform, primaryValue)

df[df$qtyTransform != 1 & !is.na(df$qtyTransform) & df$qtyTransform != Inf & !is.nan(df$qtyTransform) & 
     (df$qtyTransform < 900 | df$qtyTransform > 1200) & df$partnerDesc != 'World',] %>% 
  select(reporterDesc, partnerDesc, qty, altQty, qtyTransform, primaryValue, gold_price)

#-------------------------------------------------------------------------------
# 2nd attempt at transformation
#Make unit price column
df$unit_price <- df$primaryValue/df$altQty
df[df$unit_price == Inf,]$unit_price <- NA

#handle quantity columns, ie; transform them to proper units

temp <- data.frame(do.call(rbind, strsplit(as.character(df$unit_price), '\\.')))
temp$altQtyUnitAbbr <- df$altQtyUnitAbbr

#*handle grams*
#df[which(nchar(temp$X1) == 2),]
df[which(nchar(temp$X1) == 2 & temp$altQtyUnitAbbr == 'g'),]$altQty <- df[which(nchar(temp$X1) == 2 & temp$altQtyUnitAbbr == 'g'),]$altQty/1000
df[which(nchar(temp$X1) == 2 & temp$altQtyUnitAbbr == 'g'),]$unit_price <- df[which(nchar(temp$X1) == 2 & temp$altQtyUnitAbbr == 'g'),]$unit_price*1000

#*handle milligrams*
df[which(temp$X1 == '0' & substr(temp$X2, start = 1, stop = 1) == '0'),]$altQty <-df[which(temp$X1 == '0' & substr(temp$X2, start = 1, stop = 1) == '0'),]$altQty/1000000
df[which(temp$X1 == '0' & substr(temp$X2, start = 1, stop = 1) == '0'),]$unit_price <- df[which(temp$X1 == '0' & substr(temp$X2, start = 1, stop = 1) == '0'),]$unit_price*1000000

#*convert grams to kg*
#df[df$altQtyUnitAbbr == 'g',]$altQty <- df[df$altQtyUnitAbbr == 'g',]$altQty/1000
#df$unit_price <- df$primaryValue/df$altQty

# add the monthly gold prices to the dataset and find the difference between gold spot price and unit price
df$gdiff <- df$gold_price - df$unit_price

#-------------------------------------------------------------------------------

#Make map graph highlighting gold trade everywhere
#make a map for africa
#make a map hightlighting our two region

#-------------------------------------------------------------------------------
# Task 1 - Issues with altQty, Qty columns.
# Graph 2


cmdcode_unitprice_plot = function(df, Reporter){

  plot_df <- df[df$reporterDesc == Reporter & df$cmdCode != '7108',]
  

  plot_df$ctyGroup <- NA
  africa <- c('Cameroon', 'Dem. Rep. of the Congo', 'Nigeria', 'South Africa', 'Ethiopia', 'Eswatini')
  ME <- c('United Arab Emirates', 'Qatar')
  SA <- c('Paraguay')
  
  if(Reporter == 'Kenya'){
    plot_df[plot_df$partnerDesc %in% africa,]$ctyGroup <- 'Africa'
    plot_df[plot_df$partnerDesc %in% west,]$ctyGroup <- 'West'
    plot_df[plot_df$partnerDesc %in% c('China', 'China, Hong Kong SAR'),]$ctyGroup <- 'China'
    plot_df[plot_df$partnerDesc == 'Dem. People\'s Rep. of Korea',]$ctyGroup <- 'NK'
    plot_df[plot_df$partnerDesc %in% ME,]$ctyGroup <- 'ME'
    plot_df[plot_df$partnerDesc %in% SA,]$ctyGroup <- 'SA'
  }

  cl <- length(unique(plot_df$partnerDesc))
  
  g <- ggplot(data = plot_df, aes(x = date)) + 
    geom_point(aes(y = gold_price), color = 'gold', size=3) + 
    geom_line(aes(y = gold_price), color = 'gold') +
    geom_point(aes(y = unit_price, color = !!ifelse(Reporter == 'Kenya', sym("ctyGroup"), sym("partnerDesc"))), size=3) + 
    scale_y_continuous(limits = c(0, 75000)) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle=45, hjust = 1)) 
  
  if(cl <= 5){
    g = g + scale_color_manual(values=wes_palette(n=length(unique(plot_df$partnerDesc)), name="Darjeeling1"))
  }else{
    g = g + scale_colour_brewer(palette = "Paired") 
  }
  
  return(list(g, plot_df))
}

Reporter <- 'Kenya'

plot_stats = cmdcode_unitprice_plot(df, Reporter)

plot_stats[[1]]

hist(df$unit_price)
histprice <- hist(df$unit_price)

data.frame(sapply(1:length(histprice$breaks)-1, function(x) paste(histprice$breaks[x], '-', histprice$breaks[x+1]))[-1], histprice$counts)

#-------------------------------------------------------------------------------

# check if trends in country group price is similar to gold price
# https://stackoverflow.com/questions/60796065/comparing-two-curves-for-difference-in-trend

plot_df <- plot_stats[[2]]

ttestDF <- plot_df[plot_df$ctyGroup == 'West',] %>% select(unit_price, gold_price, date)
ttestDF <- ttestDF[ttestDF$unit_price > 10000 & ttestDF$unit_price < 70000,]
ttestDF <- ttestDF %>% group_by(date) %>% summarise(unit_price = mean(unit_price), gold_price = mean(gold_price)) 

expected <- ttestDF$gold_price
observed <- ttestDF$unit_price
time <- ttestDF$date

wide <- data.frame(expected, observed)
long <- cbind(time, stack(wide))

fm2 <- lm(values ~ ind/(time + 1) + 0, long)
fm1 <- lm(values ~ ind + time + 0, long)
anova(fm1, fm2)

# Then run a linear model with two slopes and and another with one slope. Both have two intercepts. 
# Then compare them using anova. Evidently they are not significantly different so we cannot reject the hypothesis 
# that the slopes are the same.

# https://www.r-bloggers.com/2021/11/granger-causality-test-in-r-with-example/
library(lmtest)

tsData <- data.frame(observed, expected, time)
ggplot(data = tsData, aes(x = time)) +
  geom_line(aes(y=expected), color = 'darkred') +
  geom_line(aes(y=observed), color = 'steelblue')

grangertest(observed ~ expected, order = 3, data = tsData)

# We may reject the null hypothesis of the test because the p-value is smaller than 0.05, 
# and infer that knowing the values of SMI is valuable for forecasting the future values of DAX.

grangertest(expected ~ observed, order = 3, data = tsData)


#-------------------------------------------------------------------------------
# Graph 1

plot_df <- df[df$partnerDesc %in% brics & df$reporterDesc == "Kenya" & df$cmdCode == 7108,]

plot_df <- plot_df %>% group_by(date) %>% summarise(sum = sum(primaryValue))

plot_df$gold_price <- NA

plot_df <- plot_df %>%
  complete(date = seq(date[1], date[nrow(plot_df)], by = "1 month"), fill = list(sum = 0, gold_price = 0))

plot_df$gold_price <- sapply(plot_df$date, function(x) gold_df[gold_df$date == x,]$priceUSD_troyounce)

library(scales)

yrng <- range(plot_df$sum)
xrng <- range(plot_df$date)
caption <- paste("Kenya gold export to BRICS\n", "    2017 - 2023")

from <- as.Date('2020-01-01')
to <- as.Date('2022-01-01')

shade <- data.frame(from, to)

breaks.vec <- seq(min(plot_df$date), max(plot_df$date), by = "3 month")
scaleVal <- 1/90
gpriceColor <- rgb(0.8, 0.6, 0, 0.6)

ggplot(data = plot_df, aes(x = date, y = sum)) + 
  geom_bar(stat = "identity", fill="skyblue") +
  geom_point(aes(y = gold_price/scaleVal), size=2, color=gpriceColor) +
  geom_line(aes(y = gold_price/scaleVal), size=1.5, color=gpriceColor) + 
  #scale_x_date(expand = c(0.01, 0.01), date_labels = "%b-%Y", date_breaks = "3 month") +
  scale_x_date(expand = c(0.01,0.01), breaks = breaks.vec, date_labels = "%b-%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 7000000), breaks = seq(0, 6000000, by = 1000000), 
                     labels = scales::label_dollar(scale_cut = scales::cut_short_scale()),
                     # https://finchstudio.io/blog/ggplot-dual-y-axes/
                     sec.axis = sec_axis(~.*scaleVal, name="Gold Spot Price (USD/KG)")
                     ) +
  annotate(geom = "text", x = xrng[1], y = yrng[2], label = caption, hjust = 0, vjust = 1, size = 8) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        axis.title.y = element_text(color = 'skyblue', size = 13),
        axis.title.y.right = element_text(color = gpriceColor, size = 13)) + 
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  geom_rect(data = shade, inherit.aes=FALSE, aes(xmin = from, xmax = to, ymin = -Inf, ymax = 6000000),
            color="transparent", fill="orange", alpha=0.2) +
  annotate(geom = "curve", x = as.Date('2021-08-01'), y = 6300000, xend = as.Date('2020-11-01'), yend = 6050000, 
           curvature = .3, arrow = arrow(length = unit(5, "mm"))) +
  annotate(geom = "text", x = as.Date('2021-09-01'), y = 6300000, label = "COVID-19 Period", hjust = "left") +
  xlab("Date") + ylab("Gold Export Value (USD)")
  
#-------------------------------------------------------------------------------
# Graph 3 

plot_df <- df[df$bricsflag %in% c('b', 'w') & df$cmdCode == 7108,]

plot_df <- plot_df %>% group_by(bricsflag, date) %>% summarise(sum = sum(primaryValue))
breaks.vec <- seq(min(plot_df$date), max(plot_df$date), by = "3 month")

# Barplots too spread out (high range), solution: 
# https://stackoverflow.com/questions/65669385/plotly-r-how-to-make-a-gapped-y-axis/65766833#65766833
ggplot(plot_df, aes(fill=bricsflag, y=sum, x=date)) + 
  geom_bar(position=position_dodge(preserve = "single"), stat="identity") +
  scale_x_date(expand = c(0.01,0.01), breaks = breaks.vec, date_labels = "%b-%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 7000000), breaks = seq(0, 6000000, by = 1000000), 
                     labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) 

# Graph 3.1 - with Y break for better readability
library(patchwork)
# https://stackoverflow.com/questions/7194688/using-ggplot2-can-i-insert-a-break-in-the-axis
## whole figure
ggplot(data=plot_df,aes(date,sum,fill=bricsflag))+
  geom_bar(stat = "identity", position=position_dodge(preserve = "single"))+
  theme_classic()

## first fig
p1=ggplot(data=plot_df,aes(date,sum,fill=bricsflag))+
  geom_bar(stat = "identity", position=position_dodge(preserve = "single"))+
  coord_cartesian(ylim = c(0,2900000))+ #setting the down part
  theme_classic()

## second fig
p2=ggplot(data=plot_df,aes(date,sum,fill=bricsflag))+
  geom_bar(stat = "identity", position=position_dodge(preserve = "single"))+
  coord_cartesian(ylim = c(4150000,6300000))+ #setting the upper part
  scale_y_continuous(breaks = c(4150000, 5000000, 6000000))+
  guides(x = "none")+ # remove x line
  labs(title = "Graph with broken y axis")+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank(),
        legend.position = "none", # remove legend
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

## patchwork
## bind fig1+fig2
p2/p1


#-------------------------------------------------------------------------------
# export and import differences

export <- read.csv('./export.csv', row.names = NULL)
colnames(export) <- colnames(export)[2:ncol(export)]
export <- export[1:(ncol(export)-1)]

import <- read.csv('./import.csv', row.names = NULL)
colnames(import) <- colnames(import)[2:ncol(import)]
import <- import[1:(ncol(import)-1)]

export <- export %>% group_by(partnerDesc) %>% summarise(totVal = sum(primaryValue))
import <- import %>% group_by(reporterDesc) %>% summarise(totVal = sum(primaryValue))

colnames(import) <- c('partnerDesc', 'totVal')

export[export$partnerDesc == "T\xfcrkiye",]$partnerDesc <- 'Turkey'
import[import$partnerDesc == "T\xfcrkiye",]$partnerDesc <- 'Turkey'

export <- export[export$partnerDesc %in% unique(import$partnerDesc),]
import <- import[import$partnerDesc %in% unique(export$partnerDesc),]

wide <- data.frame(export$totVal, import$totVal)
long <- cbind(export$partnerDesc, stack(wide))
colnames(long) <- c('Country', 'Value', 'Flag')
long$Value <- long$Value/1000000

long <- long[long$Country != 'United Arab Emirates',]
#long$Value <- log(long$Value/1000)


ggplot(long, aes(Value, Country)) + 
  geom_point(aes(color = Flag)) 

