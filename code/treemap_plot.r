library(ggplot2)
library(comtradr)
library(dplyr)
library(tidyverse)
library(stringi)
library(stringr)
library(treemap)

#Price of gold in Jan 4th 2023 is 59,638 USD.
#Price of gold in June 12th 2023 is 63,018 USD.
#Price of gold in Dec 26th 2023 is 66,429 USD.


options(scipen = 999)

#set_primary_comtrade_key(key = "xxxxxc678ca4dbxxxxxxxx8285r3")
HS_codes <- comtradr::ct_get_ref_table('HS')
#cc <- country_codes
#ct_get_data(commodity_code = 7108, reporter = "GHA", partner = "ARE", start_date = "2023-01", 
#            end_date = "2023-12", flow_direction = "export")


usadf <- read.csv("./US_exportsto_China_2022.csv", sep = ",", row.names=NULL)


colnames(usadf)
usadf <- usadf[c("reporterISO", "flowCode", "partnerISO", "isOriginalClassification", "cmdCode", "qtyUnitCode",
                 "qtyUnitAbbr", "isAltQtyEstimated", "cifvalue", "fobvalue")]


usadf$commodity_price <- usadf$fobvalue/usadf$isAltQtyEstimated

colnames(usadf)[which(names(usadf) == "qtyUnitAbbr")] <- "units"
colnames(usadf)[which(names(usadf) == "isAltQtyEstimated")] <- "net_unit"

str(HS_codes)
str(usadf)

d1 <- usadf[which(nchar(usadf$isOriginalClassification) == "3"),]$isOriginalClassification

usadf[which(nchar(usadf$isOriginalClassification) == "3"),]$isOriginalClassification <- gsub("^([0-9]{1,3})", "0\\1",d1)

usadf$parent <- HS_codes$parent[match(usadf$isOriginalClassification, HS_codes$id)]


us_oec <- read.csv('./US_exportsto_ChinaOEC_2022.csv')

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

us_oec$HS4.ID <- substrRight(us_oec$HS4.ID, 4)


usadf$section_id <- us_oec$Section.ID[match(usadf$isOriginalClassification, us_oec$HS4.ID)]

sum(is.na(usadf$section_id))

#fill in missing section ids based on similar parent from HS Codes df
for(i in which(is.na(usadf$section_id))){
  parent <- usadf[i,]$parent
  usadf[i,]$section_id <- names(sort(table(usadf[usadf$parent == parent,]$section_id),decreasing=TRUE)[1])
}

usadf <- usadf[!(usadf$isOriginalClassification %in% c("9999")),]



usadf$HS4 <- us_oec$HS4[match(usadf$isOriginalClassification, us_oec$HS4.ID)]

usadf$perc <- round(100*(usadf$fobvalue/sum(usadf$fobvalue)),3)

 

usadf$Section <- us_oec$Section[match(usadf$section_id, us_oec$Section.ID)]

# https://r-graph-gallery.com/web-treemap-with-annotations-labels-and-colors.html

treemap(usadf, index=c("Section","HS4"), 
        vSize="fobvalue", 
        type="index",
        align.labels = list(
          c("left", "top"),
          c("left", "bottom")),
        border.col = c("white", "white"),
        inflate.labels = F,
        bg.labels = c("transparent"),
        overlap.labels = 0,
        fontcolor.labels = c("white", "white")
        )

library(plotly)

# https://stackoverflow.com/questions/74173853/treemaps-with-plotly-blank-screen
# https://stackoverflow.com/questions/68587628/r-plotly-hovertemplate-with-conditional-content

df <- usadf[,c("fobvalue", "Section", "HS4", "isOriginalClassification")] %>% 
  mutate(ids = ifelse(Section == "", HS4, paste0(HS4, "-", Section))) %>% 
  select(ids, everything())

par_info <- df %>% group_by(Section) %>%  # group by parent
  summarise(fobvalue = sum(fobvalue)) %>%  # parent total
  rename(HS4 = Section) %>%            # parent labels for the item field
  mutate(Section = "", ids = HS4, isOriginalClassification = "") %>%  # add missing fields for my_data
  select(names(df))


df2 <- rbind(df, par_info)

fig1 <-  plot_ly(
            data = df2, branchvalues = "total",
            type = "treemap", labels = ~HS4,
            parents = ~Section, values = ~fobvalue, ids = ~ids,
            textinfo="label+value+percent parent+percent",
            text = ~HS4,
            hovertemplate = paste('<b>',df2$HS4,'</b>','\n',
                                  '<b>HS4 ID:</b>', df2$isOriginalClassification,'\n',
                                  '<b>Trade Value:</b>', scales::label_number_si(accuracy=0.1)(df2$fobvalue),'\n',
                                  '<b>Year:</b> 2022',  '<extra></extra>')) %>%
  layout(title = paste('<B>US exports to China (2022)</B>\n', '<B>Total:</B> $', 
                       scales::label_number_si(accuracy=0.1)(sum(df2$fobvalue)/2)), plot_bgcolor = "#e5ecf6")

fig1
