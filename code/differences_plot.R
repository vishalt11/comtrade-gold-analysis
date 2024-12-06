#plot to show export and import discrepancy
library(tidyverse)
library(ggplot2)

selectList <- c('Mali', 'United Rep. of Tanzania', 'Ghana', 'Togo', 'Cameroon', 'Benin', 'Ethiopia',
                'Niger', 'Madagascar', 'Rwanda', 'Mozambique', 'Senegal', 'Burundi')

export <- read.csv('./../data/uae_export.csv', row.names = NULL)
colnames(export) <- colnames(export)[2:ncol(export)]
export <- export[1:(ncol(export)-1)]

import <- read.csv('./../data/uae_import.csv', row.names = NULL)
colnames(import) <- colnames(import)[2:ncol(import)]
import <- import[1:(ncol(import)-1)]


export <- export[export$reporterDesc %in% selectList,]
export <- export %>% select(reporterDesc, primaryValue)

import <- import[import$partnerDesc %in% selectList,]
import <- import %>% select(partnerDesc, primaryValue)
colnames(import) <- c('reporterDesc', 'primaryValue')

export <- export[export$reporterDesc %in% unique(import$reporterDesc),]
import <- import[import$reporterDesc %in% unique(export$reporterDesc),]

wide <- data.frame(export$primaryValue, import$primaryValue)
long <- cbind(export$reporterDesc, stack(wide))
colnames(long) <- c('Country', 'Value', 'Flag')
long$Value <- long$Value/1000000

right_label <- long %>%
  group_by(Country) %>%
  arrange(desc(Value)) %>%
  top_n(1)

left_label <- long %>%
  group_by(Country) %>%
  arrange(desc(Value)) %>%
  slice(2)


# create data frame that identifies revenue differences over 20%
big_diff <- long %>% 
  spread(Flag, Value) %>% 
  group_by(Country) %>% 
  mutate(Max = max(import.primaryValue, export.primaryValue),
         Min = min(import.primaryValue, export.primaryValue),
         Diff = Max - Min) %>% 
  arrange(desc(Diff)) %>%
  filter(Diff > .2)

# filter the label data frames to only include those cities where the
# difference exceeds 20%
right_label <- filter(right_label, Country %in% big_diff$Country)
left_label <- filter(left_label, Country %in% big_diff$Country)

# filter the main data frame to only include those cities where the 
# difference exceeds 20%.
highlight <- filter(long, Country %in% big_diff$Country)

plot_label <- big_diff %>%
  select(Country, Value = Max, Diff) %>%
  right_join(right_label)

p <- ggplot(long, aes(Value, Country)) +
  geom_line(aes(group = Country), alpha = .5, color = 'gold', linewidth = 1.5) +
  geom_point(aes(color = Flag), size = 3, alpha = .5) +
  #geom_line(data = highlight, aes(group = Country)) +
  geom_point(data = highlight, aes(color = Flag), size = 3, pch=21) +
  geom_text(data = plot_label, aes(color = Flag, 
                                   label = round(Diff, 0)),
            size = 3, hjust = -.8)

p + scale_color_manual(labels = c("Import", "Export"),values = c('gold', 'black')) +
  scale_x_continuous(labels = scales::dollar, expand = c(0.02, 0), 
                     limits = c(0, 2500),
                     breaks = seq(0, 2000, by = 500)) +
  scale_y_discrete(expand = c(.02, 0)) +
  labs(title = "Import - Export Discrepancies", subtitle = "for the year 2016") +
  xlab('USD (millions)') +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = c(.3, 1.2),
        legend.background = element_blank(),
        legend.direction="horizontal",
        plot.title = element_text(size = 20, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))


