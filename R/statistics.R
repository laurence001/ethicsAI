library(highcharter)
library(tidyverse)
library(datapasta)

data <- data.frame(
  stringsAsFactors = TRUE,
      Organisation = c("Press Agency",
                       "Public Broadcaster","Press/Media Council","Public Broadcaster",
                       "Press/Media Council","Public Broadcaster","Press Agency",
                       "Press Agency","News Media","Professional Organisation",
                       "Professional Organisation","Press Group",
                       "Press/Media Council","News Media","News Media","News Media",
                       "News Media","News Media","News Media",
                       "Press/Media Council","News Media","Press Agency","Public Broadcaster",
                       "News Media","Press/Media Council","Press Agency",
                       "Press Group","Press Agency","Public Broadcaster","News Media",
                       "News Media"),
              Year = c("2016","2019","2019","2020",
                       "2021","2021","Undated","Undated","2023","2023",
                       "2023","2023","2023","2023","2023","2023","2023",
                       "2023","2023","2023","2023","2023","2023","2023",
                       "2023","2023","2023","2023","2023","2023","2023"),
              Text = c("Guidelines","Principles",
                       "Recommendation","Guidelines","Recommendation",
                       "Guidelines","Principles","Principles","Position","Position",
                       "Position","Guidelines","Recommendation","Guidelines",
                       "Guidelines","Guidelines","Guidelines","Principles",
                       "Charter","Ethical Code","Guidelines","Principles",
                       "Guidelines","Position","Ethical Code","Guidelines",
                       "Position","Guidelines","Guidelines","Principles","Guidelines")
)

# Transform data for pivot table-like structure
pivot_data <- data %>%
  group_by(Year, Organisation) %>%
  summarize(Count = n()) %>%
  ungroup()

pivot_data_text <- data %>%
  group_by(Year, Text) %>%
  summarize(Count = n()) %>%
  ungroup()

# Create the Highcharter chart
chart <- hchart(pivot_data, "column", hcaes(x = Year, y = Count, group = Organisation)) %>%
  hc_legend(enabled = TRUE) %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_yAxis(title = list(text = " ")) %>%
  hc_xAxis(title = list(text = " "),categories = levels(pivot_data$Year)) 


greyscale_colors <- c(
 "#000000", "#565656","#787878","#ABABAB","#CDCDCD","#EFEFEF"
)

chart <- chart %>% 
  hc_colors(greyscale_colors) %>%
  hc_labels(enabled = TRUE, format = "{point.y}")

chart

pivot_data_long <- pivot_data %>%
  pivot_longer(cols = c(Organisation),
               names_to = "Organisation",
               values_to = "Count")

greyscale_colors <- c("#747474", "#818181", "#B0B0B0", "#C5C5C5", "#E6E6E7","#F7F7F8")

ggplot(pivot_data, aes(x = Year, y = Count, fill = Organisation)) +
  geom_col(position = "stack") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = greyscale_colors) +
  labs(title = " ",
       x = " ",
       y = " ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom")

summary(data)
