#setting up packages
library('tidyverse') 
library('leaflet')
library('ggmap')
library('GGally')
library('viridis')
library('plotly')
library('IRdisplay')
library('ggrepel')
library('cowplot')
library('jtools')
library('car')
library('MASS')

options(warn = -1)

#loading data
data <- read.csv("/Users/mattdolan/Documents/DatasetsR/Github Project/efw_cc.csv")

#previewing data
head(data)
summary(data)

#plotting missing values
options(repr.plot.width=6, repr.plot.height=6)
missing_data <- data %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +xlab('variables')+
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+coord_flip()+ theme_bw()

#
options(repr.plot.width=8, repr.plot.height=6)

#factorising variables for analysis
data$year <- factor(data$year)
data$countries <- factor(data$countries)

#Renaming column to economic freedom scores
colnames(data)[4] <- "Economic_Freedom"

#plotting all countries economic freedom trends over time 
options(repr.plot.width=8, repr.plot.height=10)

data %>% filter(!is.na(Economic_Freedom) | !is.na(year)) %>% group_by(year) %>%
  ggplot(aes(year,Economic_Freedom, fill = Economic_Freedom, group = 1))+
  geom_line(aes(color = Economic_Freedom))+
  scale_color_viridis(option = "plasma",direction = -1, discrete=FALSE) +
  facet_wrap(~countries)+theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank(), 
        strip.text.x = element_text(size = 6))+
  xlab(" ") + ylab("")+ ggtitle("ECONOMIC FREEDOM OF THE WORLD 1970-2016")

#slicing data for year 2016
data1 <- data %>% filter(year == 2016)
head(data1)

#plotting 2016 data for cross-country analysis using quartiles
a1 <- ggplotly(ggplot(data1, aes(quartile,Economic_Freedom , size = -rank)) + 
                 geom_jitter(aes(color=countries, alpha=0.5)) +
                 theme_bw()+ theme(legend.position= "none")+
                 xlab("Quartile") + 
                 ggtitle("Economic Freedom Index 2016"), tooltip = c("countries"))

htmlwidgets::saveWidget(a1, "a1.html")
display_html('<iframe src="a1.html" width=100% height=450></iframe>')

#spatial view of countries according to economic freedom scores
l <- list(color = toRGB("black"), width = 0.5)

g <- list(showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = 'Mercator'))

p1 <- plot_geo(data1) %>%
  add_trace(z = ~Economic_Freedom, color = ~Economic_Freedom, colors = 'RdYlBu',
            text = ~data1$countries, locations = ~data1$ISO_code, marker = list(line = l)) %>%
  colorbar(title = 'Countries' , tickprefix = 'EF') %>%
  layout(title = 'Economic Freedom 2016')

htmlwidgets::saveWidget(p1, "p1.html")
display_html('<iframe src="p1.html" width=100% height=450></iframe>')

#ranking countries using a spatial view - based on 2016 economic freedom scores
l <- list(color = toRGB("black"), width = 0.5)

g <- list(showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'Mercator'))

p2 <- plot_geo(data1) %>%
  add_trace(z = ~rank, color = ~rank, colors = 'Blues',
            text = ~data1$countries, locations = ~data1$ISO_code, marker = list(line = l)) %>%
  colorbar(title = 'Countries',tickprefix = 'Rank') %>%
  layout(title = 'Rank 2016' )

htmlwidgets::saveWidget(p2, "p2.html")
display_html('<iframe src="p2.html" width=100% height=450></iframe>')


#spatial view using quartiles to rank countries according to 2016 economic freedom scores
l <- list(color = toRGB("black"), width = 0.5)

g <- list(showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'Mercator'))

p3 <- plot_geo(data1) %>%
  add_trace(z = ~quartile, color = ~quartile, colors = 'Greens',
            text = ~data1$countries, locations = ~data1$ISO_code, marker = list(line = l)) %>%
  colorbar(title = 'Countries', tickprefix = 'Quartile') %>%
  layout(title = 'Quartile 2016' )

htmlwidgets::saveWidget(p3, "p3.html")
display_html('<iframe src="p3.html" width=100% height=450></iframe>')





