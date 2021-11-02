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
library('caTools')
library('dplyr')
library('gather')

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

#augmenting data for regression analysis
data2 <- data1
colnames(data2) <- paste("Sub", colnames(data), sep = "_")
colnames(data2)[1] <- "year"
colnames(data2)[2] <- "ISO_codes"
colnames(data2)[3] <- "countries"
colnames(data2)[4] <- "Economic_Freedom"
colnames(data2)[5] <- "rank"
colnames(data2)[6] <- "quartile"

#sense checking data to ensure in correct shape for regression 
head(data2)

#removing year, countries, ISO codes, quartile and rank and removing missing values
data_model <- data2[, -c(1,2,3,5,6)]
data_model <- na.omit(data_model)

#sense checking new data set
str(data_model)
head(data_model)

#splitting data set into test/train
set.seed(100)
indices = sample.split(data_model$Economic_Freedom, SplitRatio = 0.7)
train = data_model[indices,]
test = data_model[!(indices),]

#both the multiple R-squared and adjusted R-squared are 1, so it's evident that multicollinear variables are present in the data
model_1 <-lm(Economic_Freedom~.,data=train)
summary(model_1)

#Step AIC function used to remove the variables
step <- stepAIC(model_1, direction="both")
step

#multicollinear variables  still exist in our data based on our r squared values. therefore we will use VIF(variance inflation factor) to get rid of redundant predictors/variables with  high multicollinearity dependencies
model_2 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
                Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
                Sub_2a_judicial_independence + Sub_2_property_rights + Sub_3a_money_growth + 
                Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
                Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
                Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
                Sub_5b_labor_market_reg + Sub_5c_business_reg, data = train)

summary(model_2)

#property_rights has high vif, so in the next model property_rights will be removed
sort(vif(model_2))

#rerunning model without property_rights based on vif results
model_3 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
                Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
                Sub_2a_judicial_independence + Sub_3a_money_growth + 
                Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
                Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
                Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
                Sub_5b_labor_market_reg + Sub_5c_business_reg, data = train)

summary(model_3)

#the Sub_5c_business_reg evidences a high VIF score, meaning it will be removed in the next model
sort(vif(model_3))

#re-running the model without Sub_5c_business_reg due to high VIF scored evidence
model_4 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
                Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
                Sub_2a_judicial_independence + Sub_3a_money_growth + 
                Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
                Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
                Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
                Sub_5b_labor_market_reg, data = train)

summary(model_4)


# Sub_4b_regulatory_trade_barriers scored high VIF score so will be removed as a resul in next model
sort(vif(model_4))

#Removing  Sub_4b_regulatory_trade_barriers due to forementioned high VIF score
model_5 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
                Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
                Sub_2a_judicial_independence + Sub_3a_money_growth + 
                Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
                Sub_4a_tariffs + Sub_4c_black_market + 
                Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
                Sub_5b_labor_market_reg, data = train)

summary(model_5)

#Removing  Sub_3b_std_inflation evidences a high VIF score so will be omitted from next model and also removing variables with without significant p value scores
sort(vif(model_5))

#Reunning model without Sub_3b_std_inflation =  allindependent variables that have vif within 2.
model_6 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
                Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
                Sub_2a_judicial_independence + Sub_3a_money_growth + 
                Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
                Sub_4a_tariffs + Sub_4c_black_market + 
                Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
                Sub_5b_labor_market_reg, data = train)

summary(model_6)

sort(vif(model_6))

#Removing  Sub_1b_transfers due to high p-value 
model_7 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
                Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
                Sub_2a_judicial_independence + Sub_3a_money_growth + 
                Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
                Sub_4a_tariffs + Sub_4c_black_market + 
                Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
                Sub_5b_labor_market_reg, data = train)

#Removing  Sub_3a_money_growth due to high p-value 
summary(model_7)


#re-running model with Sub_3a_money_growth due to high p-value 
model_8 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
                Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
                Sub_2a_judicial_independence + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
                Sub_4a_tariffs + Sub_4c_black_market + 
                Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
                Sub_5b_labor_market_reg, data = train)

summary(model_8)

#All independent variables have low p-values, now only 11 predictors are in the picture vs 30 predictors originally.
Predict <- predict(model_8,test[,-1])
test$test_efw <- Predict

# here we test the r square between the model vs.the prediction of economic freedom vs the test data
r <- cor(test$Economic_Freedom,test$test_efw)
rsquared <- cor(test$Economic_Freedom,test$test_efw)^2
rsquared

#plotting the residuals
options(repr.plot.width=6, repr.plot.height=6)
par(mfrow = c(2, 2))
plot(model_8)

#using plot_summs function from  jtools package to compare the standardized coeeficients of predictors. Predictors that have highest estimate of standardized coefficient in the plot below, offer the most analytical value.
plot_summs(model_8, scale = TRUE, plot.distributions = TRUE)


