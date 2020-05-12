library(dplyr)
library(ggplot2)
library(pander)
library(ggcorrplot)
library(factoextra)
library("Hmisc")
library(corrplot)
library("PerformanceAnalytics")

##Stage 1
#Merge all the datasets to get the covariance and correlation
#All the datasets are merged on the 'Year' as primary key
# Merged Financial Market data and CPI
Merged_01 <- merge(Financial_Market_Statistics,CPI,by='Year')
#Select only required variables
Merged_01 <- Merged_01 %>%
  select(Year,Values_per_year,CPI_Value)
#Selected data from Foreign Investment dataset
Foreign_temp <- Foreign_Investment %>%
  select(Year,Investment_Millions)
Merged_02 <- merge(Foreign_temp,Merged_01,by='Year')
#Selected data from Manufacturing dataset
Manufacturing_temp <- Manufacturing %>%
  select(Year,Inventories_Total_Millions)
Merged_03 <- merge(Manufacturing_temp,Merged_02,by='Year')
#Selected data from Merchandise dataset
Merchandise_Temp <- Merchandise_Trade %>%
  select(Year,ExIm_Value_Millions)
Merged_04 <- merge(Merchandise_Temp,Merged_03,by='Year')
#Selected data from Multipurpose dataset
Multi_temp <- Test_year%>%
  select(Year,'Grand Total')
Merged_05 <- merge(Multi_temp,Merged_04,by='Year')
#Selected data from Wages dataset
Wage_temp <- Wages%>%
  select(Year,Total_Wages_in_Millions)
Merged_06 <- merge(Wage_temp,Merged_05,by='Year')
#Selected data from Asset dataset
Asset_temp <- Asset_01%>%
  select(Year,Asset_Value_Million)
Merged_07 <- merge(Asset_temp,Merged_06,by='Year')
#Selected data from Wages dataset
Immigration_temp <- Immigration_01%>%
  select(Year,Values_in_Million)
Final_merged_set<- merge(Immigration_temp,Merged_07,by='Year')

##Part A: Training Model

Training_set_01 <- Final_merged_set %>%
  filter(Year != 2018)
res <- rcorr(as.matrix(Training_set_01[2:9]), type = c("pearson","spearman"))

# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(res$r, res$P)
res2 <- cor(as.matrix(Training_set_01[2:9]))
corrplot(res2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

Performance_analysis <- Training_set_01[2:9]
chart.Correlation(Performance_analysis, histogram=TRUE, pch=19)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res2, col = col, symm = TRUE)

#Box Plot

Asset_Value_Temp %>%
  filter(Year != 2019,Year != 2020) %>%
  ggplot(aes(group = Year,y = Asset_VALUE_in_millions)) +
  geom_boxplot() +
  labs(title = "Distribution of Values to Year",
       x = "Year",
       y = "Asset_Value_Million") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(7.5,35,2.5)) 

Consumer_Price_Index %>%
  filter(Year != 2019,Year != 2020) %>%
  ggplot(aes(group = Year,y = Product_Value)) +
  geom_boxplot() +
  labs(title = "Distribution of Values to Year",
       x = "Year",
       y = "Product_Value_Million")+
  theme_minimal() +
  scale_y_continuous(breaks = seq(7.5,35,2.5)) 

Immigrant_Tax_File %>%
  filter(REF_DATE != 2019,REF_DATE != 2020) %>%
  ggplot(aes(group = REF_DATE,y = VALUE)) +
  geom_boxplot() +
  labs(title = "Distribution of Values to Year",
       x = "Year",
       y = "Product_Value_Million")+
  theme_minimal() +
  scale_y_continuous(breaks = seq(7.5,35,2.5)) 



