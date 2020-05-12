library(dplyr)
library(ggplot2)
library(pander)
library(ggcorrplot)
library(factoextra)
Sectorwise_income <- Data_1 %>%
  select(REF_DATE,NAICS,Value_Million)
summary(Sectorwise_income)
str(Sectorwise_income)
nrow(Sectorwise_income)

Sectorwise_income <- Sectorwise_income %>%
  filter(!is.na(Value_Million))
##Non-Graphical Univariate Analysis
#Summarize data
Sectorwise_income %>%
  summarize(variable = "Value_Million", mean_value = mean(Value_Million), st_dev_value = sd(Value_Million))

#Summarize data with probability
Sectorwise_income %>%
  summarize(variable = "Value_Million",
            q0.2 = quantile(Value_Million, 0.2),
            q0.4 = quantile(Value_Million, 0.4),
            q0.6 = quantile(Value_Million, 0.6),
            q0.8 = quantile(Value_Million, 0.8))

##Bivariate Analysis
Sectorwise_income_sum <- Sectorwise_income  %>%
  group_by(REF_DATE) %>%
  summarize(mean_value = mean(Value_Million), st_dev_value = sd(Value_Million))

Sectorwise_income_sum %>%
  ggplot(aes(REF_DATE,mean_value)) +
  geom_boxplot() +
  labs(title = "Distribution of cty relative to drv",
       x = "drv",
       y = "cty") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(7.5,35,2.5)) 

##Univariate analysis of a categorical variable

Sectorwise_income %>%
  group_by(NAICS) %>%
  summarize(Value_Million = n())

Sectorwise_income_2 <- Sectorwise_income %>%
  group_by(NAICS) %>%
  summarize(Value_Million = n()) %>%
  arrange(desc(Value_Million)) %>%
  mutate(relative_frequency = Value_Million/sum(Value_Million),
         relative_cumulative_frequency = cumsum(Value_Million),
         relative_frequency = round(100*relative_frequency,2),
         relative_cumulative_frequency = round(100*relative_cumulative_frequency,2),
         nr = row_number(-Value_Million)) %>%
  select(nr, everything())
print(Sectorwise_income_2,n=nrow(Sectorwise_income_2))
 
##Bivariate analysis of a continuous variable with respect to another continuous variable

Sectorwise_income %>%
  ggplot(aes(REF_DATE, Value_Million)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relationship between Year and Value")

Sectorwise_income %>%
  ggplot(aes(REF_DATE, Value_Million)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relationship between Year and Value") +
  geom_smooth(method = "lm", se = F)

Sectorwise_income %>%
  select(REF_DATE,Value_Million) %>% 
  cor


Sector_test <- Sectorwise_income %>%
  select_if(is.numeric) %>%
  cor %>% 
  ggcorrplot()


test %>%
  select_if(is.numeric) %>%
  cor %>% 
  ggcorrplot(type = "lower", ggtheme = theme_minimal, colors = c("#6D9EC1","white","#E46726"),
             show.diag = T,
             lab = T, lab_size = 5,
             title = "Correlation Matrix for the Sector dataset",
             legend.title = "Correlation Value",
             outline.color = "white",
             hc.order = T)


rownames(data_sector_2) <- data_sector_2$NAICS
data_sector_2$NAICS <- NULL

data_sector_2.active <- data_sector_2[1:21, 1:11]
head(data_sector_2.active[, 1:11])

library("FactoMineR")
res.pca <- PCA(data_sector_2.active, graph = FALSE)
eig.val <- get_eigenvalue(res.pca)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))


fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 


ind.sup <- test[, 1:10]
ind.sup[, 1:6]
