uk_r <- read.csv("E:\\Master\\ICT_515_Foundation of Data Sci_Jan'23\\Assignment 2 _ Group\\Assignment 2_Dataset\\Online Retail _ Dataset.csv")



# Set working directory 
setwd("E://Master//ICT_515_Foundation of Data Sci_Jan'23//Assignment 2 _ Group")

# Install the necessary Packages 
install.packages("tidyverse")
install.packages('naniar')
install.packages("ggplot2")
install.packages("dlookr")
install.packages("data.table")
install.packages("rfm")
install.packages("DataExplorer")
remove.packages("rlang")
install.packages("rlang")

# Load the packages 
library(tidyverse)
library(naniar)
library(ggplot2)
library(dplyr) 
library(tidyr)
library(rfm)
library(dlookr)
library(DataExplorer)
# 3.2 Data Summary Before Data Cleaning 

summary(uk_r)

glimpse(uk_r)

# 4.----Data Pre-Processing -----

# 4.1 Data Wrangling 

# Check for Missing values

check.missing <- function(x)
{
  return(colSums(is.na(x)))
  
}
check.missing(uk_r)

# % of the missing 

gg_miss_var(uk_r)

gg_miss_var(uk_r, show_pct = TRUE)

# Remove missing values

uk_r1  <- na.omit(uk_r)

# Remove "Unspecified" countries

uk_r1 = uk_r1[uk_r1$Country != "Unspecified",]

# Check Qty that are negative
uk_r1 <- uk_r1 %>% 
  filter(Quantity >= 0,
         UnitPrice >= 1)

# Outlier Diagnosis Plot

plot_outlier(uk_r, Quantity,col = "#85B6C1")

plot_outlier(uk_r, UnitPrice,col = "#85B6C1")

# Remove (Minus) Unit Price & Qty > Goods sold were damaged or return

uk_r1 <- uk_r1 %>% 
  filter(Quantity >= 0,
         UnitPrice >= 1)

# Split "InvoiceDate" to Invoice Date & Time

uk_r1$date <- sapply(uk_r1$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})

uk_r1$time <- sapply(uk_r1$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})


# Create Month, Year 

uk_r1$month <- sapply(uk_r1$date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][2]})

uk_r1$year <- sapply(uk_r1$date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][3]})


uk_r1$date <- as.Date(uk_r1$date, "%m/%d/%Y")

# Convert Country , Invoice No, CustomerID, Description

uk_r1$Country <- as.factor(uk_r1$Country)

uk_r1$InvoiceNo<- as.factor(uk_r1$InvoiceNo)

uk_r1$CustomerID  <- as.factor(uk_r1$CustomerID)

uk_r1$Description<-as.factor(uk_r1$Description)

uk_r1$InvoiceDate  <- as.Date(uk_r1$InvoiceDate,'%m/%d/%Y %H:%M')

# New Variable > Total Price > Qty * Unit Price

uk_r1 <- uk_r1 %>% mutate(TotalPrice = Quantity * UnitPrice) 


# 4.1 Data after Cleaning in R

glimpse (uk_r1)  

# 4.3 --- Exploratory Data Analysis ---

# 4.3.1 Total Revenue in UK Online Retail 

options(repr.plot.width=4, repr.plot.height=5)

uk_r1 %>%
  group_by(year) %>%
  summarise(total_revenue = sum(TotalPrice)) %>%
  ggplot(aes(x= year, y= total_revenue/1000000,fill = year)) + 
  geom_bar(stat = "identity") + labs(x = "year", y = "Total Sales in million")


# 4.3.2 Countries with highest transactions 

Transactions_per_Country <- top_n(arrange(summarise(group_by(uk_r1, Country), 
                                                    'Number of Transcations' = n()), desc(`Number of Transcations`)), 10)

names(Transactions_per_Country) <- c("Country", "Number of Transactions")

Transaction_per_Country_plot <- ggplot(head(Transactions_per_Country,8), aes(x = reorder(Country,-`Number of Transactions`),
                                                                             y = `Number of Transactions`)) + geom_bar(stat = 'identity', fill = "Salmon") +
  geom_text(aes(label = `Number of Transactions`)) +
  ggtitle('Countries that generate the highest Number of Transactions') + xlab('Countries') +
  ylab('Number of Transactions') +
  theme_minimal() 

print(Transaction_per_Country_plot)

# 4.3.3 Most Frequent Items brought by customer

options(repr.plot.width=6, repr.plot.height=3)

uk_r1 %>% group_by(StockCode, Description) %>% summarise(count= n()) %>% arrange(desc(count)) %>% head() %>%
  ggplot(aes(x=Description, y=count, fill = count)) + geom_bar(stat= "identity") + coord_flip() + 
  labs(y="Number of items purchased", x="Product")  



# 5.1 RFM Metric

# Aggregrate Data at customer level

df_RFM = uk_r1 %>% 
  group_by(CustomerID) %>% 
  summarise(
    recency = as.numeric(max(InvoiceDate) - as.Date("2011-12-09")), # the higher the better
    frequency =(c(n_distinct(InvoiceNo))),
    monetary = sum(TotalPrice)/n_distinct(InvoiceNo)
  )

df_RFM  <- na.omit(df_RFM)

head(df_RFM,10)

summary(df_RFM)

glimpse(df_RFM)

# Visualise RFM Value

DataExplorer::plot_histogram(df_RFM,title = "RFM Metrics",
                             geom_histogram_args = list(bins = 5))

# 5.2 RFM Correlation plot

DataExplorer::plot_correlation(df_RFM)

## 6.1 ---- K-Means Clustering and Hierarchical Clustering --- 

# 6.1.2 Scaling the data

rfm_cols = c("recency", "frequency", "monetary")

df_scaled = scale(df_RFM[, rfm_cols])## Normalise RFM normalize the RFM data so that each dimension lies on a comparable scale with mean 0 and standard deviation

summary(df_scaled)


# 6.1.3 Optimal No of Cluster 

set.seed(4)

plot(x = k_clusters, y = km_wss, type = "b", main = "Elbow Plot")


plot_km = factoextra::fviz_nbclust(df_scaled, FUNcluster = kmeans, method = "wss", nstart = 20) +
  geom_vline(xintercept = 4, lty = 2)

plot_km



# 6.1.4 K-Means Clustering; K = 4 

set.seed(123)

km_best = kmeans(df_scaled, centers = 4, nstart = 100)

df_RFM$km_clusters = as.factor(km_best$cluster)

# Centroids from Data model on normalised data

km_best$centers

## Visualise K-Means Clustering

install.packages("factoextra")

library(factoextra)

library(NbClust)

set.seed(100)

fviz_cluster(km_best,data = df_scaled 
             ,pointsize = 0.01,labelsize = 10 ,main = "K-Means Clustering when K = 4")

df_RFM %>% 
  group_by(km_clusters) %>% # define column used for grouping
  summarize_if(is.numeric, mean) # aggregate all numeric columns by the mean

# The score (between_SS / total_SS) # Between cluster 

km_best$betweenss/km_best$totss


# 6.2 Hierarchical Clustering , K = 4 

# compute distance matrix > Use DF_scaled data

dist = dist(df_scaled, method = "euclidean")


# perform hierarchical clustering using a linkage method > Complete

hc1 = hclust(dist, method = "complete")


# extract clusters for, e.g., k = 4

hc1_clusters = cutree(hc1, k = 4)

# cluster assingments are stored in `hc1_clusters` (obtained by `cutree` function)

head(hc1_clusters)


# plot dendogram

plot(hc1,hang = -1,cex = 0.5)

plot(hc1,hang = -1)

# visualize clusters for, e.g., k = 4

rect.hclust(hc1, k = 4)

# perform hierarchical clustering by directly specifying k, the distance metric, and the linkage method

hc2 = factoextra::hcut(df_scaled, k = 4, hc_func = "hclust", hc_method = "complete", hc_metric = "euclidean")


head(hc2$cluster)


# 6.2.1 plot dendogram and visualize h- clusters, When K = 4

factoextra::fviz_dend(hc2, k = 4)

# Visualise using scatterplot

library(gridExtra)
library(grid)

scatter_hc1 = factoextra::fviz_cluster(hc2)

scatter_hc2 = factoextra::fviz_cluster(hc2, k = 4, choose.vars = c("recency", "monetary"), stand = FALSE)

gridExtra::grid.arrange(scatter_hc1, scatter_hc2, nrow = 1)

# 6.2.1 Visualise H-clustering when K = 4 

gridExtra::grid.arrange(scatter_hc2, nrow = 1,top=textGrob("Hierarchical clustering when K = 4"))

plot_com = factoextra::fviz_nbclust(df_scaled, FUNcluster = hcut, method = "wss",
                                    hc_func = "hclust", hc_method = "complete", hc_metric = "euclidean")

plot_com


plot_com$data


# 6.3 Compare Hierarchical Clustering and k-means using various Linkage method

plot_avg = factoextra::fviz_nbclust(df_scaled, FUNcluster = hcut, method = "wss",
                                    hc_func = "hclust", hc_method = "average", hc_metric = "euclidean")

plot_cen = factoextra::fviz_nbclust(df_scaled, FUNcluster = hcut, method = "wss",
                                    hc_func = "hclust", hc_method = "centroid", hc_metric = "euclidean")

plot_sin = factoextra::fviz_nbclust(df_scaled, FUNcluster = hcut, method = "wss",
                                    hc_func = "hclust", hc_method = "single", hc_metric = "euclidean")


compare = rbind(
  cbind(plot_km$data, method = "kmeans"),
  cbind(plot_avg$data, method = "hclust_average"),
  cbind(plot_cen$data, method = "hclust_centroid"),
  cbind(plot_com$data, method = "hclust_complete"),
  cbind(plot_sin$data, method = "hclust_single")
)

str(compare)

## Visualise using ggplot 

ggplot(compare, aes(x = clusters, y = y, col = method)) + 
  geom_point() +
  geom_line(aes(group = method)) + 
  ylab("Total Within Sum of Square")+
  labs(title = "Comparing K-Means Clustering Vs Hierarchical Clustering")


# Save File
save.image("Grp8_km & H.RData") 