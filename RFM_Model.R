#INFO 523 Data Mining
#Professor: Cristian Román-Palacios
#University of Arizona

#Author: Phillip McManus
#RFM Segmentation Model


library(dplyr)
library("ggplot2")
library(factoextra)

#Read in a sample dataset of store transactions
#This sample only contains the fields we need: 
#Customer ID, Order ID, Order Date and Order Amount (Sales)
transactions <- read.csv('data/store_transactions.csv')

#Remove any records that have NA in the required fields
transactions <- na.omit(transactions)

#Transform the text date in the dataset to an actual date format
transactions$Order.Date <- as.Date(transactions$Order.Date, format = '%m/%d/%Y')

#Since this is an old dataset, calculate the difference in time from today to the most recent date in the dataset, 
# which we will substract from each customer's recency to bring it current
data_age <- Sys.Date() - max(transactions$Order.Date)

#Group the data by customer ID and summarise the data, creating each RFM metric calculation
rfm_data <- transactions %>% group_by(Customer.ID) %>%
  summarise(recency = Sys.Date() - max(Order.Date) - data_age,
            frequency = length(unique(Order.ID)),
            monetary = sum(Sales))

#Convert the 'difftime' class into a numeric value
rfm_data$recency <- as.numeric(rfm_data$recency)

#Assign labels 1-5 to the quintiles of each metric. Recency should be in reverse order since a lower number is more recent.
rfm_data$recency_score <- cut(rfm_data$recency, 
                              breaks=c(quantile(rfm_data$recency, probs = seq(0, 1, by = 0.20))), 
                              labels=c(5,4,3,2,1))

rfm_data$frequency_score <- cut(rfm_data$frequency, 
                                breaks=c(quantile(rfm_data$frequency, probs = seq(0, 1, by = 0.20))), 
                                labels=c(1,2,3,4,5))

rfm_data$monetary_score <- cut(rfm_data$monetary, 
                               breaks=c(quantile(rfm_data$monetary, probs = seq(0, 1, by = 0.20))), 
                               labels=c(1,2,3,4,5))

#If any of the metrics were too low and caused an NA from the quantile split, assign the lowest tier '1' to them
rfm_data$recency_score[is.na(rfm_data$recency_score)] <- 1
rfm_data$frequency_score[is.na(rfm_data$frequency_score)] <- 1
rfm_data$monetary_score[is.na(rfm_data$monetary_score)] <- 1
rfm_data
#Convert scores to numeric values
#rfm_data$recency_score <- as.numeric(rfm_data$recency_score)
#rfm_data$frequency_score <- as.numeric(rfm_data$frequency_score)
#rfm_data$monetary_score <- as.numeric(rfm_data$monetary_score)


#Plot the RFM metrics and scores in different ways to visualize the data
ggplot(rfm_data, aes(recency, frequency)) +
  geom_jitter(aes(colour = monetary_score)) +
  labs(title = "Recency vs Frequency",
       subtitle = "w/ Monetary Score Weighting",
       x = "Recency",
       y = "Frequency",
       colour = "Monetary Score")

ggplot(rfm_data, aes(recency, monetary)) +
  geom_jitter(aes(colour = frequency_score)) +
  labs(title = "Recency vs Monetary",
       subtitle = "w/ Frequency Score Weighting",
       x = "Recency",
       y = "Monetary",
       colour = "Frequency Score")

ggplot(rfm_data, aes(frequency, monetary)) +
  geom_jitter(aes(colour = recency_score)) +
  labs(title = "Frequency vs Monetary",
       subtitle = "w/ Recency Score Weighting",
       x = "Frequency",
       y = "Monetary",
       colour = "Recency Score")

ggplot(rfm_data, aes(recency_score, frequency_score)) +
  geom_jitter(aes(colour = monetary_score), width = 0.25, height = 0.25) +
  labs(title = "Recency vs Frequency Scores",
       subtitle = "w/ Monetary Score Weighting",
       x = "Recency Score",
       y = "Frequency Score",
       colour = "Monetary Score")

ggplot(rfm_data, aes(frequency_score, monetary_score)) +
  geom_jitter(aes(colour = recency_score), width = 0.25, height = 0.25) +
  labs(title = "Frequency vs Monetary Scores",
       subtitle = "w/ Monetary Score Weighting",
       x = "Frequency Score",
       y = "Monetary Score",
       colour = "Recency Score")

#Save RFM Scores to a separate data frame
rfm_scores <- rfm_data[,c("recency_score", "frequency_score", "monetary_score")]

#Plot the optimal number of clusters
fviz_nbclust(rfm_scores, kmeans, method = "wss")  #The result of this plot shows the optimal number of clusters being 5 for this dataset

#Run K-Mean Clustering
set.seed(314)
rfm_groups <- kmeans(rfm_scores, centers = 5, nstart = 25)

#View the average scores for each cluster
rfm_groups$centers

#Create a new data frame using the Centers data from the kmeans function
rfm_centers <- data.frame(rfm_groups$centers)

#Plot the scores for each cluster
ggplot(rfm_centers, aes(recency_score, frequency_score, size = monetary_score)) +
  geom_point(color = "navy") + scale_size(range = c(1, 15)) +
  labs(title = "Recency vs Frequency Scores",
       subtitle = "w/ Monetary Score Sizes",
       x = "Recency Score",
       y = "Frequency Score",
       size = "Monetary Score")

#Add the cluster numbers to the original dataset
rfm_data <- cbind(rfm_data, "segment" = rfm_groups$cluster)

#Summarise the mean of each metrics for each cluster
rfm_segments <- rfm_data %>% group_by(segment) %>%
  summarise(segment_size = length(segment),
            avg_recency = round(mean(recency)),
            avg_frequency = round(mean(frequency)),
            avg_monetary = round(mean(monetary)))

#Display the table of segment averages and the visual plot of them
rfm_segments
library(ggrepel)
ggplot(rfm_segments, aes(avg_recency, avg_frequency, size = avg_monetary)) +
  geom_point(color = "navy") + scale_size(range = c(1, 15)) +
  geom_label_repel(aes(label = segment),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   size = 5) +
  labs(title = "Recency vs Frequency Mean of Clusters",
       subtitle = "w/ Monetary Mean Sizes",
       x = "Recency Mean",
       y = "Frequency Mean",
       size = "Monetary Mean")



