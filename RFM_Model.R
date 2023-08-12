#INFO 523 Data Mining
#Auther: Phillip McManus
#RFM Segmentation Model


library(dplyr)

transactions <- read.csv('data/store_transactions.csv')

summary(transactions)
head(transactions)
head(rfm_data)



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
