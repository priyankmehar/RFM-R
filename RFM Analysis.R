############################################################
### RECENCY FREQUENCY & MONETARY [RFM] ANALYSIS USING R ###
############################################################

###
#Libraries

# install.packages("dplyr")
library(dplyr)                    # For data manipulation
# install.packages("sqldf")
library(sqldf)                    # For data manipulation
# install.packages("rfm")
library(rfm)                      # For RFM analysis
# install.packages("ggplot2")
library(ggplot2)                  # For data visualization
# install.packages("RColorBrewer")
library(RColorBrewer)

#################################################
#Reading Source File whilst  converting the empty fields to NA incase any
RFM_SRC = read.csv(file.choose(),header = T,na.strings=c("NA","NaN", " "))
summary(RFM_SRC)



#Top key element of the dataset for exploratory analysis
table(head(RFM_SRC %>% select(ClientID, NoOfVisit, InActivePeriod, TotalPurchase)))

#################################################
#Runnig the RFM model
#We only need the mentioned 4 fields for this analysis
#Client ID : is unquie client identifier

#No of Visit : defines as client activityve it number of transaction or visit
#One can derive by the number of visit or transaction an individual client has made

#Inactive perriod : It define when the client has last visited.
#As in general practice one can derive this field by taking difference of 
#max timestamp in the dataset with last timestamp visit of the individual client.

#Total Purchase : Its define the average purchase made per client

RFM_MODEL <- rfm_table_customer(RFM_SRC, c(ClientID, NoOfVisit, InActivePeriod, TotalPurchase) )
str(RFM_MODEL)
head(print(RFM_MODEL))

#################################################
#Graphical Representation of RFM Model output

#RFM heatmap
rfm_heatmap(RFM_MODEL)

#RFM Bar chart
rfm_bar_chart(RFM_MODEL)

#RFM Histogram
rfm_histograms(RFM_MODEL)

#RFM Customer by orders
rfm_order_dist(RFM_MODEL)

#Scatter plot 
#Recency vs Monetary vs Monetary Value
rfm_rm_plot(RFM_MODEL)
rfm_fm_plot(RFM_MODEL)
rfm_rf_plot(RFM_MODEL)

#################################################
#Segmentation

#Creating Segment score dataframe
#There are no predefind scoring table so I have created a dataframe manually for segmentation of customers

SegmentScore <- data.frame(
  segment = c("Top", "Top", "Top", "Top", "Top", "Top", "Top", "Top", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Loyal", "Potential", "Potential", "Potential", "Potential", "Potential", "Potential", "Potential", "Potential", "Potential", "Potential", "Potential", "Potential", "Potential", "Potential", "Potential", "Potential", "Need Attention", "Need Attention", "Need Attention", "Need Attention", "Need Attention", "Need Attention", "Need Attention", "Need Attention", "Need Attention", "Need Attention", "High Spent", "High Spent", "High Spent", "High Spent", "High Spent", "High Spent", "High Spent", "High Spent", "Lost", "Lost", "Lost", "Lost", "Lost"),
  rfm_score = c(555, 554, 545, 544, 444, 455, 454, 445, 234, 235, 243, 253, 334, 335, 343, 344, 345, 353, 354, 355, 434, 435, 443, 453, 534, 535, 543, 553, 433, 533, 411, 412, 413, 421, 422, 423, 431, 432, 511, 512, 513, 521, 522, 523, 531, 532, 222, 223, 232, 322, 323, 332, 221, 321, 312, 233, 144, 145, 154, 155, 245, 244, 254, 255, 111, 112, 121, 212, 211),
  Description = c("Bought recently, buy often and spend the most", "Bought recently, buy often and spend the most", "Bought recently, buy often and spend the most", "Bought recently, buy often and spend the most", "Bought recently, buy often and spend the most", "Bought recently, buy often and spend the most", "Bought recently, buy often and spend the most", "Bought recently, buy often and spend the most", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Spend good money. Responsive to promotions", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Recent shoppper, often visit , spent average", "Average recency, frequency & monetary values", "Average recency, frequency & monetary values", "Average recency, frequency & monetary values", "Average recency, frequency & monetary values", "Average recency, frequency & monetary values", "Average recency, frequency & monetary values", "Average recency, frequency & monetary values", "Average recency, frequency & monetary values", "Average recency, frequency & monetary values", "Average recency, frequency & monetary values", "Made big purchases but often, but long time ago", "Made big purchases and often, but long time ago", "Made big purchases and often, but long time ago", "Made big purchases and often, but long time ago", "Made big purchases and often, but long time ago", "Made big purchases and often, but long time ago", "Made big purchases and often, but long time ago", "Made big purchases and often, but long time ago", "Low recency, frequency & monetary values", "Low recency, frequency & monetary values", "Low recency, frequency & monetary values", "Low recency, frequency & monetary values", "Low recency, frequency & monetary values")
)

head(SegmentScore)

#Converting SALON RFM in readable form
RFM_MODEL_X = print(RFM_MODEL)

#Creating master RFM and segment dataframe
RFM_MODEL_SEGMENT = merge(RFM_MODEL_X,SegmentScore,by = "rfm_score",type = "right")


#One can see the reduction in count after mergeing the data in above set 
#thats due to many missing score values in the manual creation of the dataframe

#################################################
#Segment Size  
RFM_MODEL_SEGMENT %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)

#################################################
#RFM Medians


#MEdian Recency
RECENCY_MEDIAN <- 
  RFM_MODEL_SEGMENT %>%
  group_by(segment) %>%
  select(segment, recency_days) %>%
  summarize(median(recency_days)) %>%
  rename(segment = segment, avg_recency = `median(recency_days)`) %>%
  arrange(avg_recency) 

n_fill <- nrow(RECENCY_MEDIAN)

ggplot(RECENCY_MEDIAN, aes(segment, avg_recency)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
  xlab("Segment") + ylab("Median Recency") +
  ggtitle("Median Recency by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#Median Frequency
FREQUENCY_MEDIAN <- 
  RFM_MODEL_SEGMENT %>%
  group_by(segment) %>%
  select(segment, transaction_count) %>%
  summarize(median(transaction_count)) %>%
  rename(segment = segment, avg_frequency = `median(transaction_count)`) %>%
  arrange(avg_frequency) 

n_fill <- nrow(FREQUENCY_MEDIAN)

ggplot(FREQUENCY_MEDIAN, aes(segment, avg_frequency)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
  xlab("Segment") + ylab("Median Frequency") +
  ggtitle("Median Frequency by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#Median Monetary Value
MONETARY_MEDIAN <- 
  RFM_MODEL_SEGMENT %>%
  group_by(segment) %>%
  select(segment, amount) %>%
  summarize(median(amount)) %>%
  rename(segment = segment, avg_monetary = `median(amount)`) %>%
  arrange(avg_monetary) 

n_fill <- nrow(MONETARY_MEDIAN)

ggplot(MONETARY_MEDIAN, aes(segment, avg_monetary)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
  xlab("Segment") + ylab("Median Monetary Value") +
  ggtitle("Median Monetary Value by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )