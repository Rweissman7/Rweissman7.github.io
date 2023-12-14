#In this project I will be analyzing data from a call center..
#The data set Call_Center.csv contains information for close to 33000 calls to a call center.
#Information included in this file includes: a timestamp, call-centers city, channel, city, customer name, reason, response time, sentiment, state, call duration in minutes,csat (customer satisfaction) score.

#load all librarys will need for plotting
library(ggplot2)
#load library to get see multiple grids on one page
library("gridExtra")
library("cowplot")

#load data set
call_Dataset <- read.csv(file.choose())

#summary stats initial observations
head(call_Dataset)
summary(call_Dataset)
#csat score has too many na's for it to be used effectively 

#1Does call duration or response time affect the sentiment
#bar plot ordered fro highest average call duration to lowest
ggplot(call_Dataset, aes(x = reorder(Sentiment, -Call.Duration.In.Minutes), y= Call.Duration.In.Minutes))+
  stat_summary(fun="mean", geom='col')+
  geom_text(aes(label = round(after_stat(y),3),group=Sentiment),stat = "summary",fun = mean, vjust=-1 )+
  labs(x = "Sentiment", y = "Mean Call Duration (Minutes)")
#it seems like the average call duration are not that different for each sentiment group but an average response of less than a minute longer is enough to 
# get the sentiment from positive to negative/neutral
#will try boxplot to check for outliers in this group
ggplot(call_Dataset, aes(x = Sentiment, y= Call.Duration.In.Minutes))+
  geom_boxplot()
#the mean and min call duration is same for all groups though negative the max is a bit higher. Call duration has no impact on very negative responses
#but might impact a negative score

#check if response time does affect the sentiment
#create 4 bar plots one for all response times and percentage of each sentiment and 3 more for calls that were responded within above and below sla
plot_all<- ggplot(call_Dataset, aes(x = Sentiment, y= Response.Time))+
  geom_col(fill = "lightgray")+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(label = sprintf("%.1f%%",after_stat(y)/nrow(call_Dataset)*100),group=Sentiment),stat = "summary",fun = sum, vjust=2, color="red" )+
  labs(x = "Sentiment", y = "All Response Times")
#majority of responses were negative or neutral very few were positive
#check for within sla
within_sla <- call_Dataset[call_Dataset$Response.Time == "Within SLA", ]
plot_within <- ggplot(within_sla, aes(x = Sentiment, y= Response.Time))+
  geom_col(fill = "lightgray")+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(label = sprintf("%.1f%%",after_stat(y)/nrow(within_sla)*100),group=Sentiment),stat = "summary",fun = sum, vjust=2, color="red" )+
  labs(x = "Sentiment", y = "Within SLA")

#check for above sla
above_sla <- call_Dataset[call_Dataset$Response.Time == "Above SLA", ]
plot_above <- ggplot(above_sla, aes(x = Sentiment, y= Response.Time))+
  geom_col(fill = "lightgray")+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(label = sprintf("%.1f%%",after_stat(y)/nrow(above_sla)*100),group=Sentiment),stat = "summary",fun = sum, vjust=2, color="red" )+
  labs(x = "Sentiment", y = "Above SLA")

#check for below sla
below_sla <- call_Dataset[call_Dataset$Response.Time == "Below SLA", ]
plot_below <- ggplot(below_sla, aes(x = Sentiment, y= Response.Time))+
  geom_col(fill = "lightgray")+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(label = sprintf("%.1f%%",after_stat(y)/nrow(below_sla)*100),group=Sentiment),stat = "summary",fun = sum, vjust=2, color="red" )+
  labs(x = "Sentiment", y = "Below SLA")

#combine plots on one page
plot_grid(plot_all, plot_within, plot_above,plot_below,ncol=2, nrow=2)

#although overall the percentages are very similar there is a minute difference in sentiment depending on wait time
#very positive percentage seems like don't depend on how long caller had to wait. Surprisingly negative is a bit less if the wait time was within sla
#but went up slightly with a above or below standard wait time. With positive sentiments the above normal waiting time had higher positive response while within had the lowest
#while with very negative sentiments, within and above sla had same percentage of responses and only below sla had a lower very negative response. Maybe the sla range should 
#be adjusted in the future as the within bracket seems to still be too long for most people to improve their sentiment

#2does any call center have a higher than average call wait or more negative/ positive sentiment?

#create the text for percentage number for each call center city
percentages <- call_Dataset %>%
  group_by(Call.Centres.City, Sentiment) %>%
  summarize(percentage = n() / sum(call_Dataset$Call.Centres.City == Call.Centres.City) * 100)
#add percentage number to data set
merged_dataset <- merge(call_Dataset,percentages, by = c("Call.Centres.City", "Sentiment"), all.x =TRUE)
#sentiments per city stacked bar chart
ggplot(merged_dataset, aes(x = Call.Centres.City, fill = Sentiment)) +
  geom_bar(position = "stack") +
  geom_text(
    aes(label = sprintf("%.1f%%", percentage)),
    stat = "count",
    position = position_stack(vjust = 0.5)
  )
#Seems like Los Angeles call center has highest percentage of very negative sentiments and Baltimore highest percentage of negative and very negative combined
#see what can improve there what makes people in other two call centers not have such negative sentiments 

#do call center have longer wait times
percentages_response <- call_Dataset %>%
  group_by(Call.Centres.City, Response.Time) %>%
  summarize(percentage = n() / sum(call_Dataset$Call.Centres.City == Call.Centres.City) * 100)

merged_dataset <- merge(call_Dataset,percentages_response, by = c("Call.Centres.City", "Response.Time"), all.x =TRUE)
ggplot(merged_dataset, aes(x = Call.Centres.City, fill = Response.Time)) +
  geom_bar(position = "stack") +
  geom_text(
    aes(label = sprintf("%.1f%%", percentage)),
    stat = "count",
    position = position_stack(vjust = 0.5)
  )
#los Angeles has lowest percentage of fastest response time maybe that is impacting their negative sentiment 

#3does the channel which the call is coming from affect the sentiment
percentages_channel <- call_Dataset %>%
  group_by(Channel, Sentiment) %>%

merged_dataset <- merge(call_Dataset,percentages_channel, by = c("Channel", "Sentiment"), all.x =TRUE)
#stacke bar chart to see the percentage of sentiments for each channel 
ggplot(merged_dataset, aes(x = Channel, fill = Sentiment)) +
  geom_bar(position = "stack") +
  geom_text(
    aes(label = sprintf("%.1f%%", percentage)),
    stat = "count",
    position = position_stack(vjust = 0.5))
#Web channel has highest percentage positive and lowest very negative so maybe get more people to try the web first before calling. 
#Or maybe customers using who prefer the web are just the easiest to please. Meanwhile the call center has the highest percentage very negative but also
#the highest percentage very positive so maybe that is just the nature of personal responses more extreme in each direction.

