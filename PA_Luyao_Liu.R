##==================================================
##=======For: Sr.Product Analyst ===================
##=======Author: Luyao Liu==========================
##==================================================

rm(list=ls())
setwd("~/Documents/Simon Strong/CMC/Job Searching/Assessment") #Please change this path to your own dataset folder

df <- read.csv('Data_Challenge_Data.csv')
summary(df) #more details about the dataset

df$Date <- as.Date(df$Date) #Transform from factor to date

##################Part I########################

##(1) How many Pageviews did Glamour have in August?

G_subset <- df[df$Brand == 'Glamour' & df$Date >= '2019-08-01' & df$Date <= '2019-08-31',]
nrow(G_subset[G_subset$Event_Name == 'Pageview',])
## 4087 Pageviews happened in August for Glamour.

##(2) How many people saw a video on CNT?

CNT_subset <- df[df$Brand == 'Conde Nast Traveler' & df$Event_Name =='Impression' & df$Component_Name == 'Video', ]
length( unique(CNT_subset$User_Id) )
## 7749 people saw a video on CNT.

##(3) What percentage of SELF’s sessions are bounces. Which device type has the highest bounce rate?

S_subset <- df[df$Brand == 'Self' & df$Event_Name == 'Pageview', ]
S_sub_groupby <- aggregate( formula = Event_Name ~ Session_Id, 
                   data = S_subset, 
                   FUN = length)
colnames(S_sub_groupby) <- c('Session_Id', 'Pageview_Counts')
pct <- nrow(S_sub_groupby[S_sub_groupby$Pageview_Counts == 1,]) / nrow(S_sub_groupby) *100
round(pct,2)
## 67.09% of SELF's sessions are bounces.

## To explore which device typy has highest bounce rate.
device <- as.list( levels(S_subset$Device) )

S_d_output <- NA
i = 1
while (i < 4) {
    S_d <- S_subset[S_subset$Device == device[i],]
    S_d_groupby <- aggregate( formula = Event_Name ~ Session_Id, 
                                   data = S_d, 
                                   FUN = length)
    colnames(S_d_groupby) <- c('Session_Id', 'Pageview_Counts')
    
    pct_device <- nrow(S_d_groupby[S_d_groupby$Pageview_Counts == 1,]) / nrow(S_d_groupby) *100
    S_d_output <- rbind( S_d_output, unlist( c(device[i], round(pct_device,2) ) ))
    i = i + 1
}

colnames(S_d_output) <- c('Device_Type', 'Bounce_Rate')
S_d_output <- as.data.frame( S_d_output[-1,] )
S_d_output
## Tablet has the highest bouce rate, which is 68.08%.


##################Part II########################

##(1) The subscription rate for each of CNT's markets?

### Subscription_Rate_1: Subscriptions/ Unique Users
### Subscription_Rate_2: Subscriptions/ Sessions

## To explore which market has highest subscription rate
CNT_subset2 <- df[df$Brand == 'Conde Nast Traveler',]
market <- as.list( levels(CNT_subset2$Market) )

CNT_m_output <- NA
j <- 1
while (j < length(market)+1) {
    CNT_m <- CNT_subset2[CNT_subset2$Market == market[j],]
    CNT_m_groupby_users <- aggregate( formula = User_Id ~ Market, 
                                     data = CNT_m, 
                                     FUN = function(x) length(unique(x)))
    colnames(CNT_m_groupby_users) <- c('Market', 'Unqiue_Users_Counts')
    
    CNT_m_groupby_sessions <- aggregate( formula = Session_Id ~ Market, 
                                      data = CNT_m, 
                                      FUN = function(x) length(unique(x)))
    colnames(CNT_m_groupby_users) <- c('Market', 'Sessions_Counts')    
    
    CNT_m_groupby_subs <- aggregate( formula = User_Id ~ Market, 
                                                   data = CNT_m[CNT_m$Event_Name == 'Subscription',], 
                                                   FUN = length)    
    colnames(CNT_m_groupby_subs) <- c('Market', 'Subscriptions_Counts')
    
    pct_1 <- CNT_m_groupby_subs[,2] / CNT_m_groupby_users[,2] *100
    pct_2 <- CNT_m_groupby_subs[,2] / CNT_m_groupby_sessions[,2] *100    
    CNT_m_output <- rbind(CNT_m_output, 
                          unlist( c(market[j], round(pct_1,2), round(pct_2,2), CNT_m_groupby_users[,2], CNT_m_groupby_sessions[,2]) ))
    j = j + 1
}

colnames(CNT_m_output) <- c('Market', 'Subscription_Rate_1', 'Subscription_Rate_2','Unique_Users', 'Sessions')
CNT_m_output <- as.data.frame(CNT_m_output[-1,])
CNT_m_output$Subscription_Rate_1 <- as.numeric(as.character(CNT_m_output$Subscription_Rate_1))
CNT_m_output$Subscription_Rate_2 <- as.numeric(as.character(CNT_m_output$Subscription_Rate_2))
CNT_m_output$Unique_Users <- as.numeric(as.character(CNT_m_output$Unique_Users))
CNT_m_output$Sessions <- as.numeric(as.character(CNT_m_output$Sessions))

CNT_m_output[ order(CNT_m_output[,2], decreasing = T), ]


## India has the highest subscription rate, which is 27.13%.
## More insights please refer to PowerPoint.


##(2) The click through rate for Ads on Glamour before and after?

## CTR Before
G_subset2 <- df[df$Brand == 'Glamour' &
                df$Component_Name == 'Advertisement' &
                df$Date >= '2019-06-01' & df$Date < '2019-09-01',]
Click1 <- nrow(G_subset2[G_subset2$Event_Name == 'Click',])
Im1 <- nrow(G_subset2[G_subset2$Event_Name == 'Impression',])
CTR_before <- Click1 / Im1
round(CTR_before*100,2) ##9.88%

## CTR After
G_subset3 <- df[df$Brand == 'Glamour' &
                df$Component_Name == 'Advertisement' &
                df$Date >= '2019-09-01' & df$Date <= '2019-12-31',]
Click2 <- nrow(G_subset3[G_subset3$Event_Name == 'Click',])
Im2 <- nrow(G_subset3[G_subset3$Event_Name == 'Impression',])
CTR_after <- Click2 / Im2  
round(CTR_after*100,2) ##8.35%

## Calculate the lift
lift <- (CTR_after - CTR_before) / CTR_before
round( lift*100,2) ##The lift is -15.46%.

## Whether the lift is statistically significant
sig.test <- rbind( c('Before', Im1, Click1, CTR_before), c('After', Im2, Click2, CTR_after) )
sig.test <- as.data.frame(sig.test)
colnames(sig.test) <- c('Duration', 'Impressions', 'Clicks', 'CTR')
sig.test[,2] <- as.numeric(as.character(sig.test[,2]))
sig.test[,3] <- as.numeric(as.character(sig.test[,3]))
sig.test[,4] <- as.numeric(as.character(sig.test[,4]))

SE1 <- sqrt( sig.test[1,4]*(1-sig.test[1,4]) / sig.test[1,2])
SE2 <- sqrt( sig.test[2,4]*(1-sig.test[2,4]) / sig.test[2,2])

z_score <- abs( (sig.test[2,4] - sig.test[1,4]) / sqrt(SE1^2 + SE2^2) )
pnorm(z_score) ##p-value is .9999, so the difference bwtween CTR before and after is not statistically significant.

###Exploration； The subscription rate of sessions including ad impression event for Glamour before and after?
G_subset4 <- df[df$Brand == 'Glamour' & 
                df$Session_Id %in% unique(G_subset2$Session_Id) &
                df$Event_Name == 'Subscription', ]
SR_before <- nrow(G_subset4) / nrow(G_subset2) *100
round(SR_before,2) ##3.36%

G_subset5 <- df[df$Brand == 'Glamour' &
                    df$Session_Id %in% unique(G_subset3$Session_Id) &
                    df$Event_Name == 'Subscription', ]
SR_after <- nrow(G_subset5) / nrow(G_subset3) *100
round(SR_after,2) ##3.36%


###Exploration； The number of events after an ad impression for Glamour before and after?
G_groupby1 <- df[df$Brand == 'Glamour' & df$Date < '2019-09-01',]
G_groupby2 <- df[df$Brand == 'Glamour' & df$Date >= '2019-09-01',]

install.packages('sqldf')
library(sqldf)

## Before
table1.1 <- sqldf("select distinct User_Id, Session_Id, min(Event_Order) AS First_AdIm_Order from `G_groupby1`
                where Component_Name = 'Advertisement'
                group by 1,2"
                )
table1.2 <- sqldf("select * from `G_groupby1` A join `table1.1` B on A.User_Id = B.User_Id
                and A.Session_Id = B.Session_Id"
                )
table1.2 <- table1.2[,-(11:12)]
table1.3 <- table1.2[table1.2$Event_Order > table1.2$First_AdIm_Order, ]

G_pct1.1 <- length(unique(table1.3$Session_Id)) / length(unique(table1.2$Session_Id))
G_pct1.2 <- nrow(table1.3) / length(unique(table1.3$Session_Id))

G_pct1 <- data.frame(Pct_of_sessions_after_ad = round(G_pct1.1 *100,2),
                     Avg_interactions_per_session_after_ad = G_pct1.2
                    )
G_pct1

## After
table2.1 <- sqldf("select distinct User_Id, Session_Id, min(Event_Order) AS First_AdIm_Order from `G_groupby2`
                where Component_Name = 'Advertisement'
                group by 1,2"
                )
table2.2 <- sqldf("select * from `G_groupby2` A join `table2.1` B on A.User_Id = B.User_Id
                and A.Session_Id = B.Session_Id"
                )
table2.2 <- table2.2[,-(11:12)]
table2.3 <- table2.2[table2.2$Event_Order > table2.2$First_AdIm_Order, ]

G_pct2.1 <- length(unique(table2.3$Session_Id)) / length(unique(table2.2$Session_Id))
G_pct2.2 <- nrow(table2.3) / length(unique(table2.3$Session_Id))

G_pct2 <- data.frame(Pct_of_sessions_after_ad = round(G_pct2.1 *100,2),
                     Avg_interactions_per_session_after_ad = G_pct2.2
                    )
G_pct2

## Build a table to include Before and After
G_pct <- rbind(G_pct1, G_pct2)
G_pct <- cbind( Duration = c('Before', 'After'), G_pct)
G_pct

## Whether the difference of Pct_of_sessions_after_ad is statistically significant
lift2 <- (G_pct2.1 - G_pct1.1) / G_pct1.1
round( lift2 *100,2)

sig.test2 <- rbind( c('Before', length(unique(table1.2$Session_Id)), length(unique(table1.3$Session_Id)), G_pct1.1), 
                    c('After', length(unique(table2.2$Session_Id)), length(unique(table2.3$Session_Id)), G_pct2.1) )
sig.test2 <- as.data.frame(sig.test2)
colnames(sig.test2) <- c('Duration', 'Sessions_including_ad', 'Sessions_continued', 'Pct_of_sessions_after_ad')
sig.test2[,2] <- as.numeric(as.character(sig.test2[,2]))
sig.test2[,3] <- as.numeric(as.character(sig.test2[,3]))
sig.test2[,4] <- as.numeric(as.character(sig.test2[,4]))

SE1.2 <- sqrt( sig.test2[1,4]*(1-sig.test2[1,4]) / sig.test2[1,2])
SE2.2 <- sqrt( sig.test2[2,4]*(1-sig.test2[2,4]) / sig.test2[2,2])

z_score2 <- abs( (sig.test2[2,4] - sig.test2[1,4]) / sqrt(SE1.2 ^2 + SE2.2 ^2) )
pnorm(z_score2) ##p-value is .9448, so the difference of Pct_of_sessions_after_ad is not statistically significant.

## Whether the difference between Avg_events_per_session_after_ad is statistically significant
lift3 <- (G_pct2.2 - G_pct1.2) / G_pct1.2
round( lift3 *100,2)

library(sqldf)
events_before <- sqldf("select distinct Session_Id, count(Event_Order) AS Events from `table1.3`
             group by 1"
)
sd_before<- sd(events_before$Events)

events_after <- sqldf("select distinct Session_Id, count(Event_Order) AS Events from `table2.3`
             group by 1"
)
sd_after<- sd(events_after$Events)

sig.test3 <- rbind( c('Before', length(unique(table1.3$Session_Id)),sd_before, G_pct1.2), 
                    c('After', length(unique(table2.3$Session_Id)), sd_after, G_pct2.2) )
sig.test3 <- as.data.frame(sig.test3)
colnames(sig.test3) <- c('Duration', 'Sessions_continued', 'SD', 'Avg_events_per_session_after_ad')
sig.test3
sig.test3[,2] <- as.numeric(as.character(sig.test3[,2]))
sig.test3[,3] <- as.numeric(as.character(sig.test3[,3]))
sig.test3[,4] <- as.numeric(as.character(sig.test3[,4]))

SE1.3 <- sqrt( sig.test3[1,3]^2 / sig.test3[1,2] + sig.test3[2,3]^2 / sig.test3[2,2])

t_stat <- abs( (sig.test3[2,4] - sig.test3[1,4]) / SE1.3 )
2*pnorm(-t_stat)

###Exploration； MoM CTR Vs Impression

G_subset4 <- df[df$Brand == 'Glamour' & df$Component_Name == 'Advertisement',]
G_subset4$month <- as.factor( months(G_subset4$Date) )
G_month <- as.vector( levels(G_subset4$month) )
G_month <- G_month[c(4,3,1,7,6,5,2)]

CTR_output <- NA
k = 1
while (k < length(G_month)+1) {
    G_subset5 <- G_subset4[months(G_subset4$Date) == G_month[k], ]
    CTR <- nrow( G_subset5[G_subset5$Event_Name == 'Click',] )/
           nrow( G_subset5[G_subset5$Event_Name == 'Impression',] ) *100
    
    CTR_output <- rbind( CTR_output, unlist( c(G_month[k], round(CTR,2), nrow( G_subset5[G_subset5$Event_Name == 'Impression',] )) ))
    k <- k + 1
}

colnames(CTR_output) <- c('Month', 'CTR', 'Impression')
CTR_output <- as.data.frame(CTR_output[-1,])
CTR_output

install.packages('ggplot2')
library(ggplot2)

CTR_output$Month <- factor(CTR_output$Month, levels = G_month)
CTR_output$CTR <- as.numeric(as.character(CTR_output$CTR))
CTR_output$Impression <- as.numeric(as.character(CTR_output$Impression))

coeff <- 300

g2 <- ggplot(CTR_output, aes(x=Month, group=1)) 
g2 <- g2 + geom_line(aes(y=CTR, color = '%CTR'))
g2 <- g2 + geom_line(aes(y=Impression/coeff, color = 'Impression'))

g2 <- g2 + scale_y_continuous(name = '%CTR', sec.axis = sec_axis(~.*coeff, name = 'Impression'))
g2 <- g2 + labs(title='%CTR Vs Impression')
g2


##(3) Cohort Retention Analysis for Pitchfork

## Rolling Cohort Retention
##(For each cohort, compared to last period, what's the percentage of users still being active in this period)
P_subset <- df[df$Brand == 'Pitchfork',]
P_subset$Month <- as.numeric(substr(P_subset$Date, 6,7))

## Build up the shell for cohort table, like cohort name, cohort size.
Coh_output <- as.data.frame(matrix(NA, nrow = length(G_month), ncol = length(G_month)+2))
colnames(Coh_output) <- c('cohort','cohort_size', 'month0','month1','month2','month3','month4','month5','month6')

library(sqldf)
co <- sqldf("select distinct User_Id, min(Month) AS Cohort,  max(Month) AS Churn from `P_subset`
             group by 1"
            )

cohort_size <- table(co$Cohort)

for (m in 1:7) {
    Coh_output[m,1] <- paste(G_month[m], '2019')
}

Coh_output$cohort_size <- cohort_size
Coh_output$month0 <- cohort_size

## Fill out the number of users still being active during each period for each cohort.
month_numeric <- 6:12

for (p in 1:7) {
    for (q in 4:9) {
        churn <- nrow(co[co$Cohort == month_numeric[p] & co$Churn <= month_numeric[p+q-4], ])
        retention <- cohort_size[p] - churn
        Coh_output[p,q] <- retention
    }
}

for (f in 2:7) {
    Coh_output[f, (11-f):9] <- NA
}

## Change from values to percentage
for (p in 1:7) {
    for (q in 9:3) {
        Coh_output[p,q] <- round( (Coh_output[p,q] / Coh_output[p,q-1])*100, 2 )
    }
}


##Draw the graph
library(ggplot2)

Coh_output_t <- as.data.frame( t(Coh_output) )
colnames(Coh_output_t) <- c('cohort1', 'cohort2', 'cohort3', 'cohort4', 'cohort5', 'cohort6', 'cohort7')
Coh_output_t <- Coh_output_t[-(1:2),]

for (t in 1:7) {
    Coh_output_t[,t] <- as.numeric( as.character(Coh_output_t [,t]) )
}

Coh_output_t$month <- c('month0','month1','month2','month3','month4','month5','month6')

g3 <- ggplot(Coh_output_t, aes(x=month, group=1)) 
g3 <- g3 + geom_line(aes(y=cohort1, color = 'cohort1')) +
           geom_line(aes(y=cohort2, color = 'cohort2')) +
           geom_line(aes(y=cohort3, color = 'cohort3')) +
           geom_line(aes(y=cohort4, color = 'cohort4')) +
           geom_line(aes(y=cohort5, color = 'cohort5')) +
           geom_line(aes(y=cohort6, color = 'cohort6')) +
           geom_line(aes(y=cohort7, color = 'cohort7'))
g3 <- g3 + labs(title='Cohort Retention Graph (Pitchfork)')
g3


##################Part III########################

### Objectve: Explore the user behavior that’s highly related to subscription event for Condé Nast Traveler.

## Build a linear regression model to see the relationship between variables and subscription.

library(sqldf)
CNT_table <- sqldf("select distinct User_Id, Session_Id, min(Event_Order) AS Subs_Order from `CNT_subset3`
                    group by 1,2"
                    )
CNT_table2 <- sqldf("select * from `CNT_subset2` A join `CNT_table` B on A.User_Id = B.User_Id and A.Session_Id = B.Session_Id")
CNT_table3 <- CNT_table2[,-(11:12)]
CNT_table4 <- CNT_table3[CNT_table3$Event_Order < CNT_table3$Subs_Order,]

# CNT_table5 : All the events that happen after subscription (inluding subs)

CNT_table5 <- CNT_table3[CNT_table3$Event_Order >= CNT_table3$Subs_Order,]
CNT_table5 <- CNT_table5[,-11]
    
install.packages('dplyr')
library(dplyr)

CNT_subset4 <- anti_join(CNT_subset2, CNT_table5) #All the events that happened before subscription including subscription (CNT)
CNT_subset4$Month <- months(CNT_subset4$Date)
CNT_subset4$Subscription[ (CNT_subset4$User_Id %in% CNT_table3$User_Id) & (CNT_subset4$Session_Id %in% CNT_table3$Session_Id) ] <- 1
CNT_subset4$Subscription[ is.na(CNT_subset4$Subscription) ] <- 0

lm <- lm(Subscription ~ Month + Device + Component_Name + Site_Section , data = CNT_subset4)
lm
anova(lm)

#Save the csv to local, and then do pivot table in Excel for CNT_subset4
write.csv(CNT_subset4, 'CNT_subeset4.csv')


## Draw the graph that shows the percentage of Event Order for subscruption (CNT)
CNT_subset3 <- df[df$Brand == 'Conde Nast Traveler' & df$Event_Name == 'Subscription',]

l <- min(CNT_subset3$Event_Order)
count_order <- NA

while (l < (max(CNT_subset3$Event_Order)+1)) {
    count <- nrow(CNT_subset3[CNT_subset3$Event_Order == l,])
    pct_subs <- count / nrow(CNT_subset3) *100
    count_order <- rbind(count_order, unlist(c(l, count, pct_subs)))
    l <- l + 1
}

colnames(count_order) <- c('Subs_Order', 'Count', 'Percentage')
count_order <- as.data.frame(count_order[-1,])
count_order

library(ggplot2)
g4 <- ggplot(count_order, aes(x=Subs_Order, y=Percentage)) + geom_point() 
g4 <- g4 + geom_line() + labs(title='The percentage of Event Order for Subscription (CNT)')
g4 <- g4 + scale_x_continuous(breaks=seq(min(CNT_subset3$Event_Order), max(CNT_subset3$Event_Order), 1))
g4


### Flow chart: Sankey Diagram

library(dplyr)
install.packages('networkD3')
library(networkD3)

## For events happen before the subscription and when the subscription occur in the 3rd of a session.

CNT_subs_order3rd <- CNT_table4[CNT_table4$Subs_Order == 3,]

# Sankey for component, subscription occur in the 3rd event of a session.
flow.data <- as.data.frame( matrix(NA, nrow = nrow(CNT_subs_order3rd[CNT_subs_order3rd$Event_Order == 1,]), ncol = 2))
colnames(flow.data) <- c('first_component', 'second_component')
flow.data$first_component <- CNT_subs_order3rd[CNT_subs_order3rd$Event_Order == 1,]$Component_Name
flow.data$second_component <- CNT_subs_order3rd[CNT_subs_order3rd$Event_Order == 2,]$Component_Name

flow.data_groupby <- group_by(flow.data, first_component, second_component)
flow.data_sum <- summarise(flow.data_groupby, count = n())

nodes <- data.frame( node = c(0:4),
                     name = c('Advertisement', 'Article', 'Gallery', 'Homepage', 'Video') )

flow <- merge(flow.data_sum, nodes, by.x = 'first_component', by.y = 'name')
flow <- merge(flow, nodes, by.x = 'second_component', by.y = 'name')
links <- flow[,c('node.x', 'node.y', 'count')]
colnames(links) <- c('source', 'target', 'value')

my_color1 <- 'd3.scaleOrdinal() .domain(["a", "b", "c", "d", "e"]) .range(["#4b8149", "#8791d4", "#6fb4dd", "#699ba6", "#dea3c5"])'

sankeyNetwork(Links = links, Nodes = nodes, Source = 'source', 
              Target = 'target', Value = 'value', NodeID = 'name',
              units = 'count', fontSize = 14, nodeWidth = 30, colourScale = my_color1)

# Sankey for site section, subscription occur in the 3rd event of a session.
flow.data1.2 <- as.data.frame( matrix(NA, nrow = nrow(CNT_subs_order3rd[CNT_subs_order3rd$Event_Order == 1,]), ncol = 2))
colnames(flow.data1.2) <- c('first_site_section', 'second_site_section')
flow.data1.2$first_site_section <- CNT_subs_order3rd[CNT_subs_order3rd$Event_Order == 1,]$Site_Section
flow.data1.2$second_site_section <- CNT_subs_order3rd[CNT_subs_order3rd$Event_Order == 2,]$Site_Section

flow.data_groupby1.2 <- group_by(flow.data1.2, first_site_section, second_site_section)
flow.data_sum1.2 <- summarise(flow.data_groupby1.2, count = n())
flow.data_sum1.2$second_site_section <- as.factor(paste0('2:', flow.data_sum1.2$second_site_section))

nodes1.2 <- data.frame( node = c(0:7),
                     name = c('Entertainment', 'Food', 'Homepage', 'Sports',
                              '2:Entertainment', '2:Food', '2:Homepage', '2:Sports') )

flow1.2 <- merge(flow.data_sum1.2, nodes1.2, by.x = 'first_site_section', by.y = 'name')
flow1.2 <- merge(flow1.2, nodes1.2, by.x = 'second_site_section', by.y = 'name')


links1.2 <- flow1.2[,c('node.x', 'node.y', 'count')]
colnames(links1.2) <- c('source', 'target', 'value')

my_color1.2 <- 'd3.scaleOrdinal() .domain(["a", "b", "c", "d", "e", "f", "g", "h"]) .range(["#6fb4dd", "#699ba6", "#4b8149", "#dea3c5", "#6fb4dd", "#699ba6", "#4b8149", "#dea3c5"])'

sankeyNetwork(Links = links1.2, Nodes = nodes1.2, Source = 'source', 
              Target = 'target', Value = 'value', NodeID = 'name',
              units = 'count', fontSize = 14, nodeWidth = 30, colourScale = my_color1.2)

## For events happen before the subscription and when the subscription occur in the 4rd of a session.

# Sankey for component name, subscription occur in the 4th event of a session.
library(dplyr)
library(networkD3)

CNT_subs_order4th <- CNT_table4[CNT_table4$Subs_Order == 4,]

flow.data2 <- as.data.frame( matrix(NA, nrow = nrow(CNT_subs_order4th[CNT_subs_order4th$Event_Order == 1,]), ncol = 3))
colnames(flow.data2) <- c('first_component', 'second_component', 'third_component')
flow.data2$first_component <- CNT_subs_order4th[CNT_subs_order4th$Event_Order == 1,]$Component_Name
flow.data2$second_component <- CNT_subs_order4th[CNT_subs_order4th$Event_Order == 2,]$Component_Name
flow.data2$third_component <- CNT_subs_order4th[CNT_subs_order4th$Event_Order == 3,]$Component_Name

flow.data_groupby2 <- group_by(flow.data2, first_component, second_component, third_component)
flow.data_sum2 <- summarise(flow.data_groupby2, count = n())
flow.data_sum2$second_component <- as.factor(paste0('2:', flow.data_sum2$second_component))
flow.data_sum2$third_component <- as.factor(paste0('3:', flow.data_sum2$third_component))

unique(flow.data_sum2$first_component)
unique(flow.data_sum2$second_component)
unique(flow.data_sum2$third_component)

nodes2 <- data.frame( node = c(0:6),
                     name = c('Article', 'Gallery', 'Homepage',
                              '2:Advertisement', '2:Video',
                              '3:Advertisement', '3:Video') )

flow2 <- merge(flow.data_sum2, nodes2, by.x = 'first_component', by.y = 'name')
flow2 <- merge(flow2, nodes2, by.x = 'second_component', by.y = 'name')
flow2 <- merge(flow2, nodes2, by.x = 'third_component', by.y = 'name')

links2.1 <- flow2[,c('node.x', 'node.y', 'count')]
colnames(links2.1) <- c('source', 'target', 'value')

links2.2 <- flow2[,c('node.y', 'node', 'count')]
colnames(links2.2) <- c('source', 'target', 'value')

links2 <- rbind(links2.1, links2.2)

nodes2$group <- as.factor(c('a','b','c',"d",'e','f','g'))

my_color2 <- 'd3.scaleOrdinal() .domain(["a", "b", "c", "d", "e", "f", "g"]) .range(["#8791d4", "#6fb4dd", "#699ba6", "#4b8149", "#dea3c5", "#4b8149", "#dea3c5"])'

sankeyNetwork(Links = links2, Nodes = nodes2, Source = 'source', 
             Target = 'target', Value = 'value', NodeID = 'name',
             units = 'count', fontSize = 12, nodeWidth = 30, colourScale = my_color2)

# Sankey for site section, subscription occur in the 4th event of a session.
flow.data2.2 <- as.data.frame( matrix(NA, nrow = nrow(CNT_subs_order4th[CNT_subs_order4th$Event_Order == 1,]), ncol = 3))
colnames(flow.data2.2) <- c('first_site_section', 'second_site_section', 'third_site_section')
flow.data2.2$first_site_section <- CNT_subs_order4th[CNT_subs_order4th$Event_Order == 1,]$Site_Section
flow.data2.2$second_site_section <- CNT_subs_order4th[CNT_subs_order4th$Event_Order == 2,]$Site_Section
flow.data2.2$third_site_section <- CNT_subs_order4th[CNT_subs_order4th$Event_Order == 3,]$Site_Section

flow.data_groupby2.2 <- group_by(flow.data2.2, first_site_section, second_site_section, third_site_section)
flow.data_sum2.2 <- summarise(flow.data_groupby2.2, count = n())
flow.data_sum2.2$second_site_section <- as.factor(paste0('2:', flow.data_sum2.2$second_site_section))
flow.data_sum2.2$third_site_section <- as.factor(paste0('3:', flow.data_sum2.2$third_site_section))

unique(flow.data_sum2.2$first_site_section)
unique(flow.data_sum2.2$second_site_section)
unique(flow.data_sum2.2$third_site_section)

nodes2.2 <- data.frame( node = c(0:11),
                      name = c('Entertainment', 'Food', 'Homepage', 'Sports',
                               '2:Entertainment', '2:Food', '2:Homepage', '2:Sports',
                               '3:Entertainment', '3:Food', '3:Homepage', '3:Sports') )

flow2.2 <- merge(flow.data_sum2.2, nodes2.2, by.x = 'first_site_section', by.y = 'name')
flow2.2 <- merge(flow2.2, nodes2.2, by.x = 'second_site_section', by.y = 'name')
flow2.2 <- merge(flow2.2, nodes2.2, by.x = 'third_site_section', by.y = 'name')

links2.1a <- flow2.2[,c('node.x', 'node.y', 'count')]
colnames(links2.1a) <- c('source', 'target', 'value')

links2.2a <- flow2.2[,c('node.y', 'node', 'count')]
colnames(links2.2a) <- c('source', 'target', 'value')

links2_2 <- rbind(links2.1a, links2.2a)

nodes2.2$group <- as.factor(c('a','b','c',"d",'e','f','g', 'h', 'i', 'j', 'k', 'l'))

my_color2.2 <- 'd3.scaleOrdinal() .domain(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"]) .range(["#6fb4dd", "#699ba6", "#4b8149", "#dea3c5", "#6fb4dd", "#699ba6", "#4b8149", "#dea3c5", "#6fb4dd", "#699ba6", "#4b8149", "#dea3c5"])'

sankeyNetwork(Links = links2_2, Nodes = nodes2.2, Source = 'source', 
              Target = 'target', Value = 'value', NodeID = 'name',
              units = 'count', fontSize = 12, nodeWidth = 30, colourScale = my_color2.2)


