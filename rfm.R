rm(list=ls())
options(scipen=200)
setwd("~/Desktop/Simon/Practical Project - Monro/Data3")
df = read.csv('Monro data 2.csv', stringsAsFactors = F)
# df<-df[-which(df$VehicleYear>2020),]
# df$VehicleYear[df$VehicleYear==1777] <- 1977
# df$VehicleYear[df$VehicleYear==0|df$VehicleYear==1] <- NA
library(lubridate)
df$InvoiceDate = as.Date(df$InvoiceDate, "%m/%d/%y")
df[df$GrossSalesAmount == ' $-   ', 'GrossSalesAmount'] = 0
df$GrossSalesAmount = as.numeric(df$GrossSalesAmount)

library(rfm)
rfm_data = df[, c(4,13,14)]
summary(rfm_data)
# rfm_result = rfm_table_order(rfm_data, IndividualId, InvoiceDate, GrossSalesAmount, Sys.Date())
#                             #recency_bins = 5, frequency_bins = 5, monetary_bins = 5
# summary(rfm_result)
# rfm_bar_chart(rfm)
# 
# segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
#                    "New Customers", "Promising", "Need Attention", "About To Sleep",
#                    "At Risk", "Can't Lose Them", "Lost")
# 
# recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
# recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
# frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
# frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
# monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
# monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
# 
# segments = rfm_segment(rfm_result, segment_names, recency_lower, recency_upper,
#             frequency_lower, frequency_upper, monetary_lower, monetary_upper)
# 
# rfm_plot_median_recency(segments)
# rfm_plot_median_frequency(segments)
# rfm_plot_median_monetary(segments)
# rfm_heatmap(rfm_result)
# rfm_bar_chart(rfm_result)
# rfm_histograms(rfm_result)
# # write.table(rfm_result$rfm,file="rfm_results.csv",sep=",",append=FALSE,row.names = FALSE)
# # rfm = read.csv('rfm_results.csv', stringsAsFactors = F)
# 
# table(segments$segment)
# other = segments[segments$segment == 'Others',]
# 
# cust_veh = df[, c(4,7)]
# library(dplyr)
# cust_veh = arrange(cust_veh, cust_veh[,1])
# cust_veh = cust_veh[!duplicated(cust_veh$VehicleId), ]
# length(unique(cust_veh$VehicleId))/length(unique(cust_veh$IndividualId)) # 1.54 cars per customer

# # cust id merge car info
# veh27 = read.csv('27brands_veh_new.csv', stringsAsFactors = F)
# cust_veh = merge(cust_veh, veh27, by = 'VehicleId')
# cust_veh = cust_veh[, -c(3,11)]
# names(cust_veh)[2] = 'customer_id'
# rfm_allcars = merge(segments, cust_veh[, 2:9], by = 'customer_id')

# rfm_last_car = list()
# for (i in unique(rfm_allcars$customer_id)) {
#     if (nrow(rfm_allcars[rfm_allcars$customer_id == i,]) != 1) {
#         rfm_last_car = rbind(rfm_last_car, 
#                              rfm_allcars[rfm_allcars$customer_id == i,][nrow(rfm_allcars[rfm_allcars$customer_id == i,]), ])
#     } else {
#         rfm_last_car = rbind(rfm_last_car, rfm_allcars[rfm_allcars$customer_id == i,])
#     }
# }

# rfm_result_2bins = rfm_table_order(rfm_data, IndividualId, InvoiceDate, GrossSalesAmount, Sys.Date(), 
#                                    recency_bins = 2, frequency_bins = 2, monetary_bins = 2)

# 2bins: Champion = 2,2,2 recent, high R, high M 
#        Loyal = 2,2,1 recent, high R, low M
#        New cust = 2,1,2 recent, low R, high M
#                 = 2,1,1 recent, low R, low M
# Should not lost = 1,2,2 not recent, high R, high M
#                 = 1,2,1 not recent, high R, low M
#        Lost = 1,1,2 not recent, low R, high M
#             = 1,1,1 not recent, low R, low M

rfm_result_3bins = rfm_table_order(rfm_data, IndividualId, InvoiceDate, GrossSalesAmount, Sys.Date(), 
                                   recency_bins = 3, frequency_bins = 3, monetary_bins = 3)

rfm_heatmap(rfm_result_3bins)
rfm_bar_chart(rfm_result_3bins)
rfm_histograms(rfm_result_3bins)


segment_names <- c("Champion","Loyal1","Loyal2","Loyal3","New1","New2","Potential","About To Sleep1","About To Sleep2","Lost")

recency_lower <- c(3,3,3,2,3,3,2,1,2,1)
recency_upper <- c(3,3,3,2,3,3,2,1,2,1)
frequency_lower <- c(3,3,2,3,1,2,2,3,1,1)
frequency_upper <- c(3,3,2,3,1,2,2,3,1,2)
monetary_lower <- c(2,1,2,1,1,1,1,1,1,1)
monetary_upper <- c(3,1,3,3,3,1,3,3,3,3)

segments = rfm_segment(rfm_result_3bins, segment_names, recency_lower, recency_upper,
                       frequency_lower, frequency_upper, monetary_lower, monetary_upper)
segments$segment[segments$segment=="Loyal1"|segments$segment=="Loyal2"|segments$segment=="Loyal3"] <- "Loyal"
segments$segment[segments$segment=="New1"|segments$segment=="New2"] <- "New"
segments$segment[segments$segment=="About To Sleep1"|segments$segment=="About To Sleep2"] <- "About To Sleep"

table(segments$segment)
table(segments$segment)/nrow(segments)

rfm_plot_median_recency(segments)
rfm_plot_median_frequency(segments)
rfm_plot_median_monetary(segments)


a = df[, c(4,7,13)]
library(dplyr)
a = arrange(a, a[,1], desc(a[,3]))

## method 1: only keep the last car info
cust_newest_car = a[!duplicated(a$IndividualId),]
veh27 <- read.csv("brands27_veh.csv",stringsAsFactors = F)
cust_newest_car = merge(cust_newest_car, veh27, by = 'VehicleId') # only include cars we have car type
cust_newest_car = cust_newest_car[, -c(3,4,12)]
names(cust_newest_car)[2] = 'customer_id'
rfm_last_car = merge(segments, cust_newest_car[, 2:9], by = 'customer_id')
df2 <- rfm_last_car


df2$Region <- NA
df2$Region[df2$State=='ME'] <- 'Northeast'
df2$Region[df2$State=='NH'] <- 'Northeast'
df2$Region[df2$State=='VT'] <- 'Northeast'
df2$Region[df2$State=='NY'] <- 'Northeast'
df2$Region[df2$State=='PA'] <- 'Northeast'
df2$Region[df2$State=='MA'] <- 'Northeast'
df2$Region[df2$State=='RI'] <- 'Northeast'
df2$Region[df2$State=='CT'] <- 'Northeast'
df2$Region[df2$State=='NJ'] <- 'Northeast'

df2$Region[df2$State=='ND'] <- 'Midwest'
df2$Region[df2$State=='SD'] <- 'Midwest'
df2$Region[df2$State=='NE'] <- 'Midwest'
df2$Region[df2$State=='KS'] <- 'Midwest'
df2$Region[df2$State=='MN'] <- 'Midwest'
df2$Region[df2$State=='IA'] <- 'Midwest'
df2$Region[df2$State=='MO'] <- 'Midwest'
df2$Region[df2$State=='WI'] <- 'Midwest'
df2$Region[df2$State=='IL'] <- 'Midwest'
df2$Region[df2$State=='IN'] <- 'Midwest'
df2$Region[df2$State=='MI'] <- 'Midwest'
df2$Region[df2$State=='OH'] <- 'Midwest'

df2$Region[df2$State=='TX'] <- 'South'
df2$Region[df2$State=='OK'] <- 'South'
df2$Region[df2$State=='AR'] <- 'South'
df2$Region[df2$State=='LA'] <- 'South'
df2$Region[df2$State=='MS'] <- 'South'
df2$Region[df2$State=='AL'] <- 'South'
df2$Region[df2$State=='TN'] <- 'South'
df2$Region[df2$State=='KY'] <- 'South'
df2$Region[df2$State=='WV'] <- 'South'
df2$Region[df2$State=='VA'] <- 'South'
df2$Region[df2$State=='DC'] <- 'South'
df2$Region[df2$State=='MD'] <- 'South'
df2$Region[df2$State=='DE'] <- 'South'
df2$Region[df2$State=='NC'] <- 'South'
df2$Region[df2$State=='SC'] <- 'South'
df2$Region[df2$State=='GA'] <- 'South'
df2$Region[df2$State=='FL'] <- 'South'

df2$Region[df2$State=='WA'] <- 'West'
df2$Region[df2$State=='OR'] <- 'West'
df2$Region[df2$State=='CA'] <- 'West'
df2$Region[df2$State=='NV'] <- 'West'
df2$Region[df2$State=='ID'] <- 'West'
df2$Region[df2$State=='MT'] <- 'West'
df2$Region[df2$State=='UT'] <- 'West'
df2$Region[df2$State=='AZ'] <- 'West'
df2$Region[df2$State=='AK'] <- 'West'
df2$Region[df2$State=='HI'] <- 'West'
df2$Region[df2$State=='MT'] <- 'West'
df2$Region[df2$State=='WY'] <- 'West'
df2$Region[df2$State=='CO'] <- 'West'
df2$Region[df2$State=='NM'] <- 'West'

###Invoice State to InoviceRegion###
# df2$InvoiceRegion <- NA
# df2$InvoiceRegion[df2$InvoiceState=='ME'] <- 'Northeast'
# df2$InvoiceRegion[df2$InvoiceState=='NH'] <- 'Northeast'
# df2$InvoiceRegion[df2$InvoiceState=='VT'] <- 'Northeast'
# df2$InvoiceRegion[df2$InvoiceState=='NY'] <- 'Northeast'
# df2$InvoiceRegion[df2$InvoiceState=='PA'] <- 'Northeast'
# df2$InvoiceRegion[df2$InvoiceState=='MA'] <- 'Northeast'
# df2$InvoiceRegion[df2$InvoiceState=='RI'] <- 'Northeast'
# df2$InvoiceRegion[df2$InvoiceState=='CT'] <- 'Northeast'
# df2$InvoiceRegion[df2$InvoiceState=='NJ'] <- 'Northeast'
# 
# df2$InvoiceRegion[df2$InvoiceState=='TX'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='OK'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='AR'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='LA'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='MS'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='AL'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='TN'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='KY'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='WV'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='VA'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='DC'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='MD'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='DE'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='NC'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='SC'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='GA'] <- 'Southeast'
# df2$InvoiceRegion[df2$InvoiceState=='FL'] <- 'Southeast'
# 
# df2$InvoiceRegion[df2$InvoiceState=='ND'] <- 'Midwest'
# df2$InvoiceRegion[df2$InvoiceState=='SD'] <- 'Midwest'
# df2$InvoiceRegion[df2$InvoiceState=='NE'] <- 'Midwest'
# df2$InvoiceRegion[df2$InvoiceState=='KS'] <- 'Midwest'
# df2$InvoiceRegion[df2$InvoiceState=='MN'] <- 'Midwest'
# df2$InvoiceRegion[df2$InvoiceState=='IA'] <- 'Midwest'
# df2$InvoiceRegion[df2$InvoiceState=='MO'] <- 'Midwest'
# df2$InvoiceRegion[df2$InvoiceState=='WI'] <- 'Midwest'
# df2$InvoiceRegion[df2$InvoiceState=='IL'] <- 'Midwest'
# df2$InvoiceRegion[df2$InvoiceState=='IN'] <- 'Midwest'
# df2$InvoiceRegion[df2$InvoiceState=='MI'] <- 'Midwest'
# df2$InvoiceRegion[df2$InvoiceState=='OH'] <- 'Midwest'
# 
# df2$InvoiceRegion[df2$InvoiceState=='WA'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='OR'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='CA'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='NV'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='ID'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='MT'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='UT'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='AZ'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='AK'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='HI'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='MT'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='WY'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='CO'] <- 'West'
# df2$InvoiceRegion[df2$InvoiceState=='NM'] <- 'West'
# 
# df2$InvoiceRegion[df2$InvoiceState=='ME'] <- 'New England'
# df2$InvoiceRegion[df2$InvoiceState=='VT'] <- 'New England'
# df2$InvoiceRegion[df2$InvoiceState=='NH'] <- 'New England'
# df2$InvoiceRegion[df2$InvoiceState=='MA'] <- 'New England'
# df2$InvoiceRegion[df2$InvoiceState=='RI'] <- 'New England'
# df2$InvoiceRegion[df2$InvoiceState=='CT'] <- 'New England'
# 
# df2$InvoiceRegion[df2$InvoiceState=='VA'] <- 'Mid-Atlantic'
# df2$InvoiceRegion[df2$InvoiceState=='WV'] <- 'Mid-Atlantic'
# df2$InvoiceRegion[df2$InvoiceState=='PA'] <- 'Mid-Atlantic'
# df2$InvoiceRegion[df2$InvoiceState=='MD'] <- 'Mid-Atlantic'
# df2$InvoiceRegion[df2$InvoiceState=='DE'] <- 'Mid-Atlantic'
# df2$InvoiceRegion[df2$InvoiceState=='NJ'] <- 'Mid-Atlantic'
# 
# df2$InvoiceRegion[df2$InvoiceState=='MN'] <- 'Great Lakes'
# df2$InvoiceRegion[df2$InvoiceState=='WI'] <- 'Great Lakes'
# df2$InvoiceRegion[df2$InvoiceState=='IL'] <- 'Great Lakes'
# df2$InvoiceRegion[df2$InvoiceState=='IN'] <- 'Great Lakes'
# df2$InvoiceRegion[df2$InvoiceState=='OH'] <- 'Great Lakes'
# df2$InvoiceRegion[df2$InvoiceState=='MI'] <- 'Great Lakes'
# 
# df2$InvoiceRegion[df2$InvoiceState=='CA'] <- 'West Coast'
# df2$InvoiceRegion[df2$InvoiceState=='NV'] <- 'West Coast'
# df2$InvoiceRegion[df2$InvoiceState=='AZ'] <- 'West Coast'


df2$carAge <- year(df2$date_most_recent) - df2$VehicleYear
df2$ageGroup=NA
df2$ageGroup[df2$carAge<=3]<-'below 3'
df2$ageGroup[df2$carAge>3&df2$carAge<=15]<-'3-15'
df2$ageGroup[df2$carAge>15&df2$carAge<=27]<-'15-27'
df2$ageGroup[df2$carAge>27&df2$carAge<=40]<-'27-40'
df2$ageGroup[df2$carAge>40]<-'above 40'

df2$isluxury<-NA
df2$isluxury[df2$Make=='Lexus'|df2$Make=='Acura'|df2$Make=='Audi'|df2$Make=='Mercedes-Benz'
            |df2$Make=='BMW'|df2$Make=='Cadillac'|df2$Make=='Infiniti'|df2$Make=='Volvo'|df2$Make=='Lincoln']<-'luxury'
df2$isluxury[-which(df2$Make=='Lexus'|df2$Make=='Acura'|df2$Make=='Audi'|df2$Make=='Mercedes-Benz'
                   |df2$Make=='BMW'|df2$Make=='Cadillac'|df2$Make=='Infiniti'|df2$Make=='Volvo'|df2$Make=='Lincoln')]<-'nonluxury'
# df2 = df2[-which(df2$Brand == 'Cheshire' | df2$Brand == 'FreeServTR' |df2$Brand == 'Kan Rock' | df2$Brand == 'Kramer' | df2$Brand == 'Lloyds' | df2$Brand == 'Skips' | df2$Brand == 'Superior'), ]
df2$MilePerY[df2$carAge>0] <- df2$MileageIn[df2$carAge>0]/df2$carAge[df2$carAge>0]
df2$MilePerY[df2$carAge==0] <- df2$MileageIn[df2$carAge==0]
df2$Car_Type[df2$Car_Type=="Motorcycle"] <- NA
df2 <- df2[-c(which(df2$Gender=='')),]

# write.table(df, file="rfm_last_car.csv", sep=",", append=FALSE, row.names = FALSE)

library(dplyr)
product_per_person = df[, c(4,7, 19:24)] %>% 
  group_by(IndividualId) %>%
  mutate(total_tire = sum(TirePurchase)) %>%
  mutate(total_brake = sum(BrakeCandidate)) %>%
  mutate(total_flush = sum(FlushCandidate)) %>%
  mutate(total_oc = sum(OCCandidate)) %>%
  mutate(total_state_insp = sum(StateInspCandidate)) %>%
  mutate(car_num = length(unique(VehicleId)))
product_per_person = arrange(product_per_person,IndividualId,VehicleId)
product_per_person = product_per_person[!duplicated(product_per_person[,1:2]),]
names(df2)[1] <- "IndividualId"

product_per_person <- merge(product_per_person,df2[,c(1:2,13,18,21)],by="IndividualId")
# df[df$IndividualId=="1000013520914"]write.csv(product_per_person,"product_person.csv")



## method 2: keep all cars info
cust_allcars = a[!duplicated(a$VehicleId),]
cust_allcars = merge(cust_allcars, veh27, by='VehicleId')
cust_allcars = cust_allcars[, -c(3,4,12)]
names(cust_allcars)[2] = 'customer_id'
rfm_allcars = merge(segments, cust_allcars[, 2:9], by = 'customer_id')
table(rfm_allcars$segment)
# write.table(rfm_allcars, file="rfm_allcars.csv", sep=",", append=FALSE, row.names = FALSE)

b <- segments[,c(1,2,7)]


