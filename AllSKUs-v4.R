# enter your directory where you have placed your data 
setwd("/Users/jimhoover/Downloads")
setwd("/Users/jimhoover/Library/CloudStorage/OneDrive-UniversityofFlorida/Research Forecasting")
setwd("C:/Users/hooverjh.UFAD/OneDrive - University of Florida/Research Forecasting")
setwd("/Users/hooverjh/Library/CloudStorage/OneDrive-UniversityofFlorida/Research Forecasting") # mac pro
setwd("/Users/jimhoover/Library/CloudStorage/OneDrive-UniversityofFlorida/Research Forecasting") # mac studio

# load libraries
library(tidyverse)
library(openxlsx)
library(fpp3)
library(scales)
library(lubridate)
library(tidyquant)
library(zoo)
library(RcppRoll)
library(tidyquant)

# Kaggle Website for data
# https://www.kaggle.com/competitions/m5-forecasting-accuracy/data?select=sales_train_evaluation.csv

mydata <- read.csv("sales_train_validation.csv")
# set rand number starting point for replication
set.seed(365)

# set up recording table
rowNum <- 1
id <- NA
metrics_table <- data.frame(id)
metrics_table$pct_acc <- NA
metrics_table$Avg_Inv_OH <- NA
metrics_table$BO_Per_Pct <- NA
metrics_table$fill_rate <- NA
metrics_table$Inv_holding_cost <- NA

for(n in 19:19){
sku <- mydata$id[n]
justone <- mydata %>% filter(id==mydata[n,1])
justone_t <- data.frame(t(justone))
justone_t <- data.frame(justone_t[-(1:6),])
colnames(justone_t) <- "demand"
justone_t$demand <- as.numeric(justone_t$demand)
justone_t <- rownames_to_column(justone_t)
colnames(justone_t)[1] <- "period"
# convert the period variable into an integer to set up the tsibble index
justone_t$period <- as.integer(justone_t$period)

# setting up the time series in tibble format
# justone_t <- tibble(justone_t)
demand1 <- as_tsibble(justone_t,
                      index = period)



# dates from the dataset calendar csv file
# https://www.kaggle.com/competitions/m5-forecasting-accuracy/data
x <- seq(as.Date("2011-01-29"), as.Date("2016-04-24"), by=1)
demand1$date <- x

# change index to date
demand1 <- as_tsibble(demand1,
                      index = date)

# create a new data frame and summarize demand to week
demand_wk <- demand1 %>%
  index_by(Week = ~ yearweek(.)) %>%
  summarize(wkTotal = sum(demand))

# create a new data frame and summarize demand to month
demand_mon <- demand1 %>%
  index_by(Mon = ~yearmonth(.)) %>%
  summarize(monTotal = sum(demand))

# create a new data frame and summarize demand by year
demand_yr <- demand1 %>%
  index_by(Year = ~year(.)) %>%
  summarize(yrTotal = sum(demand))


######################### 5/22
# implement the decreasing forecast
demandwfor <- demand1

# create the new column in the tsibble for the 50% forecast
# https://tibble.tidyverse.org/reference/add_column.html
# demandwfor <- demandwfor[,-4]
demandwfor <- demandwfor %>% relocate(date, .after = period)
demandwfor <- demandwfor %>% add_column(fifty = NA, forty = NA,
                                        thirty = NA, twenty = NA,
                                        ten = NA, five = NA, two = NA,
                                        one = NA, perfect = NA,
                                        resid50 = NA, resid40 =NA,
                                        resid30 = NA, resid20 = NA,
                                        resid10 = NA, resid5 = NA,
                                        resid2 = NA, resid1 = NA,
                                        residperfect = NA)
pct_j = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.02, 0.01, 0.0)
# length(pct_j)
demand_vec <- demandwfor$demand
d_for <- c()
rnd <- c()
rnd2 <- c()
signInd <- c()
set.seed(62363)

for(j in 1:length(pct_j)) {
    pct <- pct_j[j]
  for(i in 1:length(demand_vec)) {
    d <- demand_vec[i]
    rnd[i] <- runif(1)
    rnd2[i] <- runif(1)
    signInd[i] <- ifelse(rnd[i] >= 0.5, 1, 0)
    d_for[i] <- ifelse(signInd[i] == 1, d+(pct * d), d - (pct * d))
    d_for[i] <- ifelse(d_for[i] <0, 0, d_for[i])
    d_for[i] <- round(d_for[i], digits = 0)
    demandwfor[i,(j+3)] <- d_for[i]
    demandwfor[i,(j+12)] <- demand_vec[i]-d_for[i]
  }
}




############################################### 5/22
# calculate the forecast accuracy measures

demandwfor <- demandwfor %>%
  add_column(fiftyMAD = NA, fortyMAD = NA,
             thirtyMAD = NA, twentyMAD = NA,
             tenMAD = NA, fiveMAD = NA, twoMAD = NA,
             oneMAD = NA, perfectMAD = NA)


# traditional MAD
MAD50 <- c()
MAD50 <- sum(abs(demandwfor$resid50)) / length(demandwfor$resid50)


MADalpha <- 0.25
r_lead_time <- 7
# R_sl_factor <- 1.3 # this comes from a table in SAP (service level factor)
alpha_for_mad <- 0.3
n_initialization_period_mad <- 3
forecast_period_days <- 7


# calculate SAP MAD value
# create a vector of the mad values

for(j in 22:30){
  for(i in 1:length(demandwfor$date)){
    if(i == 1){
      demandwfor[i,j] <- (1.0 * abs(demandwfor[i,(j-9)]))
    } else {
      if(i == 2){
        demandwfor[i,j] <- (0.6 * abs(demandwfor[(i-1),(j-9)]) +
                       0.4 * abs(demandwfor[i,(j-9)]))
      } else {
        if( i==3) {
          demandwfor[i,j] <- (0.4 * abs(demandwfor[(i-2),(j-9)]) + 
                         0.4 * abs(demandwfor[(i-1),(j-9)]) +
                         0.2 * abs(demandwfor[i,(j-9)]))
        } else {
          if(i>=4){
            demandwfor[i,j] <- MADalpha * abs(demandwfor[i,(j-9)]) +
              (1-MADalpha) * demandwfor[(i-1),(j)]
          }
        }  
      }
    }
  }
}


# observe the MADs over time
ggplot(demandwfor, aes(date, fiveMAD))+
         geom_line()

# SAP Formula Number 17 Safety Stock Calculation
# modify the monthly demand forecast to create the expected 12 months demand each month
# assumes monthly recalculation of EOQ
demand_mon <- as_tsibble(demand_mon)
demand_mon_recalc <- demand_mon

# this code makes the expected demand equivalent in period 1 (with 2)
# and the last period with the period before
# this ensures that the expected demand only uses full months
# this in necessary to simulate the monthly recalculation of EOQ
for(i in 1:length(demand_mon_recalc$Mon)){
  if(i==1){
    demand_mon_recalc$monTotal[i] <- demand_mon_recalc$monTotal[i+1]
  } else {
    if(i==(length(demand_mon_recalc$Mon))){
      demand_mon_recalc$monTotal[i] <- demand_mon_recalc$monTotal[i-1]
    }
  }
}

# calculate the forecast using historical annual demand (simulation)
demand_mon_recalc$ann_demand <- NA
for(i in 1:length(demand_mon_recalc$Mon)){
  if(i <= 12){
    demand_mon_recalc$ann_demand[i] <- demand_mon_recalc$monTotal[i] * 12
  } else {
    if(i > 12) {
      demand_mon_recalc$ann_demand[i] <- sum(demand_mon_recalc$monTotal[c((i-12):(i-1))]) -
                                             demand_mon_recalc$monTotal[(i-12)] +
                                             demand_mon_recalc$monTotal[i]
    }
  }
}
      


# calculate EOQ 
demand_mon_recalc$EOQ <- NA
Ordering_Cost <- 1.00
Holding_Cost_per_year_per_unit <- 1.00
Cost_per_item <- 1.00
k <- 1.96 # the quantile of the normal distribution of the effective service levelduring the replenishment lead time
RLT <-  7 # reorder lead time in SAP APO, you can change this assumption
ESL_RLT <- NA 
SFT <- NA    # safety stock calculation in SAP APO using the normal distribution
for(i in 1:length(demand_mon_recalc$ann_demand)){
  demand_mon_recalc$EOQ[i] <- round(sqrt((2*Ordering_Cost*demand_mon_recalc$ann_demand[i])/Holding_Cost_per_year_per_unit))
}

#############################
# get the last day of each month's MAD by resid
# https://stackoverflow.com/questions/31528981/select-first-and-last-row-from-grouped-data

SD_mon <- demandwfor
SD_mon$Mon <- floor_date(demandwfor$date, "month")
SD_mon <- SD_mon %>%
  group_by(Mon) %>%
  mutate(SDfifty = sd(fifty),
         SDforty = sd(forty),
         SDthirty = sd(thirty),
         SDtwenty = sd(twenty),
         SDten = sd(ten),
         SDfive = sd(five),
         SDtwo = sd(two),
         SDone = sd(one),
         SDperfect = sd(perfect)) %>%
  ungroup()

SD_mon <- SD_mon %>%
  group_by(Mon) %>%
  mutate(SD_MAD50 = fiftyMAD*1.25,
         SD_MAD40 = fortyMAD*1.25,
         SD_MAD30 = thirtyMAD*1.25,
         SD_MAD20 = twentyMAD*1.25,
         SD_MAD10 = tenMAD*1.25,
         SD_MAD5 = fiveMAD*1.25,
         SD_MAD2 = twoMAD*1.25,
         SD_MAD1 = oneMAD*1.25,
         SD_MADperfect = perfectMAD*1.25) %>%
  ungroup()

SD_mon <- SD_mon %>%
  group_by(Mon) %>%
  slice(n()) %>%
  ungroup()

# the mad_mon contains the MAD calculation using SAP formula # 14
# the code below retrieves the last observation of each month
mad_mon <- demandwfor %>%
  index_by(Mon = ~yearmonth(.)) %>%
  slice(n())

# calculate the SS based on the last day of the month MAD
# k <- 1.96 # the quantile of the normal distribution of the effective service levelduring the replenishment lead time
# https://help.sap.com/docs/SAP_S4HANA_ON-PREMISE/af9ef57f504840d2b81be8667206d485/e26db6531de6b64ce10000000a174cb4.html?version=2020.002&q=Forecast%20Formulas
SS_mon <- mad_mon
# from SAP formula 17
qnorm(.95)
R <- 2.06  # this value comes from the service level to Factor R table in the website
r_lead_time <- 7 # this is an assumption based on measurements from the past
forecast_period_days <- 7 # this is based on the configuration of forecasts
W <- r_lead_time / forecast_period_days

SS_mon <- SS_mon %>%
  mutate(SSfifty = round(R * sqrt(W) * fiftyMAD*1.25),
         SSforty = round(R * sqrt(W) * fortyMAD*1.25),
         SSthirty = round(R * sqrt(W) * thirtyMAD*1.25),
         SStwenty = round(R * sqrt(W) * twentyMAD*1.25),
         SSten = round(R * sqrt(W) * tenMAD*1.25),
         SSfive = round(R * sqrt(W) * fiveMAD*1.25),
         SStwo = round(R * sqrt(W) * twoMAD*1.25),
         SSone = round(R * sqrt(W) * oneMAD*1.25),
         SSperfect = round(R * sqrt(W) * perfectMAD*1.25))

ggplot(SS_mon, aes(Mon, SSforty))+
  geom_line()+
  ylim(0,20)

ggplot(SD_mon, aes(fiftyMAD, SDfifty))+
  geom_point()

# calculate the ROP for each period
# first summarize the average daily demand by month
# https://search.r-project.org/CRAN/refmans/tsibble/html/index-by.html
ROP_mon <- demand1 %>%
  index_by(Mon = ~yearmonth(.))
ROP_mon1 <- ROP_mon

# this works, but awkward
ROP_mon <- demand1 %>%
  index_by(Mon = ~yearmonth(.))
ROP_mon1 <- ROP_mon
ROP_count <- data.frame(table(ROP_mon1$Mon))
colnames(ROP_count)[1:2] <- (c("Mon","num"))
ROP_count$Mon <- as.character(ROP_count$Mon)
mydates <- ym(ROP_count$Mon)
ROP_count$Mon <- mydates
ROP_mon <- left_join(ROP_mon1, ROP_count)
ROP_demand <- ROP_mon %>%
  group_by(Mon) %>%
  summarize(tot_demand_mon = sum(demand)) %>%
  ungroup()
ROP_demand <- left_join(ROP_demand, ROP_count)
ROP_demand$AvgDD <- ROP_demand$tot_demand_mon / ROP_demand$num
colnames(ROP_demand)[1] <- "date"
ROP_demand <- ROP_demand %>%
  index_by(Mon = ~yearmonth(.)) %>%
  slice(n())
ROP_demand <- ROP_demand[,-1]
ROP_demand <- ROP_demand[, c(4,1,2,3)]
SS_mon <- left_join(SS_mon, ROP_demand)
SS_mon$DoLT <- round(SS_mon$AvgDD * RLT)

demandwforbu <- demandwfor

###################################
# Calculate the ROP at multiple levels of forecast accuracy
###################################
SS_mon2 <- SS_mon[,c(31:40)]
demandwfor <- demandwfor %>%
  index_by(Mon = ~yearmonth(.))
# join the monthly levels safety stock calculation to demandwfor data frame
demandwfor <- left_join(demandwfor, SS_mon2)

# backup demandwfor
demandwfor_bu <- demandwfor

demandwfor$ROPfifty <- NA
demandwfor$ROPforty <- NA
demandwfor$ROPthirty <- NA
demandwfor$ROPtwenty <- NA
demandwfor$ROPten <- NA
demandwfor$ROPfive <- NA
demandwfor$ROPtwo <- NA
demandwfor$ROPone <- NA
demandwfor$ROPperfect <- NA

# this version of ROP utilizes the calculation from SAP where
# the foreward forecast over leadtime is used vice the monthly 
# levels calculation using days of leadtime demand
demandwfor$ROPfifty <- roll_sum(demandwfor$fifty, forecast_period_days, align = "left", fill = NA)
demandwfor$ROPforty <- roll_sum(demandwfor$forty, forecast_period_days, align = "left", fill = NA)
demandwfor$ROPthirty <- roll_sum(demandwfor$thirty, forecast_period_days, align = "left", fill = NA)
demandwfor$ROPtwenty <- roll_sum(demandwfor$twenty, forecast_period_days, align = "left", fill = NA)
demandwfor$ROPten <- roll_sum(demandwfor$ten, forecast_period_days, align = "left", fill = NA)
demandwfor$ROPfive <- roll_sum(demandwfor$five, forecast_period_days, align = "left", fill = NA)
demandwfor$ROPtwo <- roll_sum(demandwfor$two, forecast_period_days, align = "left", fill = NA)
demandwfor$ROPone <- roll_sum(demandwfor$one, forecast_period_days, align = "left", fill = NA)
demandwfor$ROPperfect <- roll_sum(demandwfor$perfect, forecast_period_days, align = "left", fill = NA)



demandwfor$ROPfifty <- demandwfor$ROPfifty + demandwfor$SSfifty
demandwfor$ROPforty <- demandwfor$ROPforty + demandwfor$SSforty
demandwfor$ROPthirty <- demandwfor$ROPthirty + demandwfor$SSthirty
demandwfor$ROPtwenty <- demandwfor$ROPtwenty + demandwfor$SStwenty
demandwfor$ROPten <- demandwfor$ROPten + demandwfor$SSten
demandwfor$ROPfive <- demandwfor$ROPfive + demandwfor$SSfive
demandwfor$ROPtwo <- demandwfor$ROPtwo + demandwfor$SStwo
demandwfor$ROPone <- demandwfor$ROPone + demandwfor$SSone
demandwfor$ROPperfect <- demandwfor$ROPperfect + demandwfor$SSperfect

demandwforbu <- demandwfor

# final days 
# column numbers
a <- length(demandwfor$date)-(forecast_period_days-1)
b <- length(demandwfor$date)
c = 41
d = 49

for(i in a:b){
  for(j in c:d){
    demandwfor[i,j]<- demandwfor[i-1,j]
  }
}


mon_levels <- SS_mon[,-c(1:30)]
# add the calculated EOQ to the mon_levels
EOQ <- demand_mon_recalc %>%
  select(Mon, EOQ)
mon_levels <- left_join(mon_levels, EOQ)
# write.xlsx(mon_levels, "mon_levels.xlsx")

ROPs <- demandwfor[,c(2, 41:49)]


############################################
# calculate the inventory position each period
############################################

for(z in 26:34){
    # rowNum <- rowNum+1
    # read in the demand stream
    ip_calc <- demand1 %>%
      select(date, demand)
    # add the Mon column to the data frame for calculations
    ip_calc <- ip_calc %>%
      index_by(Mon = ~yearmonth(.))
    # add important calculation variables to the ip_calc data frame (tsibble)
    ip_calc <- ip_calc[, c(3,1,2)]
    ip_calc$beg_inv <- 0
    ip_calc$end_inv <- 0
    ip_calc$back_ord <- 0
    ip_calc$onOrder <- 0
    ip_calc$deliveries <- 0
    ip_calc$placed_on_order <- 0
    ip_calc$ip <- 0
    ip_calc$ROQ <- 0
    ip_calc <- left_join(ip_calc, mon_levels)
    ip_calc <- left_join(ip_calc, EOQ)
    ip_calc <- left_join(ip_calc, ROPs)
    ip_calc$ROQ <- ip_calc$EOQ
    
    
    
    #####################################
    # perform inventory simulation and capture metrics
    #####################################
    
    # ip calculations for initial period
    init_inv <- as.numeric(ip_calc[1, z] + ip_calc[1 , z])
    ip_calc$beg_inv[1] <- init_inv
    ip_calc$end_inv[1] <- ip_calc$beg_inv[1] - ip_calc$demand[1] + ip_calc$deliveries[1]
    # backorder logic
    if(ip_calc$end_inv[1] <0){
      ip_calc$back_ord[1] <- -ip_calc$end_inv[1]
      ip_calc$end_inv[1] <- 0
      }
    # initial ip in period 1
    ip_calc$ip[1] <- ip_calc$end_inv[1]-ip_calc$back_ord[1]
    # reorder calculations
    if(ip_calc$ip[1] <= ip_calc[1,z]){
      ip_calc$placed_on_order[1] <- ip_calc$ROQ[1]
      ip_calc$deliveries[1+RLT] <- ip_calc$placed_on_order[1]
      ip_calc$onOrder[1] <- ip_calc$onOrder[1] + ip_calc$placed_on_order[1]
      }
    
    #######################################
    # now calculate the remaining days
    # 
    for(i in 2:length(ip_calc$date)){
      ip_calc$beg_inv[i] <- ip_calc$end_inv[i-1]+ip_calc$deliveries[i]
      ip_calc$end_inv[i] <- ip_calc$beg_inv[i] - ip_calc$back_ord[i-1] - ip_calc$demand[i]
      ip_calc$onOrder[i] <- ip_calc$onOrder[i-1] - ip_calc$deliveries[i]
      # backorder check
        if(ip_calc$end_inv[i] <0){
          ip_calc$back_ord[i] <- -ip_calc$end_inv[i]
          ip_calc$end_inv[i] <- 0 }
      # update the inventory position
      ip_calc$ip[i] <- ip_calc$end_inv[i] - ip_calc$back_ord[i] + ip_calc$onOrder[i] - ip_calc$deliveries[i]
      
      # make the reorder decision and update on order and the inventory position
      if(ip_calc$ip[i] <= ip_calc[i, z]){
        ip_calc$placed_on_order[i] <- ip_calc$ROQ[i]
        ip_calc$onOrder[i] <- ip_calc$onOrder[i-1] + ip_calc$placed_on_order[i] - ip_calc$deliveries[i]
        ip_calc$ip[i] <- ip_calc$ip[i] + ip_calc$placed_on_order[i]
        if(i+ RLT <= length(ip_calc$date)){
          ip_calc$deliveries[i+RLT] <- ip_calc$placed_on_order[i]
        
        }
            # add the metrics into the data frame
    
    
    ################################################################
    # create the metrics calculations for each level of forecast accuracy
    for_metrics <- ip_calc[c(30:1907),]
    total_periods <- length(for_metrics$date)
    for_metrics$avg_inv <- ((for_metrics$beg_inv + for_metrics$end_inv)/2)
    for_metrics$avg_inv_365 <- rollmean(for_metrics$avg_inv, 365, align = 'right', fill = NA)
    avg_inv_oh <- round(for_metrics$avg_inv_365[length(for_metrics$date)],2)
    back_order_periods <- sum(for_metrics$back_ord >0)
    back_order_percent <- round((back_order_periods / total_periods),4)
    fill_rate <- round(1-(sum(for_metrics$back_ord) / sum(for_metrics$demand)),4)
    number_orders <- sum(for_metrics$placed_on_order>0)
    inventory_cost <- round(avg_inv_oh * Holding_Cost_per_year_per_unit + 
                              number_orders * Ordering_Cost, 2)
    
    
    metrics_table[rowNum, 1] <- sku
    metrics_table[rowNum,2] <- pct_j[z-25]
    metrics_table[rowNum, 3] <- avg_inv_oh
    metrics_table[rowNum, 4] <- back_order_percent
    metrics_table[rowNum, 5] <- fill_rate
    metrics_table[rowNum, 6] <- inventory_cost
    
      }
    }
    rowNum <- rowNum+1
# the end of the single sku loop
}
################################

# the end of the multiple sku (n) loop
}
################################

write.xlsx(metrics_table, "metrics_table.xlsx")

for_plot <- ip_calc[c(30:1907),]
total_periods <- length(for_plot$date)
for_plot$avg_inv <- ((for_plot$beg_inv + for_plot$end_inv) / 2)
avg_inv_oh <- round(mean(for_plot$avg_inv),0)
back_order_periods <- sum(for_plot$back_ord >0)
back_order_percent <- round(back_order_periods / length(for_plot$date),4)
fill_rate <- round(1-(sum(for_plot$back_ord) / sum(for_plot$demand)),2)
number_orders <- sum(for_plot$placed_on_order>0)
inventory_cost <- round(avg_inv_oh * Holding_Cost_per_year_per_unit + 
                          number_orders * Ordering_Cost, 2)
for_plot$avg_inv <- (for_plot$beg_inv + for_plot$end_inv)/2
rop50 <- round(mean(for_plot$ROPfifty),0)
rop20 <- round(mean(for_plot$ROPtwenty),0)
ropperfect <- round(mean(for_plot$ROPperfect),0)
ss50 <- round(mean(for_plot$SSfifty),0)
ss20 <- round(mean(for_plot$SStwenty),0)
ssperfect <- round(mean(for_plot$SSperfect),0)
EOQ_Average <- round(mean(for_plot$EOQ),0)
backorder_qty <- sum(for_plot$back_ord)
backorder_qty_pct <- round(backorder_qty / sum(for_plot$demand),4)
safety_stock <- round(mean(for_plot$SSperfect),0)






ggplot(for_plot, aes(date, avg_inv))+
  geom_line(color = 'lightgray')+
  geom_ma(ma_fun = SMA, n = 30, color = 'red')+
  geom_ma(ma_fun = SMA, n = 365, color = 'blue')+
  ylim(0, 100) + 
  labs(x="Date", y="Average Inventory")


ggplot(for_plot, aes(date, demand))+
  geom_line(color = 'lightgray')+
  geom_ma(ma_fun = SMA, n = 30, color = 'red')+
  geom_ma(ma_fun = SMA, n = 365, color = 'blue')+
  ylim(0, 100) + 
  labs(x="Date", y="Demand")
















