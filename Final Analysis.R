library(dplyr)
library(ggplot2)

#define number of datapoints
runs <- 9099

#set up equation to get the crash speed. We had run # collisions at these different speeds.
coll_spd<- data.frame(runif(runs, 1, 30))

#setup linear effectiveness based on speed
rb_spd <- function(speed) {
  mean_rb <- (0.9 - 1)/21 * speed + 1.0
  if(mean_rb < 0.0) {mean_rb <- 0.0}
  return(mean_rb)
}

tint_spd <- function(speed) {
  mean_tint <- (0.8 - 1)/21 * speed + 1.0
  if(mean_tint < 0.0) {mean_tint <- 0.0}
  return(mean_tint)
}

uhmw_spd <- function(speed) {
  mean_uhmw <- (0.77 - 1)/21 * speed + 1.0
  if(mean_uhmw < 0.0) {mean_uhmw <- 0.0}
  return(mean_uhmw)
}

rails_spd <- function(speed) {
  mean_rails <- (0.75 - 1)/21 * speed + 1.0
  if(mean_rails < 0.0) {mean_rails <- 0.0}
  return(mean_rails)
}

est_spd <- function(speed) {
  mean_est <- (0.97 - 1)/21 * speed + 1.0
  if(mean_est < 0.0) {mean_est <- 0.0}
  return(mean_est)
}

recall_spd <- function(speed) {
  mean_recall <- (0.99 - 1)/21 * speed + 1.0
  if(mean_recall < 0.0) {mean_recall <- 0.0}
  return(mean_recall)
}

#setup and sample normal distributions
#the normal distribution here represents uncertainty in the circumstances of the crash based on our ambiguous imperfect testing. 
#for each collision speed, we have to see how the design fared
rubber_bladder <- data.frame()
tank_in_tank <- data.frame()
uhmw_shield <- data.frame()
side_rails <- data.frame()
estimate_1 <- data.frame()
recall <- data.frame()
for(x in 1:runs)
{
  rubber_bladder[x,1] <- data.frame(rnorm(1, mean = rb_spd(coll_spd[x,1]), sd = 0.06))
  tank_in_tank[x,1] <- data.frame(rnorm(1, mean = tint_spd(coll_spd[x,1]), sd = 0.08))
  uhmw_shield[x,1] <- data.frame(rnorm(1, mean = uhmw_spd(coll_spd[x,1]), sd = 0.12))
  side_rails[x,1] <- data.frame(rnorm(1, mean = rails_spd(coll_spd[x,1]), sd = 0.1))
  estimate_1[x,1] <- data.frame(rnorm(1, mean = est_spd(coll_spd[x,1]), sd = 0.05))
  recall[x,1] <- data.frame(rnorm(1, mean = recall_spd(coll_spd[x,1]), sd = 0.03))
}

#this gives us the likelihood that the chosen design was effective at this crash speed.
head(rubber_bladder)

#let's roll the dice here and see how lucky each person was in their crash
percentage <- data.frame(runif(runs, 0, 1))
head(percentage)
result_RB <- percentage
result_SR <- percentage
result_UHMW <- percentage
result_TINT <- percentage
result_EST <- percentage
result_RECALL <- percentage


#assess if that person was "safe" in this design
percentage_fails_rb <- data.frame()
percentage_fails_sr <- data.frame()
percentage_fails_tint <- data.frame()
percentage_fails_uhmw <- data.frame()
percentage_fails_est <- data.frame()
percentage_fails_recall <- data.frame()
death_limit_rb <- data.frame()
death_limit_sr <- data.frame()
death_limit_tint <- data.frame()
death_limit_uhmw <- data.frame()
death_limit_est <- data.frame()
death_limit_recall <- data.frame()
for (x in 1:runs) {
  #but if they are outside that percent, injuries are 10x as likely as deaths, so   divide up the remaining space
  percentage_fails_rb[x,1] <- 1 - rubber_bladder[x,1]
  percentage_fails_tint[x,1] <- 1 - tank_in_tank[x,1]
  percentage_fails_sr[x,1] <- 1 - side_rails[x,1]
  percentage_fails_uhmw[x,1] <- 1 - uhmw_shield[x,1]
  percentage_fails_est[x,1] <- 1 - estimate_1[x,1]
  percentage_fails_recall[x,1] <- 1 - recall[x,1]
  death_limit_rb[x,1] <- 1 - (percentage_fails_rb[x,1]/11)
  death_limit_sr[x,1] <- 1 - (percentage_fails_sr[x,1]/11)
  death_limit_tint[x,1] <- 1 - (percentage_fails_tint[x,1]/11)
  death_limit_uhmw[x,1] <- 1 - (percentage_fails_uhmw[x,1]/11)
  death_limit_est[x,1] <- 1 - (percentage_fails_est[x,1]/11)
  death_limit_recall[x,1] <- 1 - (percentage_fails_recall[x,1]/11)
  head(death_limit_rb)
  
  #assess if injury or death based on our single crash
  if(percentage[x,1] > death_limit_rb[x,1]) {result_RB[x,1] <- data.frame("Death")} else if(percentage[x,1] > rubber_bladder[x,1]){result_RB[x,1] <- data.frame("Injury")} else{result_RB[x,1] <- data.frame("No Injury")}
  
  if(percentage[x,1] > death_limit_sr[x,1]) {result_SR[x,1] <- data.frame("Death")} else if(percentage[x,1] > side_rails[x,1]){result_SR[x,1] <- data.frame("Injury")} else{result_SR[x,1] <- data.frame("No Injury")}
  
  if(percentage[x,1] > death_limit_tint[x,1]) {result_TINT[x,1] <- data.frame("Death")} else if(percentage[x,1] > tank_in_tank[x,1]){result_TINT[x,1] <- data.frame("Injury")} else{result_TINT[x,1] <- data.frame("No Injury")}
  
  if(percentage[x,1] > death_limit_uhmw[x,1]) {result_UHMW[x,1] <- data.frame("Death")} else if(percentage[x,1] > uhmw_shield[x,1]){result_UHMW[x,1] <- data.frame("Injury")} else{result_UHMW[x,1] <- data.frame("No Injury")}
  
  if(percentage[x,1] > death_limit_est[x,1]) {result_EST[x,1] <- data.frame("Death")} else if(percentage[x,1] > estimate_1[x,1]){result_EST[x,1] <- data.frame("Injury")} else{result_EST[x,1] <- data.frame("No Injury")}
  
  if(percentage[x,1] > death_limit_recall[x,1]) {result_RECALL[x,1] <- data.frame("Death")} else if(percentage[x,1] > recall[x,1]){result_RECALL[x,1] <- data.frame("Injury")} else{result_RECALL[x,1] <- data.frame("No Injury")}
}

RB_All <- data.frame(coll_spd, rubber_bladder, percentage, death_limit_rb, result_RB)
NewNames <- c('Speed (mph)', 'Design Likelihood Success', 'Percent Draw', 'Death Range', 'Accident Result')
names(RB_All) <- NewNames

SR_All <- data.frame(coll_spd, side_rails, percentage, death_limit_sr, result_SR)
names(SR_All) <- NewNames

TINT_All <- data.frame(coll_spd, tank_in_tank, percentage, death_limit_tint, result_TINT)
names(TINT_All) <- NewNames

UHMW_All <- data.frame(coll_spd, uhmw_shield, percentage, death_limit_uhmw, result_UHMW)
names(UHMW_All) <- NewNames

EST_All <- data.frame(coll_spd, estimate_1, percentage, death_limit_est, result_EST)
names(EST_All) <- NewNames

RECALL_All <- data.frame(coll_spd, recall, percentage, death_limit_recall, result_RECALL)
names(RECALL_All) <- NewNames

Grouped_RB <- RB_All %>% group_by(`Accident Result`) %>% count(`Accident Result`)
Grouped_SR <- SR_All %>% group_by(`Accident Result`) %>% count(`Accident Result`)
Grouped_TINT <- TINT_All %>% group_by(`Accident Result`) %>% count(`Accident Result`)
Grouped_UHMW <- UHMW_All %>% group_by(`Accident Result`) %>% count(`Accident Result`)
Grouped_EST <- EST_All %>% group_by(`Accident Result`) %>% count(`Accident Result`)
Grouped_RECALL <- RECALL_All %>% group_by(`Accident Result`) %>% count(`Accident Result`)

#new number of simulations for high speed
runs <- 14845

#set up data for high speed collisions
rb_lspdeff <- rb_spd(30)
sr_lspdeff <- rails_spd(30)
est_lspdeff <- est_spd(30)
recall_lspdeff <- recall_spd(30)
uhmw_lspdeff <- uhmw_spd(30)
tint_lspdeff <- tint_spd(30)

#set up high speed effectiveness functions
rb_hspd <- function(speed) {
  mean_rb <- (-1 * rb_lspdeff / 20) * speed + (rb_lspdeff + rb_lspdeff*30/20)
  if(mean_rb < 0.0) {mean_rb <- 0.0}
  return(mean_rb)
}

tint_hspd <- function(speed) {
  mean_tint <- (-1 * tint_lspdeff / 20) * speed + (tint_lspdeff + tint_lspdeff*30/20)
  if(mean_tint < 0.0) {mean_tint <- 0.0}
  return(mean_tint)
}

uhmw_hspd <- function(speed) {
  mean_uhmw <- (-1 * uhmw_lspdeff / 20) * speed + (uhmw_lspdeff + uhmw_lspdeff*30/20)
  if(mean_uhmw < 0.0) {mean_uhmw <- 0.0}
  return(mean_uhmw)
}

rails_hspd <- function(speed) {
  mean_rails <- (-1 * sr_lspdeff / 20) * speed + (sr_lspdeff + sr_lspdeff*30/20)
  if(mean_rails < 0.0) {mean_rails <- 0.0}
  return(mean_rails)
}

est_hspd <- function(speed) {
  mean_est <- (-1 * est_lspdeff / 20) * speed + est_lspdeff + est_lspdeff*30/20
  if(mean_est < 0.0) {mean_est <- 0.0}
  return(mean_est)
}

recall_hspd <- function(speed) {
  mean_recall <- (-1 * recall_lspdeff / 20) * speed + (recall_lspdeff + recall_lspdeff*30/20)
  if(mean_recall < 0.0) {mean_recall <- 0.0}
  return(mean_recall)
}

#simulate in the same way as the low speed collisions above
hcoll_spd<- data.frame(runif(runs, 31, 50))

#setup and sample normal distributions
#the normal distribution here represents uncertainty in the circumstances of the crash based on our ambiguous imperfect testing. 
#for each collision speed, we have to see how the design fared
hrubber_bladder <- data.frame()
htank_in_tank <- data.frame()
huhmw_shield <- data.frame()
hside_rails <- data.frame()
hestimate_1 <- data.frame()
hrecall <- data.frame()
for(x in 1:runs)
{
  hrubber_bladder[x,1] <- data.frame(rnorm(1, mean = rb_hspd(hcoll_spd[x,1]), sd = 0.1))
  htank_in_tank[x,1] <- data.frame(rnorm(1, mean = tint_hspd(hcoll_spd[x,1]), sd = 0.1))
  huhmw_shield[x,1] <- data.frame(rnorm(1, mean = uhmw_hspd(hcoll_spd[x,1]), sd = 0.25))
  hside_rails[x,1] <- data.frame(rnorm(1, mean = rails_hspd(hcoll_spd[x,1]), sd = 0.25))
  hestimate_1[x,1] <- data.frame(rnorm(1, mean = est_hspd(hcoll_spd[x,1]), sd = 0.05))
  hrecall[x,1] <- data.frame(rnorm(1, mean = recall_hspd(hcoll_spd[x,1]), sd = 0.05))
}

#this gives us the likelihood that the chosen design was effective at this crash speed.
head(hrubber_bladder)

#let's roll the dice here and see how lucky each person was in their crash
percentage <- data.frame(runif(runs, 0, 1))
head(percentage)
hresult_RB <- percentage
hresult_SR <- percentage
hresult_UHMW <- percentage
hresult_TINT <- percentage
hresult_EST <- percentage
hresult_RECALL <- percentage


#assess if that person was "safe" in this design
hpercentage_fails_rb <- data.frame()
hpercentage_fails_sr <- data.frame()
hpercentage_fails_tint <- data.frame()
hpercentage_fails_uhmw <- data.frame()
hpercentage_fails_est <- data.frame()
hpercentage_fails_recall <- data.frame()
hdeath_limit_rb <- data.frame()
hdeath_limit_sr <- data.frame()
hdeath_limit_tint <- data.frame()
hdeath_limit_uhmw <- data.frame()
hdeath_limit_est <- data.frame()
hdeath_limit_recall <- data.frame()

for (x in 1:runs) {
  hpercentage_fails_rb[x,1] <- 1 - hrubber_bladder[x,1]
  hpercentage_fails_tint[x,1] <- 1 - htank_in_tank[x,1]
  hpercentage_fails_sr[x,1] <- 1 - hside_rails[x,1]
  hpercentage_fails_uhmw[x,1] <- 1 - huhmw_shield[x,1]
  hpercentage_fails_est[x,1] <- 1 - hestimate_1[x,1]
  hpercentage_fails_recall[x,1] <- 1 - hrecall[x,1]
  hdeath_limit_rb[x,1] <- 1 - (hpercentage_fails_rb[x,1]/11)
  hdeath_limit_sr[x,1] <- 1 - (hpercentage_fails_sr[x,1]/11)
  hdeath_limit_tint[x,1] <- 1 - (hpercentage_fails_tint[x,1]/11)
  hdeath_limit_uhmw[x,1] <- 1 - (hpercentage_fails_uhmw[x,1]/11)
  hdeath_limit_est[x,1] <- 1 - (hpercentage_fails_est[x,1]/11)
  hdeath_limit_recall[x,1] <- 1 - (hpercentage_fails_recall[x,1]/11)
  head(hdeath_limit_rb)
  
  #assess if injury or death based on our single crash
  if(percentage[x,1] > hdeath_limit_rb[x,1]) {hresult_RB[x,1] <- data.frame("Death")} else if(percentage[x,1] > hrubber_bladder[x,1]){hresult_RB[x,1] <- data.frame("Injury")} else{hresult_RB[x,1] <- data.frame("No Injury")}
  
  if(percentage[x,1] > hdeath_limit_sr[x,1]) {hresult_SR[x,1] <- data.frame("Death")} else if(percentage[x,1] > hside_rails[x,1]){hresult_SR[x,1] <- data.frame("Injury")} else{hresult_SR[x,1] <- data.frame("No Injury")}
  
  if(percentage[x,1] > hdeath_limit_tint[x,1]) {hresult_TINT[x,1] <- data.frame("Death")} else if(percentage[x,1] > htank_in_tank[x,1]){hresult_TINT[x,1] <- data.frame("Injury")} else{hresult_TINT[x,1] <- data.frame("No Injury")}
  
  if(percentage[x,1] > hdeath_limit_uhmw[x,1]) {hresult_UHMW[x,1] <- data.frame("Death")} else if(percentage[x,1] > huhmw_shield[x,1]){hresult_UHMW[x,1] <- data.frame("Injury")} else{hresult_UHMW[x,1] <- data.frame("No Injury")}
  
  if(percentage[x,1] > hdeath_limit_est[x,1]) {hresult_EST[x,1] <- data.frame("Death")} else if(percentage[x,1] > hestimate_1[x,1]){hresult_EST[x,1] <- data.frame("Injury")} else{hresult_EST[x,1] <- data.frame("No Injury")}
  
  if(percentage[x,1] > hdeath_limit_recall[x,1]) {hresult_RECALL[x,1] <- data.frame("Death")} else if(percentage[x,1] > hrecall[x,1]){hresult_RECALL[x,1] <- data.frame("Injury")} else{hresult_RECALL[x,1] <- data.frame("No Injury")}
}

HRB_All <- data.frame(hcoll_spd, hrubber_bladder, percentage, hdeath_limit_rb, hresult_RB)
NewNames <- c('Speed (mph)', 'Design Likelihood Success', 'Percent Draw', 'Death Range', 'Accident Result')
names(HRB_All) <- NewNames

HSR_All <- data.frame(hcoll_spd, hside_rails, percentage, hdeath_limit_sr, hresult_SR)
names(HSR_All) <- NewNames

HTINT_All <- data.frame(hcoll_spd, htank_in_tank, percentage, hdeath_limit_tint, hresult_TINT)
names(HTINT_All) <- NewNames

HUHMW_All <- data.frame(hcoll_spd, huhmw_shield, percentage, hdeath_limit_uhmw, hresult_UHMW)
names(HUHMW_All) <- NewNames

HEST_All <- data.frame(hcoll_spd, hestimate_1, percentage, hdeath_limit_est, hresult_EST)
names(HEST_All) <- NewNames

HRECALL_All <- data.frame(hcoll_spd, hrecall, percentage, hdeath_limit_recall, hresult_RECALL)
names(HRECALL_All) <- NewNames

Grouped_HRB <- HRB_All %>% group_by(`Accident Result`) %>% count(`Accident Result`)
Grouped_HSR <- HSR_All %>% group_by(`Accident Result`) %>% count(`Accident Result`)
Grouped_HTINT <- HTINT_All %>% group_by(`Accident Result`) %>% count(`Accident Result`)
Grouped_HUHMW <- HUHMW_All %>% group_by(`Accident Result`) %>% count(`Accident Result`)
Grouped_HEST <- HEST_All %>% group_by(`Accident Result`) %>% count(`Accident Result`)
Grouped_HRECALL <- HRECALL_All %>% group_by(`Accident Result`) %>% count(`Accident Result`)

#calculate reputation
reputation <- function(data1, data2) {
  sum = 0
  for(x in 1:nrow(data1))
  {
    if(data1[x, 5] == 'Death') {sum <- sum + 1/data1[x,1]}
  }
  for(x in 1:nrow(data2))
  {
    if(data2[x, 5] == 'Death') {sum <- sum + 1/data2[x,1]}
  }
  reputation <- -sum
  return(reputation)
}

rb_reputation <- reputation(HRB_All, RB_All)
sr_reputation <- reputation(HSR_All, SR_All)
tint_reputation <- reputation(HTINT_All, TINT_All)
est_reputation <- reputation(HEST_All, EST_All)
uhmw_reputation <- reputation(HUHMW_All, UHMW_All)
recall_reputation <- reputation(HRECALL_All, RECALL_All)

designs_names <- c("Rubber Bladder", "Tank-in-Tank", "Side Rails", "UHMW Supports", "Company Estimate", "Recall")
relative_costs <- c(6/11,5.08/11, 2.35/11, 2.40/11, 8.0/11, 11/11)
low_speed_injury <- c(Grouped_RB[2,2], Grouped_TINT[2,2], Grouped_SR[2,2], Grouped_UHMW[2,2], Grouped_EST[2,2], Grouped_RECALL[2,2])
low_speed_death <- c(Grouped_RB[1,2], Grouped_TINT[1,2], Grouped_SR[1,2], Grouped_UHMW[1,2], Grouped_EST[1,2], Grouped_RECALL[1,2])
high_speed_injury <- c(Grouped_HRB[2,2], Grouped_HTINT[2,2], Grouped_HSR[2,2], Grouped_HUHMW[2,2], Grouped_HEST[2,2], Grouped_HRECALL[2,2])
high_speed_death <- c(Grouped_HRB[1,2], Grouped_HTINT[1,2], Grouped_HSR[1,2], Grouped_HUHMW[1,2], Grouped_HEST[1,2], Grouped_HRECALL[1,2])
reputation <- c(rb_reputation/(-1*sr_reputation), tint_reputation/(-1*sr_reputation), sr_reputation/(-1*sr_reputation), uhmw_reputation/(-1*sr_reputation), est_reputation/(-1*sr_reputation), recall_reputation/(-1*sr_reputation))
comp_mat <- data.frame(cbind(designs_names, relative_costs, low_speed_injury, low_speed_death, high_speed_injury, high_speed_death, reputation))
Names <- c('Design Type', 'Relative Design Costs', 'Rear-End Low Speed Injuries', 'Rear-End Low Speed Deaths', 'Rear-End High Speed Injuries', 'Rear-End High Speed Deaths', 'Relative Company Reputation')
names(comp_mat) <- Names

RB_All = subset(RB_All, select = -c(design_type, collision_type))
SR_All = subset(SR_All, select = -c(design_type, collision_type))
TINT_All = subset(TINT_All, select = -c(design_type, collision_type))
UHMW_All = subset(UHMW_All, select = -c(design_type, collision_type))
EST_All = subset(EST_All, select = -c(design_type, collision_type))
RECALL_All = subset(RECALL_All, select = -c(design_type, collision_type))
HRB_All = subset(HRB_All, select = -c(V1, collision_type_2))
HSR_All = subset(HSR_All, select = -c(V1, collision_type_2))
HTINT_All = subset(HTINT_All, select = -c(V1, collision_type_2))
HUHMW_All = subset(HUHMW_All, select = -c(V1, collision_type_2))
HEST_All = subset(HEST_All, select = -c(V1, collision_type_2))
HRECALL_All = subset(HRECALL_All, select = -c(V1, collision_type_2))



design_type <- data.frame()
collision_type <- data.frame()
for(i in 1:nrow(RB_All)) {
  design_type[i, 1] <- "Rubber Bladder"
  collision_type[i, 1] <- 'L'
}
RB_All <- cbind(RB_All, design_type, collision_type)
names(RB_All)[6] <- "design_type"
names(RB_All)[7] <- "collision_type"

for(i in 1:nrow(SR_All)) {
  design_type[i, 1] <- "Side Rails"
}
SR_All <- cbind(SR_All, design_type, collision_type)
names(SR_All)[6] <- "design_type"
names(SR_All)[7] <- "collision_type"

for(i in 1:nrow(TINT_All)) {
  design_type[i, 1] <- "Tank-in-Tank"
}
TINT_All <- cbind(TINT_All, design_type, collision_type)
names(TINT_All)[6] <- "design_type"
names(TINT_All)[7] <- "collision_type"

for(i in 1:nrow(UHMW_All)) {
  design_type[i, 1] <- "UHMW"
}
UHMW_All <- cbind(UHMW_All, design_type, collision_type)
names(UHMW_All)[6] <- "design_type"
names(UHMW_All)[7] <- "collision_type"

for(i in 1:nrow(EST_All)) {
  design_type[i, 1] <- "Company Estimate"
}
EST_All <- cbind(EST_All, design_type, collision_type)
names(EST_All)[6] <- "design_type"
names(EST_All)[7] <- "collision_type"

for(i in 1:nrow(RECALL_All)) {
  design_type[i, 1] <- "Recall"
}
RECALL_All <- cbind(RECALL_All, design_type, collision_type)
names(RECALL_All)[6] <- "design_type"
names(RECALL_All)[7] <- "collision_type"

collision_type_2 <- data.frame()
recall_design_type <- data.frame()
rb_design_type <- data.frame()
tint_design_type <- data.frame()
sr_design_type <- data.frame()
uhmw_design_type <- data.frame()
est_desin_type <- data.frame()
for(i in 1:nrow(HRB_All)) {
  collision_type_2 <- "H"
  recall_design_type[i, 1] <- "Recall"
  rb_design_type[i, 1] <- "Rubber Bladder"
  tint_design_type[i, 1] <- "Tank-in-Tank"
  sr_design_type[i, 1] <- "Side Rails"
  uhmw_design_type[i, 1] <- "UHMW"
  est_desin_type[i, 1] <- "Company Estimate"
}
HRECALL_All <- cbind(HRECALL_All, recall_design_type, collision_type_2)
HRB_All <- cbind(HRB_All, rb_design_type, collision_type_2)
HSR_All <- cbind(HSR_All, sr_design_type, collision_type_2)
HTINT_All <- cbind(HTINT_All, tint_design_type, collision_type_2)
HEST_All <- cbind(HEST_All, est_desin_type, collision_type_2)
HUHMW_All <- cbind(HUHMW_All, uhmw_design_type, collision_type_2)
names(HRECALL_All)[6] <- "design_type"
names(HRECALL_All)[7] <- "collision_type"
names(HRB_All)[6] <- "design_type"
names(HRB_All)[7] <- "collision_type"
names(HSR_All)[6] <- "design_type"
names(HSR_All)[7] <- "collision_type"
names(HTINT_All)[6] <- "design_type"
names(HTINT_All)[7] <- "collision_type"
names(HUHMW_All)[6] <- "design_type"
names(HUHMW_All)[7] <- "collision_type"
names(HEST_All)[6] <- "design_type"
names(HEST_All)[7] <- "collision_type"


coll_data <- data.frame()
coll_data <- rbind(RB_All, SR_All, TINT_All, UHMW_All, EST_All, RECALL_All, HRECALL_All, HEST_All, HUHMW_All, HRB_All, HSR_All, HTINT_All)

#plot_spds <- ggplot(data = coll_data, aes(x = `Speed (mph)`, y = `Design Likelihood Success`, color = design_type, alpha = 0.02)) + 
              #geom_point()
#plot_spds

coll_data$`Accident Result` <- factor(coll_data$`Accident Result`)
plot_spds <- ggplot(coll_data, aes(x = design_type, fill = `Accident Result`)) + 
  geom_bar(position = position_dodge()) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle('Accident Results by Design Type') + 
  theme(aspect.ratio=2/3) + 
  xlab("Design Type")
plot_spds

plot_spds <- ggplot(coll_data, aes(x = design_type, fill = `Accident Result`)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle('Accident Results by Design Type') + 
  theme(aspect.ratio=2/3, plot.title = element_text(size = 10)) + 
  xlab("Design Type") + 
  scale_fill_brewer(palette = "Reds")
plot_spds

plot_spds <- ggplot(coll_data, aes(x = `Speed (mph)`, fill = `Speed (mph)`)) + geom_histogram(bins = 20, fill = "brown2", color = "brown3", alpha = 0.8) + 
  ggtitle('Speed and Frequency of Generated Collisions') + 
  theme(aspect.ratio=2/3, plot.title = element_text(size = 12)) + 
  xlab("Design Type") + 
  ylab("Frequency") 
plot_spds

spds_1 <- coll_data %>% filter(`Accident Result` == "No Injury")
spds_2 <- coll_data %>% filter(`Accident Result` == "Injury")
spds_3 <- coll_data %>% filter(`Accident Result` == "Death")

coll_data <- coll_data %>%
  arrange(`Accident Result`) %>%
  mutate(`Accident Result` = factor(`Accident Result`, levels=c("No Injury", "Injury", "Death")))

plot_dist <- ggplot(coll_data, aes(x = design_type, y = `Speed (mph)`, fill = `Accident Result`, color = NA)) + geom_violin(aes(y = `Speed (mph)`, fill = `Accident Result`), bw = 2, scale = "count", position = "identity", alpha = 0.7, size = 0.005) + 
  ggtitle('Accident Results by Collision Speed') + 
  scale_fill_brewer(palette = "Reds") +
  scale_color_brewer(palette = "Reds") +
  theme(aspect.ratio=2/3) + 
  xlab("Design Type") + 
  ylab("Speed") +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 10))
plot_dist

speeds <- data.frame()
rb_eff <- data.frame()
type <- data.frame()
for(x in 1:50){
  speeds[x,1] <- x
  if(x < 30){eff <- rb_spd(x)} else {eff <- rb_hspd(x)}
  rb_eff[x,1] <- eff
  type[x,1] <- "Rubber Bladder"
}
for(x in 51:100){
  speeds[x,1] <- x-50
  if(x-50 < 30){eff <- rails_spd(x - 50)} else {eff <- rails_hspd(x - 50)}
  rb_eff[x,1] <- eff
  type[x,1] <- "Side Rail"
}
for(x in 101:150){
  speeds[x,1] <- x-100
  if(x-100 < 30){eff <- tint_spd(x - 100)} else {eff <- tint_hspd(x - 100)}
  rb_eff[x,1] <- eff
  type[x,1] <- "Tank-in-Tank"
}
for(x in 151:200){
  speeds[x,1] <- x-150
  if(x-150 < 30){eff <- uhmw_spd(x - 150)} else {eff <- uhmw_hspd(x - 150)}
  rb_eff[x,1] <- eff
  type[x,1] <- "UHMW"
}
for(x in 201:250){
  speeds[x,1] <- x-200
  if(x-200 < 30){eff <- est_spd(x - 200)} else {eff <- est_hspd(x - 200)}
  rb_eff[x,1] <- eff
  type[x,1] <- "Company Estimate"
}
for(x in 251:300){
  speeds[x,1] <- x-250
  if(x - 250 < 30){eff <- recall_spd(x - 250)} else {eff <- recall_hspd(x - 250)}
  rb_eff[x,1] <- eff
  type[x,1] <- "Recall"
}
rb_eff <- cbind(speeds, rb_eff, type)
names(rb_eff)[1] <- "Speed"
names(rb_eff)[2] <- "Effectiveness"
names(rb_eff)[3] <- "Type"

likelihood <- ggplot(rb_eff, aes(x= Speed, y= Effectiveness, color = Type))+
  geom_line() + 
  ggtitle('Design Effectiveness by Collision Speed') + 
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  theme(aspect.ratio=2/3, plot.title = element_text(size = 12)) + 
  xlab("Collision Speed") + 
  ylab("Effectiveness")
likelihood


#make reputation graph - how does reputation change with num deaths at speed
rep_calc <- function(data, val){
  sum <- 1/data[val,1] * data[val, 2]
  return(sum)
}

reps <- data.frame()
for(x in 1:50) {
  reps[x,1] <- x
  reps[x+50,1] <- x
  reps[x+100, 1] <- x
  reps[x+150, 1] <- x
  reps[x+200, 1] <- x
  reps[x,2] <- 1
  reps[x+50, 2] <- 2
  reps[x+100, 2] <- 3
  reps[x+150, 2] <- 4
  reps[x+200, 2] <- 5
  reps[x, 3] <- "1"
  reps[x+50, 3] <- "2"
  reps[x+100, 3] <- "3"
  reps[x+150, 3] <- "4"
  reps[x+200, 3] <- "5"
}
for(x in 1:250){
  reps[x,4] <- rep_calc(reps, x)
}
names(reps)[3] <- "No. of Deaths"

reputation <- ggplot(data = reps, aes(x = V1, y = V4)) + geom_line(aes(color = `No. of Deaths`)) + 
  xlab("Crash Speed") + 
  ylab("Reputation Impact (Relative)") + 
  ggtitle('Fatal Crash Speed vs Company Reputation') + 
  scale_fill_brewer(palette = "Reds") +
  scale_color_brewer(palette = "Reds") +
  theme(aspect.ratio=2/3, plot.title = element_text(size = 12))
reputation

#manipulate data to show tradeoff chart

#update values to be relative in comp_mat
comp_mat <- transform(comp_mat, Rear.End.Low.Speed.Injuries = as.numeric(Rear.End.Low.Speed.Injuries))
comp_mat <- transform(comp_mat, Rear.End.High.Speed.Injuries = as.numeric(Rear.End.High.Speed.Injuries))
comp_mat <- transform(comp_mat, Rear.End.Low.Speed.Deaths = as.numeric(Rear.End.Low.Speed.Deaths))
comp_mat <- transform(comp_mat, Rear.End.High.Speed.Deaths = as.numeric(Rear.End.High.Speed.Deaths))

comp_mat$Rear.End.Low.Speed.Injuries <- comp_mat$Rear.End.Low.Speed.Injuries/1534
comp_mat$Rear.End.Low.Speed.Deaths <- comp_mat$Rear.End.Low.Speed.Deaths/165
comp_mat$Rear.End.High.Speed.Injuries <- comp_mat$Rear.End.High.Speed.Injuries/8986
comp_mat$Rear.End.High.Speed.Deaths <- comp_mat$Rear.End.High.Speed.Deaths/939

category_dummy <- data.frame()
category <- data.frame()
#category_dummy <- data.frame(c('Rel. Design Cost', 'Low-Speed Injuries', 'Low-Speed Deaths', 'High-Speed Injuries', 'High-Speed Deaths', 'Relative Reputation'))
category_dummy <- data.frame(c(1, 2, 3, 4, 5, 6))
#category_dummy <- data.frame(t(category_dummy))
score <- data.frame()
design <- data.frame()
for(j in 1:6) {
  score_dummy <- data.frame(t(comp_mat[j, 2:7])) 
  names(score_dummy)[1] <- "n"
  score <- rbind(score, score_dummy)
  category <- rbind(category, category_dummy)
  for(i in 1:6){
  design[i,1] <- "Rubber Bladder"
  design[i+6, 1] <- "Tank-in-Tank"
  design[i+12, 1] <- "Side Rails"
  design[i+18, 1] <- "UHMW"
  design[i+24, 1] <- "Estimate"
  design[i+30, 1] <- "Recall"
  }
}

result_mat <- data.frame()
result_mat <- data.frame(cbind(category, score, design))
names(result_mat)[1] <- "Category"
names(result_mat)[2] <- "Ranking"
names(result_mat)[3] <- "Design"

result_mat <- transform(result_mat, Ranking = as.numeric(Ranking))

for(x in 1:nrow(result_mat)) {
  if(result_mat[x, 2] < 0) {
    result_mat[x,2] <- result_mat[x,2] + 1
  }
}

result_comparison <- ggplot(result_mat, aes(x = Category, y = Ranking, color = Design)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), labels = c('Rel. Design Cost', 'Low-Speed Injuries', 'Low-Speed Deaths', 'High-Speed Injuries', 'High-Speed Deaths', 'Relative Reputation')) +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 12)) + 
  ggtitle("Comparison of Designs") + 
  scale_color_brewer(palette = "Paired")
result_comparison
