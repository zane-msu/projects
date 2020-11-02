library(ggplot2)
library(dplyr)
library(rpart)
library(kernlab)
library(caret)
library(DescTools)
################## import draft data ####################
draft <- read.csv("https://raw.githubusercontent.com/potierku/talk_data_to_me/master/final_project/draft.txt",stringsAsFactors = FALSE)
#delete rows with 2017,2018,2019
draft <- draft[-which(draft$Year %in% c("2016","2017","2018","2019")),]
draft$Pos[which(draft$Pos %in% c("","W","F"))]=NA #replace blanks with NA

#replace NA values in stats with 0
draft$GP[is.na(draft$GP)] <-0
draft$G[is.na(draft$G)] <-0 
draft$A[is.na(draft$A)] <-0 
draft$PTS[is.na(draft$PTS)] <-0 
draft$plus_minus[is.na(draft$plus_minus)] <-0 
draft$PIM[is.na(draft$PIM)] <-0 

#replace POS that is blank with NI (no information) 
draft$Pos <- as.character(draft$Pos)
draft$Pos[which(is.na(draft$Pos))] <- "unknown"
draft$Pos <- as.factor(draft$Pos)

##################### import standings data ####################
standings <- read.csv("https://raw.githubusercontent.com/potierku/talk_data_to_me/master/final_project/standings.csv")
standings$Team[which(standings$Team=="Anaheim Mighty Ducks")]<- "Anaheim Ducks"
standings$Team[which(standings$Team=="Phoenix Coyotes")]<- "Arizona Coyotes"
standings$Team[which(standings$Team=="Atlanta Thrashers")]<- "Winnipeg Jets"
standings$Team[which(standings$Team=="Quebec Nordiques")]<- "Colorado Avalanche"
standings$Team[which(standings$Team=="Hartford Whalers")]<- "Carolina Hurricanes"
standings$Team[which(standings$Team=="Minnesota North Stars")]<- "Dallas Stars"
standings <- standings[which(standings$Team != "Vegas Golden Knights"),]
standings$Team <- as.character(standings$Team)
standings$Team <- as.factor(standings$Team)
table(standings$Team)

######################## import goalies data ##########################
goalies <- read.csv("https://raw.githubusercontent.com/potierku/talk_data_to_me/master/final_project/goalies_data.csv")

############################# import skaters data ########################
skaters <- read.csv("https://raw.githubusercontent.com/potierku/talk_data_to_me/master/final_project/skaters_data.csv")
skaters$ATOI <- skaters$TOI/skaters$GP
skaters$year_player <- paste(skaters$ï..Year,skaters$Player)
#this for loop is used to remove instances of a player being traded (whole season stats only)
for (i in c(1:length(skaters$year_player))){
  while (ifelse(i<length(skaters$year_player),skaters$year_player[i]==skaters$year_player[i+1],FALSE)){
    skaters<-skaters[-c(i+1),]
  }
}
skaters$Player <- as.character(skaters$Player)
#remove asterisks from skaters data
for (i in c(1:length(skaters$Player))){
  skaters$Player[i] <-(gsub("\\*","",skaters$Player[i]))
}

######################### import city temps and years old ##########################
temps <- read.csv("https://raw.githubusercontent.com/potierku/talk_data_to_me/master/final_project/NHL_city_temp_data.csv")

#################### creates 3 year draft results dataframe #####################
draft_results <- as.data.frame(colnames(c("Year","Round","Overall","Team","Player","Nat.","POS","Age","To","Amateur.Team","Amateur.Lg","GP","G","A","PTS","+_-","PIM")))
for (i in c(1: (length(draft$Player)))){
  df <- skaters[which(as.character(skaters$Player) == as.character(draft$Player[i])),]
  df <- df[which(df$GP>10),]
  df <- df[c(1:3),]
  numerics <- df[,c(6:11)]
  #combine the first 3 years of numeric data from skaters with draft info and add
  draft_results <- rbind(draft_results,as.data.frame(c(draft[i,c(1:11)],sapply(numerics,sum))))
}
#sets na values to 0
draft_results$GP[is.na(draft_results$GP)] <-0
draft_results$G[is.na(draft_results$G)] <-0 
draft_results$A[is.na(draft_results$A)] <-0 
draft_results$PTS[is.na(draft_results$PTS)] <-0 
draft_results$X...[is.na(draft_results$X...)] <-0 
draft_results$PIM[is.na(draft_results$PIM)] <-0 
#create column for NHL or not
draft_results$nhl <- ifelse(draft_results$GP>20,1,0)

################### general NHL stats ########################
#predict points using draft position, exempt goalies
draft$ppgp <- draft$PTS/draft$GP #created points per games player metric
players <- draft[which(draft$Pos!='G'),] #excludes goalies and players who have played less than 10 games
ggplot(data=players,aes(x=Overall, y=ppgp))+
  geom_point()+
  labs(title="Points Per Game vs. Overall Pick Number")+
  xlab("Overall Pick Number")+
  ylab("Points Per Game")+
  theme(plot.title = element_text(hjust = 0.5))
  
pts_overall <- lm(ppgp ~ poly(Overall,2) + Pos,data=draft)
summary(pts_overall)

#peak age calculation
#cut off at 20 Pts to exclude marginal players that may not have a full career
peak_age_data <- skaters[which(skaters$PTS>20 & skaters$Age <40),]
ggplot(data=peak_age_data,aes(x=Age, y=PTS))+
  geom_point()
pts_age <- lm(PTS ~ poly(Age,2,raw=TRUE),data=peak_age_data)
summary(pts_age)
#took derivative of results to find age
(peak_age <- ((-1)*pts_age$coefficients[2])/(2*pts_age$coefficients[3]))

#average time on ice versus points plot
ggplot(data=skaters,aes(x=ATOI, y=PTS))+
  geom_point()+
  geom_smooth(method="auto")+
  facet_wrap(~Pos)
pts_atoi <- lm(PTS~poly(ATOI,2,raw=TRUE),data=skaters)
summary(pts_atoi)

################# Does Tanking Work? #####################
#regression to predict points in any given season, uses 5 lags of total team Points
tanking <- function(team){
  team_data <- standings[which(standings$Team == team),]
  team_reg <- lm(Pts ~ lag(Pts,n=1L) + lag(Pts,n=2L) + 
                 lag(Pts,n=3L)+ lag(Pts,n=4L)+ lag(Pts,n=5L) + 
                   lag(Pts,n=6L) + lag(Pts,n=7L) + lag(Pts,n=8L),data=team_data)
  team_reg$coefficients
}
teams<- unique(standings$Team) #list of teams
tanking_results <- as.data.frame(lapply(teams,tanking)) #list apply
tanking_results <- t(tanking_results) #transpose results
colnames(tanking_results)<-c("intercept","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8")
tanking_results <- as.data.frame(tanking_results) #had issue with atomic vectors
tanking_results_stats <- as.data.frame(t(mapply(MeanCI,tanking_results,conf.level=.8)))
ggplot(data=tanking_results_stats[2:length(tanking_results_stats$mean),],aes(x=c(1:(length(tanking_results_stats$mean)-1)),y=mean))+
  geom_point()+
  geom_errorbar(aes(ymin=lwr.ci,ymax=upr.ci),width=.2)+
  labs(title="Effect of Previous Year's Team Points on This Year")+
  xlab("Number of Years Ago")+
  ylab("Mean with 80% Confidence Interval")+
  theme(plot.title = element_text(hjust = 0.5))

#figures out if temperatures and if when the team was founded matter
standings$Team <- as.character(standings$Team)
temps$ï..Team <- as.character(temps$ï..Team)
for (i in c(1:length(standings$Team))){
  standings$temps[i] <- temps$High.F.Avg[which(temps$ï..Team==standings$Team[i])]
}

for (i in c(1:length(standings$Team))){
  standings$legacy[i] <- (standings$ï..year[i] - temps$Year.Founded[which(temps$ï..Team==standings$Team[i])])
}
summary(lm(Pts ~ temps + legacy, data = standings))
#train function in caret and cross validation

#plot average, case study recent champions, add confidence intervals

############################ 3 year draft results ######################
#functions to get the rmse of the 
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}
draft_results_skaters <- draft_results[which(draft$Pos!='G'),]
draft_results_skaters$Pos <- droplevels(draft_results_skaters$Pos)

ggplot(data=draft_results_skaters,aes(x=Overall, y=PTS))+
  geom_point()+
  geom_smooth(method="auto")+
  facet_wrap(~Pos)+
  labs(title="Points after 3 years in the NHL")+
  theme(plot.title = element_text(hjust = 0.5))

#creating test and train datasets
set.seed(6)
draft_results_skaters_idx = sample(nrow(draft_results_skaters), round(nrow(draft_results_skaters) / 2))
draft_results_skaters_train = draft_results_skaters[draft_results_skaters_idx, ]
draft_results_skaters_test = draft_results_skaters[-draft_results_skaters_idx, ]

#using a polynomial to predict points based on overall position
rmse_results_poly <- c()
for (i in c(1:4)){
  poly_model <- lm(PTS ~ poly(Overall,i,raw=TRUE) + Pos,data=draft_results_skaters_train)
  rmse_results_poly[i] <- get_rmse(poly_model,draft_results_skaters_test,response="PTS")
}
optimal_poly <- which(rmse_results_poly==min(rmse_results_poly)) #which polynomial finds the best fit
(summary(lm(PTS ~ poly(Overall,optimal_poly,raw=TRUE) + Pos,data=draft_results_skaters)))

#using knn to predict points based on overall position
rmse_results_knn <- c()
for (i in c(1:25)){
  draft_results_knn <- knnreg(PTS ~ Overall + Pos,draft_results_skaters_train,k=i)
  rmse_results_knn[i] <- get_rmse(draft_results_knn,draft_results_skaters_test,response = "PTS")
}
which(rmse_results_knn==min(rmse_results_knn)) #number of nearest neighbors that minimizes rmse


#use lm to predict if play in NHL or not
summary(lm(nhl~poly(Overall,2,raw=TRUE) + Pos + Age, data=draft_results))

#odds of making the nhl based on round drafted
#adjust for modern size of nhl
draft_results$Round_modern <- trunc(1+draft_results$Overall/31)

round_probs <- as.data.frame(names(c("round","chance")))
for (i in c(1:10)){
  df <- draft_results[which(draft_results$Round_modern==i),]
  round_probs <- rbind(round_probs,c(as.integer(i),mean(df$nhl)))
}
names(round_probs) <- c("round","chance")
ggplot(data=round_probs,aes(x=round,y=chance))+
  geom_point()+
  scale_x_continuous(name="Round of Draft (31 team equivalent)",breaks=c(1:length(round_probs$round)))+
  scale_y_continuous(name= "Chance of > 20 GP in NHL")+
  labs(title="Chance of playing in the NHL")+
  theme(plot.title = element_text(hjust = 0.5))

