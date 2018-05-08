##########################################################################################################################
#################################   Darren Lin and Emily Pirie                             ###############################
#################################   Applied Analytics and Predictive Modeling: MGMT 6160   ###############################
#################################   5/8/2018                                               ###############################
#################################   Final Project                                          ###############################
##########################################################################################################################


data <- read.csv('C:/Users/schoce/Documents/Applied Analytics and Predictive Modeling/Term Project/go_data_train.csv')
val_data <- read.csv('C:/Users/schoce/Documents/Applied Analytics and Predictive Modeling/Term Project/go_data_val.csv')

##########################   Pre processing data   #######################################################################
# set up winner to be 1 if black and 0 if white for both the training and validation file
data$winner <- ifelse(data$winner == 'B',1,0)
val_data$winner<- ifelse(val_data$winner == 'B',1,0)

names(data)
set.seed(123)
#grab a sample of half of the training set to allow for more efficent modeling
sample <- data[sample(1:nrow(data), 250000,replace=FALSE),]
attach(sample)
#summary(winner)
sample <- na.omit(sample)

#plot(winner, black_rating)
#plot(winner~black_rating)
#boxplot(winner~black_moves, main="Number of Spouse Working Hours vs if Married",
#        xlab="Married",ylab="Spouse Workhours")

#hist(black_rating)
#summary(black_rating)

sample <- sample[!sample$black_rating == "Wangzuo", ]
sample <- sample[!sample$black_rating == "Shiduan", ]
sample <- sample[!sample$black_rating == "Qiwang", ]
sample <- sample[!sample$black_rating == "Maxim Cup", ]
sample <- sample[!sample$black_rating == "Female Xinren Wang", ]
sample <- sample[!sample$black_rating == "CITIC Cup", ]
sample <- sample[!sample$black_rating == "Guoshou", ]
sample <- sample[!sample$black_rating == "GS Caltex Cup", ]
sample <- sample[!sample$black_rating == "Female Honinbo", ]
sample <- sample[!sample$black_rating == "Mingren", ]
sample <- sample[!sample$black_rating == "NR", ]
sample <- sample[!sample$black_rating == "Tianyuan", ]
sample <- sample[!sample$black_rating == "GS Caltex Cup", ]


length(sample$black_moves)
names(sample)
unique(sample$black_rating)
unique(sample$group.b.ranking)

sample$group.b.ranking <- ifelse(sample$black_rating == '30k'| sample$black_rating == '29k'| sample$black_rating == '28k'
                                 | sample$black_rating == '27k'| sample$black_rating == '26k'| sample$black_rating == '25k'
                                 | sample$black_rating == '24k'| sample$black_rating == '23k'| sample$black_rating == '22k'
                                 | sample$black_rating == '21k'| sample$black_rating == '20k', "5",
                                 ifelse(sample$black_rating == '9k'|sample$black_rating == '10k' |sample$black_rating == '1k',
                                        '4',
                                        ifelse(sample$black_rating == ''| sample$black_rating == '1k'
                                               | sample$black_rating == '2k'| sample$black_rating == '3k'
                                               | sample$black_rating == '4k'| sample$black_rating == '5k'
                                               | sample$black_rating == '6k'| sample$black_rating == '7k'
                                               | sample$black_rating == '8k'| sample$black_rating == '9k', '3',
                                               ifelse(sample$black_rating == '5d'| sample$black_rating == '6d' | 
                                                        sample$black_rating == '7d ama prov.'|sample$black_rating == 'L3d'
                                                      | sample$black_rating == '1d' | sample$black_rating == '2d' 
                                                      | sample$black_rating == '3d'| sample$black_rating == '4d'
                                                      | sample$black_rating == '7d'| sample$black_rating == '8d'
                                                      | sample$black_rating == '9d'| sample$black_rating == '10d'
                                                      | sample$black_rating == '6a'| sample$black_rating == '3d ama'
                                                      | sample$black_rating == '4d ama'| sample$black_rating == '1d ama'
                                                      | sample$black_rating == '2d ama'| sample$black_rating == '5d ama'
                                                      | sample$black_rating == '6d ama'| sample$black_rating == '7d ama'
                                                      | sample$black_rating == '4d ama'| sample$black_rating == 'ama'
                                                      | sample$black_rating == '1a' | sample$black_rating == '2a' 
                                                      | sample$black_rating == '3a'| sample$black_rating == '4a'
                                                      | sample$black_rating == '7a'| sample$black_rating == '8a'
                                                      | sample$black_rating == '9a'| sample$black_rating == '10a'
                                                      | sample$black_rating == '8d & 9d'| sample$black_rating == '6d, 3d, 2d'
                                                      | sample$black_rating == '2k ama'| sample$black_rating == '7d ama, 5p'
                                                      ,'2',
                                                      ifelse(sample$black_rating == '9' | sample$black_rating == 'Female Meijin & 9p'
                                                             | sample$black_rating == '7p, HA Cup'| sample$black_rating == '6p & Honorary Tengen'
                                                             | sample$black_rating == '6p, Female Kisei'| sample$black_rating == '9p,Tengen'
                                                             | sample$black_rating == '9d, Tengen, Oza'| sample$black_rating == '1p'
                                                             | sample$black_rating == '2p'| sample$black_rating == '3p'
                                                             | sample$black_rating == '4p'| sample$black_rating == '5p'
                                                             | sample$black_rating == '6p'| sample$black_rating == '7p'
                                                             | sample$black_rating == '8p'| sample$black_rating == '9p'
                                                             | sample$black_rating == '9p, Honinbo'| sample$black_rating == 'Tengen'
                                                             | sample$black_rating == '9p, Meijin'| sample$black_rating == 'Meijin'
                                                             | sample$black_rating == '3p, Female Saikyo'| sample$black_rating == '9p, Tengen'
                                                             | sample$black_rating == '39p, NEC Cup'| sample$black_rating == '3p, Female Kisei'
                                                             | sample$black_rating == '9p, NEC Cup'| sample$black_rating == '5p & 9p'
                                                             | sample$black_rating == 'Kisei'| sample$black_rating == '3p, Female Honinbo'
                                                             | sample$black_rating == 'Female Kisei'| sample$black_rating == 'Wangwi'
                                                             ,'1',0)))))
# 1 = professional, 2 = Advanced Amatur, 3 = Intermediate Amatur, 4 = Causal Player, 5 = beginner
# code below shows the remaining cleanup efforts.
# sample$black_rating[which(sample$group.b.ranking == 0)]

sample <- sample[!sample$white_rating == "Wangzuo", ]
sample <- sample[!sample$white_rating == "Shiduan", ]
sample <- sample[!sample$white_rating == "Qiwang", ]
sample <- sample[!sample$white_rating == "Maxim Cup", ]
sample <- sample[!sample$white_rating == "Female Xinren Wang", ]
sample <- sample[!sample$white_rating == "CITIC Cup", ]
sample <- sample[!sample$white_rating == "Guoshou", ]
sample <- sample[!sample$white_rating == "GS Caltex Cup", ]
sample <- sample[!sample$white_rating == "Female Honinbo", ]
sample <- sample[!sample$white_rating == "Mingren", ]
sample <- sample[!sample$white_rating == "NR", ]
sample <- sample[!sample$white_rating == "Tianyuan", ]
sample <- sample[!sample$white_rating == "GS Caltex Cup", ]

sample$group.w.ranking <- ifelse(sample$white_rating == '30k'| sample$white_rating == '29k'| sample$white_rating == '28k'
                                 | sample$white_rating == '27k'| sample$white_rating == '26k'| sample$white_rating == '25k'
                                 | sample$white_rating == '24k'| sample$white_rating == '23k'| sample$white_rating == '22k'
                                 | sample$white_rating == '21k'| sample$white_rating == '20k', "5",
                                 ifelse(sample$white_rating == '9k'|sample$white_rating == '10k' |sample$white_rating == '1k',
                                        '4',
                                        ifelse(sample$white_rating == ''| sample$white_rating == '1k'
                                               | sample$white_rating == '2k'| sample$white_rating == '3k'
                                               | sample$white_rating == '4k'| sample$white_rating == '5k'
                                               | sample$white_rating == '6k'| sample$white_rating == '7k'
                                               | sample$white_rating == '8k'| sample$white_rating == '9k', '3',
                                               ifelse(sample$white_rating == '5d'| sample$white_rating == '6d' | 
                                                        sample$white_rating == '7d ama prov.'|sample$white_rating == 'L3d'
                                                      | sample$white_rating == '1d' | sample$white_rating == '2d' 
                                                      | sample$white_rating == '3d'| sample$white_rating == '4d'
                                                      | sample$white_rating == '7d'| sample$white_rating == '8d'
                                                      | sample$white_rating == '9d'| sample$white_rating == '10d'
                                                      | sample$white_rating == '6a'| sample$white_rating == '3d ama'
                                                      | sample$white_rating == '4d ama'| sample$white_rating == '1d ama'
                                                      | sample$white_rating == '2d ama'| sample$white_rating == '5d ama'
                                                      | sample$white_rating == '6d ama'| sample$white_rating == '7d ama'
                                                      | sample$white_rating == '4d ama'| sample$white_rating == 'ama'
                                                      | sample$white_rating == '1a' | sample$white_rating == '2a' 
                                                      | sample$white_rating == '3a'| sample$white_rating == '4a'
                                                      | sample$white_rating == '7a'| sample$white_rating == '8a'
                                                      | sample$white_rating == '9a'| sample$white_rating == '10a'
                                                      | sample$white_rating == '8d & 9d'| sample$white_rating == '6d, 3d, 2d'
                                                      | sample$white_rating == '2k ama'| sample$white_rating == '7d ama, 5p'
                                                      ,'2',
                                                      ifelse(sample$white_rating == '9' | sample$white_rating == 'Female Meijin & 9p'
                                                             | sample$white_rating == '7p, HA Cup'| sample$white_rating == '6p & Honorary Tengen'
                                                             | sample$white_rating == '6p, Female Kisei'| sample$white_rating == '9p,Tengen'
                                                             | sample$white_rating == '9d, Tengen, Oza'| sample$white_rating == '1p'
                                                             | sample$white_rating == '2p'| sample$white_rating == '3p'
                                                             | sample$white_rating == '4p'| sample$white_rating == '5p'
                                                             | sample$white_rating == '6p'| sample$white_rating == '7p'
                                                             | sample$white_rating == '8p'| sample$white_rating == '9p'
                                                             | sample$white_rating == '9p, Honinbo'| sample$white_rating == 'Tengen'
                                                             | sample$white_rating == '9p, Meijin'| sample$white_rating == 'Meijin'
                                                             | sample$white_rating == '3p, Female Saikyo'| sample$white_rating == '9p, Tengen'
                                                             | sample$white_rating == '39p, NEC Cup'| sample$white_rating == '3p, Female Kisei'
                                                             | sample$white_rating == '9p, NEC Cup'| sample$white_rating == '5p & 9p'
                                                             | sample$white_rating == 'Kisei'| sample$white_rating == '3p, Female Honinbo'
                                                             ,'1',0)))))
# 1 = professional, 2 = Advanced Amatur, 3 = Intermediate Amatur, 4 = Causal Player, 5 = beginner
# code below shows the remaining cleanup efforts.
#sample$white_rating[which(sample$group.w.ranking == 0)]



########################   linear regression   ###############################333
lm <- lm(sample$winner~group.b.ranking+group.w.ranking+black_moves+white_moves+black_avg_subsequent_distance+white_avg_subsequent_distance+black_t1_quad+
           white_t1_quad, data = sample)
summary(lm)

glm <- glm(sample$winner~group.b.ranking+group.w.ranking+black_moves+white_moves+black_avg_subsequent_distance+white_avg_subsequent_distance+black_t1_quad+
             white_t1_quad, data = sample, family = 'binomial')
summary(glm)

####################### kmeans   ################################################
#install.packages('kknn')
library(kknn)
#?kknn

model <- train.kknn(winner ~ group.b.ranking+group.w.ranking+black_avg_subsequent_distance+white_avg_subsequent_distance, data = sample, kmax = 7)
model

prediction <- predict(model, sample[,-4])

prediction
prediction <- ifelse(prediction <= .05, 0,1)
prediction <- as.data.frame(table(unlist(prediction)))
CM <- table(sample$winner, prediction)
CM

accuracy <- (sum(diag(CM)))/sum(CM)
accuracy

plot(model)

# predict using the validation dataset
prediction <- predict(model, val_data[,-4])

prediction
prediction <- ifelse(prediction <= .05, 0,1)
prediction <- as.data.frame(table(unlist(prediction)))
CM <- table(sample$winner, prediction)
CM

accuracy <- (sum(diag(CM)))/sum(CM)
accuracy

plot(model)

################## decision tree   #############################################

install.packages("randomForest")
install.packages('rattle')
library('randomForest')
library(rpart.plot)
library(rattle)
#sample <- na.omit(sample)
boxplot(sample$white_moves, main='')
# bin the average subsequent distances into ranges
hist(black_avg_subsequent_dist_from_white)
boxplot(sample$black_avg_subsequent_dist_from_white)
d<- unique(sample$black_avg_subsequent_dist_from_white)
e<- unique(sample$white_avg_subsequent_dist_from_black)

sample$sub.dist.b.ranges <- ifelse(sample$black_avg_subsequent_dist_from_white < 1,'0',
                                         ifelse(sample$black_avg_subsequent_dist_from_white > 1 & sample$black_avg_subsequent_dist_from_white <2, 1,
                                               ifelse(sample$black_avg_subsequent_dist_from_white > 2 & sample$black_avg_subsequent_dist_from_white <3,2,
                                                        ifelse(sample$black_avg_subsequent_dist_from_white > 3 & sample$black_avg_subsequent_dist_from_white <4,3,
                                                               ifelse(sample$black_avg_subsequent_dist_from_white > 4 & sample$black_avg_subsequent_dist_from_white <5,4,
                                                                      ifelse(sample$black_avg_subsequent_dist_from_white > 5 & sample$black_avg_subsequent_dist_from_white < 6,5,
                                                                             ifelse(sample$black_avg_subsequent_dist_from_white > 6,6,0)))))))

sample$sub.dist.w.ranges <- ifelse(sample$white_avg_subsequent_dist_from_black < 1,'0',
                                   ifelse(sample$white_avg_subsequent_dist_from_black > 1 & sample$white_avg_subsequent_dist_from_black <2, 1,
                                          ifelse(sample$white_avg_subsequent_dist_from_black > 2 & sample$white_avg_subsequent_dist_from_black <3,2,
                                                 ifelse(sample$white_avg_subsequent_dist_from_black > 3 & sample$white_avg_subsequent_dist_from_black <4,3,
                                                        ifelse(sample$white_avg_subsequent_dist_from_black > 4 & sample$white_avg_subsequent_dist_from_black <5,4,
                                                               ifelse(sample$white_avg_subsequent_dist_from_black > 5 & sample$white_avg_subsequent_dist_from_black < 6,5,
                                                                      ifelse(sample$white_avg_subsequent_dist_from_black > 6,6,0)))))))


table1 <- data.frame(table(sample$sub.dist.w.ranges))
table1 <- table1[order(-table1$Freq),]
head(table1)
length(sample$group.w.ranking)
length(sample$group.b.ranking)

#### tree modeling   ###
tree2 <- rpart(sample$winner ~ sample$group.b.ranking+sample$group.w.ranking+sample$sub.dist.w.ranges+sample$sub.dist.b.ranges+
                 sample$black_moves+sample$white_moves , data = sample, method = 'class')
#plot(tree2)
#text(tree2)
fancyRpartPlot(tree2)	
t_pred = predict(tree2,sample,type="class")
confMat <- table(sample$winner,t_pred)
confMat

accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

#### tree v3  ####

tree3 <- rpart(sample$winner ~ sample$group.b.ranking+sample$group.w.ranking+sample$sub.dist.w.ranges+sample$sub.dist.b.ranges+
                 sample$black_moves+sample$white_moves+black_t1_quad+black_t2_quad+black_t3_quad+white_t1_quad+white_t2_quad+
                 white_t3_quad+black_unique_quads_101110+black_unique_quads_110+black_unique_quads_111210, data = sample, method = 'class')

fancyRpartPlot(tree3)	
t_pred = predict(tree3,sample,type="class")
confMat <- table(sample$winner,t_pred)
confMat

accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

#####################################    Processing the validation data   #######################################################
# Should be run prior to modeling


val_data <- na.omit(val_data)

val_data <- val_data[!val_data$black_rating == "Wangzuo", ]
val_data <- val_data[!val_data$black_rating == "Shiduan", ]
val_data <- val_data[!val_data$black_rating == "Qiwang", ]
val_data <- val_data[!val_data$black_rating == "Maxim Cup", ]
val_data <- val_data[!val_data$black_rating == "Female Xinren Wang", ]
val_data <- val_data[!val_data$black_rating == "CITIC Cup", ]
val_data <- val_data[!val_data$black_rating == "Guoshou", ]
val_data <- val_data[!val_data$black_rating == "GS Caltex Cup", ]
val_data <- val_data[!val_data$black_rating == "Female Honinbo", ]
val_data <- val_data[!val_data$black_rating == "Mingren", ]
val_data <- val_data[!val_data$black_rating == "NR", ]
val_data <- val_data[!val_data$black_rating == "Tianyuan", ]
val_data <- val_data[!val_data$black_rating == "GS Caltex Cup", ]


length(val_data$black_moves)
names(val_data)
unique(val_data$black_rating)
unique(val_data$group.b.ranking)

val_data$group.b.ranking <- ifelse(val_data$black_rating == '30k'| val_data$black_rating == '29k'| val_data$black_rating == '28k'
                                   | val_data$black_rating == '27k'| val_data$black_rating == '26k'| val_data$black_rating == '25k'
                                   | val_data$black_rating == '24k'| val_data$black_rating == '23k'| val_data$black_rating == '22k'
                                   | val_data$black_rating == '21k'| val_data$black_rating == '20k', "5",
                                   ifelse(val_data$black_rating == '9k'|val_data$black_rating == '10k' |val_data$black_rating == '1k',
                                          '4',
                                          ifelse(val_data$black_rating == ''| val_data$black_rating == '1k'
                                                 | val_data$black_rating == '2k'| val_data$black_rating == '3k'
                                                 | val_data$black_rating == '4k'| val_data$black_rating == '5k'
                                                 | val_data$black_rating == '6k'| val_data$black_rating == '7k'
                                                 | val_data$black_rating == '8k'| val_data$black_rating == '9k', '3',
                                                 ifelse(val_data$black_rating == '5d'| val_data$black_rating == '6d' | 
                                                          val_data$black_rating == '7d ama prov.'|val_data$black_rating == 'L3d'
                                                        | val_data$black_rating == '1d' | val_data$black_rating == '2d' 
                                                        | val_data$black_rating == '3d'| val_data$black_rating == '4d'
                                                        | val_data$black_rating == '7d'| val_data$black_rating == '8d'
                                                        | val_data$black_rating == '9d'| val_data$black_rating == '10d'
                                                        | val_data$black_rating == '6a'| val_data$black_rating == '3d ama'
                                                        | val_data$black_rating == '4d ama'| val_data$black_rating == '1d ama'
                                                        | val_data$black_rating == '2d ama'| val_data$black_rating == '5d ama'
                                                        | val_data$black_rating == '6d ama'| val_data$black_rating == '7d ama'
                                                        | val_data$black_rating == '4d ama'| val_data$black_rating == 'ama'
                                                        | val_data$black_rating == '1a' | val_data$black_rating == '2a' 
                                                        | val_data$black_rating == '3a'| val_data$black_rating == '4a'
                                                        | val_data$black_rating == '7a'| val_data$black_rating == '8a'
                                                        | val_data$black_rating == '9a'| val_data$black_rating == '10a'
                                                        | val_data$black_rating == '8d & 9d'| val_data$black_rating == '6d, 3d, 2d'
                                                        | val_data$black_rating == '2k ama'| val_data$black_rating == '7d ama, 5p'
                                                        ,'2',
                                                        ifelse(val_data$black_rating == '9' | val_data$black_rating == 'Female Meijin & 9p'
                                                               | val_data$black_rating == '7p, HA Cup'| val_data$black_rating == '6p & Honorary Tengen'
                                                               | val_data$black_rating == '6p, Female Kisei'| val_data$black_rating == '9p,Tengen'
                                                               | val_data$black_rating == '9d, Tengen, Oza'| val_data$black_rating == '1p'
                                                               | val_data$black_rating == '2p'| val_data$black_rating == '3p'
                                                               | val_data$black_rating == '4p'| val_data$black_rating == '5p'
                                                               | val_data$black_rating == '6p'| val_data$black_rating == '7p'
                                                               | val_data$black_rating == '8p'| val_data$black_rating == '9p'
                                                               | val_data$black_rating == '9p, Honinbo'| val_data$black_rating == 'Tengen'
                                                               | val_data$black_rating == '9p, Meijin'| val_data$black_rating == 'Meijin'
                                                               | val_data$black_rating == '3p, Female Saikyo'| val_data$black_rating == '9p, Tengen'
                                                               | val_data$black_rating == '39p, NEC Cup'| val_data$black_rating == '3p, Female Kisei'
                                                               | val_data$black_rating == '9p, NEC Cup'| val_data$black_rating == '5p & 9p'
                                                               | val_data$black_rating == 'Kisei'| val_data$black_rating == '3p, Female Honinbo'
                                                               | val_data$black_rating == 'Female Kisei'| val_data$black_rating == 'Wangwi'
                                                               ,'1',0)))))
# 1 = professional, 2 = Advanced Amatur, 3 = Intermediate Amatur, 4 = Causal Player, 5 = beginner
val_data$black_rating[which(val_data$group.b.ranking == 0)]

val_data <- val_data[!val_data$white_rating == "Wangzuo", ]
val_data <- val_data[!val_data$white_rating == "Shiduan", ]
val_data <- val_data[!val_data$white_rating == "Qiwang", ]
val_data <- val_data[!val_data$white_rating == "Maxim Cup", ]
val_data <- val_data[!val_data$white_rating == "Female Xinren Wang", ]
val_data <- val_data[!val_data$white_rating == "CITIC Cup", ]
val_data <- val_data[!val_data$white_rating == "Guoshou", ]
val_data <- val_data[!val_data$white_rating == "GS Caltex Cup", ]
val_data <- val_data[!val_data$white_rating == "Female Honinbo", ]
val_data <- val_data[!val_data$white_rating == "Mingren", ]
val_data <- val_data[!val_data$white_rating == "NR", ]
val_data <- val_data[!val_data$white_rating == "Tianyuan", ]
val_data <- val_data[!val_data$white_rating == "GS Caltex Cup", ]

val_data$group.w.ranking <- ifelse(val_data$white_rating == '30k'| val_data$white_rating == '29k'| val_data$white_rating == '28k'
                                   | val_data$white_rating == '27k'| val_data$white_rating == '26k'| val_data$white_rating == '25k'
                                   | val_data$white_rating == '24k'| val_data$white_rating == '23k'| val_data$white_rating == '22k'
                                   | val_data$white_rating == '21k'| val_data$white_rating == '20k', "5",
                                   ifelse(val_data$white_rating == '9k'|val_data$white_rating == '10k' |val_data$white_rating == '1k',
                                          '4',
                                          ifelse(val_data$white_rating == ''| val_data$white_rating == '1k'
                                                 | val_data$white_rating == '2k'| val_data$white_rating == '3k'
                                                 | val_data$white_rating == '4k'| val_data$white_rating == '5k'
                                                 | val_data$white_rating == '6k'| val_data$white_rating == '7k'
                                                 | val_data$white_rating == '8k'| val_data$white_rating == '9k', '3',
                                                 ifelse(val_data$white_rating == '5d'| val_data$white_rating == '6d' | 
                                                          val_data$white_rating == '7d ama prov.'|val_data$white_rating == 'L3d'
                                                        | val_data$white_rating == '1d' | val_data$white_rating == '2d' 
                                                        | val_data$white_rating == '3d'| val_data$white_rating == '4d'
                                                        | val_data$white_rating == '7d'| val_data$white_rating == '8d'
                                                        | val_data$white_rating == '9d'| val_data$white_rating == '10d'
                                                        | val_data$white_rating == '6a'| val_data$white_rating == '3d ama'
                                                        | val_data$white_rating == '4d ama'| val_data$white_rating == '1d ama'
                                                        | val_data$white_rating == '2d ama'| val_data$white_rating == '5d ama'
                                                        | val_data$white_rating == '6d ama'| val_data$white_rating == '7d ama'
                                                        | val_data$white_rating == '4d ama'| val_data$white_rating == 'ama'
                                                        | val_data$white_rating == '1a' | val_data$white_rating == '2a' 
                                                        | val_data$white_rating == '3a'| val_data$white_rating == '4a'
                                                        | val_data$white_rating == '7a'| val_data$white_rating == '8a'
                                                        | val_data$white_rating == '9a'| val_data$white_rating == '10a'
                                                        | val_data$white_rating == '8d & 9d'| val_data$white_rating == '6d, 3d, 2d'
                                                        | val_data$white_rating == '2k ama'| val_data$white_rating == '7d ama, 5p'
                                                        ,'2',
                                                        ifelse(val_data$white_rating == '9' | val_data$white_rating == 'Female Meijin & 9p'
                                                               | val_data$white_rating == '7p, HA Cup'| val_data$white_rating == '6p & Honorary Tengen'
                                                               | val_data$white_rating == '6p, Female Kisei'| val_data$white_rating == '9p,Tengen'
                                                               | val_data$white_rating == '9d, Tengen, Oza'| val_data$white_rating == '1p'
                                                               | val_data$white_rating == '2p'| val_data$white_rating == '3p'
                                                               | val_data$white_rating == '4p'| val_data$white_rating == '5p'
                                                               | val_data$white_rating == '6p'| val_data$white_rating == '7p'
                                                               | val_data$white_rating == '8p'| val_data$white_rating == '9p'
                                                               | val_data$white_rating == '9p, Honinbo'| val_data$white_rating == 'Tengen'
                                                               | val_data$white_rating == '9p, Meijin'| val_data$white_rating == 'Meijin'
                                                               | val_data$white_rating == '3p, Female Saikyo'| val_data$white_rating == '9p, Tengen'
                                                               | val_data$white_rating == '39p, NEC Cup'| val_data$white_rating == '3p, Female Kisei'
                                                               | val_data$white_rating == '9p, NEC Cup'| val_data$white_rating == '5p & 9p'
                                                               | val_data$white_rating == 'Kisei'| val_data$white_rating == '3p, Female Honinbo'
                                                               ,'1',0)))))

# for the tree modeling:
val_data$sub.dist.b.ranges <- ifelse(val_data$black_avg_subsequent_dist_from_white < 1,'0',
                                     ifelse(val_data$black_avg_subsequent_dist_from_white > 1 & val_data$black_avg_subsequent_dist_from_white <2, 1,
                                            ifelse(val_data$black_avg_subsequent_dist_from_white > 2 & val_data$black_avg_subsequent_dist_from_white <3,2,
                                                   ifelse(val_data$black_avg_subsequent_dist_from_white > 3 & val_data$black_avg_subsequent_dist_from_white <4,3,
                                                          ifelse(val_data$black_avg_subsequent_dist_from_white > 4 & val_data$black_avg_subsequent_dist_from_white <5,4,
                                                                 ifelse(val_data$black_avg_subsequent_dist_from_white > 5 & val_data$black_avg_subsequent_dist_from_white < 6,5,
                                                                        ifelse(val_data$black_avg_subsequent_dist_from_white > 6,6,0)))))))

val_data$sub.dist.w.ranges <- ifelse(val_data$white_avg_subsequent_dist_from_black < 1,'0',
                                     ifelse(val_data$white_avg_subsequent_dist_from_black > 1 & val_data$white_avg_subsequent_dist_from_black <2, 1,
                                            ifelse(val_data$white_avg_subsequent_dist_from_black > 2 & val_data$white_avg_subsequent_dist_from_black <3,2,
                                                   ifelse(val_data$white_avg_subsequent_dist_from_black > 3 & val_data$white_avg_subsequent_dist_from_black <4,3,
                                                          ifelse(val_data$white_avg_subsequent_dist_from_black > 4 & val_data$white_avg_subsequent_dist_from_black <5,4,
                                                                 ifelse(val_data$white_avg_subsequent_dist_from_black > 5 & val_data$white_avg_subsequent_dist_from_black < 6,5,
                                                                        ifelse(val_data$white_avg_subsequent_dist_from_black > 6,6,0)))))))

