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
sample$black_rating[which(sample$group.b.ranking == 0)]

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
sample$white_rating[which(sample$group.w.ranking == 0)]



########################   linear regression   ###############################333
lm <- lm(sample$winner~group.b.ranking+group.w.ranking+black_moves+white_moves+black_avg_subsequent_distance+white_avg_subsequent_distance+black_t1_quad+
           white_t1_quad, data = sample)
summary(lm)

glm <- glm(sample$winner~group.b.ranking+group.w.ranking+black_moves+white_moves+black_avg_subsequent_distance+white_avg_subsequent_distance+black_t1_quad+
             white_t1_quad, data = sample, family = 'binomial')
summary(glm)
#plot(glm)

####################### kmeans   ################################################
?kknn
#install.packages('kknn')
library(kknn)
#?kknn

model <- train.kknn(winner ~ group.b.ranking+group.w.ranking+black_avg_subsequent_distance+white_avg_subsequent_distance, data = sample, kmax = 7)
model
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

prediction <- predict(model, sample[,-4])

prediction
prediction <- ifelse(prediction <= .05, 0,1)
prediction <- as.data.frame(table(unlist(prediction)))
CM <- table(sample$winner, prediction)
CM

accuracy <- (sum(diag(CM)))/sum(CM)
accuracy

plot(model)

#############   clustering   #######################################################
#install.packages("pvclust")
library(pvclust)
small.sample <- sample[sample(1:nrow(sample), 2500,replace=FALSE),]
small.sample <- na.omit(small.sample)
sum(is.na(small.sample))
names(small.sample)
small.sample <- small.sample[c('winner','komi','black_moves','white_moves')]
#Ward's method (hierarchical):
d <- dist(small.sample, method = "euclidean")
fit <- hclust(d, method="ward.D") 
plot(fit)   # this will create the dendrogram for you
pvrect(fit, alpha=.95)
#K-means:
fit <- kmeans(small.sample, 5)  #input desired number of clusters

################## decision tree   #############################################

install.packages("randomForest")
library('randomForest')
library(rpart.plot)
sample <- na.omit(sample)
sample$black_moves <- as.factor(sample$black_moves)
sample$white_moves <- as.factor(sample$white_moves)
#sample$black_avg_subsequent_dist_from_white <- as.factor(sample$black_avg_subsequent_dist_from_white)
#sample$white_avg_subsequent_dist_from_black <- as.factor(sample$white_avg_subsequent_dist_from_black)
tree <- rpart(winner ~ black_avg_subsequent_dist_from_white+white_avg_subsequent_dist_from_black , data = sample)
plot(tree)
text(tree)
prp(tree)
# bin the average subsequent distances into ranges
hist(black_avg_subsequent_dist_from_white)
boxplot(sample$black_avg_subsequent_dist_from_white)
max(black_avg_subsequent_dist_from_white)
sample$black_avg_subsequent_dist_from_white <- as.numeric((sample$black_avg_subsequent_dist_from_white))
sample$sub.dist.b.ranges <- ifelse(sample$black_avg_subsequent_dist_from_white < 1,'0',
                                         ifelse(sample$black_avg_subsequent_dist_from_white > 1 & sample$black_avg_subsequent_dist_from_white <2, 1,2))
                                          #       ifelse(sample$black_avg_subsequent_dist_from_white > 2 & sample$black_avg_subsequent_dist_from_white <3,2,
                                                        ifelse(sample$black_avg_subsequent_dist_from_white > 3 & sample$black_avg_subsequent_dist_from_white <4,3,
                                                               ifelse(sample$black_avg_subsequent_dist_from_white > 4 & sample$black_avg_subsequent_dist_from_white <5,4,
                                                                      ifelse(sample$black_avg_subsequent_dist_from_white > 5 & sample$black_avg_subsequent_dist_from_white < 6,5,
                                                                             ifelse(sample$black_avg_subsequent_dist_from_white > 6,6,0)))))))

table1 <- data.frame(table(sample$sub.dist.b.ranges))
table1 <- table1[order(-table1$Freq),]
head(table1)

tree2 <- rpart(winner ~ group.b.ranking+group.w.ranking+black_avg_subsequent_dist_from_white+white_avg_subsequent_dist_from_black , data = sample)
plot(tree2)
text(tree2)

names(sample)
lapply(sample, class)
lapply(sample, sum(unique()))
lapply(sample, as.factor)
sum(unique(winner))
sum(unique(black_moves))
