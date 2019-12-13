library(lavaan)
library(dagitty)
library(bnlearn)
library(semPlot)
library(ROCR)
library(glmnet)

#Read data
df <- read.table("divorce2.csv", header = TRUE,sep = ",", colClasses=c("NULL", 
                                                                       "double", "double", "double", 
                                                                       "double", "double", "double", "double", "double"))
labels <- read.table("divorce.csv", header = TRUE,sep = ";")[ ,c('Class')]
df$Divorce= labels
df$Divorce[df$Divorce == -1] <- 1

#Divide data into train/test sets
shuffled <- df[sample(nrow(df)),]
lg_labels <- shuffled[ ,c('Divorce')]
lg_labels_train <- lg_labels[c(1:136)]
lg_labels_test <- lg_labels[c(137:170)]
train_data <- shuffled[c(1:136),]
test_data <- shuffled[c(137:170),]

#Initial DAG
#TODO: Draw initial DAG and export from daggity
#Run the tests

#Final DAG
#model with most edges to divorce removed, this is the optimized model
g <- dagitty('dag{Aggression [exposure,pos="0.031,0.025"]
                  Avoidance [exposure,pos="0.042,0.264"]
                  Divorce [outcome,pos="1.000,1.000"]
                  EmotionalDistance [exposure,pos="0.519,0.332"]
                  GoalAlignment [exposure,pos="0.200,0.974"]
                  KnowingPartner [exposure,pos="0.276,0.800"]
                  QualityTime [exposure,pos="0.520,0.773"]
                  ResolvingConflicts [exposure,pos="0.992,0.103"]
                  SelfReflection [exposure,pos="0.538,-0.052"]
                  Avoidance -> SelfReflection
                  Aggression -> EmotionalDistance
                  GoalAlignment -> QualityTime
                  KnowingPartner -> ResolvingConflicts
                  SelfReflection -> Aggression
                  KnowingPartner -> SelfReflection
                  KnowingPartner-> GoalAlignment  
                  ResolvingConflicts -> GoalAlignment
                  SelfReflection <-> Aggression
                  ResolvingConflicts <-> SelfReflection
                  KnowingPartner -> Aggression
                  GoalAlignment -> Divorce 
                  Aggression -> Divorce
                  EmotionalDistance -> Divorce
                  EmotionalDistance -> QualityTime
                  SelfReflection -> QualityTime 
                  Aggression -> QualityTime
                  EmotionalDistance -> GoalAlignment
                  Avoidance <-> KnowingPartner
                  Avoidance <-> ResolvingConflicts
                  }
                ')

impliedConditionalIndependencies(g)
localTests(g, df)
plot(g)

#Estimate SEM coefficients
sem_model <- sem(toString(g, "lavaan"), df)
modindices(sem_model)
fitmeasures(sem_model)
coef(sem_model)
semPaths(sem_model,style="mx",layout="spring",shapeMan="circle",
         color = list(man="white",lat="black", int="black"),edge.color = "black",optimizeLatRes=TRUE, posCol="black",
         trans=FALSE, fade=FALSE,what="std",title=FALSE, cardinal = FALSE, cut = 1.0, asize=1.5)


# Causal Inference
# Get adjustment sets for each variables
adjust1 <- adjustmentSets(g, "Aggression","Divorce") 
#{ KnowingPartner, ResolvingConflicts }
adjust2 <- adjustmentSets(g, "KnowingPartner","Divorce")
adjust3 <- adjustmentSets(g, "ResolvingConflicts","Divorce") 
#{ Aggression, KnowingPartner }
adjust4 <- adjustmentSets(g, "Avoidance","Divorce")
adjust5 <- adjustmentSets(g, "QualityTime","Divorce") 
#{ Aggression, EmotionalDistance, GoalAlignment }
adjust6 <- adjustmentSets(g, "GoalAlignment","Divorce") 
#{ Aggression, EmotionalDistance }
# { EmotionalDistance, KnowingPartner, ResolvingConflicts }
adjust7 <- adjustmentSets(g, "EmotionalDistance","Divorce") 
#{Aggression}
adjust8 <- adjustmentSets(g, "SelfReflection","Divorce")

#Do logistic regression on adjusted set to get the causal effect
lr1 <- glm(Divorce ~ Aggression, df, family = "binomial")
lr2 <- glm(Divorce ~ KnowingPartner, df, family = "binomial")
lr3 <- glm(Divorce ~ ResolvingConflicts, df, family = "binomial")
lr4 <- glm(Divorce ~ Avoidance, df, family = "binomial")
lr5 <- glm(Divorce ~ QualityTime, df, family = "binomial")
lr6 <- glm(Divorce ~ GoalAlignment, df, family = "binomial")
lr7 <- glm(Divorce ~ EmotionalDistance, df, family = "binomial")
lr8 <- glm(Divorce ~ SelfReflection, df, family = "binomial")

#Aggr,KP,RC
unb1 <- coef(glmnet(as.matrix(df[c(1,5,6)]),as.matrix(df[c(9)]), family="binomial"),s=0.000001)
#QT, Aggr,ED,GA
unb2 <- coef(glmnet(as.matrix(df[c(2,3,4,6)]),as.matrix(df[c(9)]), family="binomial"),s=0.000001)
#Aggr, ED, GA
unb3 <- coef(glmnet(as.matrix(df[c(3,4,6)]),as.matrix(df[c(9)]), family="binomial"),s=0.000001)
#ED,Aggr
unb4 <- coef(glmnet(as.matrix(df[c(3,6)]),as.matrix(df[c(9)]), family="binomial"),s=0.000001)

#Predictions with SEM model
net <-model2network(toString(g,"bnlearn"))
bn_fit <- bn.fit (net, df)
bn_fit
bn_predictions <-predict(bn_fit, node="Divorce", df)
plot(bn_predictions)
bn_predictions_actual <- bn_predictions[as.numeric(rownames(test_data))]
bn_predictions_labels <- ifelse(bn_predictions_actual > 0.5,1,0)
# accuracy 
misClasificError <- mean(bn_predictions_labels != test_data$Divorce)
print(paste('Accuracy',1-misClasificError))



#Logistic Regression
lr <- glm(Divorce ~ Aggression+Avoidance+GoalAlignment+EmotionalDistance+KnowingPartner
         +ResolvingConflicts+QualityTime+SelfReflection, data = as.data.frame(train_data))
lr_predictions_actual <- predict(lr, newdata=subset(test_data,select=c(1,2,3,4,5,6,7,8)), type='response')
lr_predictions_labels <- ifelse(lr_predictions_actual > 0.5,1,0)

# accuracy 
misClasificError <- mean(lr_predictions_labels != test_data$Divorce)
print(paste('Accuracy',1-misClasificError))



#Compare LR and SEM predictions
plot(bn_predictions_actual, lr_predictions_actual,col="red",pch=19 ,xlab="SEM Predictions", ylab="Logistic Regression Predictions")
abline( a=0, b=1 )
