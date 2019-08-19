##############################################################################################################################
#Goktug Cengiz and Henry Qiu
##############################################################################################################################

#Libraries 
library(mlbench) 
library(dplyr) 
library(missForest) 
library(DMwR) 
library(mice) 
library(BaylorEdPsych) 
library(mvnmle) 
library(e1071) 
library(Hmisc) 
library(corrplot) 
library(cowplot) 
library(VIM) 
library(ggplot2) 
library(plyr) 
library(class) 
library(tree)
library(randomForest) 
library(ROCR) 
library(gridExtra) 
library(kernlab)
library(chemometrics)
library(FactoMineR)
library(factoextra)
library(caret)
library(magrittr)
library(missForest)
library(ggthemes)
library(cclust)
library(fpc)
library(rattle)
##############################################################################################################################
##############################################################################################################################
#Data Exploration
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
#Data Reading 
##############################################################################################################################
adultRawData = read.csv("C:/Users/Goktug/Desktop/adult.csv",
                        header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE) 
set.seed(1234)
adultData = adultRawData 

head(adultData, 5)

for (i in 1:ncol(adultData)) { #Missing Values was representing as ? and it was converted as NA 
  if(length(which(adultData[,i] == "?"))>0) adultData[which(adultData[,i] == "?"),i] = NA 
} 

adult.names = c("Age", 
                "Workingclass", 
                "Final_Weight", 
                "Education", 
                "Education_num", 
                "Marital_Status", 
                "Occupation", 
                "Relationship", 
                "Race", 
                "Sex", 
                "Capital_gain", 
                "Capital_loss", 
                "Hours_per_week", 
                "Native_country", 
                "Income") 

colnames(adultData) = adult.names 
str(adultData) #How many columns and rows do we have ? What are the data types of attributes ? 
summary(adultData) #Summarize the dataset 

#Creating new Data Frame for Numeric Values 

adultMatrixNum = cbind(adultData$Age,
                       adultData$Capital_gain,
                       adultData$Capital_loss,
                       adultData$Education_num, 
                       adultData$Final_Weight,  
                       adultData$Hours_per_week) 

colnames(adultMatrixNum) = c("Age",
                             "Capital_gain",
                             "Capital_loss",
                             "Education_num", 
                             "Final_Weight", 
                             "Hours_per_week") 

adultDataNum = data.frame(adultMatrixNum) 
##############################################################################################################################
##Missing Value Detection 
##############################################################################################################################
apply(adultData, 2, function(x) sum(is.na(x))) 
md.pattern(adultData) #Missing Value Visualization 
aggr_plot = aggr(adultData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,  
                  labels=names(adultData), cex.axis=.7, gap=3,  
                  ylab=c("Histogram of missing data","Pattern")) #Missing Value Visualization 

sum(is.na(adultData)) 

#Missing Value Imputation by Random Forest Imputation
ad = adultData
dataFac=ad %>% mutate_if(is.character, as.factor)
mfImp = missForest(dataFac);
adultImp <- mfImp$ximp
sum(is.na(adultImp))
adultData$Workingclass = adultImp$Workingclass
adultData$Native_country = adultImp$Native_country
adultData$Occupation = adultImp$Occupation
sum(is.na(adultData)) 
##############################################################################################################################
#Outlier Detection 
##############################################################################################################################
#Setting the theme of plots 

plot_theme = theme_classic() +  
  theme(plot.title = element_text(hjust = 0.5, size = 14,face = 'bold'), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 12), 
        axis.text.y  = element_text(size = 12)) 

#Continuous Univariate plots  

CUV = function(yfeature, ylabel) { 
  ggplot(adultData, aes(x = "", y = yfeature)) + 
    geom_boxplot(fill = "#0000FF", outlier.colour = "red", outlier.shape = 1) + 
    stat_boxplot(geom = "errorbar", width = 0.5) + 
    labs( y = ylabel, title = paste(ylabel, "Distribution")) + 
    plot_theme
  }

p1 = CUV(adultData$Age, "Age") 
p2 = CUV(adultData$Capital_gain, "Capital Gain")
p3 = CUV(adultData$Capital_loss, "Capital Loss")
p4 = CUV(adultData$Education_num, "Education_num") 
p5 = CUV(adultData$Final_Weight, "Final Weight") 
p6 = CUV(adultData$Hours_per_week, "Hours Per Week") 


plot_grid(p1, p2, p3, p4, p5, p6) 

#We decided that we will not treat capital gain and capital loss
#because it shows that all values are outlier except 0
#when we try to treat it, all values will be 0

#Outlier Treatment 

oiAge = which(adultDataNum$Age %in% boxplot.stats(adultDataNum$Age)$out) 
oiEN = which(adultDataNum$Education_num %in% boxplot.stats(adultDataNum$Education_num)$out) 
oiFW = which(adultDataNum$Final_Weight %in% boxplot.stats(adultDataNum$Final_Weight)$out) 
oiHPW = which(adultDataNum$Hours_per_week %in% boxplot.stats(adultDataNum$Hours_per_week)$out) 

adultDataNum[oiAge, ]$Age = NA 
adultDataNum[oiEN, ]$Education_num = NA 
adultDataNum[oiFW, ]$Final_Weight = NA 
adultDataNum[oiHPW, ]$Hours_per_week = NA 
adultDataNum = missForest(adultDataNum)
adultDataNum = adultDataNum$ximp

summary(adultDataNum) 

sum(is.na(adultData)) 

adultDataNum$Age = round(adultDataNum$Age) 
adultDataNum$Education_num = round(adultDataNum$Education_num) 
adultDataNum$Final_Weight = round(adultDataNum$Final_Weight) 
adultDataNum$Hours_per_week = round(adultDataNum$Hours_per_week) 
adultData$Age = adultDataNum$Age 
adultData$Education_num = adultDataNum$Education_num 
adultData$Final_Weight = adultDataNum$Final_Weight 
adultData$Hours_per_week = adultDataNum$Hours_per_week 

#After outlier treatment 

np1 = CUV(adultData$Age, "Age") 
np2 = CUV(adultData$Education_num, "Education_num") 
np3 = CUV(adultData$Final_Weight, "Final Weight") 
np4 = CUV(adultData$Hours_per_week, "Hours Per Week") 

plot_grid(np1, np2, np3, np4)

##############################################################################################################################
# Visualization
## Function for displaying histograms using ggplot2 
##############################################################################################################################

a1 = ggplot(adultData, aes(x=Age)) + ggtitle("Age") + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=5, colour="black", fill="white") + ylab("Percentage") 

a2 = ggplot(adultData, aes(x=log10(Final_Weight))) + ggtitle("log(Weight)") + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="white") + ylab("Percentage") 

a3 = ggplot(adultData, aes(x=Education_num)) + ggtitle("Years of Education") +  
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=1, colour="black", fill="white") + ylab("Percentage") 

a4 = ggplot(adultData, aes(x=Hours_per_week)) + ggtitle("Hours per Week") + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="white") + ylab("Percentage") 

a5 = ggplot(adultData, aes(x=log10(Capital_gain+1))) + ggtitle("log(Capital Gain)") + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="white") + ylab("Percentage") +  
  annotate("text", x = 3, y = 50, label = "X", colour="red", size=30, fontface="bold") 

a6 = ggplot(adultData, aes(x=log10(Capital_loss+1))) + ggtitle("log(Capital Loss)") + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="white") + ylab("Percentage") +  
  annotate("text", x = 2, y = 50, label = "X", colour="red", size=30, fontface="bold") 

grid.arrange(a1, a2, a3, a4, a5, a6, ncol=3) 

# Age, log(Weight), Years of Education, and Hours per Week have broad distributions,  
# Therefore, they will not excluded from the model.  
# Capital Gain and Capital Loss,  
# have very narrow distributions, so they will be excluded from the model. 

adultData = subset(adultData, select = -c(Capital_gain, Capital_loss)) 
adultDataNum = subset(adultDataNum, select = -c(Capital_gain, Capital_loss))

#Sort categorical variables in descending order
cat.sort = function(x){reorder(x,x,function(y){-length(y)})} ## Sorting function for categorical variables
cat.var = which(sapply(adultData, is.factor)) ## Find the categorical variables
for (i in cat.var){  ## Apply the sort function on each categorical variable
  adultData[,i] = cat.sort(adultData[,i])   
}
attach(adultData)

#Bar plots of categorical variables 

c1 = ggplot(adultData, aes(x=Workingclass)) + ggtitle("Work Class") + xlab("Work Class") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(adultData$Workingclass))) 

c2 = ggplot(adultData, aes(x=Education)) + ggtitle("Education") + xlab("Education") +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(adultData$Education))) 

c3 = ggplot(adultData, aes(x=Occupation)) + ggtitle("Occupation") + xlab("Occupation") +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(adultData$Occupation))) 

c4 = ggplot(adultData, aes(x=Native_country)) + ggtitle("Native Country") + xlab("Native Country") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(adultData$Native_country))) + 
  annotate("text", x = 21, y = 50, label = "X", colour="red", size=30, fontface="bold") 

grid.arrange(c1, c2, c3, c4, ncol=2) 

# We removde Native_country owing to the fact that it will most likely not be a very meaningful predictor.  
# Out of the 48842 observations, 90% have the value of "United States". 

adultData = subset(adultData, select = -c(Native_country)) 

#Pie charts of categorical variables 

pc1 = ggplot(adultData, aes(x=factor(1), fill=Marital_Status)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Marital Status")  

pc2 = ggplot(adultData, aes(x=factor(1), fill=Relationship)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Relationship")  

pc3 = ggplot(adultData, aes(x=factor(1), fill=Race)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Race") 

pc4 = ggplot(adultData, aes(x=factor(1), fill=Sex)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Sex") 

grid.arrange(pc1, pc2, pc3, pc4, ncol=2) 

#All these variables have reasonable spread of distribution,  
#therefore they will be considered. 

#Correlation between numerical variables 

correlations = cor(adultDataNum) # calculate a correlation matrix for numeric variables 
print(correlations) # display the correlation matrix 
corrplot(correlations) 
#The numerical variables are nearly uncorrelated.

#Recode variables 

#So as to make working with the data easier in the long run,  
#we recoded Income, to be either 1 or 0 instead of ">50K" or "<=50K". 

adultData$Income = ifelse(adultData$Income == "<=50K",0,1) 
adultData$Income = as.factor(adultData$Income) 

##Multivariate Analysis (Continuous Variables)
#Income vs <rest of continuous variables>
#Age vs Income
incomeBelow50K = (adultData$Income == "0")
xlimit = c(min(adultData$Age), max(adultData$Age))
ylimit = c(0, 1600)
h1 = qplot(Age, data = adultData[incomeBelow50K,], margins = TRUE, 
               binwidth = 2, xlim = xlimit, ylim = ylimit, colour = Income)

h2 = qplot(Age, data = adultData[!incomeBelow50K,], margins = TRUE, 
               binwidth = 2, xlim = xlimit, ylim = ylimit, colour = Income)

grid.arrange(h1, h2, nrow = 2)
#Education Number vs Income
xlimit = c(min(adultData$Education_num), max(adultData$Education_num))
ylimit = c(0, 1600)

h5 = qplot(Education_num, data = adultData[incomeBelow50K,], margins = TRUE, 
           binwidth = 2, xlim = xlimit, ylim = ylimit, colour = Income)

h6 = qplot(Education_num, data = adultData[!incomeBelow50K,], margins = TRUE, 
           binwidth = 2, xlim = xlimit, ylim = ylimit, colour = Income)

grid.arrange(h5, h6, nrow = 2)
#Hours Per Week vs Income
xlimit = c(min(adultData$Hours_per_week), max(adultData$Hours_per_week))
ylimit = c(0, 1600)

h7 = qplot(Hours_per_week, data = adultData[incomeBelow50K,], margins = TRUE, 
           binwidth = 2, xlim = xlimit, ylim = ylimit, colour = Income)

h8 = qplot(Hours_per_week, data = adultData[!incomeBelow50K,], margins = TRUE, 
           binwidth = 2, xlim = xlimit, ylim = ylimit, colour = Income)

grid.arrange(h7, h8, nrow = 2)
grid.arrange(h1, h5, h7, h2, h6, h8, nrow = 2)

# After inspecting the predictors Education_num and Education,  
# we see that they are the portraying the same information.  
# Education_num is just the numeric value of Education.  
# We will keep Education because of its interpretability and remove Education_num. 

adultData = subset(adultData, select = -c(Education_num)) 
adultDataNum = subset(adultDataNum, select = -c(Education_num)) 
# Income vs <rest of categorical variables>
#Working Class vs Income
qp1 = qplot(Income, data = adultData, fill = Workingclass) + facet_grid (. ~ Workingclass)
#Education vs Income
qp2 = qplot(Income, data = adultData, fill = Education) + facet_grid (. ~ Education)
#Marital Status vs Income
qp3 = qplot(Income, data = adultData, fill = Marital_Status) + facet_grid (. ~ Marital_Status)
#Occupation vs Income 
qp4 = qplot(Income, data = adultData, fill = Occupation) + facet_grid (. ~ Occupation)
#Relationship vs Income 
qp5 = qplot(Income, data = adultData, fill = Relationship) + facet_grid (. ~ Relationship)
#Race vs Income
qp6 = qplot(Income, data = adultData, fill = Race) + facet_grid (. ~ Race)
#Sex vs Income
qp7 = qplot(Income, data = adultData, fill = Sex) + facet_grid (. ~ Sex)
grid.arrange(qp1, qp3, qp5, qp6, nrow = 2)
#Work Class and Race show weak correlation with Income, 
#Nonetheless all of these variables were selected.

##############################################################################################################################
#Data Partitioning
##############################################################################################################################
#Splitting data into training and test set

#After looking closely at the Income variable, 
#we can see that there is class-imbalance problem.
#Check for class imbalance
ggplot(adultData, aes(x=Income)) +
  geom_bar(stat="count")
# Only about 1/4 of the observations have an Income value of ">$50K". 
# In order to solve this problem, we took sample the data, 
# taking 4000 observations for our training set with equal amounts of 
# randomly selected values for Income and 1000 randomly selected observations 
# from the remainder of the data for the test set.

##Create training and test set

#Separate values of Income
adult.GT50k = subset(adultData, adultData$Income == 1)
adult.LT50k = subset(adultData, adultData$Income == 0)

#Create indices for 2000 observations
set.seed(1)
adult.GT50k.indices = sample(1:nrow(adult.GT50k), 2000)
adult.LT50k.indices = sample(1:nrow(adult.LT50k), 2000)

#Take 2000 random observations from both subsets of Income
adult.GT50k.train = adult.GT50k[adult.GT50k.indices,]
adult.LT50k.train = adult.LT50k[adult.LT50k.indices,]

#Combine subsets and randomize
adult.train = rbind(adult.GT50k.train, adult.LT50k.train)
adult.train = adult.train[sample(nrow(adult.train)),]

#Take row names from training observations
GT50k.rows = row.names(adult.GT50k.train)
LT50k.rows = row.names(adult.LT50k.train)
GT50k.rows = as.numeric(GT50k.rows)
LT50k.rows = as.numeric(LT50k.rows)

#Create subset of adult dataset without training observations
adult.sub = adultData[-GT50k.rows,]
adult.sub = adult.sub[-LT50k.rows,]

#Take 1000 random observations for test set
set.seed(1)
test.indices = sample(1:nrow(adult.sub), 1000)
adult.test = adult.sub[test.indices,]

train_y = adult.train$Income
train_x = adult.train %>% dplyr::select(-Income)
test_y = adult.test$Income
test_x = adult.test %>% dplyr::select(-Income)

##############################################################################################################################
#CLUSTERING
##############################################################################################################################
#MCA from the training set
#Select categorical variables
trainingCat = adult.train %>%
  dplyr::select(Workingclass, Education, Marital_Status, Occupation, Relationship, Race, Sex, Income)

res.mca = MCA(trainingCat, ncp=Inf, quali.sup = c(8))

eigenvalues = res.mca$eig
plot(res.mca)

fviz_mca_ind(res.mca, #individuals
             label = "none", # hide individual labels
             habillage = c(8), # color by groups 
             palette = c("#0000CD", "#E7B800", "#A52A2A", "#DDA0DD"),
             addEllipses = TRUE, 
             ellipse.level=0.99,
             ggtheme = theme_minimal()) 
fviz_mca_ind(res.mca,
             gradient.cols = c("#A52A2A"),
             geom="point",
             col.ind = "cos2")
fviz_mca_biplot(res.mca, # Biplot of individuals and variable categories
                axes = 1:2,
                palette="jco",
                label=c("quali.sup", "quanti.sup"),
                habillage=c(8),
                repel =TRUE,
                ggtheme = theme_minimal())
#To highlight the correlation between variables (active & supplementary) and dimensions
#we used the function fviz_mca_var() with the argument choice = ¡°mca.cor¡±
fviz_mca_var(res.mca,
             axes = 1:2,
             choice="mca.cor",
             repel = TRUE)

#Interpret the first two obtained factors.
summary(res.mca, ncp = 2)
dimdesc(res.mca, axes = 1:2) #correlation between each variable and the principal component of rank s is calculated. correlation coefficients are sorted and significant ones are output

#Decide the number of significant dimensions
newEigenvalues = eigenvalues[ ,1] - mean(eigenvalues[,1])
barplot(newEigenvalues, names.arg=1:length(newEigenvalues), 
        main = "Scree Plot",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
sigDim = eigenvalues[ ,1][newEigenvalues > 0]
plot(sigDim, type="b", main="Eigenvalues")

#Once we have obtained the significant dimensions we will retail the ones that has more than 80% of inertia explained.
cumsum(100*sigDim/sum(sigDim))
barplot(sigDim, names.arg=1:length(sigDim), 
        main = "Scree Plot",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
#As we observe, in the first 18 dimensions there are 82.365290%.
numSigDim = 18


#Reduce the dimensionality and perform a hierarchical clustering
#First we will perform mca again with the significant dimensions previously obtained
res.mca = MCA(trainingCat, ncp=numSigDim, quali.sup = c(8))
#Hierarchical clustering, set clust = -1 to find the optimal clusters
hcpc <- HCPC(res.mca,nb.clust=2)
plot.HCPC(hcpc, choice ="tree")

##############################################################################################################################
#Decision Tree (1st MODEL)
##############################################################################################################################
#Fit model on training set
tree.adult = tree(Income ~ ., data=adult.train)
tree.adult = tree(Income ~ .,
                  data = adult.train,
                  control = tree.control(4000,
                                         mincut = 5,
                                         minsize = 10,
                                         mindev = .003))
#Plot tree
plot(tree.adult)
text(tree.adult, pretty = 0, cex = .6, col = "red")

#Conduct 10-fold cross-validation to prune the tree
cv = cv.tree(tree.adult, FUN=prune.misclass, K=10)

# Best size
best.cv = cv$size[which.min(cv$dev)]
best.cv

#Prune tree
tree.adult.pruned = prune.misclass(tree.adult, best=best.cv)

#Plot pruned tree
plot(tree.adult.pruned)
text(tree.adult.pruned, pretty=0, col = "blue", cex = .8)

#Create function to calculate misclassification error rate
error_rate = function(predicted.value, true.value){
  return(mean(true.value!=predicted.value))
}

#Predict pruned tree on training and test set
tree.pred.train.pruned = predict(tree.adult.pruned, adult.train, type="class")
tree.pred.test.pruned = predict(tree.adult.pruned, adult.test, type="class")

#Calculate train and test error on pruned tree
tree.errors.pruned = data.frame(train.error = error_rate(tree.pred.train.pruned, train_y),
                                test.error = error_rate(tree.pred.test.pruned, train_y))
tree.errors.pruned

summary(tree.adult.pruned)

#confusion matrix
prediction = predict(tree.adult.pruned, newdata = adult.test, type = "class")
cmDT <- confusionMatrix(prediction,
                        factor(adult.test$Income),
                        positive="1",
                        dnn = c("Prediction", "Reference")
)
cmDT$table %>%
  data.frame() %>% 
  mutate(Prediction = factor(Prediction, levels = c("1", "0"))) %>%
  group_by(Reference) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Reference, Freq / total, 0),
    frac = Freq / total * frac_fill
  ) %>%
  mutate(frac_directed = if_else(Prediction == "0", frac_fill * - 1, frac_fill)) %>%
  ggplot(aes(Prediction, Reference, fill = frac_directed)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient2(low = "red", mid = "white", high = "#badb33") +
  scale_x_discrete(position = "top")

##############################################################################################################################
#Random Forest (2nd Model)
##############################################################################################################################

#Set lists of errors to value 0 and list length eqaul to number of predictor values
train.error.rf = test.error.rf <- rep(0, length(train_x))

#Run random forest model on different number of predictors and calculate training/test errors
for(i in 1:length(train_x)){
  #Fit random forest model with 2000 trees and i predictors
  rf.train = randomForest(Income~., data = adult.train, mtry = i, ntree=2000, importance = TRUE)
  
  #Predict on training and test set
  Forest.pred.train = predict(rf.train, type="class")
  Forest.pred.test = predict(rf.train, adult.test, type="class")
  
  #Calculate train and test error
  train.error.rf[i] = error_rate(Forest.pred.train, train_y)
  test.error.rf[i] = error_rate(Forest.pred.test, test_y)
}

#Now we can create a dataframe containing all training and 
#test errors for each set number of predictors used in the model.
Forest.errors = data.frame(train.error.rf = train.error.rf,
                           test.error.rf = test.error.rf,
                           mtry = 1:length(train_x))
Forest.errors


#Choose number of predictors that has the lowest test error
best.num.predictors = Forest.errors$mtry[which.min(Forest.errors$test.error.rf)]

#Show training error, test error, and number of predictors
Forest.errors[best.num.predictors,]

#Fit model with best number of predictors
best.rf.train = randomForest(Income~.,
                              data = adult.train,
                              mtry = best.num.predictors,
                              ntree=2000,
                              importance = TRUE)

#Plot variable importance
varImpPlot(best.rf.train)

#Confusion Matrix
cmRF <- confusionMatrix(Forest.pred.test,
                        factor(adult.test$Income),
                        positive="1",
                        dnn = c("Prediction", "Reference")
)
cmRF$table %>%
  data.frame() %>% 
  mutate(Prediction = factor(Prediction, levels = c("1", "0"))) %>%
  group_by(Reference) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Reference, Freq / total, 0),
    frac = Freq / total * frac_fill
  ) %>%
  mutate(frac_directed = if_else(Prediction == "0", frac_fill * - 1, frac_fill)) %>%
  ggplot(aes(Prediction, Reference, fill = frac_directed)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient2(low = "red", mid = "white", high = "#badb33") +
  scale_x_discrete(position = "top")

##############################################################################################################################
#Support Vector Machine (3rd Model)
##############################################################################################################################
#Linear
#Fit the model on the training set
model_svm_linear <- train(
  Income ~., data = adult.train, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale")
)
#Make predictions on the test data
predicted.classes_svm_linear <- model_svm_linear %>% predict(adult.test)
head(predicted.classes_svm_linear)

#Compute model accuracy rate
mean(predicted.classes_svm_linear == adult.test$Income)

# Fit the model on the training set
model_svm_linear_tuning <- train(
  Income ~., data = adult.train, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
  preProcess = c("center","scale")
)
# Plot model accuracy vs different values of Cost
plot(model_svm_linear_tuning)

# Print the best tuning parameter C that
# maximizes model accuracy
model_svm_linear_tuning$bestTune

# Make predictions on the test data
predicted.classes_svm_linear_final <- model_svm_linear_tuning %>% predict(adult.test)
# Compute model accuracy rate
mean(predicted.classes_svm_linear_final == adult.test$Income)

cmSVM_model_svm_linear_tuning <- confusionMatrix(predicted.classes_svm_linear_final,
                                   factor(adult.test$Income),
                                   positive="1",
                                   dnn = c("Prediction", "Reference")
)
cmSVM_model_svm_linear_tuning$table %>%
  data.frame() %>% 
  mutate(Prediction = factor(Prediction, levels = c("1", "0"))) %>%
  group_by(Reference) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Reference, Freq / total, 0),
    frac = Freq / total * frac_fill
  ) %>%
  mutate(frac_directed = if_else(Prediction == "0", frac_fill * - 1, frac_fill)) %>%
  ggplot(aes(Prediction, Reference, fill = frac_directed)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient2(low = "red", mid = "white", high = "#badb33") +
  scale_x_discrete(position = "top")


#Non-linear
# Fit the model on the training set
svm_nonlinear <- train(
  Income ~., data = adult.train, method = "svmRadial",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 10
)
# Print the best tuning parameter sigma and C that
# maximizes model accuracy
svm_nonlinear$bestTune
# Make predictions on the test data
predicted.svm_nonlinear <- svm_nonlinear %>% predict(adult.test)
# Compute model accuracy rate
mean(predicted.svm_nonlinear == adult.test$Income)

#Confusion Matrix
cmSVM_nonlinear <- confusionMatrix(predicted.svm_nonlinear,
                                   factor(adult.test$Income),
                                   positive="1",
                                   dnn = c("Prediction", "Reference")
)
#Visualization 
cmSVM_nonlinear$table %>%
  data.frame() %>% 
  mutate(Prediction = factor(Prediction, levels = c("1", "0"))) %>%
  group_by(Reference) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Reference, Freq / total, 0),
    frac = Freq / total * frac_fill
  ) %>%
  mutate(frac_directed = if_else(Prediction == "0", frac_fill * - 1, frac_fill)) %>%
  ggplot(aes(Prediction, Reference, fill = frac_directed)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient2(low = "red", mid = "white", high = "#badb33") +
  scale_x_discrete(position = "top")


##############################################################################################################################
#Naive Bayes (4th Model)
##############################################################################################################################
# Build the model
model_NB <- train(Income ~., data = adult.train, method = "nb", 
                  trControl = trainControl("cv", number = 10))
# Make predictions
predicted.classes <- model_NB %>% predict(adult.test)
# Model n accuracy
mean(predicted.classes == adult.test$Income)
cmNB <- confusionMatrix(predicted.classes,
                        factor(adult.test$Income),
                        positive="1",
                        dnn = c("Prediction", "Reference")
)
cmNB$table %>%
  data.frame() %>% 
  mutate(Prediction = factor(Prediction, levels = c("1", "0"))) %>%
  group_by(Reference) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Reference, Freq / total, 0),
    frac = Freq / total * frac_fill
  ) %>%
  mutate(frac_directed = if_else(Prediction == "0", frac_fill * - 1, frac_fill)) %>%
  ggplot(aes(Prediction, Reference, fill = frac_directed)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient2(low = "red", mid = "white", high = "#badb33") +
  scale_x_discrete(position = "top")
