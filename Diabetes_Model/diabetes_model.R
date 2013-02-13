# =========================================================================
# Title: model_for_diabetes_and_food_availability.R
# Author: Matt Buckley
# Description: Data organization and regressions 
# Sources: United Nations Food and Agriculture Organization ('Food Balance
# Sheets'), World Bank, WHO ('Nutrition Labels and Health Claims: The Global
# Regulatory Environment'), WHO ('Country and Regional Data on Diabetes')
# Liscense: MIT license: http://www.opensource.org/licenses/mit-license.php 
# =========================================================================

# =========================================================================
# Population downloaded from the World Bank Human Development Indicator online
# database
# =========================================================================
populations_2000 <- read.csv('populations_2000_2.csv', header=T)
populations.df <- data.frame(population_2000=populations_2000[,'X2000'], 
                             row.names=populations_2000[,'Country.Name'])

rows <- character(0)
for (i in populations_2000[,'Country.Name']) {
  rows <- c(rows, i)
}
rows <- gsub(' ','_',rows)
rows <- gsub('Venezuela,_RB', 'Venezuela', rows)
rows <- gsub('United_States', 'United_States_of_America', rows)
rows <- gsub('United_Kingdom', 'United_Kingdom_of_Great_Britain_and_Northern_Ireland', rows)
rows <- gsub('Egypt,_Arab_Rep.', 'Egypt', rows)
rows <- gsub('Bahamas,_The', 'Bahamas', rows)
rows <- gsub('Vietnam', 'Viet_Nam', rows)
row.names(populations.df) <- rows


all_data <- data.frame()
rwnames <- gsub('./', '', list.dirs(recursive=F))

# =========================================================================
# Food availability data downloaded from UNFAOSTAT
# =========================================================================

for (subdir in list.dirs(recursive=F)) {
  setwd(subdir)
  
  csvs <- list(c_1975=read.delim('consumption_1975.csv', header=T, fileEncoding='UCS-2LE'),
               c_2005=read.delim('consumption_2005.csv', header=T, fileEncoding='UCS-2LE'))
  
  kcal_info <- numeric(0)
  
  for (i in csvs) {
    kcal_info=cbind(kcal_info, as.numeric(i$Food.supply..kcal.capita.day.))
  }
  
  colnames(kcal_info)=c('c_1975','c_2005')
  
  clean_rownames <- function(x) {
    # tolower
    x = tolower(x)
    # remove +
    x = gsub("+", "", x)
    # remove punctuation
    x = gsub("[[:punct:]]", "", x)
    # remove numbers
    x = gsub("[[:digit:]]", "", x)
    # remove tabs
    x = gsub("[ |\t]{2,}", "", x)
    # remove blank spaces
    x = gsub(" ", "", x)
    return(x)
  }
  
  c_1975 <- read.delim('consumption_1975.csv', header=T, fileEncoding='UCS-2LE')
  
  rnames <- character(0)
  
  for (name in c_1975[,3]) {
    rnames=c(rnames, name)
  }
  
  rownames(kcal_info) <- clean_rownames(rnames)
  
  kcal_info[is.na(kcal_info)] <- 0
  kcal_info <- data.frame(kcal_info[-1,])
  kcal_info <- rbind(kcal_info['vegetalproductstotal',], kcal_info['animalproductstotal',],
                     kcal_info['cerealsexcludingbeertotal',], kcal_info['wheat',],
                     kcal_info['ricemilledequivalent',], kcal_info['maize',], 
                     kcal_info['starchyrootstotal',],kcal_info['sugarsweetenerstotal',], 
                     kcal_info['pulsestotal',],kcal_info['treenutstotal',],
                     kcal_info['oilcropstotal',], kcal_info['vegetableoilstotal',], 
                     kcal_info['vegetablestotal',], kcal_info['fruitsexcludingwinetotal',], 
                     kcal_info['stimulantstotal',], kcal_info['alcoholicbeveragestotal',],
                     kcal_info['bovinemeat',], kcal_info['pigmeat',], kcal_info['poultrymeat',],
                     kcal_info['animalfatstotal',], kcal_info['milkexcludingbuttertotal',], 
                     kcal_info['eggstotal',],kcal_info['fishseafoodtotal',])
  
  k_1975 <- t(kcal_info[,'c_1975'])
  
  
  k_2005 <- t(kcal_info[,'c_2005'])
  
  k_average_1975_2005<-(k_1975+k_2005)/2
  
  newnames <- character(0)
  for (rowname in row.names(kcal_info)) {
    newnames=c(newnames, rowname)
  }
  
  colnames(k_average_1975_2005) <- newnames
  
  consumption_all <- data.frame(k_average_1975_2005)
  
  all_data <- rbind(all_data, consumption_all)
  
} # end loop thru subdirectories

row.names(all_data) <- rwnames

# =========================================================================
# Scrape data on diabetes prevalence from the relevant page on the World 
# Health Organization site
# =========================================================================
library(XML)
library(RCurl)

# the url of the section of the WHO website that contians diabetes info
who_url <- "http://www.who.int/diabetes/facts/world_figures/en/index"

# a vector to store the results
results<-character(0)

# go through all 6 pages containing the info we want, and store
# the html in a list
for (page in 1:6) {
  who_search <- paste(who_url, page, '.html', sep='')
  page = readHTMLTable(htmlParse(who_search))
  results = c(page, results)
}



df <- data.frame()

for (i in results) {
  df <- rbind(df,i)
}

df[,'2000'] <- gsub(',', '', df[,'2000'])
df[,'2030'] <- gsub(',','', df[,'2030'])
df['Country'] <- gsub(' ', '_', df[,'Country'])
df <- df[df[,1] != 'Total',]

country_subset <- df[(df[,1] %in% rownames(all_data)),]

diabetes_data <- cbind (country_subset, populations.df)

diabetes_data <- cbind(diabetes_data[,2:4], 
                       normalized_diabetes_rates_2000=(as.numeric(diabetes_data[,'2000'])/as.numeric(diabetes_data[,'population_2000'])),
                       row.names=diabetes_data[,1])

# =========================================================================
# Assign an index value corresponding to the relative strength of food
# labeling laws in individual countries (from 0 to 3, 3 corresponding 
# to the most comprehensive regulatory environment)
# Index values drawn from WHO PDF document
# =========================================================================

diabetes_data <- cbind(diabetes_data, data.frame(0)) 
colnames(diabetes_data) <- c(colnames(diabetes_data[,1:4]), 'rgltry_env')

diabetes_data['China',][,'rgltry_env'] <- 1
diabetes_data['Kuwait',][,'rgltry_env'] <- 1
diabetes_data['Saudi_Arabia',][,'rgltry_env'] <- 1
diabetes_data['United_Arab_Emirates',][,'rgltry_env'] <- 1
diabetes_data['United_States_of_America',][,'rgltry_env'] <- 3
diabetes_data['Sweden',][,'rgltry_env'] <- 2
diabetes_data['Norway',][,'rgltry_env'] <- 2
diabetes_data['Germany',][,'rgltry_env'] <- 2
diabetes_data['France',][,'rgltry_env'] <- 2
diabetes_data['Italy',][,'rgltry_env'] <- 2
diabetes_data['Greece',][,'rgltry_env'] <- 2
diabetes_data['Spain',][,'rgltry_env'] <- 2
diabetes_data['United_Kingdom_of_Great_Britain_and_Northern_Ireland',][,'rgltry_env'] <- 2
diabetes_data['Argentina',][,'rgltry_env'] <- 3
diabetes_data['Australia',][,'rgltry_env'] <- 3
diabetes_data['Brazil',][,'rgltry_env'] <- 3
diabetes_data['Israel',][,'rgltry_env'] <- 3
diabetes_data['New_Zealand',][,'rgltry_env'] <- 3
diabetes_data['Paraguay',][,'rgltry_env'] <- 3
diabetes_data['Uruguay',][,'rgltry_env'] <- 3
diabetes_data['Austria',][,'rgltry_env'] <- 2
diabetes_data['Chile',][,'rgltry_env'] <- 2
diabetes_data['Denmark',][,'rgltry_env'] <- 2
diabetes_data['Ecuador',][,'rgltry_env'] <- 2
diabetes_data['Finland',][,'rgltry_env'] <- 2
diabetes_data['Japan',][,'rgltry_env'] <- 2
diabetes_data['Mexico',][,'rgltry_env'] <- 2
diabetes_data['Netherlands',][,'rgltry_env'] <- 2
diabetes_data['Portugal',][,'rgltry_env'] <- 2
diabetes_data['South_Africa',][,'rgltry_env'] <- 2
diabetes_data['Spain',][,'rgltry_env'] <- 2
diabetes_data['Sweden',][,'rgltry_env'] <- 2
diabetes_data['Switzerland',][,'rgltry_env'] <- 2
diabetes_data['Viet_Nam',][,'rgltry_env'] <- 2
diabetes_data['United_Kingdom_of_Great_Britain_and_Northern_Ireland',][,'rgltry_env'] <- 2
diabetes_data['Costa_Rica',][,'rgltry_env'] <- 1
diabetes_data['Mauritius',][,'rgltry_env'] <- 1
diabetes_data['Morocco',][,'rgltry_env'] <- 1
diabetes_data['Nigeria',][,'rgltry_env'] <- 1
diabetes_data['Peru',][,'rgltry_env'] <- 1
diabetes_data['Poland',][,'rgltry_env'] <- 1
diabetes_data['Saudi_Arabia',][,'rgltry_env'] <- 1
diabetes_data['United_Arab_Emirates',][,'rgltry_env'] <- 1
diabetes_data['Venezuela',][,'rgltry_env'] <- 1

diabetes_data <- diabetes_data[order(row.names(diabetes_data)),]

# Normalize diabetes data and append the resulting columns to the food availability data drawn gathered earlier

all_data <- data.frame(cbind (normalized_diabetes_rates_2000=diabetes_data[,'normalized_diabetes_rates_2000'],
                              labeling_reg_index=diabetes_data[,'rgltry_env'], all_data), row.names=row.names(diabetes_data))


# =========================================================================
# investigate variable importnace with 'regsubsets' and 'randomForest';
# use optim.lm function to try to select the “best” linear model from my training data:
# =========================================================================

data<-all_data
y<-'normalized_diabetes_rates_2000'
val.size<-25
cv<-10
really.big<-T

library(MASS)
library(leaps)
library(randomForest)
n <- nrow(data)
names(data)[(names(data)==y)] <- "y"
names(data)[1] <- "y"
val.size <- round(n/4)
s <- sample(n,val.size,replace=F)
data.train <- data[-s,]
data.test <- data[s,]
p <- ncol(data)-1
data.names <- names(data)[names(data)!="y"]
regfit.full <-regsubsets(y~.,data=data,nvmax=p,nbest=1,really.big=really.big)
plot(regfit.full,scale="adjr2")
bic.lm <-lm(as.formula(paste("y~",paste(data.names[summary(regfit.full)$which[order(summary(regfit.full)$bic)[1],][-1]],collapse="+"))),data=data)
plot(bic.lm)
regfit <- regsubsets(y~.,data=data.train,nvmax=p,nbest=1,really.big=really.big)
plot(regfit, scale='adjr2')
cv.rss <- rootmse <- rep(0,p)
validation.lm <-lm(as.formula(paste("y~",paste(data.names[summary(regfit)$which[order(rootmse)[1],][-1]],collapse="+"))),data=data)
s <- sample(cv,n,replace=T)
if (cv==n)
  s <- 1:n
for (i in 1:p)
  for (j in 1:cv){
    data.train <- data[s!=j,]
    data.test <- data[s==j,]
    data.lmfit<-lm(as.formula(paste("y~",paste(data.names[summary(regfit.full)$which[i,][-1]],collapse="+"))),data=data.train)
    cv.rss[i]<-cv.rss[i]+sum((predict(data.lmfit,data.test)-data.test$y)^2)
  }
cv.rss <- sqrt(cv.rss/n)
plot(cv.rss,type='l',xlab="Number of Predictors",
     ylab="Cross-Validated Root Mean RSS",main="Cross-Validation Method")
points(cv.rss,pch=20)
points(order(cv.rss)[1],min(cv.rss),col=2,pch=20,cex=1.5)
cv.lm <-lm(as.formula(paste("y~",paste(data.names[summary(regfit.full)$which[order(cv.rss)[1],][-1]],collapse="+"))),data=data)
rand_forest<-randomForest(y~.,data=data.train,
                          xtest=data.test[,2:25], ytest=data.test[,1],importance=TRUE,proximity=TRUE,ntree=150)
varImpPlot(rand_forest, main='Random Forest: Variable Importance')
plot(rand_forest, main='Random Forest: Number of Trees vs Error')
optim.lm<-list(bic.lm = bic.lm,validation.lm=validation.lm,cv.lm=cv.lm)


# Report the model selected by BIC, validation, or CV (cross validation)
summary(optim.lm$bic)
summary(optim.lm$validation)
summary(optim.lm$cv)

# Save the output to text files for later review

capture.output(summary(optim.lm.train$bic), file='optim_lm_train_bic.txt')
capture.output(summary(optim.lm.train$validation), file='optim_lm_train_validation.txt')
capture.output(summary(optim.lm.train$cv), file='optim_lm_train_cv.txt')


# =========================================================================
# use 'lars' package to specify a model that penalizes for complexity (in 
# this case, LASSO);
# =========================================================================

library(lars)

n <- nrow(all_data)
val.size <- round(n/4)
td <- sample(n,val.size,replace=F)
data.train <- all_data[-td,]
data.test <- all_data[td,]


cv.lasso <-
  function(formula,data,subset=NULL,K=10){
    if (!is.null(subset))
      data <- data[subset,]
    y <- data[,names(data)==as.character(formula)[2]]
    x <- model.matrix(as.formula(formula),data)[,-1]
    larsfit <- cv.lars(x,y,K=K)
    larsfit
  }

lasso <-
  function(formula,data,subset=NULL){
    if (!is.null(subset))
      data <- data[subset,]
    y <- data[,names(data)==as.character(formula)[2]]
    x <- model.matrix(as.formula(formula),data)[,-1]
    larsfit <- lars(x,y,type="lasso")
    larsfit
  }


lasso.fit<-lasso(normalized_diabetes_rates_2000~., all_data, subset=td)
plot(lasso.fit, xvar='df')
lasso.fit
lasso.p<-lars(x=model.matrix(normalized_diabetes_rates_2000~., all_data), y=all_data[,1], type="lasso")
## use cv.lasso to get best s:
lasso.cv<-cv.lasso(normalized_diabetes_rates_2000~., all_data, subset=td)
s <- lasso.cv$index[which.min(lasso.cv$cv)]
s
lasso.pred <- predict.lars(lasso.p, newx=model.matrix(normalized_diabetes_rates_2000~., data.test), s=s, type='fit', mode='fraction')
mean((data.test[,1] - lasso.pred$fit)^2)
predict.lars(lasso.fit, newx=model.matrix(normalized_diabetes_rates_2000~., data.test), s=s, type='coefficients', mode='fraction')
# iterations 
s.set <- seq(0, 1, length = 100)
rss.lasso <- rep(0, 100)
for(i in 1:100){
  lasso.pred <- predict.lars(lasso.p, newx=model.matrix(normalized_diabetes_rates_2000~., data.test), s = s.set[i], type='fit', mode='norm')
  rss.lasso[i] <- mean((data.test[,1] - lasso.pred$fit)^2)
}
min(rss.lasso)
plot(rss.lasso,type="l", main="Lasso: Root Sum of Squares vs Index")
s<-s.set[order(rss.lasso)[1]]
s
final_coefs<-predict.lars(lasso.fit, newx=model.matrix(normalized_diabetes_rates_2000~., data.test), s=s, type='coefficients', mode='fraction')
final_coefs$coefficients

# save output to a text file
capture.output(final_coefs$coefficients, file='lasso_fit_coefficients.txt')

# =========================================================================
# we plot predicted mean OLS, BIC and LASSO in one plot to see which yields 
# the lowest error
# =========================================================================


rss.raw<-mean((all_data$normalized_diabetes_rates_2000[-td]-mean(all_data$normalized_diabetes_rates_2000[td]))^2)
rss.raw

lmfit<-lm(normalized_diabetes_rates_2000~.,all_data,subset=td)
rss.ols<-mean((all_data$normalized_diabetes_rates_2000[-td]-predict(lmfit,all_data)[-td])^2)
rss.ols

rss.bic<-mean((all_data$normalized_diabetes_rates_2000[-td]-predict(bic.lm,all_data)[-td])^2)
rss.bic


plot(1:100,1:100,ylim=c(1,125),ylab="Test Mean RSS",xlab="Tuning Parameter", type="n", main="Fit Comparison: OLS, Lasso, BIC")
abline(rss.raw,0,lwd=1,lty=2, col = "green")
abline(rss.ols,0,lwd=1,lty=3, col = "blue")
abline(rss.bic,0,lwd=1,lty=4, col = "grey")
lines(rss.lasso,lwd=1,lty=5, col = "red")
legend(60,100,c("Raw","OLS","BIC","LASSO"),col = c("green", "blue", "grey", "red"), lty=c(2,3,4,5),lwd=1)