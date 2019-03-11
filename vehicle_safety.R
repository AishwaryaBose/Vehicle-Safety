#install and load necessary packages
library(e1071)
library(caret)
library(lattice)
library(ggplot2)
library(mice)
library(VIM)

# Reading the data set into R
vs = read.csv("vehicle_safety_NASS2010_2000_2012.csv")

# NA value removal from output variable(OA_MAIS)
nrow(vs[!is.na(vs$OA_MAIS),]) # gives the number of records for which OA_MAIS is NULL
vs = vs[!is.na(vs$OA_MAIS),]  # removes the null records in target variable

#Splitting OA_MAIS into categories
vs$OA_MAIS = ifelse(vs$OA_MAIS == 0 ,"0% PD",
                ifelse(vs$OA_MAIS == 1, "0% PD",
                       ifelse(vs$OA_MAIS == 2, "1-2% PD",
                              ifelse(vs$OA_MAIS == 3, "8-10% PD",
                                     ifelse(vs$OA_MAIS == 4,"5-50% PD",
                                            ifelse(vs$OA_MAIS == 5,"5-50% PD",
                                                   ifelse(vs$OA_MAIS == 6,"100% PD","")))))))
    
                
vs$OA_MANUSE = ifelse(vs$OA_MANUSE == 0 , "Not used", ifelse( vs$OA_MANUSE == 1, "Used",""))              

vs$OA_MAIS = as.factor(vs$OA_MAIS)
vs$OA_MANUSE = as.factor(vs$OA_MANUSE)

#Imputation of missing values

md.pattern(vs)
#Plotting missing values in the table
aggr_plot <- aggr(vs, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(vs), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

imputed_vs = mice(vs, m=5, maxit=5, method = 'pmm', seed = 500) #using predictive mean matching method
densityplot(imputed_vs)
# 5 data sets for different imputations
vs_data1 = complete(imputed_vs,1)
vs_data2 = complete(imputed_vs,2)
vs_data3 = complete(imputed_vs,3)
vs_data4 = complete(imputed_vs,4)
vs_data5 = complete(imputed_vs,5)
write.csv(vs_data5, file = "vs_imputed_v2.csv")

# DATA exploration & optimisation
ggplot(vs, aes(x= vs$OA_MAIS)) + geom_bar() + theme_bw() # minor moderate and not injury form the majority
ggplot(vs, aes(x= vs_data5$GV_CURBWGT)) + geom_density() + theme_bw()# <1250 ; 1250:1750 ; 1750:2250 ; >2250 (39NA)
ggplot(vs, aes(x= vs_data5$GV_ENERGY)) + geom_density() + theme_bw() # (5877 NA) logistic continuous nature - fading after 2500 (skewed)
ggplot(vs, aes(x= vs$GV_LANES)) + geom_bar() + theme_bw() # (3 NA) max on 2, same for 3,4,5
ggplot(vs, aes(x= vs$GV_MODELYR)) + geom_bar() + theme_bw() #(0 NA)(dec nature)
ggplot(vs, aes(x= vs$GV_OTVEHWGT)) + geom_density() + theme_bw() # (1992 NA)(binning - 1500)
ggplot(vs, aes(x= vs$GV_SPLIMIT)) + geom_histogram(binwidth = 5) + theme_bw() # (221 NA)
ggplot(vs, aes(x= vs$GV_WGTCDTR)) + geom_bar() + theme_bw()  #(0 NA)
ggplot(vs, aes(x=vs$OA_AGE)) + geom_density() + theme_bw() #(15 NA) take log to remove skewness; followed by binning <3.25; 3.25:3.5 ; 3.5:3.75 ; >3.75
ggplot(vs, aes(x= vs$OA_BAGDEPLY)) + geom_bar() + theme_bw() # (0NA) not much difference
ggplot(vs, aes(x= vs$OA_HEIGHT)) + geom_density() + theme_bw() #(2116 NA) binning on 10 cm interval(150:200)
ggplot(vs, aes(x= vs$OA_MANUSE)) + geom_bar() + theme_bw() #(0 NA) max people have used seatbelts
ggplot(vs, aes(x= vs_data5$OA_SEX)) + geom_bar() + theme_bw() #(0NA) equally distributed between male female
ggplot(vs, aes(x= vs_data5$OA_WEIGHT)) + geom_density() + theme_bw() #(2008 NA) take log to remove skew; <4; 4:4.25 ; 4.25:4.5; >4.5
ggplot(vs, aes(x= vs_data5$VE_GAD1)) + geom_bar() + theme_bw() #(0NA) max front
ggplot(vs, aes(x= vs$VE_PDOF_TR)) + geom_density() + theme_bw() #(1864 NA) unimodal distribuition 0:100, 100:200, 200:300, >300
ggplot(vs, aes(x= vs$GV_FOOTPRINT)) + geom_density() + theme_bw()#(223 NA)
ggplot(vs, aes(x= vs$GV_DVLAT)) + geom_density() + theme_bw() #(5877 NA)
ggplot(vs, aes(x= vs$GV_DVLONG)) + geom_density() + theme_bw()#(5877 NA)

#split into test and train data
vs_imputed = read.csv("vs_imputed_v2.csv")
set.seed(2)
id = sample(2,nrow(vs_imputed), prob  = c(0.7,0.3), replace = TRUE)
vs_train = vs_imputed[id == 1,]
vs_test = vs_imputed[id == 2,]

# making of the model
vs_model = naiveBayes(OA_MAIS~ GV_CURBWGT + GV_DVLAT + GV_DVLONG + GV_ENERGY + GV_LANES + GV_MODELYR + GV_OTVEHWGT + GV_SPLIMIT + GV_WGTCDTR + OA_AGE + OA_BAGDEPLY + OA_HEIGHT + OA_MANUSE + OA_SEX + OA_WEIGHT +VE_GAD1 + VE_ORIGAVTW + VE_WHEELBAS + VE_PDOF_TR + GV_FOOTPRINT, vs_train)
vs_model
summary(vs_model)

#prediction on test data
prediction = predict(vs_model,newdata =  vs_test)
prediction
confusionMatrix(table(prediction, vs_test$OA_MAIS))

