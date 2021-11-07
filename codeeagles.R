#bald eagle for Pappa
eagles= read.csv("Maike_Logistic_Data.csv")
#drop obs that do not have values for substrate
eagles = eagles[complete.cases(eagles), ]

head(eagles)
library(plyr)
#change 'other' to 'ground'
sub = mapvalues(eagles$Substrate, from = "OTHER", to = "GROUND")
eagles$sub = sub


#look at scatter matrix
library(gclus)
df <- eagles[, c(1,2,5,6)]# get data 
str(df)
dfcorrs <- abs(cor(df)) # get correlations
dta.col <- dmat.color(dfcorrs) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
#looks like dist shoreline and dist thalweg have a logistic assocation.
dforder <- order.single(dfcorrs) 
cpairs(df, dforder, panel.colors=dta.col, gap = .5,
       main="Variables Ordered and Colored by Correlation" )

#convert age to numeric
eagles$Age_Class = as.numeric(eagles$Age_Class)
age = eagles$Age_Class
ifelse(age == 1, 1, ifelse(age == 2, 2, 3))
age=ifelse(age == 1, 1, 0)
eagles$age = age

#convert year to 0/1
Year = eagles$Year
year=ifelse(Year == 2003, 0, 1)
eagles$year01 = year

#standardize distance to shorline
eagles$dist.s = (eagles$Dist_Shoreline - mean(eagles$Dist_Shoreline)) / sd(eagles$Dist_Shoreline)


#look at distribution of obs/not observed over years
#table looks OK...not so many 0's in year 2003
with(eagles, table(Year, Observed))
with(eagles, table(sub, Observed))
with(eagles, table(age, Observed))
with(eagles, table(age, Year))


eaglemod = glm(Observed ~   dist.s + age  + year01, family = binomial(link = 'logit'), data=eagles)
summary(eaglemod)


#random forest just for fun...
library(randomForest)
head(eagles)
str(eagles)
eagles$factObs = as.factor(as.character(eagles$Observed))
str(eagles)
rfeagles = randomForest(factObs ~ Year + sub + Age_Class + log_Dist_Sh , importance = TRUE, n.trees  =1500, data = eagles)
varImpPlot(rfeagles)
imp = rfeagles$importance
plot(rfeagles)

impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 2))

for (i in seq_along(impvar)) {
  partialPlot(rfeagles, eagles, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
}

par(mfrow = c(1,1))
partialPlot(rfstream, train, "AirMwmt", xlim = c(35, 40), ylim= c(16, 17.5))

############################################################################
#bayesian model
############################################################################
newdf = as.data.frame(cbind(eagles$Observed, eagles$dist.s, eagles$age, eagles$year01))
names(newdf) = c("Observed",'DistShorlineS', 'age', 'year')
head(newdf)

#set variables for bayesian model
Observed = newdf$Observed
DistShorelineS = newdf$DistShorlineS
age = newdf$age
year = newdf$year

library(rjags)
modelstring2 = "
model {
#likelihood

for (i in 1:N) {

Observed[i] ~ dbern(mu[i])  

logit(mu[i]) <- beta0 + beta1 * DistShorelineS[i] + beta2*age[i] + beta3*year[i]

}
#priors for beta0 and beta1
beta0 ~ dnorm(0, 1/25)
beta1 ~ ddexp(0, sqrt(2))
beta2 ~ ddexp(0, sqrt(2))
beta3 ~ ddexp(0, sqrt(2))
}"


#library(rjags)
writeLines(modelstring2, con='code.jags.txt')

library(rjags)
jagsLR <- jags.model('code.jags.txt',
                     data = list('Observed' = Observed,
                                 'DistShorelineS' = DistShorelineS,
                                 "age" = age,
                                 "year" = year,
                                 'N' = length(DistShorelineS)),
                     inits<-list(
                       list('beta0' = 0.1, 'beta1' = .1, 'beta2' = -0.2, 'beta3' = -0.1),
                       list('beta0' = 0.01, 'beta1' = 0.2, 'beta2' = 0.05, 'beta3' = 0.1),
                       list('beta0' = 0.1, 'beta1' = 0.2, 'beta2' = 1, 'beta3' = -0.2),
                       list('beta0' = -0.2, 'beta1'= -0.1, 'beta2' = 0.1, 'beta3' = 0.2)),
                     n.chains = 4,
                     n.adapt = 100)

update(jagsLR, 1000)

jags.samples(jagsLR,
             c('beta0', 'beta1'),
             1000)

codaSamplesLR = coda.samples(jagsLR, c('beta0','beta1', 'beta2' , 'beta3'), 10000, 1)
jagsLR_csim = as.mcmc(do.call(rbind, codaSamplesLR))

#diagnostic plots and posterior densities
plot(codaSamplesLR, density = TRUE)

#summaries

summary(codaSamplesLR)

#trace plots
traceplot(codaSamplesLR)

#gelman diags -ok
gelman.diag(codaSamplesLR)

#autocorrelation -ok
autocorr.plot(codaSamplesLR)
par(mfrow = c(2,2))
densplot(jagsLR_csim)
HPDinterval(jagsLR_csim, prob = 0.90)
head(jagsLR_csim)


#separate coeffients 
beta1 = jagsLR_csim[,2]
beta2 = jagsLR_csim[,3]
beta3 = jagsLR_csim[,4]

#look at posterior probabilities...more can be added--these are just some 
head(jagsLR_csim)
mean(beta2 > .5)
mean(beta2 > 1)
mean(beta2 > 1.5)
mean(beta3 < -.3)
mean(beta3 < -.9)
#get coefficients (posterior means)
(pm_coef = colMeans(jagsLR_csim))


