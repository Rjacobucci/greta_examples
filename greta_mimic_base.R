
#devtools::install_github("greta-dev/greta@dev")
library(greta)
library(rstan)
library(MASS);library(lavaan)
NN <- 2000
PP = 50
#X.noise = mvrnorm(2000, rep(0,950),matrix(diag(950),950,950))
X.small = mvrnorm(NN, rep(0,PP),matrix(diag(PP),PP,PP))

f = X.small%*%rep(0.3,PP) + rnorm(NN,0,sqrt(1)) # X.noise%*%rep(0,950) + 
y1 = 0.7*f + (1-.7**2)*rnorm(NN,0,1) 
y2 = 0.7*f + (1-.7**2)*rnorm(NN,0,1)  
y3 = 0.7*f + (1-.7**2)*rnorm(NN,0,1)  
y4 = 0.7*f + (1-.7**2)*rnorm(NN,0,1)  
y5 = 0.7*f + (1-.7**2)*rnorm(NN,0,1)  
y6 = 0.7*f + (1-.7**2)*rnorm(NN,0,1)  




X = as.matrix(cbind(y1,y2,y3,y4,y5,y6))
cov= as.matrix(cbind(X.small))
N <- nrow(X)

dat <- cbind(X,cov)
colnames(dat) <- c("y1","y2","y3","y4","y5","y6",
                   "x1","x2","x3","x4","x5")

lav.mod <- "
f =~ NA*y1 + y2 + y3 + y4 + y5 + y6
f ~ x1 + x2 + x3 + x4 + x5
f ~~ 1*f"

lav.out <- sem(lav.mod,dat)
summary(lav.out)

# greta
# full bayesian

# create variables with prior distributions

var1 = uniform(0, 20)
var2 = uniform(0, 20)
var3 = uniform(0, 20)
var4 = uniform(0, 20)
var5 = uniform(0, 20)
var6 = uniform(0, 20)

fac_loadings1 = normal(0,1)
fac_loadings2 = normal(0,1)
fac_loadings3 = normal(0,1)
fac_loadings4 = normal(0,1)
fac_loadings5 = normal(0,1)
fac_loadings6 = normal(0,1)

beta = normal(rep(0,PP),rep(1,PP))


#lat_sd = uniform(0, 20)
lat_mean =  cov %*% beta
  #cov[,1]*beta1 + cov[,2]*beta2 + cov[,3]*beta3 + cov[,4]*beta4 + cov[,5]*beta5
random_latent =  normal(lat_mean, rep(1,N), dim = N)


# equation for mean sepal length
mean1 <- fac_loadings1 * random_latent 
mean2 <- fac_loadings2 * random_latent 
mean3 <- fac_loadings3 * random_latent 
mean4 <- fac_loadings4 * random_latent 
mean5 <- fac_loadings5 * random_latent 
mean6 <- fac_loadings6 * random_latent 

#likelihood of the observed data
distribution(X[,1]) = normal( mean1 , sqrt(var1))
distribution(X[,2]) = normal( mean2 , sqrt(var2))
distribution(X[,3]) = normal( mean3 , sqrt(var3))
distribution(X[,4]) = normal( mean4 , sqrt(var4))
distribution(X[,5]) = normal( mean5 , sqrt(var5))
distribution(X[,6]) = normal( mean6 , sqrt(var6))

#model creation
m <- model(beta,
           fac_loadings1,fac_loadings2,fac_loadings3,
           fac_loadings4,fac_loadings5,fac_loadings6,
           var1,var2,var3,var4,var5,var6,
           lat_mean,
           mean1,mean2,mean3,mean4,mean5,mean6,random_latent)

#m2 <- model(beta1,beta2,beta3,beta4,beta5,
#           fac_loadings1,fac_loadings2,fac_loadings3,
#           fac_loadings4,fac_loadings5,fac_loadings6,
#           var1,var2,var3,var4,var5,var6,
          # lat_mean,
           #mean1,mean2,mean3,mean4,mean5,mean6,random_latent,
#           n_cores=1)


#library(future)
#plan(multisession)

#sample from model
system.time(draws <- greta::mcmc(m, n_samples = 2000, warmup = 500,n_cores=1,#,#chains=3,n_cores=3,
                     initial_values=c(rnorm(PP,.3,.1),rnorm(6,.7,.2),runif(6,.2,.4),
                                      rnorm(N,0,.2))))
#draws2 <- opt(m,max_iterations=500,initial_values=c(rep(.3,5),rep(0.7,6),rep(0.3,6),
#                                                    rep(0,N)))

#plot fit and trace
MCMCvis::MCMCtrace(draws)

sum = summary(draws)$statistics[1:30,]
round(sum,3)

draws$par[1:17]
# https://github.com/greta-dev/greta/issues/44

