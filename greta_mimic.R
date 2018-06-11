
library(rstan)
library(MASS);library(lavaan)

#X.noise = mvrnorm(2000, rep(0,950),matrix(diag(950),950,950))
X.small = mvrnorm(200, rep(0,5),matrix(diag(5),5,5))

f = X.small%*%rep(0.1,5) + rnorm(200,0,sqrt(1)) # X.noise%*%rep(0,950) + 
y1 = 0.7*f + rnorm(200,0,sqrt(0.3)) 
y2 = 0.7*f + rnorm(200,0,sqrt(0.3))  
y3 = 0.7*f + rnorm(200,0,sqrt(0.3))  
y4 = 0.7*f + rnorm(200,0,sqrt(0.3))  
y5 = 0.7*f + rnorm(200,0,sqrt(0.3))  
y6 = 0.7*f + rnorm(200,0,sqrt(0.3))  




X = as.matrix(cbind(y1,y2,y3,y4,y5,y6))
cov= as.matrix(cbind(X.small))
N <- nrow(X)

dat <- cbind(X,cov)
colnames(dat) <- c("y1","y2","y3","y4","y5","y6",
                   "x1","x2","x3","x4","x5")

lav.mod <- "
f =~ NA*y1 + y2 + y3 + y4 + y5 + y6
#f ~ x1 + x2 + x3 + x4 + x5
f ~~ 1*f"

lav.out <- sem(lav.mod,dat)
summary(lav.out)

# greta
# full bayesian
]
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

beta1 = normal(0,1)
beta2 = normal(0,1)
beta3 = normal(0,1)
beta4 = normal(0,1)
beta5 = normal(0,1)

#lat_sd = uniform(0, 20)
lat_mean = cov[,1]*beta1 + cov[,2]*beta2 + cov[,3]*beta3 + 
  cov[,4]*beta4 + cov[,5]*beta5
random_latent =  normal(lat_mean, rep(1,200), dim = N)


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
m <- model(beta1,beta2,beta3,beta4,beta5,
           fac_loadings1,fac_loadings2,fac_loadings3,
           fac_loadings4,fac_loadings5,fac_loadings6,
           var1,var2,var3,var4,var5,var6,
           lat_mean,
           mean1,mean2,mean3,mean4,mean5,mean6,random_latent,
           n_cores=1)

m2 <- model(beta1,beta2,beta3,beta4,beta5,
           fac_loadings1,fac_loadings2,fac_loadings3,
           fac_loadings4,fac_loadings5,fac_loadings6,
           var1,var2,var3,var4,var5,var6,
          # lat_mean,
           #mean1,mean2,mean3,mean4,mean5,mean6,random_latent,
           n_cores=1)


#sample from model
draws <- greta::mcmc(m, n_samples = 300, warmup = 100,chains=1)
                    # initial_values=c(rep(.1,5),rep(0.7,6),rep(0.3,6),
                     #                 rep(0,N),rep(0,N),rep(0,N),rep(0,N),
                      #                rep(0,N),rep(0,N),rep(0,N),rep(0,N)))
draws2 <- opt(m2,max_iterations=500)

#plot fit and trace
MCMCvis::MCMCtrace(draws)

summary(draws)

draws2$par[1:17]
# https://github.com/greta-dev/greta/issues/44



# try using variable


var11 = variable(lower=0)
var12 = variable(lower=0)
var13 = variable(lower=0)
var14 = variable(lower=0)
var15 = variable(lower=0)
var16 = variable(lower=0)

#load1 = variable()
load2 = variable()
load3 = variable()
load4 = variable()
load5 = variable()
load6 = variable()

b1 = variable()
b2 = variable()
b3 = variable()
b4 = variable()
b5 = variable()

lat_var = variable(lower=0)

#lat_sd = uniform(0, 20)
lat_mean1 = cov[,1]*b1 + cov[,2]*b2 + cov[,3]*b3 + 
  cov[,4]*b4 + cov[,5]*b5
random_latent1 =  normal(lat_mean1, rep(sqrt(lat_var),N), dim = N)


# equation for mean sepal length
m1 <- 1 * random_latent1
m2 <- load2 * random_latent1 
m3 <- load3 * random_latent1 
m4 <- load4 * random_latent1 
m5 <- load5 * random_latent1 
m6 <- load6 * random_latent1 

#likelihood of the observed data
distribution(X[,1]) = normal( m1 , sqrt(var11))
distribution(X[,2]) = normal( m2 , sqrt(var12))
distribution(X[,3]) = normal( m3 , sqrt(var13))
distribution(X[,4]) = normal( m4 , sqrt(var14))
distribution(X[,5]) = normal( m5 , sqrt(var15))
distribution(X[,6]) = normal( m6 , sqrt(var16))


m22 <- model(b1,b2,b3,b4,b5,
            lat_var,load2,load3,
            load4,load5,load6,
            var11,var12,var13,var14,var15,var16,
            n_cores=1)

system.time(opt.out <- (m22,max_iterations=500))

opt.out

# try covariance



#X.noise = mvrnorm(2000, rep(0,950),matrix(diag(950),950,950))
X.small = mvrnorm(2000, rep(0,5),matrix(diag(5),5,5))

f = X.small%*%rep(0.1,5) + rnorm(2000,0,sqrt(1)) # X.noise%*%rep(0,950) + 
y1 = 0.7*f + rnorm(2000,0,sqrt(0.3)) 
y2 = 0.7*f + rnorm(2000,0,sqrt(0.3))  
y3 = 0.7*f + rnorm(2000,0,sqrt(0.3))  
y4 = 0.7*f + rnorm(2000,0,sqrt(0.3))  
y5 = 0.7*f + rnorm(2000,0,sqrt(0.3))  
y6 = 0.7*f + rnorm(2000,0,sqrt(0.3))  




X = as.matrix(cbind(y1,y2,y3,y4,y5,y6))
XX= as.matrix(cbind(X,X.small))
N <- nrow(X)

colnames(XX) <- c("y1","y2","y3","y4","y5","y6",
                   "x1","x2","x3","x4","x5")

lav.mod <- "
f =~ NA*y1 + y2 + y3 + y4 + y5 + y6
f ~ x1 + x2 + x3 + x4 + x5
f ~~ 1*f"

lav.out <- sem(lav.mod,XX)
summary(lav.out)

inspect(lav.out,"cov.ov")

covar = cov(XX)
est_var = zeros(11,11)
sds = gamma(2,2,11)#variable(lower=0,dim=11)
#sds_full = zeros(11)
#sds_full[1:6] = sds**2
#sds_full[7:11] = rep(1,5)
diag(est_var) = sds**2#sds_full
#diag(est_var$node$.value) = ?
loads_full = zeros(11)
loads = variable(dim=6)
loads_full[1:6] = loads
beta_full = zeros(11)
beta = variable(dim=5)
beta_full[7:11] = beta

#distribution(phi) = normal(mu_phi,sd=1,dim=1)
#sig = loads_full %*% 1 %*%  t(loads_full) + beta_full %*% t(beta_full) +  est_var
sig = loads_full %*% 1 %*%  t(loads_full)  +  est_var %*% est_var  + beta_full %*% t(beta_full)
distribution(covar) = multivariate_normal(rep(0,11),sig,dim=11)
#distribution(covar) = wishart(10,sig)

m.cov <- model(loads,sds,beta,n_cores=1)

#opt.out <- opt(m.cov,max_iterations=1500)
#opt.out

mcmc.out <- greta::mcmc(m.cov, n_samples = 1000, warmup = 400,chains=1)
mcmc.out
summary(mcmc.out)
MCMCvis::MCMCtrace(mcmc.out)
