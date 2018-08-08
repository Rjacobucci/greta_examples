
#Sys.setenv(TENSORFLOW_PYTHON="/afs/crc.nd.edu/x86_64_linux/t/tensorflow/1.8/gcc/python2/build/bin/python")
library(tensorflow)
#use_python("/afs/crc.nd.edu/x86_64_linux/t/tensorflow/1.8/gpu/python2/build/bin/python")
use_python(system("which python", intern = TRUE))
#tf$Session()
devtools::install_github("greta-dev/greta@dev")
library(greta)
library(rstan)
library(MASS);library(lavaan)


library(rstan); library(lavaan)

HS <- HolzingerSwineford1939[complete.cases(HolzingerSwineford1939),]


mod <- "
f1 =~ NA*x1 + x2 + x3 + 1*x4 + x5 + x6 + x7 + x8 + x9
f1 ~ sex + grade + ageyr
#f1~~1*f1
"
out <- sem(mod, HS,meanstructure=T)
summary(out)

X <- HS[,7:15]
cov <- HS[,c(2,3,6)]

N <- nrow(X)

var = uniform(0,20,dim=c(1,9))
fac_loadings = normal(0,1,dim=c(1,8))
beta = normal(rep(0,3),rep(1,3))
alpha = normal(2,5,dim=c(1,9))


lat_sd = uniform(0, 2)
lat_mean =  cov %*% beta
#cov[,1]*beta1 + cov[,2]*beta2 + cov[,3]*beta3 + cov[,4]*beta4 + cov[,5]*beta5
random_latent =  normal(lat_mean, rep(lat_sd,N), dim = N)


# equation for mean sepal length
mean1 <- alpha[,1] + fac_loadings[,1] * random_latent 
mean2 <- alpha[,2] + fac_loadings[,2] * random_latent 
mean3 <- alpha[,3] + fac_loadings[,3] * random_latent 
mean4 <- alpha[,4] + 1 * random_latent 
mean5 <- alpha[,5] + fac_loadings[,4] * random_latent 
mean6 <- alpha[,6] + fac_loadings[,5] * random_latent 
mean7 <- alpha[,7] + fac_loadings[,6] * random_latent 
mean8 <- alpha[,8] + fac_loadings[,7] * random_latent 
mean9 <- alpha[,9] + fac_loadings[,8] * random_latent 

#likelihood of the observed data
distribution(X[,1]) = normal( mean1 , sqrt(var[,1]))
distribution(X[,2]) = normal( mean2 , sqrt(var[,2]))
distribution(X[,3]) = normal( mean3 , sqrt(var[,3]))
distribution(X[,4]) = normal( mean4 , sqrt(var[,4]))
distribution(X[,5]) = normal( mean5 , sqrt(var[,5]))
distribution(X[,6]) = normal( mean6 , sqrt(var[,6]))
distribution(X[,7]) = normal( mean7 , sqrt(var[,7]))
distribution(X[,8]) = normal( mean8 , sqrt(var[,8]))
distribution(X[,9]) = normal( mean9 , sqrt(var[,9]))

#model creation
m <- model(beta,
           fac_loadings,alpha,var,
           lat_sd,
           random_latent)

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
draws <- greta::mcmc(m, n_samples = 50000, warmup = 10000,
              initial_values=c(rnorm(3,.3,.2),rnorm(8,0.5,.1),rnorm(9,3,1),
                                                  runif(9,.5,1.5),runif(1,.5,.7),
                                                  rnorm(300,0,1)))


# try assigning as variables



var = variable(0,20,dim=c(1,9))
fac_loadings = variable(dim=c(1,8))
beta = variable(dim=c(3,1))
alpha = variable(dim=c(1,9))


lat_sd = variable(0)
lat_mean =  cov %*% beta
#cov[,1]*beta1 + cov[,2]*beta2 + cov[,3]*beta3 + cov[,4]*beta4 + cov[,5]*beta5
random_latent =  normal(lat_mean, rep(lat_sd,N), dim = N)


# equation for mean sepal length
mean1 <- alpha[,1] + fac_loadings[,1] * random_latent 
mean2 <- alpha[,2] + fac_loadings[,2] * random_latent 
mean3 <- alpha[,3] + fac_loadings[,3] * random_latent 
mean4 <- alpha[,4] + 1 * random_latent 
mean5 <- alpha[,5] + fac_loadings[,4] * random_latent 
mean6 <- alpha[,6] + fac_loadings[,5] * random_latent 
mean7 <- alpha[,7] + fac_loadings[,6] * random_latent 
mean8 <- alpha[,8] + fac_loadings[,7] * random_latent 
mean9 <- alpha[,9] + fac_loadings[,8] * random_latent 

#likelihood of the observed data
distribution(X[,1]) = normal( mean1 , sqrt(var[,1]))
distribution(X[,2]) = normal( mean2 , sqrt(var[,2]))
distribution(X[,3]) = normal( mean3 , sqrt(var[,3]))
distribution(X[,4]) = normal( mean4 , sqrt(var[,4]))
distribution(X[,5]) = normal( mean5 , sqrt(var[,5]))
distribution(X[,6]) = normal( mean6 , sqrt(var[,6]))
distribution(X[,7]) = normal( mean7 , sqrt(var[,7]))
distribution(X[,8]) = normal( mean8 , sqrt(var[,8]))
distribution(X[,9]) = normal( mean9 , sqrt(var[,9]))

#model creation
m <- model(beta,
           fac_loadings,alpha,var,
           lat_sd,
           random_latent)

opt.out <- greta::opt(m,# n_samples = 50000, warmup = 10000,
               initial_values=c(rnorm(3,.3,.2),rnorm(8,0.5,.1),rnorm(9,3,1),
                                runif(9,.5,1.5),runif(1,.5,.7),
                                rnorm(300,0,1)))
