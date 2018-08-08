library(MASS);library(lavaan)
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
#f ~ x1 + x2 + x3 + x4 + x5
f ~~ 1*f"

lav.out <- sem(lav.mod,XX)
summary(lav.out)
fitmeasures(lav.out)

inspect(lav.out,"cov.ov")


covar = cov(X)
  inspect(lav.out,"cov.ov") #cov(XX)
est_var = zeros(11,11)
sds = variable(lower=0,dim=11) # gamma(2,2,11)#
sds_full = zeros(11)
sds_full[1:11] = sds
#sds_full[7:11] = rep(0,5)
diag(est_var) = sds_full#sds_full
#diag(est_var$node$.value) = ?
loads_full = zeros(11)
loads = variable(dim=6)
loads_full[1:6] = loads
beta_full = zeros(11)
beta = variable(dim=5)
beta_full[7:11] = beta

#distribution(phi) = normal(mu_phi,sd=1,dim=1)
sig = loads_full %*% 1 %*%  t(loads_full) + beta_full %*% t(beta_full) +  est_var
#sig = loads_full %*% 1 %*%  t(loads_full)  +  est_var   + beta_full %*% t(beta_full)
distribution(covar) = multivariate_normal(rep(0,11),sig,dim=11)
#distribution(covar) = wishart(11,sig)

m.cov <- model(loads,sds,beta)

opt.out <- opt(m.cov,max_iterations=1500,
               initial_values=c(rnorm(6,.7,.1),runif(6,.3,.6),rnorm(5,.1,.01)))
opt.out

mcmc.out <- greta::mcmc(m.cov, n_samples = 10000, warmup = 1000,
                        initial_values=c(rnorm(6,.7,.1),runif(11,.3,.6),rnorm(5,.1,.01)))
mcmc.out
summary(mcmc.out)
MCMCvis::MCMCtrace(mcmc.out)




# try with ram

a.mat = extractMatrices(lav.out)["A"]$A
s.mat = extractMatrices(lav.out)["S"]$S
f.mat = extractMatrices(lav.out)["F"]$F

a.est = extractMatrices(lav.out)$A_est
a.fixed = extractMatrices(lav.out)$A_fixed

s.est = extractMatrices(lav.out)$S_est
s.fixed = extractMatrices(lav.out)$S_fixed




a.gret = zeros(7,7)
a.vars =  variable(dim=6) #
a.gret[a.mat != 0] = a.vars
a.gret[a.fixed] = a.est[a.fixed]

s.gret = zeros(7,7)
s.vars = variable(lower=0,dim=6)#gamma(2,2,6) #
s.gret[s.mat != 0] = s.vars
s.gret[s.fixed] = s.est[s.fixed]

f.mat
f.gret = zeros(dim(f.mat))
f.gret[f.mat != 0] = 1
I = diag(7)

sig = f.gret %*% solve(I-a.gret) %*% s.gret %*% t(solve(I-a.gret)) %*% t(f.gret)
#f.mat %*% solve(I-a.est) %*% s.est %*% t(solve(I-a.est)) %*% t(f.mat)

#distribution(covar) = multivariate_normal(rep(0,6),sig,dim=6)
distribution(covar) = wishart(6,sig)

m.cov <- model(a.vars,s.vars)

opt.out <- opt(m.cov,max_iterations=1500,
               initial_values=c(a.vars=rnorm(6,.7,.1),s.vars=runif(6,.3,.4)))
opt.out

mcmc.out <- greta::mcmc(m.cov, n_samples = 1000, warmup = 100,
                        initial_values=c(a.vars=rnorm(6,.7,.1),s.vars=runif(6,.3,.4)))
summary(mcmc.out)
MCMCvis::MCMCtrace(mcmc.out)
