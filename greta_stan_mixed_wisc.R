wisc <- read.table("C:/Users/RJacobucci/Documents/GitHub/EDM_Labs/2015/wisc4vpe.dat")
wisc <- read.table("C:/Users/jacobucc/Documents/GitHub/EDM_Labs/2015/wisc4vpe.dat")
wisc <- read.table("/Users/rjacobuc/Documents/GitHub/EDM_Labs/2015/wisc4vpe.dat")
names(wisc)<- c("V1","V2","V4","V6","P1","P2","P4", "P6", "Moeducat")

devtools::install_github("greta-dev/greta")


library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(rstanmulticore)

wisc.verb <- wisc[,c(1:4,9)]

# create subset for plotting
ntot <- nrow(wisc.verb)    # total number of observations
wisc.verb.sel <- wisc.verb[sample(ntot, 30), ]

wisc.long <- reshape(wisc.verb, varying = c("V1", "V2", "V4", "V6"), v.names = "verbal",
                     times = c(1, 2, 4, 6), direction = "long")

wisc.long.sel <- reshape(wisc.verb.sel, varying = c("V1", "V2", "V4", "V6"),
                         v.names = "verbal", times = c(1, 2, 4, 6),
                         direction = "long")
head(wisc.long,3)
names(wisc.long)[2] <- "grade"
names(wisc.long.sel)[2] <- "grade"

head(wisc.long)




library(nlme)
mix1 <- lme(fixed = verbal ~ 1, random = ~ 1 | id, data = wisc.long, method="ML" )
summary(mix1) # get same estimates as in LGM, notice SD not VAR

library(nlme)
mix2 <- lme(fixed = verbal ~ grade, random = ~ grade | id, data = wisc.long, method="ML" )
summary(mix2) # get same estimates as in LGM, notice SD not VAR




head(wisc.long)
max(wisc.long[,"id"])




dat <- list(
  verbal = wisc.long[,"verbal"],
  grade = wisc.long[,"grade"],
  N = nrow(wisc.long),
  J = max(wisc.long[,"id"]),
  subj = as.integer(factor(wisc.long$id)))

stanmodel <- "
data {
  int N; 
  real verbal[N]; 
  real grade[N];
  int<lower=1> J; //number of subjects
  int<lower=1, upper=J> subj[N]; //subject id
} 
parameters {
  vector[2] beta;
  real<lower=0> sigma;
  vector<lower=0>[2] sigma_u;
  cholesky_factor_corr[2] L_u;
  matrix[2,J] z_u;
}
transformed parameters {
  matrix[2,J] phi;
  phi = diag_pre_multiply(sigma_u,L_u) * z_u; //subj random effects
}
model{
  real mu;
  L_u ~ lkj_corr_cholesky(2.0);
  to_vector(z_u) ~ normal(0,1);
  for (i in 1:N){
    mu = beta[1] + phi[1,subj[i]] + (beta[2]+phi[2,subj[i]])*grade[i];
    verbal[i] ~ normal(mu,sigma);
  }
}
"

mixed.model=stan(model_code=stanmodel,
               data = dat,chains=1,
               pars=c("beta","sigma","L_u","sigma_u"))
print(mixed.model)
 


# greta
# full bayesian


subj = as.integer(factor(wisc.long$id))

# create variables with prior distributions

sd = uniform(0, 20)
intercept_mean = normal(10, 20)
slope_mean = normal(5,10)

#create variables for random intercepts
intercept_sd = uniform(0, 50)
slope_sd = uniform(0, 20)
N <- 204
random_intercept =  normal(intercept_mean, intercept_sd, dim = N) 
random_slope =  normal(slope_mean, slope_sd, dim = N)

# equation for mean sepal length
mean <- random_intercept[subj] + random_slope[subj] * wisc.long$grade 

#likelihood of the observed data
distribution(wisc.long$verbal) = normal( mean , sd)

#model creation
m <- model(intercept_mean,intercept_sd, slope_mean,slope_sd, sd,n_cores=1)

m2 <- model(intercept_mean,intercept_sd, slope_mean,slope_sd, sd)

#sample from model
draws <- greta::mcmc(m, n_samples = 2000, warmup = 100,chains=1)
draws2 <- opt(m2,max_iterations=1000)

#plot fit and trace
MCMCvis::MCMCtrace(draws)

summary(draws)


# https://github.com/greta-dev/greta/issues/44