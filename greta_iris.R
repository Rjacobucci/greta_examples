

Species<-as.numeric(iris$Species) #Make Species into numeric vector

# create variables with prior distributions
coefficient = normal(0, 10)
sd = normal(0, 10)
intercept = normal(0, 10)

#create variables for random intercepts
sd_intc = normal(0, 20)
n_species <- length(levels(iris$Species))
spec_intercept =  normal(intercept, sd_intc, dim = n_species) 

# equation for mean sepal length
mean <- spec_intercept[Species] + coefficient * iris$Sepal.Width 

#likelihood of the observed data
distribution(iris$Sepal.Length) = normal( mean , sd)

#model creation
m <- model(spec_intercept, sd_intc, intercept, coefficient, sd)

#sample from model
draws <- mcmc(m, n_samples = 2000, warmup = 100, thin = 4)

#plot fit and trace
MCMCvis::MCMCtrace(draws)


# fixed parameters

library(greta)

Species<-as.numeric(iris$Species) #Make Species into numeric vector

# create variables with prior distributions
coefficient = variable(0, 10)
sd = variable(0, 10)
intercept = variable(0, 10)

#create variables for random intercepts
sd_intc = variable(0, 20)
n_species <- length(levels(iris$Species))
spec_intercept =  normal(intercept, sd_intc, dim = n_species) 

# equation for mean sepal length
mean <- spec_intercept[Species] + coefficient * iris$Sepal.Width 

#likelihood of the observed data
distribution(iris$Sepal.Length) = normal( mean , sd)

#model creation
m <- model(spec_intercept, sd_intc, intercept, coefficient, sd)

#sample from model
draws <- mcmc(m, n_samples = 2000, warmup = 100, thin = 4)

#plot fit and trace
MCMCvis::MCMCtrace(draws)
