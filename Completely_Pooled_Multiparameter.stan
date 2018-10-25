data {
int<lower=0> N;                      //Number of observations
int<lower=0> K;                  //Number of Variables including the intercept
matrix[N,K] X;                      //Covariate Matrix.Note:- First column is intercept
int<lower=0, upper=1> z[N];      //Violations Status
real<lower=0> sd_variables;         //Prios for the coefficients 
}

parameters {
vector[K] beta;                       //Coefficient
real<lower=0, upper=1> nu;            //Reporting Rate
}

model {
//priors
beta ~ normal(0,sd_variables);
nu ~ beta(1,1);   

//model
z ~ bernoulli(nu*inv_logit(X*beta));        // Pr(z=1) = nu*inverse_logit(X*beta)

}
