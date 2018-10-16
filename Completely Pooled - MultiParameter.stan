data {
int<lower=0> N;
real x1[N];
int<lower=0, upper=1> z[N];
real<lower=0> sd_a;
real<lower=0> sd_b;
}

parameters {
real a;
real b;
real<lower=0, upper=1> nu;
}

transformed parameters {
real<lower=0, upper=1> pi[N];

for(i in 1:N){
pi[i] = exp(a*x1[i]+b)/(exp(a*x1[i]+b)+1);}
}

model {
//priors
a ~ normal(0,sd_a);    //The assumption is that since this is a logit model the values cannot go that high.The mean is the output of the logit model.
b ~ normal(0,sd_b);
//Generic Weakly Informative Prior. Source:- Prior Choice Recommendations-Prior Wiki
nu ~ beta(1,1);   //The basis for this prior is that about 40% of the violations are under-reported.

//model 
for(i in 1:N)
{z[i] ~ bernoulli(pi[i]*nu);}
}
