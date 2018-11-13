####################Intialization##############
library(rstan)
library(dplyr)
options(mc.cores = parallel::detectCores()) #Local Multicore CPU excess 
print("Library Loading Complete")



##################Data_Input##########################
input_data <- read.table("Dataset for Regression.txt", sep =" ", header = TRUE)
print("Data Input successful. We now run the stan program")




########Subsetting_Dataset_to_state_level######################
#Enter State FIPS Code
state_FIPS_code <- 40
input_data <- input_data %>%
              filter(county_FIPS > state_FIPS_code*1000 & county_FIPS < (state_FIPS_code+1)*1000 ) 

input_data <- transform(input_data, id=match(county_FIPS, unique(county_FIPS)))
print("Data Subsetting successful. We now run the stan program")



#############Stan_Parameters###################
N <- dim(input_data)[1]
J <- max(input_data$id)
groups <- input_data$id
chains <- 6 
iter <- 2000
warmup <- 1000
X <- as.matrix(data.frame(1, input_data$defl_medsales_pc,input_data$defl_bottlesales_pc))
K <- dim(X)[2]
data_list <- list(N=N, K = K, J=J, X = X, z=input_data$dum_2ndwk_viol, id = groups)

#############3Running_the_Stan_Model#################
model_stan <- stan(file = "Partial_Pooling_Multiparameter.stan", data=data_list, chains = chains, iter = iter, warmup = warmup,control = list(adapt_delta = 0.95))
print("Stan Program has run and is completed")
print(model_stan, pars = c("nu", "nu_group"))

data_summary <- summary(model_stan)$summary #Here we analyze check the global parameters. 
data_draws <- as.data.frame(extract(model_stan))
print("Saving data complete")

write.table(data_summary, "Stan Model Summary - Pooled.txt", sep = " ")
write.table(data_draws, "Draws - Pooled.txt", sep = " ")



######################Plotting_the_uncertainty############
Reporting_Rate <- as.data.frame(summary(model_stan, pars = c("nu", "nu_group")))
Reporting_Rate$index <- 1:dim(Reporting_Rate)[1]
J <- dim(Reporting_Rate)[1]
ticks<- 0:(dim(Reporting_Rate)[1])



Coefficient_Estimates <- as.data.frame(summary(model_stan, pars = c("beta", "beta_group")))
Coefficient_Medicine <- Coefficient_Bottles <- Intercept <- as.data.frame(matrix(0,ncol=ncol(Coefficient_Estimates), nrow=nrow(Coefficient_Estimates)/3))
colnames(Coefficient_Medicine) <- colnames(Coefficient_Bottles) <- colnames(Intercept) <- colnames(Coefficient_Estimates)

for(i in 1:(max(input_data$id)+1)){
  index <- 3*(i-1) + 1
  Intercept[i,] <- Coefficient_Estimates[index,]
  Coefficient_Medicine[i,] <- Coefficient_Estimates[index+1,]
  Coefficient_Bottles[i,] <- Coefficient_Estimates[index+2,]
}
Coefficient_Medicine$index <- Coefficient_Bottles$index <- Intercept$index <- 1:dim(Intercept)[1]



pdf("Uncertainty_Estimates.pdf")

#Plot for the Medicine Sales 
plot (Coefficient_Medicine$index, Coefficient_Medicine$summary.mean, bty="l", pch=20, xaxt="n", xlab = "Pooled and County Medicine Sales Effect", ylab = "Med Sales Coefficient", main = "Uncertainty across effect of medicine sales")
abline(h=0, lty = 2)
axis(1,at=ticks,labels=ticks)
lines (rep(Coefficient_Medicine$index[1],2), c(Coefficient_Medicine$summary.97.5.[1], Coefficient_Medicine$summary.2.5.[1]), lwd=.5, lty = 2, col = 'red')
lines (rep(Coefficient_Medicine$index[1],2), c(Coefficient_Medicine$summary.25.[1], Coefficient_Medicine$summary.75.[1]), lwd=.5, col = 'red')
for (j in 2:J)
  {
  lines (rep(Coefficient_Medicine$index[j],2), c(Coefficient_Medicine$summary.97.5.[j], Coefficient_Medicine$summary.2.5.[j]), lwd=.5, lty = 2)
  lines (rep(Coefficient_Medicine$index[j],2), c(Coefficient_Medicine$summary.25.[j], Coefficient_Medicine$summary.75.[j]), lwd=.5)
  
}


#Plot for Bottle Sales
plot (Coefficient_Bottles$index, Coefficient_Bottles$summary.mean, bty="l", pch=20, xaxt="n", xlab = "Pooled and County Bottle Sales Effect", ylab = "Bottle Sales Coefficient", main = "Uncertainty across effect of Bottle sales")
abline(h=0, lty = 2)
axis(1,at=ticks,labels=ticks)
lines (rep(Coefficient_Bottles$index[1],2), c(Coefficient_Bottles$summary.97.5.[1], Coefficient_Bottles$summary.2.5.[1]), lwd=.5, lty = 2, col = 'red')
lines (rep(Coefficient_Bottles$index[1],2), c(Coefficient_Bottles$summary.25.[1], Coefficient_Bottles$summary.75.[1]), lwd=.5, col = 'red')
for (j in 2:J)
  {
  lines (rep(Coefficient_Bottles$index[j],2), c(Coefficient_Bottles$summary.97.5.[j], Coefficient_Bottles$summary.2.5.[j]), lwd=.5, lty = 2)
  lines (rep(Coefficient_Bottles$index[j],2), c(Coefficient_Bottles$summary.25.[j], Coefficient_Bottles$summary.75.[j]), lwd=.5)
  
}



 
plot (Reporting_Rate$index, Reporting_Rate$summary.mean, bty="l", pch=20, ylim = c(-0.1,1.1), xaxt="n", xlab = "Pooled and County Index", ylab = "Reporting Rate", main = "Uncertainty across reporting rates")
abline(h=0, lty = 2)
axis(1,at=ticks,labels=ticks)
lines (rep(Reporting_Rate$index[1],2), c(Reporting_Rate$summary.97.5.[1], Reporting_Rate$summary.2.5.[1]), lwd=.5, lty = 2, col = 'red')
lines (rep(Reporting_Rate$index[1],2), c(Reporting_Rate$summary.25.[1], Reporting_Rate$summary.75.[1]), lwd=.5, col = 'red')
for (j in 2:J){
  lines (rep(Reporting_Rate$index[j],2), c(Reporting_Rate$summary.97.5.[j], Reporting_Rate$summary.2.5.[j]), lwd=.5, lty = 2)
  lines (rep(Reporting_Rate$index[j],2), c(Reporting_Rate$summary.25.[j], Reporting_Rate$summary.75.[j]), lwd=.5)
  
}

dev.off()
