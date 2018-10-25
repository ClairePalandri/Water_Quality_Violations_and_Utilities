####################Intialization##############
library(rstan)
options(mc.cores = parallel::detectCores()) #Local Multicore CPU excess 



##################Data_Input##########################
input_data <- read.table("Dataset for Regression.txt", sep =" ", header = TRUE)



#############Stan_Parameters###################
sd_beta <- 25
chains <- 6 
iter <- 1000
warmup <- 500
X <- as.matrix(data.frame(1, input_data$defl_medsales_pc,input_data$defl_bottlesales_pc))
K <- dim(X)[2]
data_list <- list(N=dim(input_data)[1], K = K, X = X, z=input_data$dum_2ndwk_viol, sd_variables = sd_beta)



################Running the Stan Model#####################
model_stan <- stan(file = "Completely_Pooled_Multiparameter.stan", data=data_list, chains = chains, iter = iter, warmup = warmup,control = list(adapt_delta = 0.95))
print("Stan Program has run and is completed")
print(model_stan)



########################Saving_the_Results#########################
data_summary <- summary(model_stan)$summary[1:(K+1),] #Here we analyze check the global parameters. 
data_draws <- as.data.frame(extract(model_stan))[,1:(K+1)]

write.table(data_summary, "Stan Model Summary - Pooled.txt", sep = " ")
write.table(data_draws, "Draws - Pooled.txt", sep = " ")
print("Saving data complete")

pdf(file="Histrogram Plots.pdf")
for(i in 1:ncol(data_draws)){
  hist(data_draws[,i], freq = FALSE, main = colnames(data_draws)[i], xlab = colnames(data_draws)[i])
  abline(v = mean(data_draws[,i]), col = 'red', lwd = 2)
}
dev.off()
print("Plotting is complete")
##############Code_Complete#############################################
