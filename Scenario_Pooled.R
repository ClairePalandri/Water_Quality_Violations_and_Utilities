
.libPaths("/rigel/cwc/users/yva2000/rpackages/")
library(rstan)
options(mc.cores = parallel::detectCores()) #Local Multicore CPU excess 
print("Library Loading Complete")


input_data <- read.table("Dataset for Regression.txt", sep =" ", header = TRUE)
print("We now run the stan program")



#Stan_Parameters
sd_a <- 10  #Effect Size
sd_b <- 10 #Intercept
chains <- 4 
iter <- 1000
warmup <- 500
data_list <- list(N=dim(input_data)[1], x1=input_data$Detrended*1000, z=input_data$dum_1stwk_viol, sd_a = sd_a, sd_b = sd_b)

#Running the Stan Model. 
model_stan <- stan(file = "Completely Pooled - MultiParameter.stan", data=data_list, chains = chains, iter = iter, warmup = warmup)
print("Stan Program has run and is completed")

data_summary <- summary(model_stan)$summary[1:3,] #Here we analyze check the global parameters. 
data_draws <- as.data.frame(extract(model_stan))[,1:3]
colnames(data_draws) <- c("Effect_Size", "Intercept", "Reporting_Rate")
print("Saving data complete")

write.table(data_summary, "Stan Model Summary - Pooled.txt", sep = " ")
write.table(data_draws, "Draws - Pooled.txt", sep = " ")



pdf(file="Histrogram Plots.pdf")
for(i in 1:ncol(data_draws)){
  hist(data_draws[,i], freq = FALSE, main = colnames(data_draws)[i], xlab = colnames(data_draws)[i])
  abline(v = mean(data_draws[,i]), col = 'red', lwd = 2)
}
dev.off()
print("Plotting is complete")