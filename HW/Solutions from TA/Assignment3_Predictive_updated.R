#Question 1

#Part-c
library(data.table)

n_values <- c(1, 2, 3, 5, 10, 50, 100, 1000, 3000)

results <- data.table(n = integer(length(n_values)),
                      mean = numeric(length(n_values)),
                      var = numeric(length(n_values)))

for (i in seq_along(n_values)) {
  n <- n_values[i]
  X <- runif(n, 0, 2)
  mean_X <- mean(X)
  var_X <- var(X)
  results[i, `:=`(n=n_values[i], mean = mean_X, var = var_X)]
}

results

#Part-d
n_values <- c(1, 2, 3, 5, 10, 50, 100, 1000, 3000)

results <- data.table(n = integer(length(n_values)),
                      mean = numeric(length(n_values)),
                      var = numeric(length(n_values)),
                      abs_diff = numeric(length(n_values)))

for (i in seq_along(n_values)) {
  n <- n_values[i]
  X <- runif(n, 0, 2)
  mean_X <- mean(X)
  var_X <- var(X)
  abs_diff_X <- abs(mean_X - 1)
  results[i, `:=`(n = n_values[i], mean = mean_X, var = var_X, abs_diff = abs_diff_X)]
}

results

#Part-e
n_values <- c(1, 2, 3, 5, 10, 50, 100, 1000, 3000)

results <- data.table(n = integer(length(n_values)),
                      mean = numeric(length(n_values)),
                      var = numeric(length(n_values)),
                      abs_diff_X1 = numeric(length(n_values)),
                      abs_diff_fX1 = numeric(length(n_values)))

for (i in seq_along(n_values)) {
  n <- n_values[i]
  X <- runif(n, 0, 2)
  mean_X <- mean(X)
  var_X <- var(X)
  abs_diff_X <- abs(mean_X - 1)
  f_mean_X <- 2*mean_X^2 - 5*mean_X + 1 + 1/(3*mean_X)
  abs_diff_fX <- abs(f_mean_X - 5/3)
  results[i, `:=`(n = n_values[i], mean = mean_X, var = var_X, abs_diff_X1 = abs_diff_X, abs_diff_fX1 = abs_diff_fX)]
}

results

#Question 2

#Part-e
set.seed(123)  # for reproducibility

n_vec <- c(1, 2, 3, 5, 10, 50, 100, 1000, 3000)  # vector of sample sizes
t_vec <- 1:2500  # vector of t values
Vn_mat <- matrix(0, nrow = 2500, ncol = length(n_vec))  # matrix to store sample mean vectors

for (i in seq_along(n_vec)) {
  n <- n_vec[i]
  for (t in t_vec) {
    X <- runif(n, min = 0, max = 2)  # generate n independent Uniform[0, 2] random variables
    Vn_mat[t, i] <- mean(X)  # calculate sample mean and store in matrix
  }
}
Vn_mat


#Part-f
Vn2500 <- apply(Vn_mat, 2, mean)  # calculate mean of each column
Vn_var <- apply(Vn_mat, 2, var)  # calculate variance of each column

# display mean and variance of Vn2500 for each value of n
data.frame(n = n_vec, mean = Vn2500, variance = Vn_var)


#Part-g
# calculate E[Xi] and store as a constant
EXi <- (0 + 2) / 2  # mean of Uniform[0, 2] distribution
n_samples=2500
# initialize matrix to store transformed vectors
Yn_mat <- matrix(0, nrow = n_samples, ncol = length(n_vec))

# loop over values of n and generate random samples
for (i in seq_along(n_vec)) {
  n <- n_vec[i]
  
  # loop over each sample for this value of n
  for (j in 1:n_samples) {
    # generate sample of Uniform[0, 2] random variables
    X <- runif(n, min = 0, max = 2)
    
    # calculate sample mean
    X_bar <- mean(X)
    
    # calculate transformed value Yn
    Yn <- sqrt(n) * (X_bar - EXi)
    
    # store transformed value in matrix
    Yn_mat[j, i] <- Yn
  }
}

# display Yn2500 matrix
head(Yn_mat)

#Part-h
# calculate mean of transformed vectors
Yn_mean <- apply(Yn_mat, MARGIN = 2, FUN = mean)

# calculate variance of transformed vectors
Yn_var <- apply(Yn_mat, MARGIN = 2, FUN = var)

# display mean and variance
data.frame(n = n_vec, mean = Yn_mean, variance = Yn_var)

#Part-i
# load ggplot2 library
library(ggplot2)

# create data frame with transformed vectors
Yn_df <- data.frame(Yn2500 = as.vector(Yn_mat), n = rep(n_vec, each = 2500))

# plot histogram of transformed vectors using ggplot2
ggplot(Yn_df, aes(x = Yn2500)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, color = "black", fill = "lightblue") +
  facet_wrap(~ n, scales = "free_x") +
  labs(title = "Histogram of Yn2500", x = "Transformed Value", y = "Density")






#Part-j
# Define the number of simulations and sample sizes
n_sim <- 2500
n_vec <- c(1, 2, 3, 5, 10, 50, 100, 1000, 3000)

# Define the matrix to store the transformed data
zn_mat <- matrix(0, nrow = n_sim, ncol = length(n_vec))

# Calculate E[Xi] and Var(Xi)
EXi <- 1
VarXi <- 1/3

# Loop over the sample sizes
for (i in seq_along(n_vec)) {
  n <- n_vec[i]
  # Generate the n x n_sim matrix of uniform random variables
  X_mat <- matrix(runif(n * n_sim, 0, 2), ncol = n)
  # Calculate the sample means
  X_bar <- rowMeans(X_mat)
  # Transform the sample means using Z-score
  Z_n <- sqrt(n) * (X_bar - EXi) / sqrt(VarXi)
  # Store the transformed data in the matrix
  zn_mat[, i] <- Z_n
}

# display Yn2500 matrix
head(zn_mat)

#Part-h
# calculate mean of transformed vectors
Zn_mean <- apply(zn_mat, MARGIN = 2, FUN = mean)

# calculate variance of transformed vectors
Zn_var <- apply(zn_mat, MARGIN = 2, FUN = var)

# display mean and variance
data.frame(n = n_vec, mean = Zn_mean, variance = Zn_var)

#Part-i
# load ggplot2 library
library(ggplot2)

# create data frame with transformed vectors
Zn_df <- data.frame(zn2500 = as.vector(zn_mat), n = rep(n_vec, each = 2500))

# plot histogram of transformed vectors using ggplot2
ggplot(Zn_df, aes(x = zn2500)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, color = "black", fill = "lightblue") +
  facet_wrap(~ n, scales = "free_x") +
  labs(title = "Histogram of Zn2500", x = "Transformed Value", y = "Density")



#Question 3
#Part-a
n_values <- c(2,3,4,5,6,7,8,9,10,15,20,30,50,75,100,250,500,1000,2000,3000) # vector of n values to loop over

for (n in n_values) {
  set.seed(123) # set seed for reproducibility
  x <- runif(n, min = 0, max = 12) # generate n x 1 column vector of Uniform[0,12] random variable
  print(head(x)) # print the first few values of x
}

#Part-b
n_values <- c(2,3,4,5,6,7,8,9,10,15,20,30,50,75,100,250,500,1000,2000,3000) # vector of n values to loop over

for (n in n_values) {
  set.seed(123) # set seed for reproducibility
  u <- runif(n, min = -4, max = 4) # generate n x 1 column vector of Uniform[-4,4] random variable
  print(head(u)) # print the first few values of u
}

#Part-c
n_values <- c(2,3,4,5,6,7,8,9,10,15,20,30,50,75,100,250,500,1000,2000,3000) # vector of n values to loop over

for (n in n_values) {
  set.seed(123) # set seed for reproducibility
  x <- runif(n, min = 0, max = 12) # generate n x 1 column vector of Uniform[0,12] random variable
  u <- runif(n, min = -4, max = 4) # generate n x 1 column vector of Uniform[-4,4] random variable
  y <- 3 + 2 * x + u # generate n x 1 column vector of y using the formula yi=3+2xi+ui
  
  print(head(y)) # print the first few values of y
}

#Part-d
n_values <- c(2,3,4,5,6,7,8,9,10,15,20,30,50,75,100,250,500,1000,2000,3000) # vector of n values to loop over

for (n in n_values) {
  set.seed(123) # set seed for reproducibility
  x <- runif(n, min = 0, max = 12) # generate n x 1 column vector of Uniform[0,12] random variable
  u <- runif(n, min = -4, max = 4) # generate n x 1 column vector of Uniform[-4,4] random variable
  y <- 3 + 2 * x + u # generate n x 1 column vector of y using the formula yi=3+2xi+ui
  
  # estimate beta using OLS
  beta_hat <- cov(x,y)/var(x)
  
  # print the estimated value of beta
  print(paste0("For n = ", n, ", beta_hat = ", beta_hat))
}

#Question 4
#Part-a
# Set the sample sizes
ns <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 30, 50, 75, 100, 250, 500, 1000, 2000, 3000)

# Create a list to store the x vectors for each sample size
x_list <- list()

# Generate the x vector for each sample size and store it in the list
for (n in ns) {
  x <- runif(n, 0, 12)
  x_list[[n]] <- x
}

#Part-b
# Set the sample sizes
ns <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 30, 50, 75, 100, 250, 500, 1000, 2000, 3000)

# Create a list to store the u vectors for each sample size
u_list <- list()

# Generate the u vector for each sample size and store it in the list
for (n in ns) {
  u <- runif(n, -4, 4)
  u_list[[n]] <- u
}

#Part-c
# Set the sample sizes
ns <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 30, 50, 75, 100, 250, 500, 1000, 2000, 3000)

# Create a list to store the y vectors for each sample size
y_list <- list()

# Generate the y vector for each sample size and store it in the list
for (n in ns) {
  # Generate x and u vectors for the current sample size
  x <- runif(n, 0, 12)
  u <- runif(n, -4, 4)
  
  # Calculate the y vector using the formula yi = 3 + 2xi + ui
  y <- 3 + 2*x + u
  
  # Store the y vector in the list
  y_list[[n]] <- y
}

#Part-d
# Set the sample sizes
ns <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 30, 50, 75, 100, 250, 500, 1000, 2000, 3000)

# Create a vector to store the OLS estimates of beta for each sample size
beta_ols <- rep(NA, length(ns))

# Estimate the beta OLS for each sample size
for (i in 1:length(ns)) {
  # Extract the x and y vectors for the current sample size
  x <- runif(ns[i], 0, 12)
  u <- runif(ns[i], -4, 4)
  y <- 3 + 2*x + u
  
  # Calculate the OLS estimate of beta using the formula Cov(x,y)/Var(x)
  beta_ols[i] <- cov(x, y) / var(x)
}

# Save the OLS estimates of beta in memory
save(beta_ols, file = "beta_ols.RData")

#Part-e
# set the sample sizes and number of simulations
n_values <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 30, 50, 75, 100, 250, 500, 1000, 2000, 3000)
num_sims <- 2500

# create a matrix to store the OLS estimates of beta
beta_hat <- matrix(0, nrow = num_sims, ncol = length(n_values))

# loop over sample sizes
for (i in seq_along(n_values)) {
  n <- n_values[i]
  # repeat the Monte-Carlo simulation num_sims times
  for (j in 1:num_sims) {
    # generate the data
    x <- runif(n, min = 0, max = 12)
    u <- runif(n, min = -4, max = 4)
    y <- 3 + 2 * x + u
    # estimate beta using OLS
    beta_hat[j, i] <- cov(x, y) / var(x)
  }
}
# save the OLS estimates in a vector b

# part-f
beta_mean <- apply(beta_hat, MARGIN = 2, FUN = mean)

# calculate variance of transformed vectors
beta_var <- apply(beta_hat, MARGIN = 2, FUN = var)

# display mean and variance
data.frame(n = n_values, mean = beta_mean, variance = beta_var)



# create data frame with transformed vectors
beta_df <- data.frame(beta2500 = as.vector(beta_hat), n = rep(n_values, each = 2500))
beta_df<-subset(beta_df,beta_df$n%in%c(5,10,15,20,50,100,500,1000,3000))

# plot histogram of transformed vectors using ggplot2
ggplot(beta_df, aes(x = beta2500)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, color = "black", fill = "lightblue") +
  facet_wrap(~ n, scales = "free_x") +
  labs(title = "Histogram of beta2500", x = "Transformed Value", y = "Density")



#Question 5
#Part a
library(data.table)
# load the dataset
vg_data = fread(file = "videogamesales_main.csv",
             na.strings = c("NA", ""), 
             sep = "auto",
             stringsAsFactors = FALSE,
             data.table = TRUE
)

# create frequency tables
freq_table_platform <- table(vg_data$Platform)
freq_table_genre <- table(vg_data$Genre)
freq_table_rating <- table(vg_data$Rating)

# print the tables
print(freq_table_platform)
print(freq_table_genre)
print(freq_table_rating)

#Part b


# Load the dataset
vg_sales <- vg_data

# Create categorical variables for platform, genre, and rating
vg_sales[, platform_cat := as.factor(Platform)]
vg_sales[, genre_cat := as.factor(Genre)]
vg_sales[, rating_cat := as.factor(Rating)]

# Create age variable relative to year 2013
vg_sales[, age := 2013 - Year_of_Release]
vg_sales

#Part c
# Run regression with all relevant X variables
model <- lm(Global_Sales ~ Critic_Score + User_Score + platform_cat + genre_cat + rating_cat + age, data = vg_sales)

# Report adjusted R-squared
adj_r_sq <- summary(model)$adj.r.squared
cat("Adjusted R-squared:", adj_r_sq, "\n")

#Part d
# Generate natural logs of selected variables
vg_sales$ln_global_sales <- log(vg_sales$Global_Sales)
vg_sales$ln_critic_score <- log(vg_sales$Critic_Score)
vg_sales$ln_critic_count <- log(vg_sales$Critic_Count)
vg_sales$ln_user_score <- log(vg_sales$User_Score)
vg_sales$ln_user_count <- log(vg_sales$User_Count)

#Part e
# Create a new variable with the natural log of global sales
vg_sales$log_global_sales <- log(vg_sales$Global_Sales)

# Run a linear regression with log global sales as the response variable
# and all other relevant variables as predictors
model1 <- lm(vg_sales$log_global_sales ~ ., data = vg_sales[,c(4:8, 11:14)])

# Report adjusted R-squared
summary(model1)$adj.r.squared

#Part f
# generate log of Y and X variables
vg_sales$log_global_sales <- log(vg_sales$Global_Sales)
vg_sales$log_critic_score <- log(vg_sales$Critic_Score)
vg_sales$log_critic_count <- log(vg_sales$Critic_Count)
vg_sales$log_user_score <- log(vg_sales$User_Score)
vg_sales$log_user_count <- log(vg_sales$User_Count)

# run regression with log of Y and X variables
model2 <- lm(log_global_sales ~ log_critic_score + log_critic_count + log_user_score + log_user_count + platform_cat + genre_cat + rating_cat + age, data=vg_sales)

# report adjusted R-squared
summary(model2)$adj.r.squared




