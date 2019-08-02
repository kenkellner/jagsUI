#Functions for assessment of convergence of MCMC chains
#Drawn heavily from Vehtari, Gelman, Simpson, Carpenter and Burkner (2019).
#Rank-normalization, folding, and localization: An improved Rhat for 
#assessing convergence of MCMC.
#And related GPL-3 code in the `rstan` package

#Split each chain in half (effectively doubling chains)
#Detects lack of convergence within-chains
split_chains <- function(mcmc_mat){
  niter <- nrow(mcmc_mat)
  if(niter == 1) return(mcmc_mat)
  
  if(niter%%2 == 1){
    half <- nrow(mcmc_mat)/2
    mcmc_mat <- mcmc_mat[-ceiling(half),]
  }

  matrix(mcmc_mat, ncol=ncol(mcmc_mat)*2)
}

#Rank-normalize samples with inverse normal transformation
#Helps when there are heavy tails in posterior
rank_norm <- function(mcmc_mat){
  r = rank(mcmc_mat, na.last='keep', ties.method='average')
  z <- stats::qnorm( (r-0.5) / length(r))
  matrix(z, nrow(mcmc_mat), ncol(mcmc_mat))
}

#Calculate Rhat (formula according to Stan manual)
Rhat <- function(mcmc_mat){
  if(is_constant(mcmc_mat)) return(NA) 
  N <- nrow(mcmc_mat)
  if(N < 2) return(NA)

  theta_dot <- colMeans(mcmc_mat, na.rm=TRUE)
  s2 <- apply(mcmc_mat, 2, stats::var, na.rm=TRUE)
  B <- N * stats::var(theta_dot)
  W <- mean(s2, na.rm=TRUE)
  var_est <-  (N-1)*W/N + B/N
  sqrt(var_est/W)
}

#Calculate bulk and tail Rhats and keep the larger value
Rhat_min <- function(mcmc_mat){
  sc <- split_chains(mcmc_mat)
  bulk_rhat <- Rhat(rank_norm(sc))
  mc_fold <- abs(sc - stats::median(sc,na.rm=TRUE))
  tail_rhat <- Rhat(rank_norm(mc_fold))
  max(bulk_rhat, tail_rhat)
}

#------------------------------------------------------------------------------
#The following effective sample size code is modified from package rstan
#Used under terms of GPL v3
#rstan would be a heavy dependency for just this function

is_constant <- function(mcmc_mat){
  dif <- abs(max(mcmc_mat, na.rm=TRUE) - min(mcmc_mat, na.rm=TRUE)) 
  dif < .Machine$double.eps
}

fft_next_good_size <- function(N) {
  # Find the optimal next size for the FFT so that
  # a minimum number of zeros are padded.
  if (N <= 2)
    return(2)
  while (TRUE) {
    m <- N
    while ((m %% 2) == 0) m <- m / 2
    while ((m %% 3) == 0) m <- m / 3
    while ((m %% 5) == 0) m <- m / 5
    if (m <= 1)
      return(N)
    N <- N + 1
  }
}

autocovariance <- function(y) {
  N <- length(y)
  M <- fft_next_good_size(N)
  Mt2 <- 2 * M
  yc <- y - mean(y)
  yc <- c(yc, rep.int(0, Mt2 - N))
  transform <- stats::fft(yc)
  ac <- stats::fft(Conj(transform) * transform, inverse = TRUE)
  # use "biased" estimate as recommended by Geyer (1992)
  ac <- Re(ac)[1:N] / (N^2 * 2)
  ac
}

ess <- function(mcmc_mat) {
  
  if(nrow(mcmc_mat)==1) return(ncol(mcmc_mat))
  if(any(is.na(mcmc_mat))) return(NA)
  if(is_constant(mcmc_mat)) return(NA) 

  chains <- ncol(mcmc_mat)
  n_samples <- nrow(mcmc_mat)
  acov <- lapply(seq_len(chains), function(i) autocovariance(mcmc_mat[, i]))
  acov <- do.call(cbind, acov)
  chain_mean <- apply(mcmc_mat, 2, mean)
  mean_var <- mean(acov[1, ]) * n_samples / (n_samples - 1)
  var_plus <- mean_var * (n_samples - 1) / n_samples
  if (chains > 1)
    var_plus <- var_plus + stats::var(chain_mean)

  # Geyer's initial positive sequence
  rho_hat_t <- rep.int(0, n_samples)
  t <- 0
  rho_hat_even <- 1
  rho_hat_t[t + 1] <- rho_hat_even
  rho_hat_odd <- 1 - (mean_var - mean(acov[t + 2, ])) / var_plus
  rho_hat_t[t + 2] <- rho_hat_odd
  while (t < nrow(acov) - 5 && !is.nan(rho_hat_even + rho_hat_odd) &&
         (rho_hat_even + rho_hat_odd > 0)) {
    t <- t + 2
    rho_hat_even = 1 - (mean_var - mean(acov[t + 1, ])) / var_plus
    rho_hat_odd = 1 - (mean_var - mean(acov[t + 2, ])) / var_plus
    if ((rho_hat_even + rho_hat_odd) >= 0) {
      rho_hat_t[t + 1] <- rho_hat_even
      rho_hat_t[t + 2] <- rho_hat_odd
    }
  }
  max_t <- t
  # this is used in the improved estimate
  if (rho_hat_even>0)
      rho_hat_t[max_t + 1] <- rho_hat_even
  
  # Geyer's initial monotone sequence
  t <- 0
  while (t <= max_t - 4) {
    t <- t + 2
    if (rho_hat_t[t + 1] + rho_hat_t[t + 2] >
        rho_hat_t[t - 1] + rho_hat_t[t]) {
      rho_hat_t[t + 1] = (rho_hat_t[t - 1] + rho_hat_t[t]) / 2;
      rho_hat_t[t + 2] = rho_hat_t[t + 1];
    }
  }
  ess <- chains * n_samples
  # Geyer's truncated estimate
  # tau_hat <- -1 + 2 * sum(rho_hat_t[1:max_t])
  # Improved estimate reduces variance in antithetic case
  tau_hat <- -1 + 2 * sum(rho_hat_t[1:max_t]) + rho_hat_t[max_t+1]
  # Safety check for negative values and with max ess equal to ess*log10(ess)
  tau_hat <- max(tau_hat, 1/log10(ess))
  ess <- ess / tau_hat
  ess
}

#Calculate bulk rank-normalized, split-chain ESS
ess_bulk <- function(mcmc_mat){
  sc <- split_chains(mcmc_mat)
  ess(rank_norm(sc))
}
