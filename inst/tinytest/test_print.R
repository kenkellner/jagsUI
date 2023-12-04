out <- readRDS("longley_reference_fit.Rds")

# Check standard output
pr <- capture.output(print(out))
expect_true(grepl(" 750 ", pr[5]))
expect_true(grepl("Rhat", pr[8]))
expect_true(grepl("n.eff", pr[8]))
expect_true(grepl(" 51.876 ", pr[9]))
expect_true(grepl("Successful", pr[30]))
expect_true(grepl("overlap0", pr[34]))
expect_true(grepl("DIC", pr[38]))

# Rounded
pr2 <- capture.output(print(out, digits=2))
expect_equal(substr(pr2[9], 10,15), "51.88 ")

# With bad Rhat
out2 <- out
out2$summary[1,"Rhat"] <- 1.2
pr2 <- capture.output(print(out2))
expect_true(grepl("convergence failure", pr2[30]))

# With an NA rhat
out2 <- out
out2$summary[1,"Rhat"] <- NA
pr2 <- capture.output(print(out2))
expect_true(grepl("WARNING", pr2[30]))

# With all NA rhats
out2 <- out
out2$summary[,"Rhat"] <- NA
pr2 <- capture.output(print(out2))
expect_true(grepl("WARNING", pr2[30]))

# With 1 chain
out2 <- out
out2$mcmc.info$n.chains <- 1
pr2 <- capture.output(print(out2))
expect_false(grepl("Rhat", pr2[8]))
expect_false(grepl("n.eff", pr2[8]))
expect_true(grepl("overlap0", pr2[30]))

# bugs.format
out2 <- out
out2$bugs.format <- TRUE
pr2 <- capture.output(print(out2))
expect_true(grepl("Bugs", pr2[1]))
expect_true(grepl("and Rhat", pr2[27]))
