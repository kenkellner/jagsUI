# Handle missing values in plots-----------------------------------------------
ref <- readRDS('longley_reference_fit.Rds')

# Rhat_min
pdf(NULL)
test1 <- traceplot(ref, Rhat_min=1.0005)
expect_equal(test1, NULL)
expect_error(traceplot(ref, Rhat_min=1.1))
dev.off()

# Plotting array parameter
pdf(NULL)
test2 <- traceplot(ref, "mu", ask=FALSE)
expect_equal(test2, NULL)
dev.off()

# Plotting range
pdf(NULL)
test3 <- traceplot(ref, "mu[1:4]", ask=FALSE)
expect_equal(test3, NULL)
dev.off()

# Handling NAs
for (i in 1:3){
  ref$samples[[i]][,'alpha'] <- NA
}
ref$samples[[1]][,"beta"] <- NA
ref$samples[[1]][1,"sigma"] <- NA
ref$summary["alpha","Rhat"] <- NA

pdf(NULL)
test4 <- traceplot(ref, c("alpha", "beta", "sigma"))
expect_equal(test4, NULL)
test5 <- whiskerplot(ref, c("alpha", "beta", "sigma"))
expect_equal(test5, NULL)
# Not ideal, maybe fix someday
test6 <- densityplot(ref, c("alpha", "beta", "sigma"))
expect_equal(test6, NULL)
test7 <- plot(ref, ask=FALSE)
expect_equal(test7, NULL)
dev.off()

# Incorrect object
expect_error(traceplot(ref$samples))
# Parameter not found
expect_error(traceplot(ref, "fake"))
expect_error(densityplot(ref, "fake"))
expect_error(whiskerplot(ref, "fake"))
expect_error(plot(ref, "fake"))
