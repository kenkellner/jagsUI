# load_modules loads and unloads correctly

if (rjags::jags.version() < as.numeric_version("5")) {
    default.modules <- c('basemod','bugs')
    test.factories <- 1:2
} else {
    default.modules <- c('basemod','bugs','diag') # diag provides deviance monitor
    test.factories <- c(6,1) # change in precedence of bugs::binomSlice factory
}

jagsUI:::set.modules(NULL, FALSE)
expect_equal(rjags::list.modules(),default.modules)
jagsUI:::set.modules('glm', FALSE)
expect_equal(rjags::list.modules(),c(default.modules,'glm'))
jagsUI:::set.modules(NULL, FALSE)
expect_equal(rjags::list.modules(),default.modules)
jagsUI:::set.modules(NULL, DIC=TRUE)
expect_equal(rjags::list.modules(),c(default.modules,'dic'))

# load_factories works correctly
jagsUI:::set.factories(NULL)
expect_equal(rjags::list.factories('sampler')[test.factories,2], c(TRUE, TRUE))
jagsUI:::set.factories(c('bugs::BinomSlice sampler FALSE', 'bugs::RW1 sampler FALSE'))
expect_equal(rjags::list.factories('sampler')[test.factories,2], c(FALSE, FALSE))
jagsUI:::set.factories(c('bugs::BinomSlice sampler TRUE', 'bugs::RW1 sampler TRUE'))
expect_equal(rjags::list.factories('sampler')[test.factories,2], c(TRUE, TRUE))
expect_error(jagsUI:::set.factories(c('bad bad')))
expect_error(jagsUI:::set.factories(c('bugs::fake sampler FALSE')))
