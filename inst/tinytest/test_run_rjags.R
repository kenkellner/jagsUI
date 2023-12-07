# load_modules loads and unloads correctly
jagsUI:::set.modules(NULL, FALSE)
expect_equal(rjags::list.modules(),c('basemod','bugs'))
jagsUI:::set.modules('glm', FALSE)
expect_equal(rjags::list.modules(),c('basemod','bugs','glm'))
jagsUI:::set.modules(NULL, FALSE)
expect_equal(rjags::list.modules(),c('basemod','bugs'))
jagsUI:::set.modules(NULL, DIC=TRUE)
expect_equal(rjags::list.modules(),c('basemod','bugs','dic'))


# load_factories works correctly
jagsUI:::set.factories(NULL)
expect_equal(rjags::list.factories('sampler')[1:2,2], c(TRUE, TRUE))
jagsUI:::set.factories(c('bugs::BinomSlice sampler FALSE',
                                   'bugs::RW1 sampler FALSE'))
expect_equal(rjags::list.factories('sampler')[1:2,2], c(FALSE, FALSE))
jagsUI:::set.factories(c('bugs::BinomSlice sampler TRUE',
                                   'bugs::RW1 sampler TRUE'))
expect_equal(rjags::list.factories('sampler')[1:2,2], c(TRUE, TRUE))
expect_error(jagsUI:::set.factories(c('bad bad')))
expect_error(jagsUI:::set.factories(c('bugs::fake sampler FALSE')))
