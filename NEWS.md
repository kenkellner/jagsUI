# jagsUI 1.6.2

* Fix broken tests on CRAN

# jagsUI 1.6.1

 * Extensive refactoring of internal code
 * Don't drop output dimensions when only one parameter saved
 * Summary method now returns the summary matrix
 * Add tests
 * Add vignette

# jagsUI 1.5.2

 * Remove View function due to problematic interactions with RStudio
 * Improvements to traceplot and densityplot
 * Make sure output is reproducible when parallel=FALSE
 * Minor bugfixes

# jagsUI 1.5.1

 * Fix issue when chains adapted for different numbers of iterations
 * Fix crash when DIC was requested but JAGS couldn't calculate it
 * Stop auto-converting 1 row/1 col matrices into vectors
 * Improve pp.check plotting function
 * Add some warnings about functions and arguments that will be deprecated soon

# jagsUI 1.5.0

 * Fix issues with NAs in parameters.
 * Fix missing arguments in autojags().
 * Handle errors in output processing so that samples are not lost completely.
 * Make sure a specified random seed is used for initial value functions.
 * Fix error where summary stats were printed in the wrong order for some parameters.
 * Don't collapse 1 row/1col matrices into vectors automatically.

# jagsUI 1.4.9

 * Fix bug with DIC output introduced in 1.4.7.

# jagsUI 1.4.8

 * Minor changes to prep for CRAN submission.
 * Fix more issues with random seeds and problems with DIC.
 * Fix issue with setting seed affecting other random number generation.
 * Allow turning deviance/DIC monitoring on/off when updating an existing model.
 * Overhaul handling of adaptation; default to running a sequence of adapt() calls until JAGS reports adapation is adequate. Suggest minimum of 1000 adaptation iterations otherwise in man pages. 
 * Change output of summary function. Add ability to control factories.

# jagsUI 1.4.4

 * Change default approach to generating random seeds. 
 * Add S3 method for View() 
 * Convert traceplot() to an S3 method from S4.
 * Fix to allow running in parallel when dependencies are in non-standard libraries. 
 * Fix bug when updating jagsbasic class.

# jagsUI 1.4.2

 * Changes to NAMESPACE to get clean build check on R-devel.
 * Add ability to specify number of CPU cores to use when running in parallel.
 * Fix a problem where saving output for a single scalar parameter broke output processing.
 * Fix a problem where nonscalar estimated parameters with missing (i.e., non-estimated) values broke the display of summary stats.
 * Explicitly import functions from default packages to meet new standards for building with R-devel. 
 * Change minimum required version of rjags to 3-13.

# jagsUI 1.3.7

 * Add verbose argument to functions to allow suppression of all text output in the console as the function runs.
 * Change method for closing connection to clusters when running in parallel to avoid closing unrelated connections.
 * Adjust output for jags.basic.
 * Fix problem with max.iter argument in autojags(). Clarify documentation and output for autojags().
 * Add option save.all.iter in autojags() function to combine MCMC samples from all iterative updates into final posterior.
 * Fix issue with calculating stats that sometimes occurred with a constant posterior distribution.
 * Fix error where autojags did not handle NA rhat values properly.
 * Added more helpful warning when at least one rhat value = NA.

# jagsUI 1.3.1

 * Initial CRAN upload.
