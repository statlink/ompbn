omp.bn <- function(x, R = NULL, method = "pvalue", tol = 0.05, restart = 10, score = "bic-g") {
  ## score -> "loglik-g", "aic-g", "bic-g", "bge"
  runtime <- proc.time()
  a <- omp.bn::omp.network(x, R = R, method = method, tol = tol)
  nama <- colnames(a$G)
  vale <- which(a$G == 1)
  dag <- NULL
  score <- NULL
  if ( length(vale) > 0 ) {
    x <- as.data.frame(x)
    mhvale <- as.data.frame( which(a$G == 0, arr.ind = TRUE) )
    mhvale[, 1] <- nama[ mhvale[, 1] ]
    mhvale[, 2] <- nama[ mhvale[, 2] ]
    colnames(mhvale) <- c("from", "to")
    dag <- bnlearn::hc(x, blacklist = mhvale, score = score, restart = restart)
    score <- bnlearn::score(x = dag, data = x, type = score )
  }
  runtime <- proc.time() - runtime
  list(ini = a, dag = dag, score = score, runtime = runtime)
}












