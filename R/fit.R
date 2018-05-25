#' Fit Q Matrix
#'
#' @param scores table containing the students scores
#' @param scores.test validation set.
#' @param concepts.no number of concepts to fit
#' @param delta step size for matrix exploration.
#' @param qmat predefined qmatrix. If NULL (default) it will be initialized with one of the predefined methods.
#' @param niter maximum number of iteration.
#' @param init.method if qmat is null it will be created using one of the two methods - 'NMF'
#'        is based on Nonnegative Matrix Factorization or 'random' which is a random matrix.
#' @param verbose if true, some additional information will be printed.
#'
#' @export
#' @importFrom stats runif
#' @importFrom graphics matplot
#' 
#' @importFrom NMF nmf
#' @importFrom NMF coef
#' @importFrom stats na.omit
#' @examples
#' 
#' library(ltm)
#' data("WIRS")
#' 
#' fitQmat(WIRS, verbose = TRUE)
#' 
#' # Predefined matrix initialized with 0
#' fitQmat(WIRS, concepts.no = 2, qmat = matrix(0, ncol(WIRS), nrow = 2),verbose = TRUE)
#' # Predefined matrix initialized with 1
#' fitQmat(WIRS, concepts.no = 2, qmat = matrix(1, ncol(WIRS), nrow = 2),verbose = TRUE)
#' 
fitQmat <-
  function(scores,
           scores.test = scores,
           concepts.no = 2,
           delta = 0.1,
           qmat = NULL,
           niter = 50,
           init.method = c("NMF", "random"),
           verbose = FALSE) {
    
    # It seems that plyr has a problem with strange column names.
    # The easiest solution is to remove old colnames
    orgColNames <- colnames(scores)
    colnames(scores) <- paste0("V", seq_len(ncol(scores)))
    colnames(scores.test) <- paste0("V", seq_len(ncol(scores.test)))
    
    count.scores <- getUniqueAnswersFreqs(scores)
    freq <- count.scores$freq
    unique.scores <- count.scores$unique.scores
    
    count.scores.test <- getUniqueAnswersFreqs(scores.test)
    freq.test <- count.scores.test$freq
    unique.scores.test <- count.scores.test$unique.scores
    
    questions.no <- ncol(unique.scores)
    
    # Number of significant digits in delta
    ndig <- nchar(strsplit(as.character(delta), split = "\\.")[[1]][[2]])
    
    if(is.null(qmat)) {
      
      init.method <- init.method[[1]]
      if(init.method == "random") {
        qmat <- matrix(round(runif(concepts.no * questions.no), ndig),
                       nrow = concepts.no,
                       ncol = questions.no)
      } else if(init.method == "NMF") {
        
        fit <- nmf(scores + 0.00001, concepts.no)
        qmat <- pmin(coef(fit), 1)
                
      } else {
        stop("Please select initialization method from 'NMF' or 'random'")
      }
      
      qmat <- round(qmat, ndig)
      
    } else {
      if(nrow(qmat) != concepts.no) stop("Number of rows in qmat must match concepts.no")
      if(ncol(qmat) != questions.no) stop("Number of columns in qmat must match the number of questions.")
    }
    
    current.error <-
      getQError(qmat = qmat, unique.scores, freq = freq)
    final.mat <- qmat
    
    test.erros <- errors <- rep(NA, niter)
    
    for (i in seq_len(niter)) {
      keep.going <- FALSE
      
      for (j in seq_len(nrow(qmat))) {
        for (k in seq_len(ncol(qmat))) {
          kk <- qmat[j, k]
          
          if ((1 - kk) > delta / 10) {
            
            qmat.tmp <- qmat
            qmat.tmp[j, k]  <- qmat.tmp[j, k] + delta
            
            err <- getQError(qmat = qmat.tmp, unique.scores, freq = freq)
            
            if (err < current.error) {
              current.error <- err
              final.mat <- qmat <- qmat.tmp
              keep.going <- TRUE
            }
            
          }
          
          if (kk > delta / 10) {
            qmat.tmp <- qmat
            qmat.tmp[j, k]  <- qmat.tmp[j, k] - delta
            
            err <-
              getQError(qmat = qmat.tmp, unique.scores, freq = freq)
            if (err < current.error) {
              current.error <- err
              final.mat <- qmat <- qmat.tmp
              keep.going <- TRUE
            }
          }
        }
      }
      
      
      errors[[i]]     <- current.error
      test.erros[[i]] <- getQError(qmat = final.mat, unique.scores.test, freq = freq.test)
      
      if (verbose) {
        matplot(cbind(errors, test.erros), type = "l", lwd = 2, ylab = "Error", xlab = "Iteration")
      }
      
      if (!keep.going) break
      
    }
    
    test.error <- getQError(qmat = final.mat, unique.scores.test, freq = freq.test)
    train.error <- getQError(qmat = final.mat, unique.scores, freq = freq)
    
    colnames(final.mat) <- orgColNames
    rownames(final.mat) <- paste0("Item", seq_len(nrow(final.mat)))
    result <- list(
      final.mat = round(final.mat, digits = ndig), 
      train.error = train.error, 
      test.error = test.error, 
      errors = as.numeric(na.omit(errors)), 
      test.erros = as.numeric(na.omit(test.erros)))
    
    attr(result, which = "class") <- "CDMQMat"
    result
}

#' Get error for given Q-matrix
#'
#' @param qmat q-matrix
#' @param unique.scores unique scores 
#' @param freq frequency of unique scores.
#'
#' @export
#'
#' @examples
#'
#' library(ltm)
#' data("WIRS") 
#' scores <- getUniqueAnswersFreqs(WIRS)
#' qmat <- fitQmat(WIRS, concepts.no = 2, qmat = matrix(1, ncol(WIRS), nrow = 2))$final.mat
#' getQError(qmat, scores$unique.scores, scores$freq)
#'
getQError <- function(qmat, unique.scores, freq) {
  n.co <- nrow(qmat)
  
  all.idr <-
    iterpc::getall(iterpc::iterpc(
      2,
      n.co,
      labels = c(0, 1),
      ordered = TRUE,
      replace = TRUE
    ))
  
  idr <- (!all.idr) %*% qmat
  idr[idr > 1] <- 1
  idr <- t(1 - idr)
  
  sum(vapply(seq_len(nrow(unique.scores)), FUN.VALUE = 0.0, function(i) {
    a <- unique.scores[i, ]
    min(colSums(abs(a - idr))) * freq[[i]]
  }))
}


#' Create frequency of the answers patterns for given dataset.
#'
#' @param scores data.frame with scores.
#'
#' @return
#' 
#' List with three elements:
#' \itemize{
#'  \item{"count.scores"}{data frame with all answers patterns with a frequency column.}
#'  \item{"unique.scores"}{data frame with patterns without frequencies.}
#'  \item{"freq"}{a vector with frequencies of given pattern.}
#' }
#' 
#' @details This function mith be usefull for calculating q-error using \code{\link{getQError}} function.
#' 
#' @export
#'
#' @examples
#' library(ltm)
#' data("WIRS") 
#' scores <- getUniqueAnswersFreqs(WIRS)
#' 
getUniqueAnswersFreqs <- function(scores) {
  orgColNames <- colnames(scores)
  colnames(scores) <- paste0("V", seq_len(ncol(scores)))
  
  count.scores <- plyr::count(scores, vars = colnames(scores))
  freq <- count.scores$freq
  unique.scores <- as.matrix(count.scores[, -ncol(count.scores)])
  
  colnames(unique.scores) <- orgColNames
  colnames(count.scores)[-ncol(count.scores)] <- orgColNames
  
  list(count.scores = count.scores, unique.scores = unique.scores, freq = freq)
}
