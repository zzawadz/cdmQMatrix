#' Fit Q Matrix
#'
#' @param scores table containing the students scores
#' @param scores.test validation set.
#' @param concepts.no number of concepts to fit
#' @param verbose if true, some additional information will be printed.
#'
#' @export
#' @importFrom stats runif
#' @importFrom graphics matplot
#' 
#' @examples
#' 
#' library(ltm)
#' data("WIRS")
#' scores <- WIRS
#' 
fitQmat <-
  function(scores,
           scores.test = scores,
           concepts.no = 2,
           delta = 0.1,
           qmat = NULL,
           niter = 50,
           verbose = FALSE) {
    
    # It seems that plyr has a problem with strange column names.
    # The easiest solution is to remove old colnames
    colnames(scores) <- paste0("V", seq_len(ncol(scores)))
    colnames(scores.test) <- paste0("V", seq_len(ncol(scores.test)))
    
    count.scores <- plyr::count(scores, vars = colnames(scores))
    freq <- count.scores$freq
    unique.scores <-
      as.matrix(count.scores[, -ncol(count.scores)])
    
    count.scores.test <-
      plyr::count(scores.test, vars = colnames(scores.test))
    freq.test <- count.scores.test$freq
    unique.scores.test <-
      as.matrix(count.scores.test[, -ncol(count.scores.test)])
    
    questions.no <- ncol(unique.scores)
    
    if(is.null(qmat)) {
      
      ndig <- nchar(strsplit(as.character(delta), split = "\\.")[[1]][[2]])
      qmat <- matrix(round(runif(concepts.no * questions.no), ndig),
                     nrow = concepts.no,
                     ncol = questions.no)
    } else {
      if(ncol(qmat) == concepts.no) stop("")
      if(nrow(qmat) == questions.no) stop("")
      qmat <- t(qmat)
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
          
          if (!isTRUE(all.equal(kk, 1))) {
            qmat.tmp <- qmat
            qmat.tmp[j, k]  <- qmat.tmp[j, k] + delta
            
            err <- getQError(qmat = qmat.tmp, unique.scores, freq = freq)
            
            if (err < current.error) {
              if (verbose) cat(err," Cr: ", current.error, " j: ", j, " k: ", k," Plus\n")
              
              current.error <- err
              final.mat <- qmat <- qmat.tmp
              keep.going <- TRUE
            }
            
          }
          
          if (!isTRUE(all.equal(kk, 0))) {
            qmat.tmp <- qmat
            qmat.tmp[j, k]  <- qmat.tmp[j, k] - delta
            
            err <-
              getQError(qmat = qmat.tmp, unique.scores, freq = freq)
            if (err < current.error) {
              if (verbose) cat(err, " Cr: ", current.error, " j: ", j, " k: ", k, " Minus\n")
              
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
    
    list(final.mat = t(final.mat), error = current.error)
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
