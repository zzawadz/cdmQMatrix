fitQmat <-
  function(concepts.no,
           scores,
           scores.test,
           verbose = FALSE) {
    count.scores <- plyr::count(scores, vars = colnames(scores))
    freq <- count.scores$freq
    unique.scores <-
      count.scores[, -ncol(count.scores)] %>% as.matrix()
    
    
    count.scores.test <-
      plyr::count(scores.test, vars = colnames(scores.test))
    freq.test <- count.scores.test$freq
    unique.scores.test <-
      count.scores.test[, -ncol(count.scores.test)] %>% as.matrix()
    
    delta = 0.1
    questions.no <- ncol(unique.scores)
    qmat <- matrix(round(runif(concepts.no * questions.no), 1),
                   nrow = concepts.no,
                   ncol = questions.no)
    
    current.error <-
      getQError(qmat = qmat, unique.scores, freq = freq)
    final.mat <- qmat
    
    test.erros <- errors <- rep(NA, 50)
    
    for (i in 1:50) {
      keep.going <- FALSE
      
      for (j in 1:nrow(qmat)) {
        for (k in 1:ncol(qmat)) {
          kk <- qmat[j, k]
          
          if (!isTRUE(all.equal(kk, 1))) {
            qmat.tmp <- qmat
            qmat.tmp[j, k]  <- qmat.tmp[j, k] + delta
            
            err <-
              getQError(qmat = qmat.tmp, unique.scores, freq = freq)
            if (err < current.error) {
              if (verbose) {
                cat(err,
                    " Cr: ",
                    current.error,
                    " j: ",
                    j,
                    " k: ",
                    k,
                    " Plus\n")
              }
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
              if (verbose) {
                cat(err,
                    " Cr: ",
                    current.error,
                    " j: ",
                    j,
                    " k: ",
                    k,
                    " Minus\n")
              }
              current.error <- err
              final.mat <- qmat <- qmat.tmp
              keep.going <- TRUE
            }
          }
        }
      }
      
      
      errors[[i]]     <- current.error
      test.erros[[i]] <-
        getQError(qmat = qmat.tmp, unique.scores.test, freq = freq.test)
      
      if (verbose) {
        matplot(cbind(errors, test.erros),
                type = "l",
                lwd = 2)
      }
      
      if (!keep.going)
        break
      
      print(i)
    }
    
    list(final.mat = final.mat, error = current.error)
  }

############
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
  
  sum(vapply(1:nrow(unique.scores), FUN.VALUE = 0.0, function(i) {
    a <- unique.scores[i, ]
    min(colSums(abs(a - idr))) * freq[[i]]
  }))
}
