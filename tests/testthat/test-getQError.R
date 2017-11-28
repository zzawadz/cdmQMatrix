test_that("Perfect match", {

  def.q <- matrix(0, nrow = 6, ncol = 2)
  def.q[1:4,1] <- 1
  def.q[4:6,2] <- 1
  def.q <- t(def.q)
  
  scores <- rbind(
    c(1,1,1,0,0,0),
    c(1,1,1,1,1,1),
    c(0,0,0,0,1,1)
  )
  expect_equal(0, getQError(def.q, scores, c(1,1,1))) 
})

test_that("Small error", {
  
  def.q <- matrix(0, nrow = 6, ncol = 2)
  def.q[1:4,1] <- 1
  def.q[4:6,2] <- 1
  def.q <- t(def.q)
  
  scores <- rbind(
    c(1,1,1,0,0,0),
    c(1,1,1,1,1,1),
    c(0,0,0,1,1,1)
  )
  expect_equal(10, getQError(def.q, scores, c(1,1,10))) 
})
