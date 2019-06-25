random.step <- function() {
  sign(rnorm(n=1, mean=0))
}

random.walk <- function(nsteps, x0=0) {
  x <- numeric(nsteps + 1)
  x[1] <- x0
  for (i in seq_len(nsteps)) {
    x[i+1] <- x[i] + random.step()
  }
  x
}


set.seed(1)
plot(random.walk(400), type="l", xlab="Step number", ylab="Position")


set.seed(1)
nsteps <- 200
nrep <- 40
cols <- rainbow(nrep)
plot(NA, xlim=c(1, nsteps+1), ylim=c(-30, 30), xlab="Step number", ylab="Position")
for (i in 1:nrep){
  lines(random.walk(200), col=cols[i])
}
