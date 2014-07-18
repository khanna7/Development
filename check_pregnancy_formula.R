## 3 Sep 2013: Check formula for pregnancy:
## binomial or linear approximation?

## Let p_y = prob. of getting pregnant over a year and
## let beta.d = prob. of getting pregnant per day.

## Then, is 1-(1-beta.d)^365=p_y OR is beta.d = p_y/365.

beta.d <- seq(0, 10/1e3, by=1e-5)

py.1 <- 1-(1-beta.d)^365

py.2 <- 365*beta.d

plot(beta.d, py.1, type="l") # py on y-axis
lines(beta.d, py.2, type="l", col="blue") 



plot(py.1, beta.d, type="l") #py on x-axis, since that is from data
lines(py.2, beta.d, type="l", col="blue")

py <- seq(0, 1, by=1e-3)

binom.beta.d.byhand <- 1-exp((1/365)*log(1-py))
##solving for beta.d by binom form
beta.d.linear <- py/365 #solving for beta.d by linear assumption
lines(py, binom.beta.d.byhand, col="red")
## blue and red exactly coincide -- so my solution for beta.d by
## binomial formula is correct.

pdf(file = "pregnancy_models.pdf")
plot(py, binom.beta.d.byhand, col="blue", type="l",
     xlab="Annual probability of pregnancy",
     ylab="Daily probability of pregnancy",
     ylim = c(0, 0.01))
lines(py, beta.d.linear, col="red")
legend("topleft", c("Binomial model", "Linear model"),
       col = c("blue", "red"),
       lty=1,
       title = "")
dev.off()
