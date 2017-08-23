# Original code downloaded from Wesley's blog
# http://statistical-research.com/significant-p-values-and-overlapping-confidence-intervals
# 2013-03-26

# 'Cleaned up' and modified by Mike Meredith to look at (1) Type I error rates and
#   (2) cases where CIs overlap, but each mean is outside the other CI.
# For discussion see
# http://www.mikemeredith.net/blog/Comparison_of_confidence_intervals.htm


#' Eric might edit this file someday into an Rmd file

#Set some constants
alpha = 0.05 # significance level for t-test
ci <- 0.95  # confidence interval
# ci <- 0.85  # confidence interval
m = 15
n = 15
nsim = 20000
# nsim = 1e5 # This took 2.5 minutes with n = m = 15 on my old laptop.

# Function to generate data
# =========================
# ...and calculate confidence intervals, etc:

calcInterval = function(){
x = rnorm(m, 0, 1)
#y = rnorm(n, 1, 0.8) # Different mean and SD : Wesley's original scenario.
# Use these to check Type I error:
# y = rnorm(n, 0, 1) # Same mean, same SD for both samples, null is true.
y = rnorm(n, 0, 2) # Same mean, different SD, null is true.
ret.val <- c(t.test(x,y, var.equal=TRUE)$p.val,
              mean(x), mean(y),
              t.test(x, conf.level=ci)$conf,
              t.test(y, conf.level=ci)$conf)
names(ret.val) <- c("p", "xbar", "ybar", "ci.x.ll", "ci.x.ul", "ci.y.ll", "ci.y.ul")
return(ret.val)
}

# Generate lots of samples, get CIs
# =================================
set.seed(123)
system.time(
  my.sims <- as.data.frame( t(replicate(nsim, calcInterval())) )
)
head(my.sims)

# The "overlap test"
# ==================
# Which ones overlap:
Overlap <- my.sims$ci.x.ul > my.sims$ci.y.ll & my.sims$ci.y.ul > my.sims$ci.x.ll

# Type I error rate analysis:
# --------------------------
# What proportion DON'T overlap?
mean(!Overlap)
 # 0.005 for 95% CI, which is conservative, but not what we want
 # 0.041 for 85% CI with equal SE, which is about right
 # 0.052 for 85% CI with SE ratio 2, which is again about right

# Comparision with t-test p-value:
# -------------------------------
# Q1: does non-overlap always correspond to p < alpha?
# How many non-overlappers have p > alpha?
mean(my.sims$p[!Overlap] > alpha ) # zero, so 'yes' for 95% CIs
# Q1a: and how big is p?
mean(my.sims$p[!Overlap])
max(my.sims$p[!Overlap]) # so mostly significant at 0.01 level

# Q2: does overlap always correspond to p > alpha?
# How many overlappers have p < alpha?
mean(my.sims$p[Overlap] < alpha )  # 0.60, so 'no' (more if variances differ)


# The "mean outside" test
# =======================
xbarOut <- my.sims$xbar < my.sims$ci.y.ll | my.sims$xbar > my.sims$ci.y.ul
ybarOut <- my.sims$ybar < my.sims$ci.x.ll | my.sims$ybar > my.sims$ci.x.ul

# Type I error rate analysis
# --------------------------
mean(xbarOut & ybarOut)
 # 0.108 for equal SE, n = m = 15
 # 0.074 for SE ratio 2, n = m = 15
 # 0.128 for equal SE, n = m = 30
 # 0.078 for SE ratio 2, n = m = 30
 # 0.147 for equal SE, n = m = 100
 # 0.079 for SE ratio 2, n = m = 100

# Comparision with t-test p-value:
# --------------------------------
# Q1: does mean-outside-other-CI always correspond to p < alpha?
mean(my.sims$p[Overlap & xbarOut & ybarOut] < alpha ) # 0.81, not always.
# Q1a: and how big is p?
mean(my.sims$p[Overlap & xbarOut & ybarOut])
max(my.sims$p[Overlap & xbarOut & ybarOut]) 


