# Robustness function
# Authors: Tom Fanshawe and Jason Oke, Nuffield Department of Primary Care Health Sciences, University of Oxford
# License: GPL v3, https://www.gnu.org/licenses/gpl.html
#
#Allow the following arguments to change according to user input:
#distribution1: "normal" or "skewed"
#distribution2: "normal" or "skewed"
#mean1: mean of distribution 1 (allow values of 0,0.5,1,1.5)
#mean2: mean of distribution 2 (allow values of 0,0.5,1,1.5)
#sd1: standard deviation of distribution 1 (allow values between 0.01 and 2)
#sd2: standard deviation of distribution 1 (allow values between 0.01 and 2)
#N1: number of data-points in sample 1 (allow integers between 5 and 1000)
#N2: number of data-points in sample 2 (allow integers between 5 and 1000)
#reps: number of samples (allow integers between 5 and 50000)
#alpha1: for the skewed distribution 1 (allow values -20,-10,-5,-2,0,2,5,10,20)
#alpha2: for the skewed distribution 2 (allow values -20,-10,-5,-2,0,2,5,10,20)
#sig.level: significance level (allow values 0.001,0,01,0.05,0.1)


library(sn)

g <-
  function(distribution1 = "normal",
           distribution2 = "normal",
           mean1 = 0,
           mean2 = 0,
           sd1 = 1,
           sd2 = 1,
           alpha1 = 5,
           alpha2 = 5,
           N1 = 20,
           N2 = 20,
           reps = 1000,
           sig.level = 0.05) {
    if (distribution1 == "skewed") {
      v1 <- sd1 ^ 2
      m1 <- mean1
      xi1 <- m1 - sign(alpha1) * sqrt(2 * v1 * alpha1 ^ 2 / (pi + alpha1 ^
                                                               2 * (pi - 2)))
      delta1 <- sqrt((m1 - xi1) ^ 2 * pi / (2 * (v1 + (m1 - xi1) ^ 2)))
      omega1 <- sqrt(v1 + (m1 - xi1) ^ 2)
    }
    if (distribution2 == "skewed") {
      v2 <- sd2 ^ 2
      m2 <- mean2
      xi2 <- m2 - sign(alpha2) * sqrt(2 * v2 * alpha2 ^ 2 / (pi + alpha2 ^
                                                               2 * (pi - 2)))
      delta2 <- sqrt((m2 - xi2) ^ 2 * pi / (2 * (v2 + (m2 - xi2) ^ 2)))
      omega2 <- sqrt(v2 + (m2 - xi2) ^ 2)
    }
    
    equal <- (mean1 == mean2)
    
    x <- matrix(NA, reps, N1)
    if (distribution1 == "normal") {
      for (i in 1:reps) {
        x[i, ] <- rnorm(N1, mean = mean1, sd = sd1)
      }
    }
    if (distribution1 == "skewed") {
      for (i in 1:reps) {
        x[i, ] <- rsn(N1,
                      xi = xi1,
                      omega = omega1,
                      alpha = alpha1)
      }
    }
    
    y <- matrix(NA, reps, N2)
    if (distribution2 == "normal") {
      for (i in 1:reps) {
        y[i, ] <- rnorm(N2, mean = mean2, sd = sd2)
      }
    }
    if (distribution2 == "skewed") {
      for (i in 1:reps) {
        y[i, ] <- rsn(N2,
                      xi = xi2,
                      omega = omega2,
                      alpha = alpha2)
      }
    }
    
    #print(summary(c(y)))
    
    p <- rep(NA, reps)
    for (i in 1:reps) {
      p[i] <- t.test(x[i, ], y[i, ])$p.v
    }
    
    #print(summary(p))
    
    par(mfrow = c(3, 1), mar = c(4, 2, 2, 2))
    switch(
      distribution1,
      normal = curve(
        dnorm(x, mean = mean1,sd=sd1),
        -4,
        4,
        xaxp = c(-4, 4, 8),
        yaxt = "n",
        xlab = "",
        ylab = "",
        main = "Population 1",
        cex.axis = 1.5,
        cex.main = 2
      ),
      skewed = curve(
        dsn(
          x,
          xi = xi1,
          omega = omega1,
          alpha = alpha1
        ),
        -4,
        4,
        xaxp = c(-4, 4, 8),
        yaxt = "n",
        xlab = "",
        ylab = "",
        main = "Population 1",
        cex.axis = 1.5,
        cex.main = 2
      )
    )
    switch(
      distribution2,
      normal = curve(
        dnorm(x, mean = mean2,sd=sd2),
        -4,
        4,
        xaxp = c(-4, 4, 8),
        yaxt = "n",
        xlab = "",
        ylab = "",
        main = "Population 2",
        cex.axis = 1.5,
        cex.main = 2
      ),
      skewed = curve(
        dsn(
          x,
          xi = xi2,
          omega = omega2,
          alpha = alpha2
        ),
        -4,
        4,
        xaxp = c(-4, 4, 8),
        yaxt = "n",
        xlab = "",
        ylab = "",
        main = "Population 2",
        cex.axis = 1.5,
        cex.main = 2
      )
    )
    plot(
      0,
      0,
      type = "n",
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      bty = "n",
      xlim = c(-0.03, 1),
      ylim = c(0, 1.2)
    )
    rect(-0.03, 0.5, 1, 1.5, col = "lightblue", border = NA)
    text(
      0,
      1,
      paste("Distribution 1 :", distribution1),
      font = 2,
      cex = 1.5,
      adj = 0
    )
    text(
      0,
      0.75,
      paste("Distribution 2 :", distribution2),
      font = 2,
      cex = 1.5,
      adj = 0
    )
    text(0.3,
         1.2,
         "N",
         font = 2,
         cex = 1.5,
         adj = 0.5)
    text(
      rep(0.3, 2),
      c(1, 0.75),
      c(N1, N2),
      font = 2,
      cex = 1.5,
      adj = 0.5
    )
    text(0.5,
         1.2,
         "Mean",
         font = 2,
         cex = 1.5,
         adj = 0.5)
    text(
      rep(0.5, 2),
      c(1, 0.75),
      c(mean1, mean2),
      font = 2,
      cex = 1.5,
      adj = 0.5
    )
    text(0.7,
         1.2,
         "SD",
         font = 2,
         cex = 1.5,
         adj = 0.5)
    text(
      rep(0.7),
      c(1, 0.75),
      c(sd1, sd2),
      font = 2,
      cex = 1.5,
      adj = 0.5
    )
    text(0.9,
         1.2,
         "Skew",
         font = 2,
         cex = 1.5,
         adj = 0.5)
    text(
      rep(0.9),
      c(1, 0.75),
      c(switch(
        distribution1, normal = 0, skewed = alpha1
      ),
      switch(
        distribution2, normal = 0, skewed = alpha2
      )),
      font = 2,
      cex = 1.5,
      adj = 0.5
    )
    rect(-0.03, 0, 1, 0.5, col = switch(1 + equal, "tomato1", "lightseagreen"), border =
           NA)
    text(
      0.3,
      0.25,
      paste("Reps:", reps),
      font = 2,
      cex = 1.5,
      adj = 0
    )
    text(
      0.5,
      0.25,
      paste("Sig. level:", sig.level),
      font = 2,
      cex = 1.5,
      adj = 0
    )
    switch(
      1 + equal,
      text(
        0.7,
        0.25,
        paste("Power:", round(sum(p < sig.level) / reps, 3)),
        font = 2,
        cex = 1.5,
        adj = 0
      ),
      text(
        0.7,
        0.25,
        paste("Type 1 Error:", round(sum(p < sig.level) / reps, 3)),
        font = 2,
        cex = 1.5,
        adj = 0
      )
    )
    text(0.7, 0.15, paste0("(", sum(p < sig.level), " sig, ", sum(p >= sig.level), " not sig)"), adj =
           0)
  }