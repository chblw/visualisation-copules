library(copula)

dEFGM <- function(u1, u2, kendallTau) {
  param <- 9 * kendallTau / 2
  1 + param * (1 - 2 * u1) * (1 - 2 * u2)
}

dClayton <- function(u1, u2, kendallTau) {
  param <- 2 * (1 / kendallTau - 1) ^ -1
  (1 + param) / (u1 * u2) ^ (param + 1) * (u1 ^ -param + u2 ^ -param - 1) ^ (-2 - 1 / param)
}

dAMH <- function(u1, u2, kendallTau) {
  MinimizeTauAMH <- function(param) {
    abs( kendallTau - (1 - 2 * ( (1 - param) ^ 2 * log(1 - param) + param) / (3 * param ^ 2)))
  }
  param <- optimize(MinimizeTauAMH, c(-1, 1))$minimum
  (1 - param + 2 * param * u1 * u2 / (1 - param * (1 - u1) * (1 - u2))) / (1 - param * (1 - u1) * (1 - u2)) ^ 2
}

dFrank <- function(u1, u2, kendallTau) {
  MinimizeTauFrank <- function(param) {
    if(param == 0) {
      10000
    } else {
      abs( kendallTau - (1 + 4 / param * (debye1(param) - 1)))
    }
  }
  param <- optimize(MinimizeTauFrank, c(-100, 100))$minimum
  param * exp(- param * (u1 + u2)) * (1 - exp(-param)) /
    (exp(-param * (u1 + u2)) - exp(-param * u1) - exp(-param * u2) + exp(-param)) ^ 2
}

dGumbel <- function(u1, u2, kendallTau) {
  param <- (1 - kendallTau) ^ -1
  gumbel.cop <- archmCopula("gumbel", param)
  dCopula(c(u1, u2), gumbel.cop)
}

dGumbel <- Vectorize(dGumbel)

dNorm <- function(u1, u2, kendallTau) {
  param <- sin(kendallTau * pi / 2)
  norm.cop <- normalCopula(param)
  dCopula(c(u1, u2), norm.cop)
}

dNorm <- Vectorize(dNorm)
