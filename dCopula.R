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

pGumbel <- function(u1, u2, param) {
  exp(- ((-(log(u1))) ^ param + (-(log(u2))) ^ param) ^ (1 / param))
}

dGumbel <- function(u1, u2, kendallTau) {
  param <- (1 - kendallTau) ^ -1
  pGumbel(u1, u2, param) * ((-log(u1)) ^ (param - 1) * (-log(u2)) ^ (param - 1)) / (u1 * u2) *
    ((-log(u1)) ^ param + (-log(u2)) ^ param) ^ (1 / param - 2) * 
    (param - 1 + ((-log(u1)) ^ param + (-log(u2)) ^ param) ^ (1 / param))
}

dNormal <- function(u1, u2, kendallTau) {
  param <- sin(kendallTau * pi / 2)
  1 / sqrt(1 - param ^ 2) * exp(- ( qnorm(u1) ^ 2 - 2 * param * qnorm(u1) * qnorm(u2) + qnorm(u2) ^ 2) / (2 * (1 - param ^ 2))) *
    exp((qnorm(u1) ^ 2 + qnorm(u2) ^ 2) / 2)
}

dStudent <- function(u1, u2, kendallTau, v = 6) {
  MinimizeTauStudent <- function(current_param) {
    abs(kendallTau - tau(tCopula(dim = 2, param = current_param)))
  }
  param <- optimize(MinimizeTauStudent, c(-1, 1))$minimum
  densite <- param ^ (-0.5) * (gamma((v + 2) / 2 * gamma(v / 2))) / (gamma((v + 1) / 2)) ^ 2 *
    (1 + (qt(u1, df = v) ^ 2 - 2 * param * qt(u1, df = v) * qt(u2, df = v) + qt(u2, df = v) ^ 2) / (v * (1 - param ^ 2))) ^((v + 2) / 2) *
    (1 + qt(u1, df = v) / v) ^ ((v + 2) / 2) * (1 + qt(u2, df = v) / v) ^ ((v + 2) / 2)
  pmax(pmin(densite, 1000), -1000)
}
