
fcf_generator <- function(beta0 = 1, beta1 = -1, beta2 = -1) {
    f <- function(x1, x2) beta0 + exp(beta1 * x1 + beta2 * x2)
    attr(f, "betas") <- c(beta0, beta1, beta2)

    return(f)
}

corte_generator <- function(f, x) {
    betas <- attr(f, "betas")[-1]

    dfdx_t <- t(c(
        betas[1] * exp(betas[1] * x[1] + betas[2] * x[2]),
        betas[2] * exp(betas[1] * x[1] + betas[2] * x[2])
    ))
    y0 <- f(x[1], x[2])

    function(x1, x2) c(y0 + dfdx_t %*% (matrix(c(x1, x2), 2, byrow = TRUE) - x))
}
