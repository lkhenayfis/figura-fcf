library(plotly)

plota_fcf <- function(f, return_data = FALSE) {

    x1 <- x2 <- seq(0, 1, by = .01)
    X <- expand.grid(x1 = x1, x2 = x2)

    y <- with(X, f(x1, x2))
    y <- matrix(y, length(x1), length(x2), byrow = TRUE)

    if (return_data) {
        X <- cbind(X, y = c(t(y)))
        return(X)
    }

    p <- plot_ly(x = x1, y = x2, z = y) %>% add_surface()
    attr(p, "f") <- f

    invisible(p)
}

plota_corte <- function(fp, x, return_data) UseMethod("plota_corte")

plota_corte.function <- function(f, x = list(), return_data = FALSE) {

    x0s <- do.call(rbind, x)
    y0s <- f(x0s[, 1], x0s[, 2])

    cortes <- lapply(x, function(xi) corte_generator(f, xi))

    x1 <- x2 <- seq(0, 1, by = .01)
    X <- expand.grid(x1 = x1, x2 = x2)

    ys <- lapply(cortes, function(fc) with(X, fc(x1, x2)))
    ys <- do.call(cbind, ys)
    y_max <- apply(ys, 1, max)
    y_max <- matrix(y_max, length(x1), length(x2), byrow = TRUE)

    if (return_data) {
        X <- cbind(X, y = c(t(y_max)))
        X$tipo <- "superficie"
        x0s <- as.data.frame(cbind(x0s, y0s))
        x0s$tipo <- "ponto"
        colnames(x0s) <- colnames(X)
        X <- rbind(X, x0s)
        return(X)
    }

    p <- plot_ly() %>% add_surface(x = x1, y = x2, z = y_max) %>%
        add_markers(x = x0s[, 1], y = x0s[, 2], z = y0s)

    invisible(p)
}
