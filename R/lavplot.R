


## functions
unL <- function(x, r = TRUE) {
    unlist(x, use.names = FALSE, recursive = r)
}
plot_blank <- function(w, h) {
    par(mar=c(0, 0, 0, 0))
    plot(1, type = "n", bty = "n",
         axes = FALSE, xlab = "", ylab = "",
         xlim = c(-w, w), ylim = c(-h, h),
         asp = 1)
}
bump_color <- function(x, n = 1, a = 0) {
    scale <- c(0:9, letters[1:6])
    x <- sub("^#", "", x)
    x <- strsplit(x, "")[[1]]
    if (length(x) == 8) {
        prs <- list(1:2, 3:4, 5:6, 7:8)
    } else {
        prs <- list(1:2, 3:4, 5:6)
    }
    x <- unlist(lapply(prs, function(i) paste0(x[i], collapse = "")))
    a.cycle <- function(n) {
        a <- rep(as.character(n), 16)
        b <- c(0:9, letters[1:6])
        paste0(a, b)
    }
    col.range <- unlist(lapply(scale, a.cycle))
    col.vals <- vapply(x,
                       function(x) which(col.range == x), integer(1))

    col.left <- 256 - col.vals

    f <- function(n) {
        vals <- col.vals + n
        vals[vals > 256] <- 255
        vals[vals < 1] <- 1
        x <- col.range[vals]
        paste0("#", paste0(x, collapse = ""))
    }
    adj <- 133 - mean(col.vals)
    lo <- -133 + adj
    hi <- 133 + adj
    cols <- vapply(lo:hi, f, character(1))
    lng <- length(cols)
    cols[round(seq(1, lng, length.out = 100), 0)]
}
scale_cols <- function(col1, col2) {
    scale <- c(0:9, letters[1:6])

    a.cycle <- function(n) {
        a <- rep(as.character(n), 16)
        b <- c(0:9, letters[1:6])
        paste0(a, b)
    }
    b.cycle <- function(n) {
        b <- rep(as.character(n), 16)
        a <- c(0:9, letters[1:6])
        paste0(a, b)
    }

    col1 <- sub("^#", "", col1)
    col1 <- strsplit(col1, "")[[1]]
    col2 <- sub("^#", "", col2)
    col2 <- strsplit(col2, "")[[1]]
    col1[c(1, 3, 5)] <-


    bump.col1(col1)

    x[x > 16] <- 16
    x[x < 1] <- 1
}

bump.col1 <- function(x, pos = TRUE) {
        if (pos) n <- 1
        if (!pos) n <- -1
        x <- vapply(x[c(1, 3, 5)],
                    function(x) which(scale == x) + n, double(1))
        x[x > 16] <- 16
        x[x < 1] <- 1
        x
}

bump_color <- Vectorize(bump_color,
                        USE.NAMES = FALSE)
