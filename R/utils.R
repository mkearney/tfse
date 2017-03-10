#' sn2id
#'
#' @param screen_name Twitter handle
#' @seealso See \url{https://dev.twitter.com/
#' overview/documentation} for more information on using
#' Twitter's API.
#' @return response Twitter account user id
#' @import rvest
#' @export
sn2id <- function(screen_name) {
    if (!requireNamespace("rvest", quietly = TRUE)) {
        stop("Rvest needed for this function to work.
         Please install it.",
         call. = FALSE)
    }
    user_id <- xml2::read_html(
        paste0("http://twitter.com/",
               screen_name))
    user_id %>%
        html_nodes(".ProfileNav") %>%
        html_attr("data-user-id")
}
sn2id <- Vectorize(sn2id, SIMPLIFY = FALSE)


#' id2sn
#'
#' Converts user ids to screen names.
#'
#' @param user_id Vector of user_id(s)
#' @return Named vector of screen names.
#' @importFrom httr GET
#' @importFrom rvest html_text html_nodes
#' @importFrom xml2 read_html
id2sn <- function(user_id) {
    h <- httr::GET(paste0(
        "https://twitter.com/intent/user?user_id=",
        user_id))
    h %>% read_html() %>%
        html_nodes("p span.nickname") %>%
        html_text() %>%
        gsub("@", "", .)
}
id2sn <- Vectorize(id2sn, SIMPLIFY = FALSE)

#' get_api
#'
#' @param url API url address.
#' @return Response formatted as nested list.
#' Assumes response object is json object.
#' @details httr jsonlite
#' @export
get_api <- function(url, token = NULL) {
  if (is.null(token)) {
    req <- httr::GET(url)
  } else {
    req <- httr::GET(url, httr::config(token = token))
  }

  if (httr::http_error(req)) {
    return(NA)
  }

  from_js(req)
}

#' qprint
#'
#' Fast and dirty data frame previewer
#' @param x Data frame
#' @param n Number of rows to preview
#' @param w Width in characters of all columns
#' @param print.gap Size of gap between columns
#' @param \dots Passed on to print function.
#' @noRd
#' @keywords internal
qprint <- function(x, n = 10, w = 12,
                   print.gap = 2, ...) {
    dn <- paste0(names(x), "              ")
    dn <- substr(dn, 1, w)
    nr <- NROW(x)
    nc <- ncol(x)
    x <- x[seq_len(n), ]
    x <- lapply(x, iconv, "UTF-8", "ASCII", "")
    d <- data.frame(lapply(x, substr, 1, w))
    names(d) <- dn
    message("****", nr, " rows*****")
    message("****", nc, " columns*****")
    print(d, print.gap = print.gap, ...)
}

.id2sn <- function(x) {
    require(rvest)
    h <- httr::GET(paste0(
        "https://twitter.com/intent/user?user_id=", x))
    h %>% read_html() %>%
        html_nodes("p span.nickname") %>%
        html_text() %>%
        gsub("@", "", .)
}
#' id2sn
#'
#' Returns screen_names for given ids
#'
#' @param user_id Vector of user ids
#' @return Vector of screen names
#' @export
id2sn <- function(user_id) {
    unlist(lapply(user_id, .id2sn), use.names = FALSE)
}

#' @importFrom grDevices rgb
alphacolor <- function(cols, a = .99) {
    cols <- t(col2rgb(cols, alpha = TRUE)) / 255
    rgb(cols, alpha = a)
}

#' @importFrom grDevices col2rgb
is.color <- function(x) {
    if (all(grepl("^#", x))) return(TRUE)
    x <- tryCatch(col2rgb(x),
        error = function(e) return(NULL))
    if (!is.null(x)) return(TRUE)
    FALSE
}


#' rtaes
#'
#' Sets aesthetics
#'
#' @param x Variable x either longitudinal coordinates or
#'    time
#' @param y Variable y either latitude coordinates or
#'    freq
#' @param color Variable used to determine color
#' @param alpha Alpha level used to set transparency
#' @param size Size of plot units
#' @param shape Shape of plot units
#' @noRd
#' @export
rtaes <- function(x, y,
    color = NULL,
    alpha = NULL,
    size = NULL,
    shape = NULL) {

    if (is.null(alpha)) alpha <- .99
    if (is.null(size)) size <- .4
    if (is.null(shape)) shape <- 16L

    if (!is.null(color)) {
        if (is.color(color)) {
            colors <- color
        } else {
            if (is.factor(color)) {
                cols <- length(levels(color))
                cols <- sample(getcols(cols), cols)
                colors <- cols[match(color, levels(color))]
            } else {
                cols <- length(unique(color))
                cols <- sample(getcols(cols), cols)
                colors <- cols[match(color, unique(color))]
            }
        }
    } else {
        colors <- "#0066aa"
    }
    colors <- alphacolor(colors, alpha)

    data.frame(x = x, y = y,
        color = colors,
        size = size,
        shape = shape,
        stringsAsFactors = FALSE)
}

#' rtpoint
#'
#' Plots map coordinate points
#'
#' @param dat Data piped from rtaes function
#' @param noise Logical indicating whether to apply minor
#'    jitter function to points.
#' @importFrom graphics points
#' @importFrom stats sd runif
#' @noRd
rtpoint <- function(long, lat, noise = FALSE) {
    if (noise) {
        stdv <- sd(as.double(long),
            na.rm = TRUE) / 1000
        long <- runif(length(long), -stdv * 2,
            stdv * 2) + long
        lat <- runif(length(lat), -stdv * 2,
            stdv * 2) + lat
    }
    points(long,
        lat,
        col = dat[["color"]],
        cex = dat[["size"]],
        pch = dat[["shape"]])
    invisible(dat)
}


#' rtline
#'
#' Plots time series line
#'
#' @param dat Data piped from rtaes function
#' @param new Logical indicating whether to revert to base r plot
#'   defaults to false.
#' @param \dots Args passed to base plot.
#' @noRd
rtline <- function(dat, new = FALSE, ...) {
    names(dat)[1:2] <- c("time", "freq")
    dat$time <- as.numeric(dat$time)
    dat$freq <- as.numeric(dat$freq)
    if (new) {
        with(dat, plot(time, freq, type = "l", lty = lty, ...))
    } else {
        with(dat, lines(time, freq, ...))
    }
    invisible(dat)
}


tweet.world <- function(x, color = NULL,
                        bg = NULL,
                        pch = 21,
                        cex = .25,
                        xlim = c(-170, 180),
                        ylim = c(-65, 75),
                        blank.plot = FALSE) {
    if (!is.null(color)) {
        if (is.color(color)) {
            col <- color
            if (is.null(bg)) bg <- col
        } else {
            n <- length(unique(x[[color]]))
            cols <- mapcols(n)
            cols <- cols[match(
                x[[color]], unique(x[[color]]))]
            col <- paste0(cols, "aa")
            bgs <- mapcols(n, c = 90)
            bgs <- bgs[match(
                x[[color]], unique(x[[color]]))]
            bg <- paste0(bgs, "aa")
        }
    } else {
        col <- "#224488aa"
        bg <- "#002244aa"
    }
    if (blank.plot) {
        par(mar = c(0, 0, 0, 0),
            bty = "n", yaxt = "n", xaxt = "n")
        plot(NA, xlim = xlim, ylim = ylim)
    }
    points(x[["long"]], x[["lat"]],
           pch = pch, cex = cex, col = col, bg = bg)
}

