
#' show connections
#'
#' Displays active connections as a tidy tibble
#'
#' @return Prints and invisibly returns data frame.
#' @export
show_connections <- function() {
  ## get connections
  conns <- showConnections(all = TRUE)

  ## convert to data frame
  conns <- data.frame(description = as.character(conns[, 1]),
    class = as.character(conns[, 2]),
    mode = as.character(conns[, 3]),
    text = as.character(conns[, 4]),
    is_open = as.character(conns[, 5]),
    can_read = as.character(conns[, 6]),
    can_write = as.character(conns[, 7]),
    stringsAsFactors = FALSE)

  ## convert is_ and can_ columns to logical
  iscan <- grepl("^is_|^can_", names(conns))
  conns[iscan] <- lapply(conns[iscan], function(.x)
    ifelse(.x == "opened", TRUE,
      ifelse(.x == "closed", FALSE,
        ifelse(.x == "yes", TRUE,
          ifelse(.x == "no", FALSE, NA)))))

  ## if new connection(s) move to top
  if (nrow(conns) > 3L) {
    conns <- conns[c(4:nrow(conns), 1:3), ]
  }

  ## reset row names and return data
  row.names(conns) <- NULL
  conns
}

