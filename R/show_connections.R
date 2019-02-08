
#' show connections
#'
#' Displays active connections as a tidy tibble
#'
#' @return Prints and invisibly returns data frame.
#' @export
show_connections <- function() {
  ## get connections
  cons <- showConnections(all = TRUE)

  ## convert to data frame
  cons <- data.frame(
    description = as.character(cons[, 1]),
    class = as.character(cons[, 2]),
    mode = as.character(cons[, 3]),
    text = as.character(cons[, 4]),
    is_open = as.character(cons[, 5]),
    can_read = as.character(cons[, 6]),
    can_write = as.character(cons[, 7]),
    stringsAsFactors = FALSE
  )

  ## convert is_ and can_ columns to logical
  iscan <- grepl("^is_|^can_", names(cons))
  cons[iscan] <- lapply(cons[iscan], function(.x)
    ifelse(.x == "opened", TRUE,
      ifelse(.x == "closed", FALSE,
        ifelse(.x == "yes", TRUE,
          ifelse(.x == "no", FALSE, NA)))))

  ## if new connection(s) move to top
  if (nrow(cons) > 3L) {
    cons <- cons[c(4:nrow(cons), 1:3), ]
  }

  ## reset row names and return data
  row.names(cons) <- NULL
  cons
}

