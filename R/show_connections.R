
#' show connections
#'
#' Displays active connections as a tidy tibble
#'
#' @return Prints and invisibly returns tibble
#' @export
show_connections <- function() {
  ## get connections
  conns <- showConnections(all = TRUE)

  ## convert to tbl_df
  conns <- data_frame(description = as.character(conns[, 1]),
    class = as.character(conns[, 2]),
    mode = as.character(conns[, 3]),
    text = as.character(conns[, 4]),
    is_open = as.character(conns[, 5]),
    can_read = as.character(conns[, 6]),
    can_write = as.character(conns[, 7]))

  ## convert is_ and can_ columns to logical
  conns <- conns %>%
    dplyr::mutate_if(grepl("^is_|^can_", names(conns)),
    ~ case_when(
      .x == "opened" ~ TRUE,
      .x == "closed" ~ FALSE,
      .x == "yes" ~ TRUE,
      .x == "no" ~ FALSE,
      TRUE ~ NA))

  ## if new connection(s) move to top
  if (nrow(conns) > 3L) {
    conns <- conns[c(4:nrow(conns), 1:3), ]
  }

  ## print conns tbl_df
  print_tbl(conns, title = "Connections:")

  ## return
  invisible(conns)
}

