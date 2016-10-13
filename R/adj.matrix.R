
gf <- function(x, token, ...) {
  tryCatch(get_friends(x, token = token, ...),
    error = function(e) return(NULL))
}
ply_gf <- function(ids, tokens) {
  mapply(gf, ids, tokens)
}

adj_matrix <- function(x, tokens) {
  screen_names <- x[["screen_name"]]
  user_ids <- x[["user_id"]]
  n <- nrow(x)
  j <- cut(seq_len(n),
    breaks = seq(0, (ceiling(n / 15) * 15), 15),
    labels = FALSE)
  f <- ply_gf(user_ids, tokens[j])
  names(f)

  adj.mat <- unlist(lapply(f, function(x) {
    as.integer(user_ids %in% x[[1]])
  }), use.names = FALSE)

  adj.mat <- matrix(adj.mat, n, n)
  colnames(adj.mat) <- screen_names
  rownames(adj.mat) <- screen_names
  adj.mat
}
