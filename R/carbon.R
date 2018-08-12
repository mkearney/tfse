
#' Create carbon.sh image in browser
#'
#' @param x Code to export to carbon for image capture
#' @return Opens browser to carbon.sh with supplied code
#' @export
carbon.sh <- function(x) {
  ex <- rlang::enquo(x)
  chr <- tryCatch(is.character(x), error = function(e) return(FALSE))
  if (!chr) {
    x <- sub("~", "", rlang::expr_text(ex))
    x <- gsub("^\\(|\\)$", "", x)
    if (grepl("^\\s{0,}\\{", x)) {
      x <- gsub("\n\\s{4}", "\n", x)
      x <- gsub("(^\\s{0,}\\{\\s+)|(\\s+\\}\\s{0,}$)", "", x)
    }
    x <- gsub("[ ]{4}", "  ", x)
  }
  Rcode <- utils::URLencode(x, reserved = TRUE)
  url <- glue::glue("https://carbon.now.sh/?bg=rgba(171,%20184,%20195,%201)&t=material&wt=none&l=r&ds=true&dsyoff=20px&dsblur=68px&wc=true&wa=true&pv=48px&ph=32px&ln=false&fm=Hack&fs=14px&lh=133%25&si=false&code={Rcode}&es=2x&wm=false")
  utils::browseURL(url)
}


