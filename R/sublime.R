
#' @export
sublime_project <- function(proj = ".") {
	path <- normalizePath(proj)
	p <- jsonlite::toJSON(list(
		folders = list(list(path = path))), 
		pretty = TRUE,
		auto_unbox = TRUE)
	w <- jsonlite::toJSON(list(
		expanded_folders = c(path),
		settings = list(
			menu_visible = TRUE, 
			show_minimap = FALSE,
			show_open_files = FALSE,
			show_tabs = TRUE,
			side_bar_visible = TRUE,
			status_bar_visible = TRUE)),
		pretty = TRUE,
		auto_unbox = TRUE)
	cat(as.character(p), 
		file = paste0(path, ".sublime-project"),
		fill = TRUE)
	cat(as.character(w), 
		file = paste0(path, ".sublime-workspace"),
		fill = TRUE)
}
