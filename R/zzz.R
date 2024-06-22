#' @importFrom doFuture `%dofuture%`
#' @importFrom lmtp static_binary_on
#' @importFrom lmtp static_binary_off
NULL

.onLoad <- function(...) {
	S7::methods_register()
}
