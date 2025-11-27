#' Checks for WhiteboxTools executable
#'
#' @return If \code{WhiteboxTools} cannot be found, a message explaining what to do is displayed.
#'
#' @author Kevin Shook
#'
#' @examples
#' # Only proceed if Whitebox executable is installed
#' library(whitebox)
#' if (check_whitebox_binary()){
#'   bb_wbt_check_whitebox()
#' } else {
#'   message("Example not run as Whitebox executable not found")
#' }
#'
#' @importFrom whitebox check_whitebox_binary
#' @export bb_wbt_check_whitebox
bb_wbt_check_whitebox <- function() {
  wb_found <- whitebox::check_whitebox_binary(silent = TRUE)
  if (!wb_found) {
    msg <- paste("The WhiteboxTools executable could not be found.\n",
                 "Make sure that you have run install_whitebox().\n",
                 "If you have already done this, try setting the path to the executable using wbt_init().", sep = "")
    stop(msg)
  }
}
