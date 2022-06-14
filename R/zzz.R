# This is currently commented out because it refers to the non-private integral

# .onLoad <- function(...) {
#   try(check_ic_up_to_date())
# }
#
# check_ic_up_to_date <- function() {
#   rem <- structure(remotes:::package2remote("integral"), class = "github_remote")
#   if(is.na(rem[["sha"]])) return()
#   rem <- remotes:::remote_sha(rem)
#
#   loc <- remotes:::local_sha("integral")
#
#   if(loc != rem) {
#     cli::cli_alert_info(cli::bg_red(cli::col_white(cli::style_bold("A newer version of \`integral\` is available.  Run \`ic_update()\` to update."))))
#   }
#
# }
