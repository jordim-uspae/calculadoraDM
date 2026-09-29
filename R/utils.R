#' Arrodoniment estil Excel
#'
#' Arrodoneix un valor numèric de la mateixa manera que ho fa Excel
#' (arrodoniment "half away from zero"), eliminant primer el soroll de
#' coma flotant típic de R (p. ex. 984.7349999999999 -> 984.735).
#'
#' @param x Vector numèric a arrodonir.
#' @param digits Nombre de decimals (per defecte 2).
#'
#' @return Vector numèric arrodonit.
#' @keywords internal
round_excel <- function(x, digits = 2) {
  x <- round(x, 9)
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

#' Substitueix NA per 0
#'
#' @param x Valor numèric (longitud 1).
#'
#' @return `x`, o `0` si `x` és `NA`.
#' @keywords internal
num0 <- function(x) {
  if (is.na(x)) 0 else x
}
