#' Càlcul de la despesa meritada per urgències d'hivern
#'
#' @param QC Quantitat contractada.
#' @param tarifa Tarifa unitària.
#' @param QF Quantitat facturada/realitzada.
#'
#' @return Un numèric amb la despesa meritada agregada.
#' @export
#'
#' @examples
#' urgencies_hivern_meritada(QC = c(300, 200), tarifa = c(60, 55), QF = c(320, 190))
urgencies_hivern_meritada <- function(QC, tarifa, QF) {
  arguments <- as.list(environment())

  stopifnot(
    is.numeric(c(QC, tarifa, QF)),
    all(c(QC, tarifa, QF) >= 0, na.rm = TRUE)
  )

  df_meritada <- tryCatch(
    data.frame(arguments),
    error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
  )

  F_total <- sum(QC * tarifa)
  V_total <- sum(QF * tarifa)

  if (V_total > F_total) {
    meritada <- round_excel(F_total, 2)
  } else {
    meritada <- round_excel(V_total, 2)
  }

  return(meritada)
}
