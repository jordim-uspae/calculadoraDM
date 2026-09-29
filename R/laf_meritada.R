#' Càlcul de la despesa meritada per LAF
#'
#' @param tarifa Tarifa unitària.
#' @param QF Quantitat facturada/realitzada.
#'
#' @return Vector numèric amb la despesa meritada.
#' @export
#'
#' @examples
#' laf_meritada(tarifa = 45, QF = 200)
laf_meritada <- function(tarifa, QF) {
  arguments <- as.list(environment())

  stopifnot(
    is.numeric(c(tarifa, QF)),
    all(c(tarifa, QF) >= 0, na.rm = TRUE)
  )

  df_meritada <- tryCatch(
    data.frame(arguments),
    error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
  )

  funcio_calcul_meritada <- function(tarifa, QF) {
    tarifa <- num0(tarifa); QF <- num0(QF)

    meritada <- round_excel(tarifa * QF, 2)

    meritada
  }

  df_meritada$meritada <- mapply(funcio_calcul_meritada, tarifa, QF)

  return(df_meritada$meritada)
}
