#' Càlcul de la despesa meritada per bombes d'insulina
#'
#' @param QC Quantitat contractada.
#' @param tarifa Tarifa unitària.
#' @param QF Quantitat facturada/realitzada.
#'
#' @return Un numèric amb la despesa meritada agregada.
#' @export
#'
#' @examples
#' bombes_insulina_meritada(QC = c(40, 30), tarifa = c(3500, 4000), QF = c(38, 28))
bombes_insulina_meritada <- function(QC, tarifa, QF) {
  arguments <- as.list(environment())

  # Validació d'inputs
  stopifnot(
    is.numeric(c(QC, tarifa, QF)),
    all(c(QC, tarifa, QF) >= 0, na.rm = TRUE)
  )

  df_meritada <- tryCatch(
    data.frame(arguments),
    error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
  )

  # Agregació de les modalitats
  F_total <- sum(QC * tarifa)   # pressupost total contractat
  V_total <- sum(QF * tarifa)   # pressupost total realitzat

  if (V_total > F_total) {
    meritada <- round_excel(F_total, 2)
  } else {
    meritada <- round_excel(V_total, 2)
  }

  return(meritada)
}
