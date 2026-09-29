#' Càlcul de la despesa meritada per reproducció humana assistida
#'
#' @param QC Quantitat contractada.
#' @param tarifa Tarifa unitària.
#' @param QF Quantitat facturada/realitzada.
#'
#' @return Un numèric amb la despesa meritada agregada.
#' @export
#'
#' @examples
#' reproduccio_humana_assistida_meritada(QC = c(20, 10), tarifa = c(1200, 900), QF = c(22, 9))
reproduccio_humana_assistida_meritada <- function(QC, tarifa, QF) {
  arguments <- as.list(environment())

  # Llindar propi de la Reproducció Humana Assistida
  pct_tarifa_excés <- 0.50

  # Validació d'inputs
  stopifnot(
    is.numeric(c(QC, tarifa, QF)),
    all(c(QC, tarifa, QF) >= 0, na.rm = TRUE)
  )

  # Construïm el df a partir dels arguments (valida que les 3 mides coincideixen)
  df_meritada <- tryCatch(
    data.frame(arguments),
    error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
  )

  F_total <- sum(QC * tarifa)   # pressupost total contractat
  V_total <- sum(QF * tarifa)   # pressupost total realitzat

  if (V_total > F_total) {
    meritada <- round_excel(F_total + (V_total - F_total) * pct_tarifa_excés, 2)
  } else {
    meritada <- round_excel(V_total, 2)
  }

  return(meritada)
}
