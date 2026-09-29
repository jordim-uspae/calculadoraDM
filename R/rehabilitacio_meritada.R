#' Càlcul de la despesa meritada per rehabilitació
#'
#' @param QC Quantitat contractada.
#' @param tarifa Tarifa unitària.
#' @param QF Quantitat facturada/realitzada.
#'
#' @return Un numèric amb la despesa meritada agregada.
#' @export
#'
#' @examples
#' rehabilitacio_meritada(QC = c(500, 300), tarifa = c(20, 25), QF = c(520, 310))
rehabilitacio_meritada <- function(QC, tarifa, QF) {
  arguments <- as.list(environment())

  # Llindar propi de Rehabilitació
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

  # Agregació dels processos

  F_total <- sum(QC * tarifa)   # pressupost total contractat
  V_total <- sum(QF * tarifa)   # pressupost total realitzat

  if (V_total > F_total) {
    meritada <- round_excel(F_total + (V_total - F_total) * pct_tarifa_excés, 2)
  } else {
    meritada <- round_excel(V_total, 2)
  }

  return(meritada)
}
