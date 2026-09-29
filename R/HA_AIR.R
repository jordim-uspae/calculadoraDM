#' Càlcul de la despesa meritada — Hospitalització d'Aguts: AIR
#'
#' @param QC Quantitat/import contractat.
#' @param QF Quantitat/import facturat (realitzat).
#' @param tarifa Tarifa unitària.
#'
#' @return Despesa meritada calculada.
#' @export
HA_AIR<- function(QC,QF,tarifa) {
  arguments <- as.list(environment())
  
  # Llindar propi de l'AIR (Atenció a la Insuficiència Renal)
  pct_tarifa_excés <- 0.90
  
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
  
  # Agregació de les modalitats
  C_total <- sum(QC * tarifa)   # pressupost total contractat
  F_total <- sum(QF * tarifa)   # pressupost total realitzat
  
  if (F_total > C_total) {
    meritada <- round_excel(C_total + (F_total - C_total) * pct_tarifa_excés, 2)
  } else {
    meritada <- round_excel(F_total, 2)
  }
  
  return(meritada)
}
