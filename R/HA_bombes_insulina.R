#' Càlcul de la despesa meritada — Hospitalització d'Aguts: bombes insulina
#'
#' @param QC Quantitat/import contractat.
#' @param QF Quantitat/import facturat (realitzat).
#' @param tarifa Tarifa unitària.
#'
#' @return Despesa meritada calculada.
#' @export
HA_bombes_insulina<- function(QC,QF,tarifa) {
  arguments <- as.list(environment())
  
  # Validació d'inputs
  stopifnot(
    is.numeric(c(QC,QF,tarifa)),
    all(c(QC,QF,tarifa) >= 0, na.rm = TRUE)
  )
  
  df_meritada <- tryCatch(
    data.frame(arguments),
    error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
  )
  
  # Agregació de les modalitats
  C_total <- sum(QC * tarifa)   # pressupost total contractat
  F_total <- sum(QF * tarifa)   # pressupost total realitzat
  
  if (F_total > C_total) {
    meritada <- round_excel(C_total, 2)
  } else {
    meritada <- round_excel(F_total, 2)
  }
  
  return(meritada)
}
