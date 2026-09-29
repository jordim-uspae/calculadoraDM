#' Càlcul de la despesa meritada — Hospitalització d'Aguts: lipoatrofia facial
#'
#' @param QF Quantitat/import facturat (realitzat).
#' @param tarifa Tarifa unitària.
#'
#' @return Despesa meritada calculada.
#' @export
HA_lipoatrofia_facial<- function(QF,tarifa) {
  arguments <- as.list(environment())
  
  stopifnot(
    is.numeric(c(tarifa, QF)),
    all(c(tarifa, QF) >= 0, na.rm = TRUE)
  )
  
  df_meritada <- tryCatch(
    data.frame(arguments),
    error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
  )
  
  funcio_calcul_meritada <- function(QF,tarifa) {
    tarifa <- num0(tarifa); QF <- num0(QF)
    
    meritada <- round_excel(tarifa * QF, 2)
    
    meritada
  }
  
  df_meritada$meritada <- mapply(funcio_calcul_meritada, QF,tarifa)
  
  return(df_meritada$meritada)
}
