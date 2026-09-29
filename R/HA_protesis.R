#' Càlcul de la despesa meritada — Hospitalització d'Aguts: protesis
#'
#' @param QF Quantitat/import facturat (realitzat).
#' @param tarifa Tarifa unitària.
#' @param casos Nombre de casos.
#' @param preu_mig Preu mitjà.
#'
#' @return Despesa meritada calculada.
#' @export
HA_protesis<- function(QF,tarifa,casos,preu_mig) {
  arguments <- as.list(environment())[c("tarifa", "QF")]
  
  # Llindars propis de Pròtesis
  llindar_casos    <- 0.10   # % d'excés de casos permès abans de regularitzar
  pct_excés_casos  <- 0.25   # % aplicat al casos que excedeixen el llindar
  
  # Validació d'inputs
  stopifnot(
    is.numeric(casos), casos >= 0,
    is.numeric(preu_mig), preu_mig >= 0,
    is.numeric(c(tarifa, QF)),
    all(c(tarifa, QF) >= 0, na.rm = TRUE)
  )
  
  # casos i preu_mig han de ser constants dintre de cada UP
  casos_unic <- unique(casos[!is.na(casos)])
  preu_mig_unic <- unique(preu_mig[!is.na(preu_mig)])
  if (length(casos_unic) != 1L) {
    stop("***casos ha de tenir un únic valor dintre de cada UP***")
  }
  if (length(preu_mig_unic) != 1L) {
    stop("***preu_mig ha de tenir un únic valor dintre de cada UP***")
  }
  # Convertir els vectors repetits en valors escalars
  casos <- casos_unic[[1]]
  preu_mig <- preu_mig_unic[[1]]
  
  df_meritada <- tryCatch(
    data.frame(arguments),
    error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
  )
  
  tarifa <- vapply(df_meritada$tarifa, num0, numeric(1))
  QF <- vapply(df_meritada$QF, num0, numeric(1))
  
  # Agregació de totes les activitats de pròtesis
  casos_realitzats  <- sum(QF)
  import_realitzat  <- sum(QF * tarifa)
  
  preu_mig_realitzat <- if (casos_realitzats == 0) 0 else round_excel(import_realitzat / casos_realitzats, 2)
  
  # Regularització del nombre de casos (casos): excés per sobre del 10% es paga al 25%
  if (casos_realitzats > casos * (1 + llindar_casos)) {
    casos_regularitzats <- (casos_realitzats - casos * (1 + llindar_casos)) * pct_excés_casos +
      casos * (1 + llindar_casos)
  } else {
    casos_regularitzats <- casos_realitzats
  }
  
  # Regularització del preu mig (preu_mig): mai es paga per sobre del preu mig contractat
  preu_mig_regularitzat <- if (preu_mig_realitzat > preu_mig) preu_mig else preu_mig_realitzat
  
  meritada <- round_excel(casos_regularitzats * preu_mig_regularitzat, 2)
  
  return(meritada)
}
