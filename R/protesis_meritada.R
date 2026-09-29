#' Càlcul de la despesa meritada per pròtesis
#'
#' @param CS Nombre de casos contractats (longitud 1).
#' @param PM Preu mig contractat (longitud 1).
#' @param tarifa Tarifa unitària per cas.
#' @param QF Quantitat facturada/realitzada per cas.
#'
#' @return Un numèric amb la despesa meritada.
#' @export
#'
#' @examples
#' protesis_meritada(CS = 100, PM = 3000, tarifa = c(2800, 3200), QF = c(60, 45))
protesis_meritada <- function(CS, PM, tarifa, QF) {
  arguments <- as.list(environment())[c("tarifa", "QF")]

  # Llindars propis de Pròtesis
  llindar_casos    <- 0.10   # % d'excés de casos permès abans de regularitzar
  pct_excés_casos  <- 0.25   # % aplicat al casos que excedeixen el llindar

  # Validació d'inputs
  stopifnot(
    is.numeric(CS), length(CS) == 1, CS >= 0,
    is.numeric(PM), length(PM) == 1, PM >= 0,
    is.numeric(c(tarifa, QF)),
    all(c(tarifa, QF) >= 0, na.rm = TRUE)
  )

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

  # Regularització del nombre de casos (CS): excés per sobre del 10% es paga al 25%
  if (casos_realitzats > CS * (1 + llindar_casos)) {
    casos_regularitzats <- (casos_realitzats - CS * (1 + llindar_casos)) * pct_excés_casos +
      CS * (1 + llindar_casos)
  } else {
    casos_regularitzats <- casos_realitzats
  }

  # Regularització del preu mig (PM): mai es paga per sobre del preu mig contractat
  preu_mig_regularitzat <- if (preu_mig_realitzat > PM) PM else preu_mig_realitzat

  meritada <- round_excel(casos_regularitzats * preu_mig_regularitzat, 2)

  return(meritada)
}
