#' Càlcul de la despesa meritada per CMA complexa
#'
#' @param QC Quantitat contractada.
#' @param tarifa Tarifa unitària.
#' @param QF Quantitat facturada/realitzada.
#'
#' @return Un numèric amb la despesa meritada agregada.
#' @export
#'
#' @examples
#' cma_complexa_meritada(QC = c(50, 30), tarifa = c(800, 600), QF = c(55, 28))
cma_complexa_meritada <- function(QC, tarifa, QF) {
  arguments <- as.list(environment())
  # Llindars i pesos normatius
  llindar_minim_CF        <- 0.80
  pct_pagament_contractat <- 0.70
  pct_pagament_realitzat  <- 0.30

  # Llindars propis de la CMA complexa (files 60-62)
  tall_trams       <- 0.10
  pct_tarifa_tram1 <- 0.09
  pct_tarifa_tram2 <- 0.01

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

  # Agregació de les modalitats (equivalent a F59 i SUM(V60:V62))
  F_total <- sum(QC * tarifa)   # pressupost total contractat
  V_total <- sum(QF * tarifa)   # pressupost total realitzat

  part_fixa <- round_excel(F_total * pct_pagament_contractat, 2)

  if (V_total > F_total) {

    if ((V_total - F_total) > (tall_trams * F_total)) {
      base <- F_total * pct_pagament_realitzat +
        F_total * tall_trams * pct_tarifa_tram1 +
        (V_total - F_total * (1 + tall_trams)) * pct_tarifa_tram2
    } else {
      base <- F_total * pct_pagament_realitzat +
        (V_total - F_total) * pct_tarifa_tram1
    }
    base <- round_excel(base, 2)
    meritada <- round_excel(base + part_fixa, 2)

  } else if (V_total < F_total * llindar_minim_CF) {

    meritada <- round_excel(V_total, 2)

  } else {

    base <- round_excel(V_total * pct_pagament_realitzat, 2)
    meritada <- round_excel(base + part_fixa, 2)
  }

  return(meritada)
}
