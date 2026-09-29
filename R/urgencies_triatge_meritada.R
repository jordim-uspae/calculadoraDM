#' Càlcul de la despesa meritada per urgències (triatge)
#'
#' @param QC Quantitat contractada.
#' @param tarifa Tarifa unitària.
#' @param QF Quantitat facturada/realitzada.
#'
#' @return Vector numèric amb la despesa meritada.
#' @export
#'
#' @examples
#' urgencies_triatge_meritada(QC = 100, tarifa = 50, QF = 130)
urgencies_triatge_meritada <- function(QC, tarifa, QF) {
  arguments <- as.list(environment())

  llindar_minim_CF        <- 0.80
  pct_pagament_contractat <- 0.70
  pct_pagament_realitzat  <- 0.30
  tall_trams       <- 0.25
  pct_tarifa_tram1 <- 1.00
  pct_tarifa_tram2 <- 0.10

  stopifnot(
    is.numeric(c(QC, tarifa, QF)),
    all(c(QC, tarifa, QF) >= 0, na.rm = TRUE)
  )

  df_meritada <- tryCatch(
    data.frame(arguments),
    error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
  )

  funcio_calcul_meritada <- function(QC, tarifa, QF) {
    QC <- num0(QC); tarifa <- num0(tarifa); QF <- num0(QF)

    import <- QC * tarifa
    part_fixa <- round_excel(import * pct_pagament_contractat, 2)

    if (QF > QC) {
      if ((QF - QC) > (tall_trams * QC)) {
        base <- QC * tarifa * pct_pagament_realitzat +
          QC * tall_trams * tarifa * pct_tarifa_tram1 +
          (QF - QC * (1 + tall_trams)) * tarifa * pct_tarifa_tram2
      } else {
        base <- QC * tarifa * pct_pagament_realitzat +
          (QF - QC) * tarifa * pct_tarifa_tram1
      }
      base <- round_excel(base, 2)
      meritada <- round_excel(base + part_fixa, 2)
    } else if (QF < QC * llindar_minim_CF) {
      meritada <- round_excel(tarifa * QF, 2)
    } else {
      base <- round_excel(QF * tarifa * pct_pagament_realitzat, 2)
      meritada <- round_excel(base + part_fixa, 2)
    }

    meritada
  }

  df_meritada$meritada <- mapply(funcio_calcul_meritada, QC, tarifa, QF)

  return(df_meritada$meritada)
}
