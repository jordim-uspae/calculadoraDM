#' Càlcul de la despesa meritada per hospitalització (AH)
#'
#' @param QC Quantitat contractada.
#' @param tarifa Tarifa unitària.
#' @param QF Quantitat facturada/realitzada.
#' @param reingressos Nombre de reingressos.
#'
#' @return Vector numèric amb la despesa meritada.
#' @export
#'
#' @examples
#' hospi(QC = 100, tarifa = 500, QF = 110, reingressos = 2)
hospi <- function(QC, tarifa, QF, reingressos) {
  arguments <- as.list(environment())

  # Llindars i pesos normatius
  llindar_minim_CF        <- 0.80
  pct_pagament_contractat <- 0.70
  pct_pagament_realitzat  <- 0.30
  tall_trams        <- 0.10
  pct_tarifa_tram1  <- 0.30
  pct_tarifa_tram2  <- 0.05

  stopifnot(
    is.numeric(c(QC, tarifa, QF, reingressos)),
    all(c(QC, tarifa, QF, reingressos) >= 0, na.rm = TRUE)
  )

  df_meritada <- tryCatch(
    data.frame(arguments),
    error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
  )

  funcio_calcul_meritada <- function(QC, tarifa, QF, reingressos) {
    QC <- num0(QC); tarifa <- num0(tarifa); QF <- num0(QF); reingressos <- num0(reingressos)

    import <- round_excel(QC * tarifa, 2)
    marginalitat <- calc_u_marginalitat(QC, QF)
    part_fixa <- round_excel(import * pct_pagament_contractat, 2)
    penalitzacio <- round_excel(reingressos * tarifa * pct_pagament_contractat, 2)

    if ((QF + reingressos) < (QC * llindar_minim_CF)) {
      meritada <- QF * tarifa
    } else if (is.na(marginalitat) || marginalitat == 0) {
      part_var <- QF * round_excel(tarifa * pct_pagament_realitzat, 2)
      meritada <- round_excel(part_var + part_fixa - penalitzacio, 2)
    } else if (marginalitat <= tall_trams) {
      part_var <- QC * round_excel(tarifa * pct_pagament_realitzat, 2) +
        (QF - QC) * round_excel(tarifa * pct_tarifa_tram1, 2)
      meritada <- round_excel(part_var + part_fixa - penalitzacio, 2)
    } else {
      part_var <- QC * round_excel(tarifa * pct_pagament_realitzat, 2) +
        QC * tall_trams * round_excel(tarifa * pct_tarifa_tram1, 2) +
        (QF - QC * (1 + tall_trams)) * round_excel(tarifa * pct_tarifa_tram2, 2)
      meritada <- round_excel(part_var + part_fixa - penalitzacio, 2)
    }

    meritada
  }

  df_meritada$meritada <- mapply(funcio_calcul_meritada, QC, tarifa, QF, reingressos)

  return(df_meritada$meritada)
}
