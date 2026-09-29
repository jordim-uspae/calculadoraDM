#' Càlcul de la despesa meritada per TTPE
#'
#' Aplica la taula de percentatges per `codi_activitat` (extreta de
#' "Tarifes Aguts") a cada cas.
#'
#' @param QC Quantitat contractada.
#' @param tarifa Tarifa unitària.
#' @param QF Quantitat facturada/realitzada.
#' @param codi_activitat Codi d'activitat (character), ha d'existir a la
#'   taula interna de percentatges.
#'
#' @return Vector numèric amb la despesa meritada.
#' @export
#'
#' @examples
#' ttpe_meritada(QC = 100, tarifa = 400, QF = 110, codi_activitat = "143205")
ttpe_meritada <- function(QC, tarifa, QF, codi_activitat) {
  arguments <- as.list(environment())

  # Taula de percentatges per codi_activitat (extreta de 'Tarifes Aguts', files 133-203)
  taula_percentatges <- data.frame(
    codi_activitat = c(
      "143205", "143400", "143450", "143705", "143710", "143715", "143720", "143725",
      "143730", "143800", "144210", "144220", "144230", "145001", "145002", "145003",
      "149800", "149810", "149820", "149830", "149840", "149701", "149702", "149703",
      "149711", "EO", "149704", "149705", "149706", "149707", "149708", "149709",
      "149710", "147000", "146000", "149300", "149200", "149100", "149150", "149350",
      "149700", "149450", "149601", "149602", "149603", "179012", "144240", "149604",
      "146100", "146101", "146110", "146111", "146112", "146113", "146120", "147100",
      "142002", "179552", "149451"
    ),
    tall_trams = c(
      0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
      0.05, NA, NA, 0.05, 0.05, 0.05, 0.05, 0.05, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
      NA, 0.05, 0.05, 0.05, NA, NA, 0.05, 0.05, 0.05, 0.05, NA, NA, 0.1, NA, 0.05
    ),
    pct_tarifa_tram1 = c(
      0.35, 0.35, 0.35, 0.10, 0.10, 0.10, 0.10, 0.50, 0.35, 0.35, 0.35, 0.35, 0.35,
      0.35, 1, 1, 0.35, 0.35, 0.35, 0.35, 0.35, 1, 1, 1, 1, 1, NA, NA, NA, NA, NA,
      1, 1, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.35, 0.75, 0.75, 0,
      0.80, 0.35, 0.75, 1, 1, 0.35, 0.35, 0.35, 0.35, 0.80, 0, 0.09, 0, 0.35
    ),
    pct_tarifa_tram2 = c(
      0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
      0.01, NA, NA, 0.01, 0.01, 0.01, 0.01, 0.01, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
      NA, 0.01, 0.01, 0.01, NA, NA, 0.01, 0.01, 0.01, 0.01, NA, NA, 0.01, NA, 0.01
    ),
    stringsAsFactors = FALSE
  )

  # Validació d'inputs
  stopifnot(
    is.numeric(c(QC, tarifa, QF)),
    all(c(QC, tarifa, QF) >= 0, na.rm = TRUE)
  )

  df_meritada <- tryCatch(
    data.frame(arguments),
    error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
  )

  funcio_calcul_meritada <- function(QC, tarifa, QF, codi_activitat) {
    QC <- num0(QC); tarifa <- num0(tarifa); QF <- num0(QF)

    percentatges <- taula_percentatges[taula_percentatges$codi_activitat == codi_activitat, ]
    if (nrow(percentatges) == 0) {
      stop(paste0('***No s\'ha trobat el codi_activitat "', codi_activitat, '" a la taula de percentatges***'))
    }
    tall_trams       <- percentatges$tall_trams[1]
    pct_tarifa_tram1 <- percentatges$pct_tarifa_tram1[1]
    pct_tarifa_tram2 <- percentatges$pct_tarifa_tram2[1]

    import <- QC * tarifa

    if (QF <= QC) {
      # Fins a l'activitat contractada -> tarifa plena, sense mecanisme 70/30
      meritada <- QF * tarifa

    } else if (is.na(tall_trams) || tall_trams == 0) {
      # Tècniques amb un sol tram -> el mateix % per a tot l'excés
      meritada <- import + (QF - QC) * tarifa * pct_tarifa_tram1

    } else if ((QF - QC) > (tall_trams * QC)) {
      # Excés per sobre del llindar propi de la tècnica -> dos trams
      meritada <- import +
        (QC * tall_trams) * tarifa * pct_tarifa_tram1 +
        (QF - QC * (1 + tall_trams)) * tarifa * pct_tarifa_tram2

    } else {
      # Excés dins del llindar propi de la tècnica -> un sol tram
      meritada <- import + (QF - QC) * tarifa * pct_tarifa_tram1
    }

    round_excel(meritada, 2)
  }

  df_meritada$meritada <- mapply(
    funcio_calcul_meritada,
    QC, tarifa, QF, codi_activitat
  )

  return(df_meritada$meritada)
}
