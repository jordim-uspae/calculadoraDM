#' Càlcul de la despesa meritada TRD
#'
#' @param import_contracte_total Import total contractat (longitud 1).
#' @param tarifa Tarifa unitària per servei.
#' @param QF Quantitat facturada/realitzada per servei.
#' @param pct_objectius Percentatge d'acompliment d'objectius (opcional,
#'   afegeix un bonus anual si s'informa).
#'
#' @return Un numèric amb la despesa meritada.
#' @export
#'
#' @examples
#' trd_meritada(import_contracte_total = 500000, tarifa = c(50, 80), QF = c(4000, 2000))
trd_meritada <- function(import_contracte_total, tarifa, QF, pct_objectius = NULL) {
  arguments <- as.list(environment())[c("tarifa", "QF")]

  # Llindars i pesos normatius (TRD)
  llindar_base        <- 0.95   # % de la tarifa que es factura per activitat realitzada
  llindar_tram        <- 0.70   # % del llindar del 95% on canvia el tram de l'excés
  pct_tarifa_tram1    <- 0.60   # tarifa aplicada a l'excés fins al 70% del llindar
  pct_tarifa_tram2    <- 0.10   # tarifa aplicada a l'excés per sobre del 70% del llindar
  pct_bonus_objectius <- 0.05   # % màxim de bonus anual per acompliment d'objectius

  # Validació d'inputs
  stopifnot(
    is.numeric(import_contracte_total), length(import_contracte_total) == 1,
    import_contracte_total >= 0,
    is.numeric(c(tarifa, QF)),
    all(c(tarifa, QF) >= 0, na.rm = TRUE)
  )

  df_meritada <- tryCatch(
    data.frame(arguments),
    error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
  )

  tarifa <- vapply(df_meritada$tarifa, num0, numeric(1))
  QF <- vapply(df_meritada$QF, num0, numeric(1))

  # Llindar del 95% de l'import contractat
  llindar_95 <- import_contracte_total * llindar_base

  # Import realitzat, facturat al 95% de la tarifa de cada servei
  import_facturat_95 <- sum(round_excel(QF * tarifa * llindar_base, 2))

  excés_import <- import_facturat_95 - llindar_95
  excés_pct <- if (llindar_95 == 0) 0 else excés_import / llindar_95

  # Import base: el menor entre el realitzat i el llindar del 95%
  import_base <- round_excel(
    if (import_facturat_95 > llindar_95) llindar_95 else import_facturat_95,
    2
  )

  # Tram 1: excés fins al 70% del llindar, facturat al 60%
  import_tram1 <- if (excés_pct > 0) {
    if (excés_pct > llindar_tram) round_excel(llindar_95 * llindar_tram * pct_tarifa_tram1, 2)
    else round_excel(excés_import * pct_tarifa_tram1, 2)
  } else 0

  # Tram 2: excés per sobre del 70% del llindar, facturat al 10%
  import_tram2 <- if (excés_pct > llindar_tram) {
    round_excel((excés_import - llindar_95 * llindar_tram) * pct_tarifa_tram2, 2)
  } else 0

  meritada <- round_excel(import_base + import_tram1 + import_tram2, 2)

  # Bonus anual per acompliment d'objectius (opcional)
  if (!is.null(pct_objectius)) {
    import_realitzat_brut_95 <- round_excel(sum(QF * tarifa) * llindar_base, 2)
    bonus <- if (import_realitzat_brut_95 > llindar_95) {
      round_excel(import_contracte_total * pct_bonus_objectius * pct_objectius, 2)
    } else {
      round_excel((import_realitzat_brut_95 / llindar_base) * pct_bonus_objectius * pct_objectius, 2)
    }
    meritada <- round_excel(meritada + bonus, 2)
  }

  return(meritada)
}
