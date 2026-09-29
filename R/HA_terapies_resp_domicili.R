#' Càlcul de la despesa meritada — Hospitalització d'Aguts: terapies resp domicili
#'
#' @param IC_total Import contractat.
#' @param QF Quantitat/import facturat (realitzat).
#' @param tarifa Tarifa unitària.
#' @param pct_objectius Percentatge d'acompliment d'objectius.
#'
#' @return Despesa meritada calculada.
#' @export
HA_terapies_resp_domicili<- function(IC_total,QF,tarifa,pct_objectius = NULL) {
      # IMPORTANT: IC_total i pct_objectius generals per cada UP, tarifa i QF son diferents per cada concepte de facturació
      
      arguments <- as.list(environment())[c("tarifa", "QF")]
      
      # Llindars i pesos normatius (TRD)
      llindar_base        <- 0.95   # % de la tarifa que es factura per activitat realitzada
      llindar_tram        <- 0.70   # % del llindar del 95% on canvia el tram de l'excés
      pct_tarifa_tram1    <- 0.60   # tarifa aplicada a l'excés fins al 70% del llindar
      pct_tarifa_tram2    <- 0.10   # tarifa aplicada a l'excés per sobre del 70% del llindar
      pct_bonus_objectius <- 0.05   # % màxim de bonus anual per acompliment d'objectius
      
      # Validació d'inputs
      stopifnot(is.numeric(IC_total),
                all(IC_total >= 0, na.rm = TRUE),
                
                is.numeric(pct_objectius),
                all(pct_objectius >= 0 & pct_objectius <= 1, na.rm = TRUE),
                
                is.numeric(c(tarifa, QF)),
                all(c(tarifa, QF) >= 0, na.rm = TRUE))
      
      # IC_total i pct_objectius han de ser constants dintre de cada UP
      IC_total_unic <- unique(IC_total[!is.na(IC_total)])
      pct_objectius_unic <- unique(pct_objectius[!is.na(pct_objectius)])
      
      if (length(IC_total_unic) != 1L) {
        stop("***IC_total ha de tenir un únic valor dintre de cada UP***")}
      if (length(pct_objectius_unic) != 1L) {
        stop("***pct_objectius ha de tenir un únic valor dintre de cada UP***")}
      
      # Convertir els vectors repetits en valors escalars
      IC_total <- IC_total_unic[[1]]
      pct_objectius <- pct_objectius_unic[[1]]
      
      df_meritada <- tryCatch(
        data.frame(arguments),
        error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
      )
      
      tarifa <- vapply(df_meritada$tarifa, num0, numeric(1))
      QF <- vapply(df_meritada$QF, num0, numeric(1))
      
      # Llindar del 95% de l'import contractat
      llindar_95 <- IC_total * llindar_base
      
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
      
      # Bonus anual per acompliment d'objectius
      import_realitzat_brut_95 <- round_excel(sum(QF * tarifa) * llindar_base, 2)
      bonus <- if (import_realitzat_brut_95 > llindar_95) {
        round_excel(IC_total * pct_bonus_objectius * pct_objectius, 2)
      } else {
        round_excel((import_realitzat_brut_95 / llindar_base) * pct_bonus_objectius * pct_objectius, 2)
      }
      
      meritada <- round_excel(meritada + bonus, 2)
      return(meritada)
    }
