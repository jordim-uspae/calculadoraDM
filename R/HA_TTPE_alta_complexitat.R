#' Càlcul de la despesa meritada — Hospitalització d'Aguts: TTPE alta complexitat
#'
#' @param QC Quantitat/import contractat.
#' @param QF Quantitat/import facturat (realitzat).
#' @param tarifa_base Tarifa base.
#' @param tarifa_plus Tarifa addicional.
#' @param codi_activitat Codi d'activitat.
#'
#' @return Despesa meritada calculada.
#' @export
HA_TTPE_alta_complexitat<- function(QC, QF,tarifa_base, tarifa_plus,codi_activitat) {
  # IMPORTANT! Aquelles activitats de compra que presentin diferent tarifa a partir d'un cert nombre d'activitat presenten dues tarifes
  # tarifa_base per el tram inicial i tarifa plus per tota l'activitat a partir del tall. 
  # Les activitats que no tenen aqeusta particularitat presenten tarifa_plus = 0
  
  arguments <- as.list(environment())
  
  # Taula de percentatges per codi_activitat (extreta de 'Tarifes Aguts', files 262-343)
  taula_percentatges <- data.frame(
    codi_activitat = c(
      "143220", "143225", "143230", "143235", "143355", "143360",
      "143365", "143300", "143211", "143212", "143213", "143215",
      "145004", "145005", "145006", "145007", "143130", "143140",
      "143150", "143170", "143110", "143120", "143160",
      "143605", "143610", "143505", "143510",
      "143515", "143535", "143540", "143545", "143520",
      "143525", "143530", "148100", "148105", "145500", "145510",
      "145515", "149900", "149910", "149915", "149920", "149925",
      "149930", "149935", "149940", "170520", "170525", "170530",
      "170536", "170537", "170540", "170515", "170545", "170510",
      "170538", "143900", "143905", "143902", "143903", "143904",
      "147001", "147002", "147011", "147012", "147010", "147020",
      "147021"
    ),
    tall_trams = c(
      0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
      0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.2,
      0.2, 0.2, 0.2, 0.1, 0.1, 0.1, 0.1,
      0.1, 0.25, 0.25, 0.2, 0.1, 0.1, 0.1, 0.2, 0.2,
      NA, 0.1, 0.1, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA
    ),
    pct_tarifa_tram1 = c(
      0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35,
      0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.75, 0.75, 0.75, 0.75,
      0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35,
      0.35, 1, 1, 0.8, 0.35, 0.35, 0.35, 1, 1,
      0.5, 1, 0.35, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8,
      0.8, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 0.8, 0.8, 0.8, 0.8, 0.8, 1, 1, 1.0, 1.0,
      0.8, 1.0, 1.0
    ),
    pct_tarifa_tram2 = c(
      0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
      0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.5, 0.5, 0.5, 0.5,
      0.25, 0.25, 0.25, 0.01, 0.01, 0.01, 0.01,
      0.01, 0.75, 0.75, 0.5, 0.01, 0.01, 0.01, 0.5, 0.5,
      NA, 0.01, 0.01, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA
    ),
    grup_complexitat = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE
    ),
    stringsAsFactors = FALSE
  )
  
  # Taula de les 4 parelles amb "+" (cap = llindar on comença el tram plus)
  taula_parelles <- data.frame(
    codi_activitat_base = c("143110", "143505", "143510", "143515"),
    cap                  = c(500, 600, 150, 600),
    tall_trams           = c(0.2, 0.1, 0.1, 0.1),
    pct_tarifa_tram1     = c(0.35, 0.35, 0.35, 0.35),
    pct_tarifa_tram2     = c(0.25, 0.01, 0.01, 0.01),
    stringsAsFactors = FALSE
  )
  
  # Validació d'inputs
  stopifnot(
    is.numeric(c(QC, tarifa_base, QF)),
    all(c(QC, tarifa_base, QF) >= 0, na.rm = TRUE)
  )
  
  df_meritada <- tryCatch(
    data.frame(arguments),
    error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
  )
  
  funcio_calcul_meritada <- function(QC, QF,tarifa_base, tarifa_plus,codi_activitat) {
    QC <- num0(QC); tarifa_base <- num0(tarifa_base); QF <- num0(QF)
    
    # És una de les 4 parelles especials? -> lògica combinada
    if (codi_activitat %in% taula_parelles$codi_activitat_base) {
      
      tarifa_plus <- num0(tarifa_plus)
      parella <- taula_parelles[taula_parelles$codi_activitat_base == codi_activitat, ]
      cap              <- parella$cap[1]
      tall_trams       <- parella$tall_trams[1]
      pct_tarifa_tram1 <- parella$pct_tarifa_tram1[1]
      pct_tarifa_tram2 <- parella$pct_tarifa_tram2[1]
      
      QC_base <- min(QC, cap); QC_plus <- max(QC - cap, 0)
      QF_base <- min(QF, cap); QF_plus <- max(QF - cap, 0)
      
      import_base <- QC_base * tarifa_base
      if (QF_base <= QC_base) {
        meritada_base <- QF_base * tarifa_base
      } else {
        excés_base <- QF_base - QC_base
        if (excés_base > tall_trams * QC_base) {
          meritada_base <- import_base +
            QC_base * tall_trams * tarifa_base * pct_tarifa_tram1 +
            (QF_base - QC_base * (1 + tall_trams)) * tarifa_base * pct_tarifa_tram2
        } else {
          meritada_base <- import_base + excés_base * tarifa_base * pct_tarifa_tram1
        }
      }
      meritada_base <- round_excel(meritada_base, 2)
      
      import_plus <- QC_plus * tarifa_plus
      if (QF_plus <= QC_plus) {
        meritada_plus <- QF_plus * tarifa_plus
      } else {
        excés_combinat   <- QF - QC
        llindar_combinat <- QC * tall_trams
        if (excés_combinat > llindar_combinat) {
          meritada_plus <- import_plus +
            QC * tall_trams * tarifa_plus * pct_tarifa_tram1 +
            (excés_combinat - llindar_combinat) * tarifa_plus * pct_tarifa_tram2
        } else {
          meritada_plus <- import_plus + (QF_plus - QC_plus) * tarifa_plus * pct_tarifa_tram1
        }
      }
      meritada_plus <- round_excel(meritada_plus, 2)
      
      return(round_excel(meritada_base + meritada_plus, 2))
    }
    
    # Codi normal -> lògica estàndard (tarifa_plus s'ignora)
    percentatges <- taula_percentatges[taula_percentatges$codi_activitat == codi_activitat, ]
    if (nrow(percentatges) == 0) {
      stop(paste0('***No s\'ha trobat el codi_activitat "', codi_activitat, '" a cap taula***'))
    }
    tall_trams        <- percentatges$tall_trams[1]
    pct_tarifa_tram1  <- percentatges$pct_tarifa_tram1[1]
    pct_tarifa_tram2  <- percentatges$pct_tarifa_tram2[1]
    grup_complexitat  <- percentatges$grup_complexitat[1]
    
    import <- QC * tarifa_base
    
    if (QF <= QC) {
      if (grup_complexitat) {
        meritada <- (import * 0.25) + (QF * tarifa_base * 0.75)
      } else {
        meritada <- QF * tarifa_base
      }
    } else if (is.na(tall_trams) || tall_trams == 0) {
      meritada <- import + (QF - QC) * tarifa_base * pct_tarifa_tram1
    } else if ((QF - QC) > (tall_trams * QC)) {
      meritada <- import +
        (QC * tall_trams) * tarifa_base * pct_tarifa_tram1 +
        (QF - QC * (1 + tall_trams)) * tarifa_base * pct_tarifa_tram2
    } else {
      meritada <- import + (QF - QC) * tarifa_base * pct_tarifa_tram1
    }
    
    round_excel(meritada, 2)
  }
  
  df_meritada$meritada <- mapply(
    funcio_calcul_meritada,
    QC, QF,tarifa_base, tarifa_plus,codi_activitat
  )
  
  return(df_meritada$meritada)
}
