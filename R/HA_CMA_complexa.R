#' Càlcul de la despesa meritada — Hospitalització d'Aguts: CMA complexa
#'
#' @param QC Quantitat/import contractat.
#' @param QF Quantitat/import facturat (realitzat).
#' @param tarifa Tarifa unitària.
#'
#' @return Despesa meritada calculada.
#' @export
HA_CMA_complexa<- function(QC,QF,tarifa) {
  # IMPORTANT: group_by(UP) i incloure CMA complexitat baixa, mitja i alta per cada centre
  # si no s'inclouen els 3 el calcul no és correcte !!!
  
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
          C_total <- sum(QC * tarifa)   # pressupost total contractat
          F_total <- sum(QF * tarifa)   # pressupost total realitzat
          
          part_fixa <- round_excel(C_total * pct_pagament_contractat, 2)
          
          if (F_total > C_total) {
            
            if ((F_total - C_total) > (tall_trams * C_total)) {
              base <- C_total * pct_pagament_realitzat +
                C_total * tall_trams * pct_tarifa_tram1 +
                (F_total - C_total * (1 + tall_trams)) * pct_tarifa_tram2
            } else {
              base <- C_total * pct_pagament_realitzat +
                (F_total - C_total) * pct_tarifa_tram1
            }
            base <- round_excel(base, 2)
            meritada <- round_excel(base + part_fixa, 2)
            
          } else if (F_total < C_total * llindar_minim_CF) {
            
            meritada <- round_excel(F_total, 2)
            
          } else {
            
            base <- round_excel(F_total * pct_pagament_realitzat, 2)
            meritada <- round_excel(base + part_fixa, 2)
          }
          
          return(meritada)
}
