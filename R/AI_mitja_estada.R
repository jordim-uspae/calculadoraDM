#' Càlcul de la despesa meritada — Atenció Intermèdia: mitja estada
#'
#' @param QC_C Quantitat/import contractat.
#' @param QC_CP Quantitat/import contractat.
#' @param QF_C Quantitat/import facturat (realitzat).
#' @param QF_C_sida Quantitat/import facturat (realitzat).
#' @param QF_CP Quantitat/import facturat (realitzat).
#' @param tarifa_C Tarifa unitària.
#' @param tarifa_C_GENERAL Tarifa unitària.
#' @param tarifa_C_sida Tarifa unitària.
#' @param tarifa_CP Tarifa unitària.
#' @param cpr_CP Contraprestació per resultats (proporció entre 0 i 1).
#' @param cpr_C Contraprestació per resultats (proporció entre 0 i 1).
#' @param pct_prog_CP Percentatge del programa d'implantació.
#' @param ambit Àmbit pel qual es vol calcular la despesa meritada.
#'
#' @return Despesa meritada calculada.
#' @export
AI_mitja_estada<- function( 
                           QC_C,    # contracte convalescencia  
                           QC_CP,   # contracte cures pal
                           QF_C,    # facturació convalescencia
                           QF_C_sida,
                           QF_CP,   # facturació cures pal
                           tarifa_C,
                           tarifa_C_GENERAL,# ????? general per dir-li algo - preu a la fila de pagament variable (?) :(
                           tarifa_C_sida,
                           tarifa_CP,
                           cpr_CP,
                           cpr_C,
                           pct_prog_CP,
                           ambit) { # ambit pel que es vol calcular la DM: conva = "C", cures pal = "CP", tot mitja estada = "ME"
  
                          arguments<-as.list(environment())
                          
                          # llindars de variables fixes
                          llindar_minim_CF <- 0.80  # per sota de quin punt de la relació QC i QF es paga només activitat realitzada?
                          pes_fix <- 0.70           # percentatge tarifa part fixa
                          pes_variable <- 0.30      # percentatge tarifa part variable
                          pes_cpr <- 0.03           # pes de contraprestació per resultats
                          pes_programa_CP <- 0.02   # pes del programa d'implantació del nou SP CP
                          
                          # validació d'inputs
                          stopifnot(all(sapply(arguments[names(arguments) != "ambit"], is.numeric)),
                                    all(unlist(arguments[names(arguments) != "ambit"]) >= 0, na.rm = TRUE),
                                    is.character(ambit),
                                    all(cpr_CP >= 0 & cpr_CP <= 1, na.rm = TRUE),
                                    all(cpr_C >= 0 & cpr_C <= 1, na.rm = TRUE),
                                    all(pct_prog_CP >= 0 & pct_prog_CP <= 1, na.rm = TRUE))
                          
                          #creem df a partir dels arguments (Permetrà treballar amb vectors)  
                          df_meritada <- tryCatch(data.frame(arguments),
                                                  error=function(e){
                                                    stop('***Revisa que tots els vectors son de la mateixa mida perque no es pot crear un dataframe***') #control 
                                                  }
                          )
                          
                          # funció ME
                          funcio_calcul_meritada <- function( QC_C,    # contracte convalescencia  
                                                              QC_CP,   # contracte cures pal
                                                              QF_C,    # facturació convalescencia
                                                              QF_C_sida,
                                                              QF_CP,   # facturació cures pal
                                                              tarifa_C,
                                                              tarifa_C_GENERAL,# ????? general per dir-li algo - preu a la fila de pagament variable (?) :(
                                                              tarifa_C_sida,
                                                              tarifa_CP,
                                                              cpr_CP,
                                                              cpr_C,
                                                              pct_prog_CP,
                                                              ambit) {
                            
                            # Imports utilitzats per determinar el cas
                            import_facturat_comparable <-
                              (QF_CP * tarifa_CP) +
                              (QF_C + QF_C_sida) * tarifa_C_GENERAL
                            
                            import_contractat_comparable <-
                              (QC_CP * tarifa_CP) +
                              (QC_C * tarifa_C_GENERAL)
                            
                            # Denominador comú dels percentatges CP i C
                            import_total_facturat <-
                              (QF_CP * tarifa_CP) +
                              (QF_C * tarifa_C) +
                              (QF_C_sida * tarifa_C_sida)
                            
                            # import del programa vinculat a la implantació del nou sistema de pagament de CP
                            tarifa_prog_CP <- QC_CP*tarifa_CP*pes_programa_CP
                            
                            if (import_facturat_comparable < import_contractat_comparable * llindar_minim_CF) {
                              
                              ## Cas 1: activitat inferior al 80% ##
                              #@sm:llavors es paga l'import total facturat, i l'import fix =0
                              
                              # CP 
                              import_variable_CP <- (QF_CP * tarifa_CP)  
                              pct_CP <- (QF_CP * tarifa_CP)/((QF_CP * tarifa_CP)+(QF_C * tarifa_C) + (QF_C_sida * tarifa_C_sida)) # ratio facturat CP/total facturat(cp+c)
                              import_cpr_CP <- ((QC_CP*tarifa_CP)+(QC_C * tarifa_C))*pes_cpr*cpr_CP*pct_CP
                              import_prog_CP <- pct_prog_CP*tarifa_prog_CP  # això se suposa que pot incrementar la cpr fins al 5%, pero son dos parametres més i ja ?
                              
                              meritada_CP <- round_excel(import_variable_CP, 2) + round_excel(import_cpr_CP, 2) + round_excel(import_prog_CP, 2)
                              
                              # CONVA
                              import_variable_C <- (QF_C * tarifa_C) + (QF_C_sida * tarifa_C_sida)
                              pct_C <- ((QF_C * tarifa_C) + (QF_C_sida * tarifa_C_sida))/((QF_CP * tarifa_CP)+(QF_C * tarifa_C) + (QF_C_sida * tarifa_C_sida))
                              import_cpr_C <- ((QC_CP*tarifa_CP)+(QC_C*tarifa_C))*pes_cpr*cpr_C*pct_C
                              
                              meritada_C <- round_excel(import_variable_C, 2) + round_excel(import_cpr_C, 2)
                              
                              meritada <- meritada_CP + meritada_C
                              
                            } else if (import_facturat_comparable < import_contractat_comparable) {
                              ## Cas 2: activitat entre 80% i 100% ##
                              
                              # CP 
                              import_fix_CP <- (QC_CP * tarifa_CP * pes_fix)
                              import_variable_CP <- (QF_CP*tarifa_CP*pes_variable)
                              pct_CP <- (QF_CP * tarifa_CP)/((QF_CP * tarifa_CP)+(QF_C * tarifa_C) + (QF_C_sida * tarifa_C_sida))
                              import_cpr_CP <- ((QC_CP*tarifa_CP)+(QC_C * tarifa_C))*pes_cpr*cpr_CP*pct_CP
                              import_prog_CP <- pct_prog_CP*tarifa_prog_CP  # això se suposa que pot incrementar la cpr fins al 5%, pero son dos parametres més i ja ?
                              
                              meritada_CP <- round_excel(import_fix_CP,2) + round_excel(import_variable_CP,2) + round_excel(import_cpr_CP,2) + round_excel(import_prog_CP,2)
                              
                              # CONVA
                              import_fix_C <- (QC_C* tarifa_C_GENERAL * pes_fix)
                              import_variable_C <- if ((QF_C + QF_C_sida) < QC_C) {
                                
                                (QF_C * tarifa_C * pes_variable) +
                                  (QF_C * (tarifa_C - tarifa_C_GENERAL) * pes_fix) +
                                  (QF_C_sida * tarifa_C_sida * pes_variable) +
                                  (QF_C_sida * (tarifa_C_sida - tarifa_C_GENERAL) * pes_fix)
                                
                              } else {
                                (QF_C * tarifa_C * pes_variable) +
                                  ((QC_C - QF_C_sida) * (tarifa_C - tarifa_C_GENERAL) * pes_fix) +
                                  (QF_C_sida * tarifa_C_sida * pes_variable) +
                                  (QF_C_sida * (tarifa_C_sida - tarifa_C_GENERAL) * pes_fix)
                              }
                              
                              pct_C <- ((QF_C * tarifa_C) + (QF_C_sida * tarifa_C_sida))/((QF_CP * tarifa_CP)+(QF_C * tarifa_C) + (QF_C_sida * tarifa_C_sida))
                              import_cpr_C <- ((QC_CP*tarifa_CP)+(QC_C*tarifa_C))*pes_cpr*cpr_C*pct_C
                              
                              meritada_C <- round_excel(import_fix_C,2) + round_excel(import_variable_C,2) + round_excel(import_cpr_C,2)
                              
                              meritada <- meritada_CP + meritada_C
                              
                              
                            } else {
                              ## Cas 3: activitat superior al 100% ##
                              
                              # CP
                              import_fix_CP <- (QC_CP * tarifa_CP * pes_fix)
                              import_variable_CP <- (QC_CP*tarifa_CP* pes_variable)
                              pct_CP <- (QF_CP * tarifa_CP)/((QF_CP * tarifa_CP)+(QF_C * tarifa_C) + (QF_C_sida * tarifa_C_sida))
                              import_cpr_CP <- ((QC_CP*tarifa_CP)+(QC_C * tarifa_C))*pes_cpr*cpr_CP*pct_CP 
                              import_prog_CP <- pct_prog_CP*tarifa_prog_CP  # això se suposa que pot incrementar la cpr fins al 5%, pero son dos parametres més i ja ?
                              
                              meritada_CP <- round_excel(import_fix_CP,2) + round_excel(import_variable_CP,2) + round_excel(import_cpr_CP,2) + round_excel(import_prog_CP,2)
                              
                              # CONVA
                              import_fix_C <- (QC_C* tarifa_C_GENERAL * pes_fix)
                              import_variable_C <- if ((QF_C + QF_C_sida) < QC_C) {
                                ((QC_C - QF_C_sida) * tarifa_C_GENERAL * pes_variable) +
                                  (QF_C * (tarifa_C - tarifa_C_GENERAL)) +
                                  (QF_C_sida * tarifa_C_sida * pes_variable) +
                                  (QF_C_sida * (tarifa_C_sida - tarifa_C_GENERAL) * pes_fix)
                                
                              } else {
                                ((QC_C - QF_C_sida) * tarifa_C * pes_variable) +
                                  ((QC_C - QF_C_sida) * (tarifa_C - tarifa_C_GENERAL) * pes_fix) +
                                  (QF_C_sida * tarifa_C_sida * pes_variable) +
                                  (QF_C_sida * (tarifa_C_sida - tarifa_C_GENERAL) * pes_fix)
                              }
                              
                              pct_C <- ((QF_C * tarifa_C) + (QF_C_sida * tarifa_C_sida))/((QF_CP * tarifa_CP)+(QF_C * tarifa_C) + (QF_C_sida * tarifa_C_sida))
                              import_cpr_C <- ((QC_CP*tarifa_CP)+(QC_C*tarifa_C))*pes_cpr*cpr_C*pct_C
                              meritada_C <- round_excel(import_fix_C,2) + round_excel(import_variable_C,2) + round_excel(import_cpr_C,2)
                              
                              meritada <- meritada_CP + meritada_C
                            }
                            
                            list_meritada <- list("meritada_CP" = meritada_CP,
                                                  "meritada_C" =meritada_C,
                                                  "meritada" =meritada)
                            
                            return(list_meritada)
                          }
                          
                          
                          # aplicar la funció a cada fila del df
                          # (aquí pas intermedi perque la funció té més d'un output)
                          resultats <- mapply(funcio_calcul_meritada,
                                              QC_C,    # contracte convalescencia  
                                              QC_CP,   # contracte cures pal
                                              QF_C,    # facturació convalescencia
                                              QF_C_sida,
                                              QF_CP,   # facturació cures pal
                                              tarifa_C,
                                              tarifa_C_GENERAL,# ????? general per dir-li algo - preu a la fila de pagament variable (?) :(
                                              tarifa_C_sida,
                                              tarifa_CP,
                                              cpr_CP,
                                              cpr_C,
                                              pct_prog_CP,
                                              ambit, 
                                              SIMPLIFY = FALSE)
                          
                          res_df <- dplyr::bind_rows(resultats)
                          df_meritada <- dplyr::bind_cols(df_meritada, res_df)
                          
                          # que imprimim? depen de l'ambit seleccionat al principi
                          df_meritada <- df_meritada |> 
                            dplyr::mutate(DM = dplyr::case_when(ambit == "C" ~ meritada_C,
                                                  ambit == "CP" ~ meritada_CP,
                                                  ambit == "ME" ~ meritada))
                          return(df_meritada$DM)    }
