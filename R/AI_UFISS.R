#' Càlcul de la despesa meritada — Atenció Intermèdia: UFISS
#'
#' @param IC_geriatria Import contractat.
#' @param IC_pal Import contractat.
#' @param IC_transt_cond Import contractat.
#' @param cpr_geriatria Contraprestació per resultats (proporció entre 0 i 1).
#' @param cpr_mixta Contraprestació per resultats (proporció entre 0 i 1).
#' @param cpr_pal Contraprestació per resultats (proporció entre 0 i 1).
#' @param cpr_transt_cond Contraprestació per resultats (proporció entre 0 i 1).
#'
#' @return Despesa meritada calculada.
#' @export
AI_UFISS<-function(IC_geriatria, #(import contractat)
                            IC_mixta,
                            IC_pal,
                            IC_transt_cond,
                            cpr_geriatria,
                            cpr_mixta,
                            cpr_pal,
                            cpr_transt_cond
                            ){
                            arguments<-as.list(environment())
                            cpr_vars <- unlist(arguments[startsWith(names(arguments), "cpr_")])
                            
                            # llindars de variables fixes
                            pes_fix <- 0.98           # percentatge tarifa part fixa
                            pes_cpr <- 0.02           # pes de contraprestació per resultats
                            
                            # validació d'inputs
                            stopifnot(all(sapply(arguments, is.numeric)),
                                      all(unlist(arguments) >= 0, na.rm = TRUE),
                                      all(unlist(cpr_vars) >= 0 & unlist(cpr_vars) <= 1, na.rm = TRUE))
                            
                            #creem df a partir dels arguments (Permetrà treballar amb vectors)  
                            df_meritada <- tryCatch(data.frame(arguments),
                                                    error=function(e){
                                                      stop('***Revisa que tots els vectors son de la mateixa mida perque no es pot crear un dataframe***') #control 
                                                    }
                            )
                            
                            # funció UFISS
                            funcio_calcul_meritada <- function(IC_geriatria, #(import contractat)
                                                               IC_mixta,
                                                               IC_pal,
                                                               IC_transt_cond,
                                                               cpr_geriatria,
                                                               cpr_mixta,
                                                               cpr_pal,
                                                               cpr_transt_cond) {
                              
                              import_fix <- (IC_geriatria*pes_fix)+(IC_pal*pes_fix)+(IC_transt_cond*pes_fix)+(IC_mixta*pes_fix)
                              import_variable <- (IC_geriatria*pes_cpr*cpr_geriatria)+
                                (IC_pal*pes_cpr*cpr_pal)+
                                (IC_transt_cond*pes_cpr*cpr_transt_cond)+
                                (IC_mixta*pes_cpr*cpr_mixta)
                              
                              meritada<- round_excel(import_fix,2)+ round_excel(import_variable,2)
                              return(meritada)
                            }
                            
                            # apliquem la funció a cada fila del df
                            df_meritada$DM = mapply(funcio_calcul_meritada,
                                                    IC_geriatria, #(import contractat)
                                                    IC_mixta,
                                                    IC_pal,
                                                    IC_transt_cond,
                                                    cpr_geriatria,
                                                    cpr_mixta,
                                                    cpr_pal,
                                                    cpr_transt_cond) 
                            
                            return(df_meritada$DM)
                            
                          }
