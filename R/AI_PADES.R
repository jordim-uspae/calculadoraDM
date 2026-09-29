#' Càlcul de la despesa meritada — Atenció Intermèdia: PADES
#'
#' @param IC_PADES Import contractat.
#' @param cpr Contraprestació per resultats (proporció entre 0 i 1).
#' @param ampliacio_PADES Paràmetre `ampliacio_PADES`.
#'
#' @return Despesa meritada calculada.
#' @export
AI_PADES<-function(IC_PADES,
                   cpr,
                   ampliacio_PADES
                  ){
  
                    arguments<-as.list(environment())
                    
                    # llindars de variables fixes
                    pes_fix <- 0.98           # percentatge tarifa part fixa
                    pes_cpr <- 0.02           # pes de contraprestació per resultats
                    
                    # validació d'inputs
                    stopifnot(all(sapply(arguments, is.numeric)),
                              all(unlist(arguments) >= 0, na.rm = TRUE),
                              all(cpr >= 0 & cpr <= 1, na.rm = TRUE))
                    
                    #creem df a partir dels arguments (Permetrà treballar amb vectors)  
                    df_meritada <- tryCatch(data.frame(arguments),
                                            error=function(e){
                                              stop('***Revisa que tots els vectors son de la mateixa mida perque no es pot crear un dataframe***') #control 
                                            }
                    )
                    
                    # funció PADES
                    funcio_calcul_meritada <- function(IC_PADES,
                                                       cpr,
                                                       ampliacio_PADES) {
                      
                      PADES_import_fix<-IC_PADES*pes_fix
                      PADES_cpr<-(IC_PADES*pes_cpr)*cpr
                      
                      PADES<-round_excel(PADES_import_fix,2)+round_excel(PADES_cpr,2)
                      
                      meritada <- PADES+ampliacio_PADES
                      
                      return(meritada)
                    }
                    # apliquem la funció a cada fila del df
                    df_meritada$DM = mapply(funcio_calcul_meritada,
                                            IC_PADES,
                                            cpr,
                                            ampliacio_PADES) 
                    
                    return(df_meritada$DM)
                  }
