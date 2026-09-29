#' Càlcul de la despesa meritada — Atenció Intermèdia: PIUC
#'
#' @param QC_AC_D Quantitat/import contractat.
#' @param QC_AC_G Quantitat/import contractat.
#' @param QC_MC_D Quantitat/import contractat.
#' @param QC_MC_G Quantitat/import contractat.
#' @param QC_MF_D Quantitat/import contractat.
#' @param QC_MF_G Quantitat/import contractat.
#' @param QF_AC_D Quantitat/import facturat (realitzat).
#' @param QF_AC_G Quantitat/import facturat (realitzat).
#' @param QF_MC_D Quantitat/import facturat (realitzat).
#' @param QF_MC_G Quantitat/import facturat (realitzat).
#' @param QF_MF_D Quantitat/import facturat (realitzat).
#' @param QF_MF_G Quantitat/import facturat (realitzat).
#' @param tarifa_AC_D Tarifa unitària.
#' @param tarifa_AC_G Tarifa unitària.
#' @param tarifa_MC_D Tarifa unitària.
#' @param tarifa_MC_G Tarifa unitària.
#' @param tarifa_MF_D Tarifa unitària.
#' @param tarifa_MF_G Tarifa unitària.
#' @param reforç_PADES Paràmetre `reforç_PADES`.
#'
#' @return Despesa meritada calculada.
#' @export
AI_PIUC<-function(
                QC_AC_D,              
                QC_AC_G,#Altes Alta complexitat (G=gener-març, D=Desembre)
                QC_MC_D,
                QC_MC_G,#Altes mitja complexitat (G=gener-març, D=Desembre)
                QC_MF_D,
                QC_MF_G,#Altes Malalts fràgils (G=gener-març, D=Desembre)
                QF_AC_D,
                QF_AC_G,
                QF_MC_D,
                QF_MC_G,
                QF_MF_D,
                QF_MF_G, 
                tarifa_AC_D, 
                tarifa_AC_G,
                tarifa_MC_D,
                tarifa_MC_G,
                tarifa_MF_D, 
                tarifa_MF_G, 
                reforç_PADES #reforç PADES 
                 ){
                  arguments<-as.list(environment())
                  
                  # validació d'inputs
                  stopifnot(all(sapply(arguments, is.numeric)),
                            all(unlist(arguments) >= 0, na.rm = TRUE))
                  
                  #creem df a partir dels arguments (Permetrà treballar amb vectors)  
                  df_meritada <- tryCatch(data.frame(arguments),
                                          error=function(e){
                                            stop('***Revisa que tots els vectors son de la mateixa mida perque no es pot crear un dataframe***') #control 
                                          }
                  )
                  
                  # funció UFISS
                  funcio_calcul_meritada <- function(QC_AC_D,              
                                                     QC_AC_G,#Altes Alta complexitat (G=gener-març, D=Desembre)
                                                     QC_MC_D,
                                                     QC_MC_G,#Altes mitja complexitat (G=gener-març, D=Desembre)
                                                     QC_MF_D,
                                                     QC_MF_G,#Altes Malalts fràgils (G=gener-març, D=Desembre)
                                                     QF_AC_D,
                                                     QF_AC_G,
                                                     QF_MC_D,
                                                     QF_MC_G,
                                                     QF_MF_D,
                                                     QF_MF_G, 
                                                     tarifa_AC_D, 
                                                     tarifa_AC_G,
                                                     tarifa_MC_D,
                                                     tarifa_MC_G,
                                                     tarifa_MF_D, 
                                                     tarifa_MF_G, 
                                                     reforç_PADES #reforç PADES 
                  ) {
                    
                    f.calcul.import<-function(QC,QF,tarifa){
                      if (QF>QC){
                        import<-QC*tarifa
                      }else{
                        import<-QF*tarifa
                      }
                    }
                    
                    import.mitja.complexitat<-f.calcul.import(QC_MC_G,QF_MC_G,tarifa_MC_G)+
                      f.calcul.import(QC_MC_D,QF_MC_D,tarifa_MC_D)
                    
                    import.alta.complexitat<-f.calcul.import(QC_AC_G,QF_AC_G,tarifa_AC_G)+
                      f.calcul.import(QC_AC_D,QF_AC_D,tarifa_AC_D)
                    
                    import.malalts.fragils<-f.calcul.import(QC_MF_G,QF_MF_G,tarifa_MF_G)+
                      f.calcul.import(QC_MF_D,QF_MF_D,tarifa_MF_D)
                    
                    meritada<-round_excel(import.mitja.complexitat,2)+round_excel(import.alta.complexitat,2)+round_excel(import.malalts.fragils,2)+round_excel(reforç_PADES,2)
                    return(meritada)
                  }
                  
                  # apliquem la funció a cada fila del df
                  df_meritada$DM = mapply(funcio_calcul_meritada,
                                          QC_AC_D,              
                                          QC_AC_G,#Altes Alta complexitat (G=gener-març, D=Desembre)
                                          QC_MC_D,
                                          QC_MC_G,#Altes mitja complexitat (G=gener-març, D=Desembre)
                                          QC_MF_D,
                                          QC_MF_G,#Altes Malalts fràgils (G=gener-març, D=Desembre)
                                          QF_AC_D,
                                          QF_AC_G,
                                          QF_MC_D,
                                          QF_MC_G,
                                          QF_MF_D,
                                          QF_MF_G, 
                                          tarifa_AC_D, 
                                          tarifa_AC_G,
                                          tarifa_MC_D,
                                          tarifa_MC_G,
                                          tarifa_MF_D, 
                                          tarifa_MF_G, 
                                          reforç_PADES) #reforç PADES
                  
                  return(df_meritada$DM)
                  
                }
