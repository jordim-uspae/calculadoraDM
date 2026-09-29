#' Càlcul de la despesa meritada — Atenció Intermèdia: EAIA
#'
#' @param QC_AG Quantitat/import contractat.
#' @param QC_AP Quantitat/import contractat.
#' @param QC_ATC Quantitat/import contractat.
#' @param QC_PV Quantitat/import contractat.
#' @param QC_VS Quantitat/import contractat.
#' @param QF_AG Quantitat/import facturat (realitzat).
#' @param QF_AP Quantitat/import facturat (realitzat).
#' @param QF_ATC Quantitat/import facturat (realitzat).
#' @param QF_PV Quantitat/import facturat (realitzat).
#' @param QF_VS Quantitat/import facturat (realitzat).
#' @param tarifa_AG Tarifa unitària.
#' @param tarifa_AP Tarifa unitària.
#' @param tarifa_ATC Tarifa unitària.
#' @param tarifa_visites Tarifa unitària.
#'
#' @return Despesa meritada calculada.
#' @export
AI_EAIA<-function(QC_AG,#avaluació geriàtrica
                           QC_AP,#avaluació Pal·liatius
                           QC_ATC,#avaluació transt. cognitiu
                           QC_PV, #primeres visites
                           QC_VS,#visites successives
                           QF_AG,
                           QF_AP,
                           QF_ATC,
                           QF_PV,
                           QF_VS,
                           tarifa_AG,
                           tarifa_AP,
                           tarifa_ATC,
                           tarifa_visites
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
                            
                            # funció EAIA
                            funcio_calcul_meritada <- function(QC_AG,#avaluació geriàtrica
                                                               QC_AP,#avaluació Pal·liatius
                                                               QC_ATC,#avaluació transt. cognitiu
                                                               QC_PV, #primeres visites
                                                               QC_VS,#visites successives
                                                               QF_AG,
                                                               QF_AP,
                                                               QF_ATC,
                                                               QF_PV,
                                                               QF_VS,
                                                               tarifa_AG,
                                                               tarifa_AP,
                                                               tarifa_ATC,
                                                               tarifa_visites) {
                              
                              #Import Visites
                              Visites.tot.cont<-QC_PV*(1+QC_VS) 
                              Visites.tot.fact<-QF_PV*(1+QC_VS) ## això no té cap tipus de sentit
                              
                              # cas 1: factu > contracte = es paga només el contracte
                              if(Visites.tot.fact>Visites.tot.cont){
                                import.visites<-Visites.tot.cont*tarifa_visites
                              }else{
                                # cas 2: factu <= contracte = es paga només l'activitat facturada
                                import.visites<-Visites.tot.fact*tarifa_visites
                              }
                              
                              #import procés avaluació geriatria (mateixa logica!)
                              if(QF_AG>QC_AG){
                                import.aval.geriatrica<-QC_AG*tarifa_AG
                              }else{
                                import.aval.geriatrica<-QF_AG*tarifa_AG
                              }
                              #import procés avaluació pal·liatius
                              if(QF_AP>QC_AP){
                                import.aval.paliatius<-QC_AP*tarifa_AP
                              }else{
                                import.aval.paliatius<-QF_AP*tarifa_AP
                              }
                              #import procés avaluacio trantorn cognitiu
                              if(QF_ATC>QC_ATC){
                                import.aval.trantorn<-QC_ATC*tarifa_ATC
                              }else{
                                import.aval.trantorn<-QF_ATC*tarifa_ATC
                              }
                              
                              meritada<-round_excel(import.visites, 2)+round_excel(import.aval.geriatrica, 2)+round_excel(import.aval.paliatius, 2)+round_excel(import.aval.trantorn, 2)
                              return(meritada)
                            }
                            
                            # apliquem la funció a cada fila del df
                            df_meritada$DM = mapply(funcio_calcul_meritada,
                                                    QC_AG,#avaluació geriàtrica
                                                    QC_AP,#avaluació Pal·liatius
                                                    QC_ATC,#avaluació transt. cognitiu
                                                    QC_PV, #primeres visites
                                                    QC_VS,#visites successives
                                                    QF_AG,
                                                    QF_AP,
                                                    QF_ATC,
                                                    QF_PV,
                                                    QF_VS,
                                                    tarifa_AG,
                                                    tarifa_AP,
                                                    tarifa_ATC,
                                                    tarifa_visites) 
                            
                            return(df_meritada$DM)}
