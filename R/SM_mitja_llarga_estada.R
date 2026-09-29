#' Càlcul de la despesa meritada — Salut Mental: mitja llarga estada
#'
#' @param QC_RM Quantitat/import contractat.
#' @param QC_RM_TC Quantitat/import contractat.
#' @param QC_TM_PSIQ Quantitat/import contractat.
#' @param tarifa_RM Tarifa unitària.
#' @param pct_TM_PSIQ Paràmetre `pct_TM_PSIQ`.
#'
#' @return Despesa meritada calculada.
#' @export
SM_mitja_llarga_estada<-function(
          QC_RM=0,
          QC_RM_TC=0,
          QC_TM_PSIQ=0,
          tarifa_RM=0, #Retard mental
          tarifa_RM_TC=0, #Retard mental i greu transtorn de la conducta
          tarifa_TM_PSIQ=0, #transtorn mnental alta dependencia psiq.
          pct_RM=0,#pct assoliment pla salut
          pct_RM_TC=0,
          pct_TM_PSIQ=0
          
      ){
        arguments<-as.list(environment())
        pct_args <- arguments[startsWith(names(arguments), "pct_")]
        
        # validació d'inputs
        stopifnot(
          all(sapply(arguments, is.numeric)),
          all(unlist(arguments) >= 0, na.rm = TRUE),
          all(sapply(pct_args,function(x) all(x >= 0 & x <= 1, na.rm = TRUE)))
        )
        
        
        #creem df a partir dels arguments (Permetrà treballar amb vectors)  
        df.meritada<-tryCatch(
          data.frame(arguments),
          error=function(e){
            stop('***Revisa que tots els vectors son de la mateixa mida perque no es pot crear un dataframe***') #control 
          }
        )
        
        #Fixem pesos de cada import 
        pes_fix<-0.95
        pes_pla_salut<-0.05
        
        
        #La funció a aplicar per calcular la DM  
        funcio.calcul.meritada<-function(QC_RM=0,
                                         QC_RM_TC=0,
                                         QC_TM_PSIQ=0,
                                         tarifa_RM=0, #Retard mental
                                         tarifa_RM_TC=0, #Retard mental i greu transtorn de la conducta
                                         tarifa_TM_PSIQ=0, #transtorn mnental alta dependencia psiq.
                                         pct_RM=0,#pct assoliment pla salut
                                         pct_RM_TC=0,
                                         pct_TM_PSIQ=0){
          
          import_fix<-(QC_RM*tarifa_RM+QC_TM_PSIQ*tarifa_TM_PSIQ+QC_RM_TC*tarifa_RM_TC)*pes_fix
          #PS:Pla de salut
          import.pla.salut<-(QC_RM*tarifa_RM*pct_RM*pes_pla_salut+
                               QC_TM_PSIQ*tarifa_TM_PSIQ*pct_TM_PSIQ*pes_pla_salut+
                               QC_RM_TC*tarifa_RM_TC*pct_RM_TC*pes_pla_salut)
          
          meritada<-import_fix+import.pla.salut
          return(meritada)
        }
        #Apliquem la funció a cada row del df:
        df.meritada$DM=mapply(
          funcio.calcul.meritada,
          QC_RM,
          QC_RM_TC,
          QC_TM_PSIQ,
          tarifa_RM, #Retard mental
          tarifa_RM_TC, #Retard mental i greu transtorn de la conducta
          tarifa_TM_PSIQ, #transtorn mnental alta dependencia psiq.
          pct_RM,#pct assoliment pla salut
          pct_RM_TC,
          pct_TM_PSIQ)
        
        return(df.meritada$DM)
      }
