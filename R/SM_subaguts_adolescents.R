#' Càlcul de la despesa meritada — Salut Mental: subaguts adolescents
#'
#' @param QC Quantitat/import contractat.
#' @param EM Paràmetre `EM`.
#' @param tarifa Tarifa unitària.
#' @param pct_pla Percentatge d'assoliment del pla de salut.
#'
#' @return Despesa meritada calculada.
#' @export
SM_subaguts_adolescents<-function(
            QC,      # pdf diu com si anés per pressu, però al excel ho calcula en base a altes contractades*EM*tarifa
            EM,
            tarifa,
            pct_pla   #pct assoliment pla salut
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
            pes.fix<-0.9
            pes.pla_salut<-0.1
            #La funció a aplicar per calcular la DM  
            funcio.calcul.meritada<-function(
              QC,      # pdf diu com si anés per pressu, però al excel ho calcula en base a altes contractades*EM*tarifa
              EM,
              tarifa,
              pct_pla){
              import_fix <- QC*EM*tarifa*pes.fix
              #PS: Pla de salut
              import_PS <- (QC*EM*tarifa*pct_pla*pes.pla_salut)
              meritada<-round_excel(import_fix,2)+round_excel(import_PS,2)
              return(meritada)}
            
            #Apliquem la funció a cada row del df:
            df.meritada$DM=mapply(
              funcio.calcul.meritada,
              QC,      # pdf diu com si anés per pressu, però al excel ho calcula en base a altes contractades*EM*tarifa
              EM,
              tarifa,
              pct_pla
            )
            return(df.meritada$DM)
          }
