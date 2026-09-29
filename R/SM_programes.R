#' Càlcul de la despesa meritada — Salut Mental: programes
#'
#' @param pressupost Pressupost contractat del programa.
#' @param default 100%
                codi_programa Paràmetre `default 100%
                codi_programa`.
#'
#' @return Despesa meritada calculada.
#' @export
SM_programes<-function(
                pressupost,             # pressupostpost del programa
                pct_pla = 1,             # assoliment del pla de salut, default 100%
                codi_programa      # estic utilitzant la denominació T-num que hi ha a la memoria del programa. crec que no hi ha codis??
            ){
              arguments<-as.list(environment())
              arguments_num<-arguments[!startsWith(names(arguments), "codi_")]
              pct_args <- arguments[startsWith(names(arguments), "pct_")]
              programa_args<-arguments[startsWith(names(arguments), "codi_")]
              
              # validació d'inputs
              stopifnot(
                all(sapply(arguments_num, is.numeric)),
                all(unlist(arguments_num) >= 0, na.rm = TRUE),
                all(sapply(pct_args,function(x) all(x >= 0 & x <= 1, na.rm = TRUE))),
                all(sapply(programa_args, function(x)is.character(x) && all(grepl("^T-\\d{1,2}", x)))) #controlem el format "T-dd"
              )
              
              #Crrem un df amb els programes vigents:
              
              data_programes <- data.frame(
                codi_programa = c("T-1", "T-2", "T-3", "T-4", "T-5", "T-6", "T-7", "T-8", "T-9", 
                                  "T-11", "T-12", "T-14", "T-15", "T-16", "T-18", "T-19", "T-20",
                                  "T-21", "T-22", "T-23", "T-24", "T-25", "T-26", "T-27", "T-28", "T-29", "T-30",
                                  "T-31", "T-32", "T-33", "T-34", "T-35", "T-36", "T-37", "T-38", "T-39"),
                #T10, T13, T17 no existeixen
                pct_fix = c(0.95, 0.90, 0.95, 0.9, 0.9, 1, 0.9, 0.9, 0.95, 
                            0.9, 0.9, 0.9, 0.9, 0.9, 0.95, 0.95, 0.9,
                            0.9, 0.9, 0.95, 0.9, 0.9, 0.9, 0.95, 0.9, 0.9, 0.95,
                            0.9, 0.9, 0.9, 0.95, 0.9, 0.9, 0.9, 0.95, 0.7),
                pct_var = 1-pct_fix
              )
              
              #creem df a partir dels arguments (Permetrà treballar amb vectors)  
              df.meritada<-tryCatch(
                data.frame(arguments),
                error=function(e){
                  stop('***Revisa que tots els vectors son de la mateixa mida perque no es pot crear un dataframe***') #control 
                }
              )
              
              #La funció a aplicar per calcular la DM  
              funcio.calcul.meritada<-function(
                pressupost,             # pressupostpost del programa
                pct_pla = 1,             # assoliment del pla de salut, default 100%
                codi_programa ){
                    pct <- data_programes |> dplyr::filter(codi_programa == !!codi_programa)
                    if (nrow(pct) == 0) {
                      stop("Programa no trobat")
                    }
                    import_fix <- pressupost*pct$pct_fix
                    
                    import_PS <- pressupost*pct_pla*pct$pct_var
                    
                    meritada <- round_excel(import_fix,2) + round_excel(import_PS,2)
                    return(meritada)}
              
              #Apliquem la funció a cada row del df:
              df.meritada$DM=mapply(
                funcio.calcul.meritada,
                pressupost,             # pressupostpost del programa
                pct_pla,
                codi_programa
              )
              return(df.meritada$DM)}            
