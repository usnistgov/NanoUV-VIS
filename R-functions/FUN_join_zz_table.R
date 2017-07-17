#-------------------------------------------------------------------------------
# Function: "peak_uvvis()" - evaluate FWHM statistics for each wavelength data #
#-------------------------------------------------------------------------------

#files_names <- arq_names

join_zz_table <- function(files_names){
  
  # Create NULL vectors to store objects from all data
  zz <- NULL
  label_x <- label_y <- NULL
  unities <- NULL
  
  # FOR: read each ".csv" file, merge spectrum data and evaluate FWHM statistics
  for(i in 1:length(files_names)){
    #i=1
    dados_0 <- read.csv(files_names[i], header=FALSE, skip=0, colClasses="character")
    #head(dados_0)
    
    #----------------#
    # Pre-processing # Clean data and save information
    #----------------#
    # Save "times"
    line1      <- unlist(strsplit(dados_0[1, 2], " "))
    label_x[i] <- as.numeric(line1[1])
    
    # Save "unities" and wavelength (label_y) (from first file only)
    if(i==1){
      unit_x  <- line1[2]
      unit_y  <- dados_0[2, 1]
      unit_z  <- dados_0[2, 2]
      label_y <- as.numeric(dados_0[-(1:2), 1])
    }
    unities <-c(unit_x, unit_y, unit_z)
    
    # Save matrix with spectrum and wavelength vectors only
    dados <- matrix(c(as.numeric(dados_0[-(1:2), 1]),
                      as.numeric(dados_0[-(1:2), 2])), ncol=2)
    colnames(dados) <- c("wvlen", "spec")
    #head(dados)
    
    # Append data
    zz <- cbind(zz, dados[, 2])
    #head(zz)
  }
  
  # Order vectors and matrices, according to the "time" (x axis)
  ind_ord_x <- order(label_x)
  label_x   <- label_x[ind_ord_x]
  zz        <- zz[, ind_ord_x]

  zz_table <- data.frame(label_y, zz)
  names(zz_table) <- c("wavelength", label_x)
  
  # Create a list with objects to be returned
  RES <- list(zz_table=zz_table,
              label_x=label_x,
              label_y=label_y,
              unities=unities)
  return(RES)
} 
  
  
  
  
  
  


