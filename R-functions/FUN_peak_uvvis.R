#==============================================================================#
# Function: "peak_uvvis()" - evaluate FWHM statistics for each wavelength data #
#==============================================================================#

## Data to test
#spec=zz[, 35] ; wvlen=label_y

#-------------------------------------------------------------------------------
peak_uvvis <- function(spec, wvlen=NULL){
  # Handle NULL wavelengths
  if(is.null(wvlen)) wvlen <- 1:length(spec)
  ind_wl <- order(wvlen)
  wvlen <- wvlen[ind_wl]
  spec <- spec[ind_wl]
  names(spec) <- as.character(wvlen)
  
  # Find maximun value of spectrum (peak or mode) 
  ind_max_y <- which(spec==max(spec, na.rm=TRUE))
  max_x     <- mean(wvlen[ind_max_y])
  max_y     <- max(spec, na.rm=TRUE)

  #------------------------------------------------#
  # Split spectrum according to it's maximun value #
  #------------------------------------------------#
  # BEFORE peak (group 1 or "g1") #---------------------------------------------
  ind_g1 <- which(wvlen <= max_x)
  wl_g1   <- wvlen[ind_g1]
  spec_g1 <- spec[ind_g1]
  
  # Find minimun spectrum value (x-y coordinates) of "g1"
  ind_min_y_g1 <- which(spec_g1==min(spec_g1, na.rm=TRUE))
  min_x_g1     <- max(wl_g1[ind_min_y_g1])
  min_y_g1     <- min(spec_g1, na.rm=TRUE)
  
  # AFTER peak (group 2 or "g2") #----------------------------------------------
  ind_g2  <- which(wvlen > max_x)
  wl_g2   <- wvlen[ind_g2]
  spec_g2 <- spec[ind_g2]
  
  # Find minimun spectrum value (x-y coordinates) of "g2"
  ind_min_y_g2 <- which(spec_g2==min(spec_g2, na.rm=TRUE))
  min_x_g2     <- min(wl_g2[ind_min_y_g2])
  min_y_g2     <- min(spec_g2, na.rm=TRUE)
  
  #----------------------------------------------------------------------------#
  # Find x-y coordinates of "minimun absolute" point, given by the intersec.   #
  # of vertical line (wavelenght) of maximun absorbance and the line which     #
  # conects the minimuns points of of g1 and g2                                #
  #----------------------------------------------------------------------------#
  m     <- (min_y_g2 - min_y_g1)/(min_x_g2 - min_x_g1)
  min_x <- max_x
  min_y <- min_y_g1 + m*(min_x - min_x_g1)

  # Evaluate "half height" of spectrum (median point)
  half_h <- (max_y + min_y)/2
  
  #----------------------------------------------------------------------------#
  # Evaluate intersection point between "half height" and the spectrum values
  #----------------------------------------------------------------------------#
  # BEFORE peak (group 1 or "g1") #---------------------------------------------
  if(any(spec_g1==half_h)){
    ind_diff_spec_g1 <- which(spec_g1==half_h)
    intersec_g1      <- wl_g1[ind_diff_spec_g1][1]
  } else {
    diff_g1 <- spec_g1 - half_h
    # Linear interpolation
    ind_M <- as.numeric(names(which.min(diff_g1[which(diff_g1>0)])))
    ind_m <- as.numeric(names(which.max(diff_g1[which(diff_g1<0)])))
    M <- spec[which(wvlen==ind_M)]
    m <- spec[which(wvlen==ind_m)]
    intersec_g1 <- approx(y=c(ind_m, ind_M), x=c(m, M),
                          xout=half_h, method="linear", n=50)$y
  }

  # AFTER peak (group 2 or "g2") #----------------------------------------------
  if(any(spec_g2==half_h)){
    ind_diff_spec_g2 <- which(spec_g2==half_h)
    intersec_g2      <- wl_g2[ind_diff_spec_g2][1]
  } else {
    diff_g2 <- spec_g2 - half_h
    # Linear interpolation
    ind_M <- as.numeric(names(which.min(diff_g2[which(diff_g2>0)])))
    ind_m <- as.numeric(names(which.max(diff_g2[which(diff_g2<0)])))
    M <- spec[which(wvlen==ind_M)]
    m <- spec[which(wvlen==ind_m)]
    intersec_g2 <- approx(y=c(ind_m, ind_M), x=c(m, M),
                          xout=half_h, method="linear", n=50)$y
  }

  # Evaluate FWHM
  intersec_x <- c(intersec_g1, intersec_g2)
  FWHM       <- diff(intersec_x)
  
  # Output
  out <- list()
  out <- c(max_y, FWHM, max_x,                     # important statistics
           min_y, min_x, half_h,                   # coord. of min + halh-height
           intersec_g1, intersec_g2,               # intersec(s) of half_h
           min_y_g1, min_x_g1, min_y_g2, min_x_g2) # coord. of min (g1 and g2)
  names(out) <- c("Abs_max", "FWHM", "Wl_max",
                  "Abs_min", "Wl_min", "Half_height",
                  "FWHM_L1", "FWHM_L2",
                  "Abs_min_1", "Wl_min_1", "Abs_min_2", "Wl_min_2")
  return(out)
} 
#==============================================================================#
#================================== END =======================================#
#==============================================================================#