#==============================================================================#
# Function: "plot_spec()" - create plot_ly graphs of spectrums
#==============================================================================#

## Data to test
#my_i=35
#ZZ=zz[, my_i]
#UV=uvvis_data[my_i, ]
#labY=label_y
#labX=label_x[my_i]

#-------------------------------------------------------------------------------
plot_spec <- function(ZZ, UV, labX, labY, unX, unY){
  # inputs
  spec       <- ZZ
  min_y      <- as.numeric(UV["Abs_min"])
  max_y      <- as.numeric(UV["Abs_max"])
  max_x      <- as.numeric(UV["Wl_max"])
  intersec_x <- as.numeric(UV[c("FWHM_L1", "FWHM_L2")])
  half_h     <- as.numeric(UV["Half_height"])
  FWHM       <- as.numeric(UV["FWHM"])
  min_y_g1   <- as.numeric(UV["Abs_min_1"])
  min_x_g1   <- as.numeric(UV["Wl_min_1"])
  min_y_g2   <- as.numeric(UV["Abs_min_2"])
  min_x_g2   <- as.numeric(UV["Wl_min_2"])

  # Define plot colors
  plot_colors <- c("#619CFF", "#F8766D", "#00BA38")
  
  # Order data
  dados <- data.frame(spec=as.numeric(spec),
                      wl=as.numeric(labY))[order(labY), ]
  
  # Text in legentes (hover)
  hover_text0 <- paste("<b>Wavelength:</b> ", dados$wl, " ", unY, "<br>",
                      "<b>Absorbance:</b> ", dados$spec, sep="")
  hover_text1 <- paste("<b>Wavelength:</b> ", round(intersec_x, 2), " ", unY, "<br>",
                       "<b>Absorbance:</b> ", round(c(half_h, half_h), 4), sep="")
  hover_text2 <- paste("<b>Wavelength:</b> ", max_x, " ", unY, "<br>",
                       "<b>Absorbance:</b> ", max_y, sep="")
  hover_text3 <- paste("<b>FWHM:</b> ", round(FWHM, 2), " ", unY, sep="")
  
  # Margins and fonts
  marg <- list(b=87, l=68, t=65, r=28, pad=0)
  font_title  <- list(size=20, family="Arial", color="black")
  font_labels <- list(size=14, family="Arial", color="black")
  font_ticks  <- list(size=14, family="Arial", color="black")
  
  # Plot
  plot_ly(dados, x=~wl) %>%
    # Wavelength data
    add_lines(y=~spec, line=list(color=plot_colors[1], width=4),
              hoverinfo="text", text=hover_text0, name="Spetrum") %>%
    add_markers(x=round(intersec_x, 2), y=c(half_h, half_h),
                marker=list(size=10, color=plot_colors[3]),
                hoverinfo="text", text=hover_text1, showlegend=FALSE) %>%
    add_markers(x=max_x, y=max_y,  marker=list(size=10, color=plot_colors[2]),
                hoverinfo="text", text=hover_text2, showlegend=FALSE) %>%
    add_trace(x=seq(from=intersec_x[1], to=intersec_x[2], length=100),
              y=rep(half_h, 100),
              mode="lines", line=list(color=plot_colors[3], width=4),
              hoverinfo="text", text=hover_text3, showlegend=FALSE) %>%
    layout(title       = paste("<b>Time", " = ", labX, " (", unX, ")</b>", sep=""),
           titlefont   = font_title,
           showlegend  = FALSE,
           xaxis       = list(title=paste("Wavelength (", unY, ")", sep=""),
                              titlefont=font_labels, tickfont=font_ticks,
                              linewidth=1, showline=TRUE, showgrid=TRUE, showticklabels=TRUE,
                              tickangle=0, tickwidth=1, ticklen=6, zeroline=FALSE),
           yaxis       = list(title="Abs", titlefont=font_labels, tickfont=font_ticks,
                              linewidth=1, showline=TRUE, showgrid=TRUE, showticklabels=TRUE,
                              tickangle=0, tickwidth=1, ticklen=6, zeroline=FALSE),
           margin      = marg,
           annotations = list(
             list(xref="x", yref="y",
                  x=intersec_x[2]*1.4, y=half_h, showarrow=FALSE, align="right",
                  text=hover_text3, font=list(size=15, color=plot_colors[3]),
                  showarrow=TRUE),
             list(xref="x", yref="y", x=max_x, y=max_y, text="SPR peak",
                  align="right", showarrow = TRUE, arrowhead=100,
                  col=plot_colors[2])
           )
    )
}
#==============================================================================#
#================================== END =======================================#
#==============================================================================#