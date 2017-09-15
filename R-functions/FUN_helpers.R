#==============================================================================#
# General Functions - css, html, etc. 
#==============================================================================#

#-------------------------------------------------------------------------------
# Transform ".rmd" about data to ".md"
about_file_rmd <- "ABOUT.Rmd"
sapply(X=about_file_rmd, FUN=knitr::knit, quiet=TRUE)

#-------------------------------------------------------------------------------
# Create a little question mark link that shows a help popup on hover
helpPopup <- function(content, title=NULL) {
  a(href = "#",
    class = "popover-link",
    `data-toggle`  = "popover",
    `data-title`   = title,
    `data-content` = content,
    `data-html`    = "true",
    `data-trigger` = "hover",
    icon("question-circle")
  )
}

#-------------------------------------------------------------------------------
# Create an upper red asterisk, indicating a mandatory input
redAsterisk <- function(label) {
  tagList(label, span("*", class="mandatory_star"))
}

#-------------------------------------------------------------------------------
# spinner for "load" plot status
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class="btn-loading-container",
      hidden(
        img(src="ajax-loader-bar.gif", class="btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

withBusyIndicatorServer <- function(buttonId, expr) {
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl    <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl     <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector=loadingEl)
  })
  
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

#-------------------------------------------------------------------------------
# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl      <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg   <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

#-------------------------------------------------------------------------------
# CSS codes
appCSS <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}
"
#==============================================================================#
#================================== END =======================================#
#==============================================================================#