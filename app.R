remotes::install_github("louisahsmith/evalue@install_github")
library(shiny)
library(shinythemes)
library(sortable)
library(MEValue)
library(bsplus)

source("setup.R")

#### UI component --------------------------------------------------
ui <- navbarPage(
  title = "Multiple-bias sensitivity analysis using bounds",
  id = "navbar",
  theme = shinytheme("united"),

  tabPanel(
    shinyjs::useShinyjs(),
    title = "Sensitivity analysis",
    # prevent page from greying out after 10 seconds
    tags$script(src = "keep_alive.js"),
    tags$head(
      # workaround to get math in there (can't get inline mathjax to work)
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css",
        integrity = "sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
        crossorigin = "anonymous"
      ),
      HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" 
           integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" 
           crossorigin="anonymous"></script>'),
      HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" 
           integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" 
           crossorigin="anonymous" onload="renderMathInElement(document.body);"></script>'),
      HTML('<script> document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, { delimiters: [{left: "$", right: "$", display: false}]
        }); })</script>'),
      # Google analytics CHANGE
      HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=UA-112795291-2"></script>
      <script> 
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag("js", new Date());
        gtag("config", "UA-112795291-2");
     </script>'),
      # my CSS additions
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),

    sidebarLayout(
      mainPanel(
        # providing content for models
        bs_modal(
          id = "modal_pop_group",
          title = "Target population",
          body = includeMarkdown("content/target_pop.md"),
          size = "medium"
        ),
        bs_modal(
          id = "modal_assumptions",
          title = "Additional assumptions",
          body = includeMarkdown("content/selection_assumptions.md"),
          size = "medium"
        ),
        bs_modal(
          id = "modal_parameters",
          title = "Parameter definitions",
          body = includeMarkdown("content/parameters.md"),
          size = "medium"
        ),

        # BIAS CHOICES ----
        h2("Choose biases of interest for sensitivity analysis"),
        # from example at https://rstudio.github.io/sortable/
        # choose biases
        bucket_list( # ADD MODAL HERE!!!
          header = "Drag the biases of interest to the empty box in the order in which they occur in the data.",
          group_name = "draggable",
          orientation = "horizontal",
          add_rank_list(
            text = "Options",
            labels = list(
              "confounding" = "Unmeasured confounding",
              "selection" = "Selection bias",
              "misclassification" = "Differential misclassification"
            ),
            input_id = "opts"
          ),
          add_rank_list(
            text = "Chosen",
            labels = NULL,
            input_id = "biases"
          )
        ),

        fluidRow(
          # if misclassification chosen
          column(
            6,
            conditionalPanel(
              condition = "input.biases.includes('misclassification')",
              radioButtons(
                "misclassType",
                label = h3("Misclassification of"),
                choices = c(
                  "Outcome" = "outcome",
                  "Exposure" = "exposure"
                ),
                width = "100%",
                inline = TRUE,
                selected = character(0)
              ), # end misclassification of... panel
              conditionalPanel(
                condition = "input.misclassType=='exposure'",
                radioButtons(
                  "rareOutcome",
                  label = span(
                    h3("Rareness"),
                    p("Is the outcome sufficiently rare that the estimate is approximately equivalent to a risk ratio?")
                  ),
                  choices = c(
                    "No" = "no",
                    "Yes" = "yes"
                  ),
                  width = "100%", inline = TRUE, selected = character(0)
                ),

                radioButtons(
                  "rareExposure",
                  label = p("Is the exposure sufficiently rare that the odds ratio for the exposure is approximately equivalent to a risk ratio?"),
                  choices = c(
                    "No" = "no",
                    "Yes" = "yes"
                  ),
                  width = "100%",
                  inline = TRUE, selected = character(0)
                )
              ), # end rareness panel
            ), # end misclassification conditional panel

            conditionalPanel( # disabled version of above
              condition = "!input.biases.includes('misclassification')",
              radioButtons(
                "misclassTypeFake",
                label = h3("Misclassification of"),
                choices = c(
                  "Outcome" = "outcome",
                  "Exposure" = "exposure"
                ),
                width = "100%",
                inline = TRUE,
                selected = character(0)
              )
            ), # end misclassification of... panel
            conditionalPanel(
              condition = "!input.biases.includes('misclassification') || input.misclassType!='exposure'",
              radioButtons(
                "rareOutcomeFake",
                label = span(
                  h3("Rareness"),
                  p("Is the outcome sufficiently rare that the estimate is approximately equivalent to a risk ratio?")
                ),
                choices = c(
                  "No" = "no",
                  "Yes" = "yes"
                ),
                width = "100%", inline = TRUE, selected = character(0)
              )
            ), # end conditional rare outcome
            conditionalPanel(
              condition = "!input.biases.includes('misclassification') || input.misclassType!='exposure'",
              radioButtons(
                "rareExposureFake",
                label = p("Is the exposure sufficiently rare that the odds ratio for the exposure is approximately equivalent to a risk ratio?"),
                choices = c(
                  "No" = "no",
                  "Yes" = "yes"
                ),
                width = "100%",
                inline = TRUE, selected = character(0)
              )
            ) # end conditional rare exposure panel
          ), # end misclassification column

          # if selection bias chosen
          column(
            6, conditionalPanel(
              condition = "input.biases.includes('selection')",
              radioButtons(
                "pop_group_S",
                label = HTML('<label class="h3">Target population</label>
        <a href="#" data-toggle="modal" data-target="#modal_pop_group_S">
             <i class="fa fa-info-circle"></i>
             </a>'),
                choices = c(
                  "Entire population" = "general",
                  "Selected population" = "selected"
                ),
                selected = character(0),
                width = "102%",
                inline = TRUE
              )
            ), # end population panel
            conditionalPanel(
              condition = "input.biases.includes('selection') && input.pop_group_S != 'selected'",
              checkboxGroupInput(
                "assump_S",
                label = HTML('<label class="h3">Additional assumptions about selection</label>
        <a href="#" data-toggle="modal" data-target="#modal_assumptions_S">
             <i class="fa fa-info-circle"></i>
             </a>'),
                choices = c(
                  "Unmeasured factor a defining characteristic of selection" = "S_U",
                  "Selection always associated with increased risk of outcome in both exposure groups" = "increased_risk",
                  "Selection always associated with decreased risk of outcome in both exposure groups" = "decreased_risk"
                ),
                width = "100%"
              )
            ), # end assumptions panel
            conditionalPanel(
              condition = "!input.biases.includes('selection')",
              radioButtons(
                "pop_group_SFake",
                label = HTML('<label class="h3">Target population</label>
        <a href="#" data-toggle="modal" data-target="#modal_pop_group_S">
             <i class="fa fa-info-circle"></i>
             </a>'),
                choices = c(
                  "Entire population" = "general",
                  "Selected population" = "selected"
                ),
                selected = character(0),
                width = "102%",
                inline = TRUE
              )
            ), # end population panel
            conditionalPanel(
              condition = "!(input.biases.includes('selection') && input.pop_group_S != 'selected')",
              checkboxGroupInput(
                "assump_SFake",
                label = HTML('<label class="h3">Additional assumptions about selection</label>
        <a href="#" data-toggle="modal" data-target="#modal_assumptions_S">
             <i class="fa fa-info-circle"></i>
             </a>'),
                choices = c(
                  "Unmeasured factor a defining characteristic of selection" = "S_U",
                  "Selection always associated with increased risk of outcome in both exposure groups" = "increased_risk",
                  "Selection always associated with decreased risk of outcome in both exposure groups" = "decreased_risk"
                ),
                width = "100%"
              )
            )
          ), # end selection bias choices
          width = 6
        ), # end fluid column
        width = 7
      ), # ends main panel

      # SENSITIVITY ANALYSIS TABSET ----
      sidebarPanel(
        width = 5,
        tabsetPanel(
          type = "pills",
          id = "sensOpts",

          # enter estimate ----
          tabPanel(
            "Observed estimate",
            br(),
            div(p("Estimate type")),
            selectInput(
              "measure",
              label = NULL,
              choices = c(
                "Risk ratio" = "RR",
                "Hazard ratio" = "HR",
                "Odds ratio" = "OR"
              ), width = "50%"
            ),
            conditionalPanel(
              condition = "input.measure == 'OR' | input.measure == 'HR'",
              div(p("Is the outcome sufficiently rare that the estimate is approximately equivalent to a risk ratio?")),
              radioButtons(
                "rareOutcomeEst",
                label = NULL,
                choices = c(
                  "No" = "no",
                  "Yes" = "yes"
                ),
                width = "50%", inline = TRUE, selected = "no"
              ),
            ), # end rareness
            div(p("Point estimate")),
            numericInput("est",
              label = NULL, value = NA, min = 0, step = .1,
              width = "50%"
            ),
            div(p("Confidence interval (optional)")),
            flowLayout(
              numericInput("lo", label = tags$small("Lower limit"), value = NA, min = 0, step = .1),
              numericInput("hi", label = tags$small("Upper limit"), value = NA, min = 0, step = .1),
              cellArgs = list(style = "width:110px;")
            )
          ), # end estimate tab

          # calculate bound ----
          tabPanel(
            "Multi-bias bound",
            # display results
            br(),
            conditionalPanel(
              condition = "output.b1",
              div(p("Enter values for sensitivity parameters:"))
            ),
            # UI output
            splitLayout(
              uiOutput("b1"),
              uiOutput("b2")
            ),
            splitLayout(
              uiOutput("b3"),
              uiOutput("b4")
            ),
            splitLayout(
              uiOutput("b5"),
              uiOutput("b6")
            ),
            splitLayout(
              uiOutput("b7")
            ),

            uiOutput("bound"),
            conditionalPanel(
              condition = "output.bound",
              flowLayout(
                actionButton("showcode", "Show R code"),
                # actionButton("clearbutton", "Clear"),
                cellArgs = list(style = "width:110px;")
              )
            ),
            br(),
            conditionalPanel(
              condition = "input.showcode && output.bound",
              uiOutput("bound_code",
                container = tags$pre
              )
            ),
            use_bs_popover(),
          ), # end parameters panel

          # calculate e-value ----
          tabPanel(
            "Multi-bias E-value",
            conditionalPanel(
              condition = "!output.evalue",
              br()
              ),
            uiOutput("evalue"),
            br(),
            conditionalPanel(
              condition = "input.est",
              tags$span(
                p("The default multi-bias E-value is the strength of bias that 
                  would be sufficient to shift the estimate to the null value. 
                  If you'd like to calculate a non-null E-value, change the \"true\" value here.")
              ),
              numericInput("true",
                           label = NULL,
                           value = 1, min = 0, step = .1, width = "25%"
              )
            ),
            conditionalPanel(
              condition = "output.evalue",
              flowLayout(
                actionButton("showcodeE", "Show R code"),
                # actionButton("clearbutton", "Clear"),
                cellArgs = list(style = "width:110px;")
              )
            ),
            br(),
            conditionalPanel(
              condition = "input.showcodeE && output.evalue",
              uiOutput("evalue_code",
                       container = tags$pre
              )
            ),
            textOutput("keep_alive")
          )
        ) # end tabset panel
      ) # end sidebar
    )
  ), # end tab panel
  tabPanel(
    title = "Resources",
    h1("BLAHLBLAHBLAH")
  )
) # end fluid page


#### server component ------------------------------------------------
server <- function(input, output, session) {
  shinyjs::disable("misclassTypeFake")
  shinyjs::disable("rareExposureFake")
  shinyjs::disable("rareOutcomeFake")
  shinyjs::disable("pop_group_SFake")
  shinyjs::disable("assump_SFake")

  # what will be blank text to prevent page grey-out
  output$keep_alive <- renderText({
    req(input$alive_count)
    input$alive_count
  })

  # remove all extra assumptions if selected pop chosen
  observe({
    if (!is.null(input$pop_group_S) && input$pop_group_S == "selected") {
      x <- character(0)
    } else {
      x <- input$assump_S
    }
    updateCheckboxGroupInput(session, "assump_S", selected = x)
  })

  #### BIAS CHOICES -----------------------------------------------
  biases <- reactive({
    # don't run if nothing entered
    req(input$biases)

    # which biases
    yes_confounding <- "confounding" %in% input$biases
    yes_selection <- "selection" %in% input$biases
    yes_misclassification <- "misclassification" %in% input$biases

    # selection arguments -----
    # can't do selection without choice of population
    if (yes_selection) req(input$pop_group_S)
    pop_group_S <- input$pop_group_S
    S_eq_U <- "S_U" %in% input$assump_S && pop_group_S != "selected"
    risk_inc <- "increased_risk" %in% input$assump_S && pop_group_S != "selected"
    risk_dec <- "decreased_risk" %in% input$assump_S && pop_group_S != "selected"
    selection_args <- c("general", "selected", "S = U", "increased risk", "decreased risk")
    selection_args <- selection_args[c(
      pop_group_S == "general",
      pop_group_S == "selected",
      S_eq_U, risk_inc, risk_dec
    )]

    # misclassification arguments ----
    # can't do misclassification without choice of type
    if (yes_misclassification) req(input$misclassType)
    misclassification_type <- input$misclassType
    # can't do exposure misclassification without input on rareness
    if (yes_misclassification && misclassification_type == "exposure") {
      req(input$rareOutcome)
      req(input$rareExposure)
      rare_outcome <- input$rareOutcome == "yes"
      rare_exposure <- input$rareExposure == "yes"
    } else {
      rare_outcome <- rare_exposure <- FALSE
    } # defaults

    # misclassification before selection
    misclass_first <- yes_selection && yes_misclassification &&
      (which(input$biases == "misclassification") <
        which(input$biases == "selection"))

    # put it all together
    arg1 <- if (yes_confounding) confounding()
    arg2 <- if (yes_selection) poss_selection(selection_args)
    arg3 <- if (yes_misclassification) {
      poss_misclassification(misclassification_type, # return NA if error
        rare_outcome = rare_outcome,
        rare_exposure = rare_exposure
      )
    }

    # should be the only possible errors (given validation)
    if (yes_misclassification && !is.null(arg3) && is.na(arg3)) {
      return(NULL)
    }
    if (yes_selection && !is.null(arg2) && is.na(arg2)) {
      return(NULL)
    }

    if (misclass_first) {
      arg4 <- arg2
      arg2 <- arg3
      arg3 <- arg4
    }

    args <- list(arg1, arg2, arg3)
    args[sapply(args, is.null)] <- NULL
    biases <- do.call(multi_bias, args)
    return(biases)
  }) # end biases()

  # for the conditional panel -- how many (out of 7) show up?
  lapply(1:7, function(i) {
    output[[paste0("a", i)]] <- reactive({
      n <- nrow(summary(biases()))
      ifelse(is.null(n), "no",
        ifelse(i > n, "no", "yes")
      )
    })
  })

  outputOptions(output, "a1", suspendWhenHidden = FALSE)
  outputOptions(output, "a2", suspendWhenHidden = FALSE)
  outputOptions(output, "a3", suspendWhenHidden = FALSE)
  outputOptions(output, "a4", suspendWhenHidden = FALSE)
  outputOptions(output, "a5", suspendWhenHidden = FALSE)
  outputOptions(output, "a6", suspendWhenHidden = FALSE)
  outputOptions(output, "a7", suspendWhenHidden = FALSE)

  # create as many as 7 conditional panels, one for each parameter
  lapply(1:7, function(i) {
    output[[paste0("b", i)]] <- renderUI({

      # validate for the first one (so messages don't output 7 times)
      if (i == 1) {
        validate(need(input$biases, "Select biases..."))
        if ("misclassification" %in% input$biases) {
          validate(need(input$misclassType, "Select outcome or exposure misclassification..."))
        }
        if ("selection" %in% input$biases) {
          validate(need(input$pop_group_S, "Choose target population..."))
        }
        if ("selection" %in% input$biases && "increased_risk" %in% input$assump_S) {
          validate(need(!"decreased_risk" %in% input$assump_S, "Conflicting assumptions!"))
        }

        if ("misclassification" %in% input$biases && input$misclassType == "exposure") {
          validate(
            need(input$rareOutcome, "Specify whether outcome is rare..."),
            need(input$rareExposure, "Specify whether exposure is rare..."),
            need(
              is.null(input$rareOutcome) || (!is.null(input$rareOutcome) && input$rareOutcome == "yes"),
              "Sorry! Currently only available for exposure misclassification with rare outcomes."
            )
          )
        }
      } # end first one only

      summary_biases <- summary(biases(), latex = TRUE)
      # needs to be an object with a non-null number of rows
      req(nrow(summary_biases))

      # make the last one smaller so they match
      wi <- ifelse(i == 7, "50%", "100%")
      tagList(
        conditionalPanel(
          condition = paste0("output.a", i, "=='yes'"),

          numericInput(summary_biases$argument[i],
            label = summary_biases$latex[i],
            value = NA, min = 1,
            step = .1, width = wi
          )
        ),

        tags$script(paste0('renderMathInElement(document.getElementById("b', i, '"),
                        {delimiters: [{left: "$", right: "$", display: false}]});'))
      )
    }) # end b outputs
  }) # end lapply

  # CALCULATE BOUND ----
  param_vals <- reactive({
    req(biases())
    args <- summary(biases())$argument
    vals <- lapply(args, function(i) {
      req(input[[i]])
      input[[i]]
    })
    setNames(vals, args)
  })

  bounds <- reactive({
    vals <- param_vals()
    vals$biases <- biases()
    do.call(multi_bound, vals)
  })
  
  rcode_bound <- reactive({
    req(bounds())
    bias_code <- get_multibias_args(biases())
    vals <- param_vals()
    vals <- paste(paste(names(vals), vals, sep = " = "),
                  collapse = ", "
    )
    styler::style_text(
      paste0("library(EValue)\n\nmulti_bound(\nbiases = ", bias_code, ",\n", vals, ")")
    )
  })
  
  output$bound <- renderUI({
    h4("Bound: ", round(bounds(), 2))
  })
  
  output$bound_code <- renderPrint({
    rcode_bound()
  })

  # CALCULATE E-VALUE ----
  vals_for_eval <- reactive({
    req(input$measure)
    req(input$est)
    est <- if (input$measure == "RR") {
      RR(input$est)
    } else if (input$measure == "HR") {
      HR(input$est, rare = input$rareOutcomeEst == "yes")
    } else if (input$measure == "OR") {
      OR(input$est, rare = input$rareOutcomeEst == "yes")
    } 
    list(
      est = est,
      lo = input$lo,
      hi = input$hi,
      true = input$true
    )
  })

  evalues <- reactive({
    validate(
      need(input$biases, "Select biases..."),
      need(input$est, "Enter an estimate...")
    )
    vals <- vals_for_eval()
    res <- multi_evalue(
      biases = biases(),
      est = vals$est,
      lo = vals$lo,
      hi = vals$hi,
      true = vals$true
    )
    first <- paste0("Multi-bias E-value: ", round(summary(res), 2))
    second <- ifelse(any(!is.na(res[2, 2:3])),
      paste0("E-value for confidence limit closest to the null: ", round(res[2, 2:3][!is.na(res[2, 2:3])], 2)),
      ""
    )
    list(first, second)
  })

  rcode_evalue <- reactive({
    req(evalues())
    bias_code <- get_multibias_args(biases())
    vals <- vals_for_eval()
    att <- attributes(vals$est)
    measure <- paste0(att$class[1], "(", vals$est, {
      if (!is.null(att$rare)) paste0(", rare = ", att$rare)
    }, "), ")
                      
    ecode <- paste0(
      "library(EValue)\n\nmulti_evalue(biases = ", bias_code, ",\n est = ",
      measure, {
        if (!is.na(vals$lo)) paste0("lo = ", vals$lo, ", ")
        if (!is.na(vals$hi)) paste0("hi = ", vals$hi, ", ")
      }, "true = ", vals$true, ")")
    
    styler::style_text(ecode)
  })

  output$evalue <- renderUI({
    tags$div(
      h4(evalues()[[1]]),
      h5(evalues()[[2]])
    )
  })
  
  output$evalue_code <- renderPrint({
    rcode_evalue()
  })
}

# Run the application
shinyApp(ui = ui, server = server)