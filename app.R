library(shiny)
library(shinythemes)
library(sortable)
library(EValue)
library(bsplus)
library(rlang)
poss_misclassification <- purrr::possibly(misclassification, otherwise = NA)
options(shiny.sanitize.errors = TRUE)

#### UI component --------------------------------------------------
ui <- navbarPage(
  title = "Multiple-bias sensitivity analysis using bounds",
  id = "navbar",
  theme = shinytheme("united"),
  tabPanel(
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
    textOutput("keep_alive"),
    use_bs_popover(),
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
        column(6, conditionalPanel(
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
        ),
        conditionalPanel(
          condition = "input.misclassType=='exposure'",
          h3("Rareness"),
          p("Is the outcome sufficiently rare that the estimate is approximately equivalent to a risk ratio?"),
          radioButtons(
            "rareOutcome",
            label = NULL,
            choices = c(
              "No" = "no",
              "Yes" = "yes"
            ),
            width = "100%", inline = TRUE, selected = character(0)),
          p("Is the exposure sufficiently rare that the odds ratio for the exposure is approximately equivalent to a risk ratio?"),
          radioButtons(
            "rareExposure",
            label = NULL,
            choices = c(
              "No" = "no",
              "Yes" = "yes"
            ),
            width = "100%",
            inline = TRUE, selected = character(0))
        ),
      )), # end misclassification options
      
      # if selection bias chosen
      column(6, conditionalPanel(
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
          width = "100%",
          inline = TRUE
        )
      ),
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
        
      )), # end selection bias choices
      
      width = 6
    ) # end fluid column
    ), # ends main panel
    
    # SENSITIVITY ANALYSIS TABSET ----
    sidebarPanel(
      tabsetPanel(type = "pills",
                  id = "sensOpts",
                  
        # calculate bound ----
      tabPanel("Bias parameters",
      # display results
      h3("Sensitivity parameters"),
      # UI output
      splitLayout(
        uiOutput("b1"),
        uiOutput("b2")),
      splitLayout(
        uiOutput("b3"),
        uiOutput("b4")),
      splitLayout(
        uiOutput("b5"),
        uiOutput("b6")),
      splitLayout(
        uiOutput("b7")),
                  ), # end parameters panel
      
      # calculate e-value ----
      tabPanel("other", h1("OTHER"),
               )
      ) # end tabset panel
    ) # end sidebar
  )
  ), # end tab panel
  tabPanel(title = "Resources",
           h1("BLAHLBLAHBLAH"))
) # end fluid page
 

#### server component ------------------------------------------------
server <- function(input, output, session) {
  
  # what will be blank text to prevent page grey-out
  output$keep_alive <- renderText({
    req(input$alive_count)
    input$alive_count
  })
  
  # remove all extra assumptions if selected pop chosen
  observe({
    if (!is.null(input$pop_group_S) && input$pop_group_S == "selected")
      x <- character(0) else x <- input$assump_S
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
    selection_args <- selection_args[c(pop_group_S == "general",
                                       pop_group_S == "selected", 
                                       S_eq_U, risk_inc, risk_dec)]
  
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
    } else rare_outcome <- rare_exposure <- FALSE # defaults
    
    # misclassification before selection
    misclass_first <- yes_selection && yes_misclassification &&
      (which(input$biases == "misclassification") < 
         which(input$biases == "selection"))
    
    # put it all together
    arg1 <- if (yes_confounding) confounding()
    arg2 <- if (yes_selection) selection(selection_args)
    arg3 <- if (yes_misclassification) {
      poss_misclassification(misclassification_type, # return NA if error
                        rare_outcome = rare_outcome, 
                        rare_exposure = rare_exposure)
    }
    
    # should be the only possible error (given validation)
    if (yes_misclassification && !is.null(arg3) && is.na(arg3)) return(NULL)
    
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
    output[[paste0('a', i)]] <- reactive({
      n <- nrow(summary(biases()))
      ifelse(is.null(n), "no",
             ifelse(i > n, "no", "yes"))
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
    
    output[[paste0('b', i)]] <- renderUI({
      
      # validate for the first one (so messages don't output 7 times)
      if (i == 1) {
        validate(need(input$biases, "Select biases"))
        if ("misclassification" %in% input$biases) {
          validate(need(input$misclassType, "Select outcome or exposure misclassification"))
        }
        if ("selection" %in% input$biases) {
          validate(need(input$pop_group_S, "Choose target population"))
        }
        if ("misclassification" %in% input$biases && input$misclassType == "exposure") {
          validate(need(input$rareOutcome, "Specify whether outcome is rare"),
                   need(input$rareExposure, 
                        "Specify whether exposure is rare"),
                   need(is.null(input$rareOutcome) || (!is.null(input$rareOutcome) && input$rareOutcome == "yes"), 
                        "Sorry! Currently only available for exposure misclassification with rare outcomes"))
        }
      } # end first one only
      
      summary_biases <- summary(biases(), latex = TRUE)
      # needs to be an object with a non-null number of rows
      req(nrow(summary_biases))
   
      # make the last one smaller so they match
      wi <- ifelse(i == 7, "50%", "100%")
      tagList(
        conditionalPanel(
          condition = paste0("output.a",i,"=='yes'"),
          
          numericInput(summary_biases$argument[i], 
                       label = summary_biases$latex[i], 
                       value = NA, min = 1, 
                       step = .1, width = wi)
        ),
        
        tags$script(paste0('renderMathInElement(document.getElementById("b', i, '"),
                        {delimiters: [{left: "$", right: "$", display: false}]});'))
      )
    }) # end b outputs
  }) # end lapply
}

# Run the application
shinyApp(ui = ui, server = server)