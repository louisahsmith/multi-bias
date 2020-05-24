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
  #### intro tab -----------------------------------------------------
  tabPanel(
    title = "Introduction",
    
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
    
    # content to open on
    fluidRow(
      column(8,
             # info about multiple bias
             includeMarkdown("content/intro.md")
      ),
      column(4,
             wellPanel(includeMarkdown("content/intro_side.md"))
      )
    ),
    fluidRow(
      column(12,
             includeMarkdown("content/notation.md"))
    ),
    fluidRow(
      column(10, offset = 1, tabsetPanel(
        tabPanel("Vitamins and leukemia", wellPanel(includeMarkdown("content/leukemia.md"), style = "background: white")),
        tabPanel("HIV infection and wasting", wellPanel(includeMarkdown("content/hiv.md"), style = "background: white"))
      ))
    ),
    fluidRow(column(12))
    
  ), # end opening panel
  
  
  #### compute bound tab ------------------------------------------------
  tabPanel(
    title = "Compute bound",
    mainPanel(
      # this can go anywhere, just providing modal for assumptions
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
      h3("Biases of concern"),
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
      
      # selectInput(
      #   "outcomeType",
      #   label = h3("Outcome type"),
      #   choices = c(
      #     "Risk ratio" = "RR",
      #     "Hazard ratio" = "HR",
      #     "Odds ratio" = "OR"
      #   )
      # ), 
      
      
      # HTML('<label class="control-label">Estimated/hypothesized values for parameters</label>
      # <a href="#" data-toggle="modal" data-target="#modal_parameters">
      #      <i class="fa fa-info-circle"></i>
      #      </a>'),
      
      width = 6
    ) # end fluid column
    ), # ends computation input/output panel
    
    # panel for explaining how to calculate a bound 
    sidebarPanel(
      # includeMarkdown("content/choose_biases.md"),
      # display results
      h3("Necessary parameters to specify"),
      # wellPanel(span(uiOutput("result.text"))),
      
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
      
      
      width = 4
    ) # end explanation sidebar
  ), # end direct computation of bounds tab
  
  #### selection e-value tab ------------------------------------------
  # tabPanel(
  #   title = "E-values for selection bias",
  #   mainPanel(
  #     bs_modal(
  #       id = "modal_assumptions_S",
  #       title = "Additional assumptions",
  #       body = includeMarkdown("content/selection_assumptions.md"),
  #       size = "medium"
  #     ),
  #     bs_modal(
  #       id = "modal_parameters_S",
  #       title = "Parameter definitions",
  #       body = includeMarkdown("content/parameters.md"),
  #       size = "medium"
  #     ),
  #     bs_modal(
  #       id = "modal_pop_group_S",
  #       title = "Target population",
  #       body = includeMarkdown("content/target_pop.md"),
  #       size = "medium"
  #     ),
  #     selectInput(
  #       "outcomeType_S",
  #       label = "Outcome type",
  #       choices = c(
  #         "Risk ratio" = "RR",
  #         "Odds ratio" = "OR"
  #       )
  #     ), 
  #     
  #     # if they choose OR there's only one option
  #     conditionalPanel(
  #       condition = "input.outcomeType_S == 'OR'",
  #       p("Bound only available for odds ratio when bias is due to control selection in a case-control study. 
  #         For other types of selection bias, choose risk ratio or difference."),
  #       checkboxGroupInput(
  #         "control_sel_assump_S",
  #         label = "Necessary assumptions",
  #         choices = c(
  #           "No unmeasured confounding \\({(Y^a \\amalg A \\mid C)}\\)" = "no_confound",
  #           "Selection of cases independent of exposure \\({(S \\amalg A \\mid Y = 1, C )}\\)" = "case_indep",
  #           "Selection of controls independent of exposure, conditional on some possibly unmeasured factor(s) 
  #           \\(U\\) 
  #           \\({(S \\amalg A \\mid  Y = 0, U, C)}\\)" = "control_indep"
  #         ),
  #         selected = c("case_indep", "control_indep", "no_confound"),
  #         width = "100%"
  #       )
  #     ), # end conditional panel for control selection
  #     
  #     # if not OR, what is the target of inference
  #     conditionalPanel(
  #       condition = "input.outcomeType_S != 'OR'",
  #       radioButtons(
  #         "pop_group_S",
  #         label = HTML('<label class="control-label">Target population</label>
  #       <a href="#" data-toggle="modal" data-target="#modal_pop_group_S">
  #            <i class="fa fa-info-circle"></i>
  #            </a>'),
  #         choices = c(
  #           "Entire population" = "general",
  #           "Selected population" = "selected"
  #         ),
  #         selected = "general",
  #         width = "100%"
  #       )
  #     ),
  #     
  #     conditionalPanel( # "Inference only in selected population"
  #       condition = "input.outcomeType_S != 'OR' && input.pop_group_S == 'sel_pop'",
  #       
  #       checkboxGroupInput(
  #         "sel_pop_assump_S",
  #         label = "Necessary assumptions",
  #         choices = c(
  #           "No unmeasured confounding \\({(Y_a \\amalg A \\mid C)}\\)" = "no_confound",
  #           "All selection bias is captured by possibly unmeasured factor(s) \\(U\\) \\({(Y_a \\amalg A \\mid S = 1, U, C)}\\)" = 
  #             "U_indep"
  #         ),
  #         selected = c("U_indep", "no_confound"),
  #         width = "100%"
  #       )
  #      ), # end selected population option
  #     
  #     conditionalPanel(
  #       condition = "input.outcomeType_S != 'OR' && input.pop_group_S != 'sel_pop'",
  #       checkboxGroupInput(
  #         "whole_pop_assump_S",
  #         label = "Necessary assumptions",
  #         choices = c(
  #           "No unmeasured confounding \\({(Y_a \\amalg A \\mid C)}\\)" = "no_confound",
  #           "Selection is only related to outcome via unmeasured factor(s) \\(U\\) \\({(Y \\amalg S \\mid A, U, C)}\\)" = 
  #             "U_indep"
  #         ),
  #         selected = c("U_indep", "no_confound"),
  #         width = "100%"
  #       ),
  #       
  #       checkboxGroupInput(
  #         "assump_S",
  #         label = HTML('<label class="control-label">Additional assumptions</label>
  #       <a href="#" data-toggle="modal" data-target="#modal_assumptions_S">
  #            <i class="fa fa-info-circle"></i>
  #            </a>'),
  #         choices = c(
  #           "Unmeasured factor a defining characteristic of selection" = "S = U",
  #           "Selection always associated with increased risk of outcome in both exposure groups" = "increased risk",
  #           "Selection always associated with decreased risk of outcome in both exposure groups" = "decreased risk"
  #         ),
  #         width = "100%"
  #       )
  #     ),
  #     
  #     numericInput("est_S", "Point estimate", NA, min = 0),
  #     splitLayout(
  #       numericInput("lo_S", "CI lower limit (optional)", NA, min = 0),
  #       numericInput("hi_S", "CI upper limit (optional)", NA, min = 0)
  #     ),
  #     numericInput("true_S", "True causal effect to which to shift estimate", 1, min = 0),
  #     
  #     # display results
  #     wellPanel(span(textOutput(("result.text_S")))),
  #     uiOutput("message.text_S"),
  #     width = 6
  #   ), # ends panel for input/output of e-value
  #   
  #   # panel for info
  #   sidebarPanel(
  #     includeMarkdown("content/evalue_side.md"),
  #     width = 6
  #   ) # end e-value explanation sidebar
  # ), # end tab for computing e-values
  
  #### additional resources tab ----------------------------------------
  tabPanel(
    "More resources",
    mainPanel(
      includeMarkdown("content/resources.md"),
      
      # hide the text for keeping page from greying out (so doesn't create extra blank space)
      textOutput("keep_alive")
    )
  ),
  use_bs_popover()
)

#### server component ------------------------------------------------
server <- function(input, output, session) {
  # what will be blank text to prevent page grey-out
  output$keep_alive <- renderText({
    req(input$alive_count)
    input$alive_count
  })
  
  observe({
    
    # remove all extra assumptions if selected pop chosen
    if (!is.null(input$pop_group_S) && input$pop_group_S == "selected")
      x <- character(0) else x <- input$assump_S
      
      updateCheckboxGroupInput(session, "assump_S",
                               selected = x
      )
  })
  
  #### biases tab -----------------------------------------------
  
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
    
    
    # misclassification arguments -----
    
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
    
    arg1 <- if (yes_confounding) confounding()
    arg2 <- if (yes_selection) selection(selection_args)
    arg3 <- if (yes_misclassification) {
      poss_misclassification(misclassification_type, 
                        rare_outcome = rare_outcome, 
                        rare_exposure = rare_exposure)
    }
    
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
  })
  
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
  
  lapply(1:7, function(i) {
    
    output[[paste0('b', i)]] <- renderUI({
      
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
      }
      
      summary_biases <- summary(biases(), latex = TRUE)
      req(nrow(summary_biases))
      
      math <- summary_biases$latex
      args <- summary_biases$argument
      wi <- ifelse(i == 7, "50%", "100%")
      tagList(
        conditionalPanel(
          condition = paste0("output.a",i,"=='yes'"),
          
          numericInput(args[i], label = math[i], value = NA, min = 1, 
                       step = .1, width = wi)
        ),
        
        tags$script(paste0('renderMathInElement(document.getElementById("b', i, '"),
                        {delimiters: [{left: "$", right: "$", display: false}]});'))
      )
    })
  })
  
  
  # 
  # 
  # output$result.text <- renderUI({
  #   summary_biases <- summary(biases(), latex = TRUE)
  #   math <- summary_biases$latex
  #   args <- summary_biases$argument
  #   splitLayout(
  #     for (i in 1:length(math)) {
  #       numericInput(inputId = args[i], label = math[i], 
  #                    value = NA, min = 1),
  #     }
  # 
  #   )
  
  # Provide informative error messages
  # validate(
  #   need(
  #     (!all(c("risk_inc", "risk_dec") %in% input$assump) & 
  #        all(c("U_indep", "no_confound") %in% input$whole_pop_assump)) &
  #       all(c("U_indep", "no_confound") %in% input$sel_pop_assump) & 
  #       all(c("case_indep", "control_indep", "no_confound") %in% input$control_sel_assump),
  #     "You have missing or incompatible assumptions"
  #   ),
  #   need(
  #     (!anyNA(c(input$RRUA1, input$RRS0U, input$RRUA0, input$RRS1U)) & input$outcomeType == "OR") |
  #       (!anyNA(c(input$RRAUS1, input$RRUYS1)) & input$pop_group == "sel_pop") |
  #       (!anyNA(c(input$RRUY01, input$RRSU01, input$RRUY11, input$RRSU11)) & !"risk_inc" %in% input$assump & 
  #          !"risk_dec" %in% input$assump & !"S_eq_U" %in% input$assump) |
  #       (!anyNA(c(input$RRUY02, input$RRUY12)) & "S_eq_U" %in% input$assump & !"risk_dec" %in% input$assump & 
  #          !"risk_dec" %in% input$assump) |
  #       (!is.na(input$RRUY13) & "S_eq_U" %in% input$assump & "risk_dec" %in% input$assump & 
  #          !"risk_dec" %in% input$assump) | 
  #       (!is.na(input$RRUY03) & "S_eq_U" %in% input$assump & !"risk_dec" %in% input$assump & 
  #          "risk_dec" %in% input$assump) |
  #       (!anyNA(c(input$RRUY14, input$RRSU14)) & !"S_eq_U" %in% input$assump & 
  #          "risk_dec" %in% input$assump & !"risk_dec" %in% input$assump) |
  #       (!anyNA(c(input$RRUY04, input$RRSU04)) & !"S_eq_U" %in% input$assump & 
  #          !"risk_dec" %in% input$assump & "risk_dec" %in% input$assump),
  #     "Please enter values for the parameters above"
  #   )
  # )
  # sepr <- ifelse(input$outcomeType %in% c("RR", "OR"), "/", "-")
  # b <- paste0(
  #   "$", input$outcomeType, "_{true} \\geq ",
  #   input$outcomeType, "_{obs} ", sepr,
  #   round(bounds(), 2), "$"
  # )
  # tagList(
  #   tags$h3(b),
  #   # make sure math is printed
  #   tags$script('renderMathInElement(document.getElementById("result.text"), 
  #               {delimiters: [{left: "$", right: "$", display: false}]});')
  # )
  # })
  
  #### selection e-value tab -------------------------------------------
  # svals <- reactive({
  #   # Provide informative error messages
  #   validate(
  #     need(!is.na(input$est_S), "Please enter a point estimate"),
  #     need(
  #       (!all(c("risk_inc", "risk_dec") %in% input$assump_S) & 
  #          all(c("U_indep", "no_confound") %in% input$whole_pop_assump_S)) &
  #         all(c("U_indep", "no_confound") %in% input$sel_pop_assump_S) & 
  #         all(c("case_indep", "control_indep", "no_confound") %in% input$control_sel_assump_S),
  #       "You have missing or incompatible assumptions"
  #     ),
  #     need(!is.na(input$true_S), "Please enter a value to shift the estimate to")
  #   )
  #   
  #   sval_args <- list(
  #     est = input$est_S,
  #     lo = input$lo_S,
  #     hi = input$hi_S,
  #     true = input$true_S,
  #     sel_pop = (input$pop_group_S == 'sel_pop' & 
  #       input$outcomeType_S == "RR"),
  #     S_eq_U = ("S_eq_U" %in% input$assump_S & 
  #                 input$outcomeType_S == "RR"),
  #     risk_inc = ("risk_inc" %in% input$assump_S & 
  #                   input$outcomeType_S == "RR"),
  #     risk_dec = ("risk_dec" %in% input$assump_S & 
  #                   input$outcomeType_S == "RR")
  #   )
  #   # doesn't use function from EValue package anymore, but function from setup.R
  #   svals <- round(do.call(svalues, sval_args)[2, ], 2)
  #   
  #   return(svals)
  # })
  # 
  # # message that explains what parameters the selection bias e-value refers to
  # mess_S <- reactive({
  #   
  #   sel_pop <- input$pop_group_S == 'sel_pop'
  #   S_eq_U <- "S_eq_U" %in% input$assump_S
  #   risk_inc <- "risk_inc" %in% input$assump_S
  #   risk_dec <- "risk_dec" %in% input$assump_S
  #   cont_sel <- input$outcomeType_S == "OR"
  #   
  #   m1 <- "This value refers to the minimum value of each of"
  #   m3 <- "that would explain away your point estimate."
  #   
  #   if (sel_pop) {
  #     m2 <- "$\\text{RR}_{UY|S=1}$ and $\\text{RR}_{AU|S=1}$"
  #     return(paste(m1, m2, m3))
  #   }
  #   if (!S_eq_U & !risk_inc & !risk_dec & !cont_sel) {
  #     m2 <- "$\\text{RR}_{UY \\mid (A = 0)}$, $\\text{RR}_{UY \\mid (A = 1)}$, $\\text{RR}_{SU \\mid (A = 0)}$, $\\text{RR}_{SU \\mid (A = 1)}$"
  #     return(paste(m1, m2, m3))
  #   }
  #   if (!S_eq_U & !risk_inc & !risk_dec) {
  #     m2 <- "$\\text{RR}_{UA_1}$, $\\text{RR}_{UA_0}$, $\\text{RR}_{S_1U}$, $\\text{RR}_{S_0U}$"
  #     return(paste(m1, m2, m3))
  #   }
  #   if (S_eq_U & !risk_inc & !risk_dec) {
  #     m2 <- "$\\text{RR}_{UY \\mid (A = 0)}$ and $\\text{RR}_{UY \\mid (A = 1)}$"
  #     return(paste(m1, m2, m3))
  #   }
  #   if (S_eq_U & risk_inc) {
  #     m2 <- "$\\text{RR}_{UY \\mid (A = 1)}$"
  #     return(paste(m1, m2, m3))
  #   }
  #   if (S_eq_U & risk_dec) {
  #     m2 <- "$\\text{RR}_{UY \\mid (A = 0)}$"
  #     return(paste(m1, m2, m3))
  #   }
  #   if (risk_inc) {
  #     m2 <- "$\\text{RR}_{UY \\mid (A = 1)}$ and $\\text{RR}_{SU \\mid (A = 1)}$"
  #     return(paste(m1, m2, m3))
  #   }
  #   if (risk_dec) {
  #     m2 <- "$\\text{RR}_{UY \\mid (A = 0)}$ and $\\text{RR}_{SU \\mid (A = 0)}$"
  #     return(paste(m1, m2, m3))
  #   }
  # })
  # 
  # # print the selection bias e-value
  # output$result.text_S <- renderText({
  #   s <- svals()
  #   # if there is input for the CI (either lower or upper)
  #   if (!is.na(svals()[2]) | !is.na(svals()[3])) {
  #     sval.CI <- min(svals(), na.rm = TRUE)
  #     result.string_S <- paste("Selection bias E-value for point estimate: ", svals()[1],
  #                              ", and for confidence interval: ", sval.CI,
  #                              sep = ""
  #     )
  #     # if user only gave point estimate
  #   } else {
  #     result.string_S <- paste("Selection bias E-value for point estimate: ",
  #                              s[1],
  #                              sep = ""
  #     )
  #   }
  #   return(result.string_S)
  # })
  # 
  # # print message about what parameters e-value refers to
  # output$message.text_S <- renderUI({
  #   m <- mess_S()
  #   
  #   if (!is.na(input$true_S) & input$true_S != 1) m <- paste(m, nonnull.mess, sep = " ")
  #   tagList(
  #     helpText(m, tags$a(href = "#", "data-toggle" = "modal", "data-target" = "#modal_parameters_S",
  #                        tags$i(class = "fa fa-info-circle"))),
  #     # make sure math is printed
  #     tags$script('renderMathInElement(document.getElementById("message.text_S"),
  #                 {delimiters: [{left: "$", right: "$", display: false}]});')
  #   )
  # })
}

# Run the application
shinyApp(ui = ui, server = server)