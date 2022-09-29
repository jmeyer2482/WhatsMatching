#' Info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Info_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
                    div.MathJax_Display{
                    text-align: left !important;
                    }"))
    ),

  fluidPage(
    tabsetPanel(type="pills",
      tabPanel("Causal Inference",
               span("Some info about matching.")),
      tabPanel("PSM Paradox",
         span("In 2019, King and Neilsen published a paper presenting a paradox that arises when using the propensity score for matching2. They were able to demonstrate that, when compared to other matching distances like the Mahalanobis distance, matching on the propensity score only approximated a randomised experiment as opposed to a more efficient fully blocked experiment. ", br(), br(),
              "As the matching algorithm proceeds, unmatched observations are pruned from the dataset. Initially, this reduces the imbalance between the treated and untreated groups with corresponding improvements to the estimated treatment effect. However, after a certain threshold is reached, further pruning the dataset starts to result in more biased estimates of the treatment effect. This is the PSM paradox. ", br(), br(),
              "The authors postulated that this made matching on the propensity score inferior and were able to demonstrate this concept visually but were unable to provide mathematical proofs of the implications.", br(), br(),
              "The question arises, does this paradox occur in the two other applications of the propensity score discussed above (weighting and stratification)? A review of recent literature would suggest not.")),
      tabPanel("Matching",
         span("Matching requires that a type of distance between units is calculated. This distance is then used to pair or match units that are close together while discarding or pruning those that are not. Once pruning has occurred to the satisfaction of the analyst, the treatment effect can be estimated on the pruned dataset. Many methods of matching have been explored in the literature and while the propensity score is perhaps the most used distance metric, other distance metrics are available, including the Mahalanobis distance.", br(), br(),
              "It is important to note that estimates should generally be based on the average treatment effect on the treated (ATT) when using matching methods. This makes sense intuitively as matching effectively removes individuals from the data that could not be considered both treated and untreated. This is referred to as strongly ignorable treatment assignment and is a key assumption for providing unbiased estimates with the propensity score.", br(), br(),
"Propensity score matching is usually conducted using nearest neighbour matching. The application of callipers can be used to control matching units that are considered too far apart. Matching can be performed with or without replacement and it should be noted that the order of the dataset when passed to the matching algorithm can play a significant role in how units are matched when matching without replacement.
")),
      tabPanel("Propensity Score",
         span("The propensity score allows the reduction of a multidimensional space to a single dimension for easy comparison between treated and untreated units. Mathematically, it can be formulated as", withMathJax("$$e(x_i) = pr(Z_i=1|x)$$"),
              "where \\(e(x_i)\\) is the propensity score, \\(Z\\) is the treatment variable, \\(x\\) is a vector of covariates, and \\(i\\) represents an individual. Therefore, the propensity score is the probability of receiving treatment given covariates \\(x\\).
              In observational data there is no ability to control which people receive treatment and which do not. In fact, there is often little control over the available covariates that may help inform the outcomes. This potentially exposes observational data to selection bias and confounding. The objective of using the propensity score in data analysis is to replicate a randomised experiment within observational data as a means of bias reduction and covariate balancing. The propensity score can be used in multiple way to achieve this, notably matching, weighting and stratification (aka subclassification)."),
         h4("Weighting"),
         span("Weighting with the propensity score differs from matching as no units are pruned from the dataset. This is an advantage over matching in the sense that all data can remain in the analysis, giving the opportunity to calculate the average treatment effect on all units (ATE), as opposed to the ATT.", br(), br(),
              "The formulas for weighting vary depending on the method you want to employ. The data being analysed, by virtue of its source, may be more appropriately analysed for the ATT and not the ATE. The formulas for these weights are:", br(), br(),
              div("$$ATE, w(W,x) = \\frac{W}{\\hat{e}(x)}+\\frac{1-W}{1-\\hat{e}(x)}$$"),
              div("$$ATE, w(W,x) = W+(1-W)\\frac{\\hat{e}(x)}{1-\\hat{e}(x)}$$"),
              div("where \\(\\hat{e}(x)\\) indicates the estimated propensity score conditional on observed x covariates.")
),
      h4("Stratification"),
         span("The stratification method varies from matching and weighting in that it relies on the use of the propensity score to group units into quantiles. Once the number of groups has been established, the literature suggests this is between five and ten depending on the size of the dataset5, the treatment effect is estimated across the groups and then averaged to give a mean difference ATE.")),
      tabPanel("References",
         span("Some text here"))

     )#tabpanel

    )#fluidpage

  )#taglist
}

#' Info Server Functions
#'
#' @noRd
mod_Info_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Info_ui("Info_1")

## To be copied in the server
# mod_Info_server("Info_1")
