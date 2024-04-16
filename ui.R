
#  --------------------------------------------------------------------
#
#  This file is part of Luna.
#
#  LUNA is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  Luna is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with Luna. If not, see <http://www.gnu.org/licenses/>.
#
#  Please see LICENSE.txt for more details.
#
#  --------------------------------------------------------------------

# source("ui.R"); source("server.R"); shinyApp( ui, server )

library(shiny)
library(luna)
library(shinybusy)
library(DT)
library(shinyFiles)
library(fs)
library(shinyjs)

# ------------------------------------------------------------
# Options

# Max EDF file size ( default = 200Mb ) here --> 3G
options(shiny.maxRequestSize = 3000 * 1024^2)

# set error handler for lunaR
lmoonlight_mode()

# M1, M2
pops.path <- "./pops"
pops.libs <- c("s2", "s2")
pops.versions <- c("20-Jan-2023", "20-Dec-2022")

# local mode: is default (look to 'host') unless running as server
cat("server mode MOONLIGHT_SERVER_MODE = [", Sys.getenv("MOONLIGHT_SERVER_MODE"), "]\n", sep = "")
local.mode <- Sys.getenv("MOONLIGHT_SERVER_MODE") == ""

# canonical file
canonical.sigs <- "https://gitlab-scm.partners.org/zzz-public/nsrr/-/raw/master/common/resources/canonical/harm.txt"
canonical.annots <- "https://gitlab-scm.partners.org/zzz-public/nsrr/-/raw/master/common/resources/canonical/annots.txt"

pal10 <- c(
  rgb(255, 88, 46, max = 255),
  rgb(1, 56, 168, max = 255),
  rgb(177, 212, 18, max = 255),
  rgb(255, 128, 237, max = 255),
  rgb(1, 199, 86, max = 255),
  rgb(171, 0, 120, max = 255),
  rgb(85, 117, 0, max = 255),
  rgb(251, 180, 179, max = 255),
  rgb(95, 32, 0, max = 255),
  rgb(164, 209, 176, max = 255)
)



# ------------------------------------------------------------
# Define UI

ui <- fluidPage( # theme = shinytheme("yeti"),

  # App title ----
  #  titlePanel( h3( "Moonlight/Luna" ) ),

  add_busy_spinner(spin = "fading-circle"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 2,
      h4("Moonlight/Luna"),
      if (local.mode) {
        shinyFilesButton("lfiles",
          label = "Load data",
          title = "Please select EDF/annotations files", multiple = T,
          style = "width: 100%"
        )
      } else {
        fileInput("files", label = NULL, multiple = T, accept = c(".edf", ".edfz", ".gz", ".idx", ".xml", ".annot", ".eannot"))
      },
      if (!local.mode) div(style = "margin-top: -15px"),
      textOutput("text.header1a"),
      hr(),
      #      div(style = "margin-top: -10px"),
      fluidRow(
        column(6, actionButton("moonbeam", "Moonbeam")),
        column(6, actionButton("load.default", "Example"))
      ),
      #      div(style = "margin-top: -10px"),
      hr(),

      # Select channels
      selectInput("channels",
        label = h5("Channels"),
        choices = list(),
        multiple = T,
        selectize = T
      ),

      # Select annotations
      div(style = "margin-top: -10px"),
      selectInput("annots",
        label = h5("Annotations"),
        choices = list(),
        multiple = T,
        selectize = T
      ),
      div(style = "margin-top: -10px"),
      selectInput("psd.ch",
        label = h5("Spectrogram"),
        choices = list(), multiple = F, selectize = F
      ),
      div(style = "margin-top: -10px"),
      selectInput("disp.ann",
        h5("Listed annotations"), list(),
        multiple = TRUE, selectize = TRUE
      ),
      div(style = "margin-top: -10px"),
      selectInput("sel.inst", h5("Instances"), list(),
        multiple = TRUE, selectize = FALSE
      ),
      fluidRow(
        column(6, actionButton("reepoch", "Re-epoch")),
        column(6, actionButton("reset", "Refresh"))
      )
    ),


    #
    # Main panel for displaying outputs ----
    #

    mainPanel(
      width = 10,
      br(),
      plotOutput("psd.plot", width = "100%", height = "80px"),
      plotOutput("hypnogram",
        width = "100%", height = "40px",
        dblclick = "hypno_dblclick",
        brush = brushOpts(id = "hypno_brush", direction = "x", resetOnNew = F)
      ),
      plotOutput("mask.plot", width = "100%", height = "20px"),
      div(style = "margin-top: 10px"),
      tabsetPanel(
        id = "maintabs",
        tabPanel(
          "Moonbeam",
          selectizeInput("moonbeam.indivs", label = "Individuals", choices = NULL, multiple = F, options = list(maxOptions = 10000)),
          hr(),
          DT::dataTableOutput("moonbeam.pheno")
        ),
        tabPanel(
          "Headers",
          fluidRow(
            column(4, DT::dataTableOutput("table.header3")),
            column(8, DT::dataTableOutput("table.header2"))
          )
        ),
        tabPanel(
          "Structure",
          tabsetPanel(
            id = "tab1",
            tabPanel(
              "Segments",
              textOutput("text.segments"),
              plotOutput("plot.segments", width = "100%", height = "150px"),
              DT::dataTableOutput("table.segments")
            ),
            tabPanel(
              "Epochs",
              hr(col = "white"),
              fluidRow(
                column(4, textOutput("basic.ecount"), DT::dataTableOutput("epoch.table1")),
                column(4, textOutput("aligned.ecount"), DT::dataTableOutput("epoch.table2")),
                column(4, textOutput("selected.ecount"), DT::dataTableOutput("epoch.table3"))
              )
            ),
          ),
        ),
        tabPanel(
          "Hypnogram",
          tabsetPanel(
            tabPanel("Summaries", DT::dataTableOutput("table.hypno", width = "100%")),
            tabPanel("Times", DT::dataTableOutput("table.hypno.times", width = "100%")),
            tabPanel("Stages", DT::dataTableOutput("table.hypno.stages")),
            tabPanel("Cycles", DT::dataTableOutput("table.hypno.cycles")),
            tabPanel("Epochs", DT::dataTableOutput("table.hypno.epochs")),
            tabPanel(
              "Stage annotations",
              fluidRow(
                column(2, selectInput("hypno.n1", label = h5("N1"), choices = list(), multiple = F, selectize = F)),
                column(2, selectInput("hypno.n2", label = h5("N2"), choices = list(), multiple = F, selectize = F)),
                column(2, selectInput("hypno.n3", label = h5("N3"), choices = list(), multiple = F, selectize = F)),
                column(2, selectInput("hypno.r", label = h5("R"), choices = list(), multiple = F, selectize = F)),
                column(2, selectInput("hypno.w", label = h5("W"), choices = list(), multiple = F, selectize = F))
              ),
              fluidRow(
                column(2, selectInput("hypno.u", label = h5("?"), choices = list(), multiple = F, selectize = F)),
                column(2, selectInput("hypno.l", label = h5("L"), choices = list(), multiple = F, selectize = F))
              ),
              hr(), actionButton("hypno.assign", "Assign")
            ),
            tabPanel(
              "SOAP",
              br(),
              fluidRow(
                column(3, selectInput("soap.ch", label = h5("Channel"), choices = list(), multiple = F, selectize = F)),
                column(1, hr(), actionButton("soap.run", "Run SOAP"))
              ),
              plotOutput("plot.soap", width = "100%", height = "125px"),
              fluidRow(
                column(6, DT::dataTableOutput("table.soap", width = "95%")),
                column(6, DT::dataTableOutput("table.soap.stages", width = "95%"))
              )
            ),
            tabPanel(
              "POPS",
              fluidRow(
                column(
                  9,
                  tabsetPanel(
                    id = "popstabs",
                    tabPanel(
                      "M1",
                      fluidRow(
                        column(4, selectInput("pops.m1.eeg1", label = h5("EEG (C4-M1)"), choices = list(), multiple = F, selectize = F))
                      )
                    ),
                    tabPanel(
                      "M2",
                      fluidRow(
                        column(4, selectInput("pops.m2.eeg1", label = h5("EEG1 (C3-M2)"), choices = list(), multiple = F, selectize = F)),
                        column(4, selectInput("pops.m2.eeg2", label = h5("EEG2 (C4-M1)"), choices = list(), multiple = F, selectize = F))
                      )
                    )
                  )
                ),
                column(
                  3, hr(), shinyjs::useShinyjs(), actionButton("pops.run", "Run POPS", width = "100%"),
                  actionButton("do.cmref.pops", "Do CM re-referencing", width = "100%"),
                  checkboxInput("popsshap", label = "SHAP", value = F),
                  checkboxInput("pops.filter", label = "Pre-filter", value = T),
                  downloadButton("download.pops", label = "Download POPS staging", style = "width:100%;"),
                  actionButton("pops.hypnogram", "Use POPS staging", width = "100%"),
                  actionButton("orig.hypnogram", "Use observed staging", width = "100%")
                )
              ),
              tabsetPanel(
                tabPanel(
                  "Summaries", plotOutput("plot.pops", width = "100%", height = "150px"),
                  fluidRow(
                    column(6, DT::dataTableOutput("table.pops")),
                    column(6, DT::dataTableOutput("table.pops.stages"))
                  )
                ),
                tabPanel("Epochs", DT::dataTableOutput("table.pops.epochs")),
                tabPanel(
                  "Features", fluidRow(
                    column(3, selectInput("sel.pops.features2", h5("Features"), list(), multiple = T, selectize = F)),
                    column(8, hr(col = "white"), plotOutput("plot.pops.features", width = "100%", height = "150px"))
                  ),
                  DT::dataTableOutput("sel.pops.features"),
                  tags$head(tags$style("#sel.pops.features2{height: 800px; width: 20px; font-size: 100px;"))
                )
              )
            )
          )
        ),
        tabPanel(
          "Annots",
          plotOutput("annot.view", width = "100%", height = "175px"),
          br(),
          tabsetPanel(
            tabPanel("Summary", dataTableOutput("annot.summary")),
            tabPanel("Instances", dataTableOutput("annot.table"))
          )
        ),
        tabPanel(
          "Signals",
          fluidRow(
            column(width = 1, offset = 0, actionButton("button_epoch_prv", " < Prev", width = "100%")),
            column(width = 1, actionButton("button_epoch_nxt", "Next > ", width = "100%")),
            column(width = 1, actionButton("winin", "In", width = "100%")),
            column(width = 1, actionButton("winex", "Out", width = "100%")),
            column(width = 1, offset = 0, actionButton("entire.record", "All", width = "100%")),
            column(width = 1, offset = 0, actionButton("bandpass", "Filter", width = "100%")),
            column(width = 3, offset = 0, sliderInput("flt.freq", NULL,
              width = "100%",
              min = 0, max = 100, step = 0.25, value = c(0.3, 35)
            )),
            column(width = 3, htmlOutput("info2"))
          ),
          plotOutput("signal.master",
            width = "100%", height = "30px", click = "master_click", dblclick = "master_dblclick",
            brush = brushOpts(id = "master_brush", direction = "x", resetOnNew = F)
          ),
          plotOutput("signal.master2", width = "100%", height = "10px"),
          br(),
          plotOutput("signal.view",
            width = "100%", height = "50vh", dblclick = "zoom_dblclick",
            brush = brushOpts(id = "zoom_brush", direction = "x", resetOnNew = F)
          ),
          hr()
        ),
        tabPanel(
          "Stats",
          tabsetPanel(
            tabPanel("Channel", DT::dataTableOutput("cstats.table")),
            tabPanel("Epoch", DT::dataTableOutput("estats.table"))
          )
        ),
        tabPanel(
          "Time/freq",
          tabsetPanel(
            tabPanel(
              "Spectrogram",
              fluidRow(
                column(2, selectInput("mtm.ch", label = h5("Channel(s)"), choices = list(), multiple = F, selectize = F)),
                column(2, numericInput("mtm.flwr", label = h5("Lower freq. (Hz)"), min = 0, max = 256, value = 0.5)),
                column(2, numericInput("mtm.fupr", label = h5("Upper freq. (Hz)"), min = 0, max = 256, value = 25)),
                column(2, numericInput("mtm.winsor", label = h5("Winsorization"), min = 0, max = 0.4, value = 0.02, step = 0.01)),
                column(2, hr(col = "white"), actionButton("do.mtm", "Run MTM"))
              ), hr(),
              plotOutput("mtm.view1", width = "100%", height = "75px", click = "mtm1_click"),
              plotOutput("mtm.tr12", width = "100%", height = "25px"),
              plotOutput("mtm.view2", width = "100%", height = "75px", click = "mtm2_click"),
              plotOutput("mtm.tr23", width = "100%", height = "25px"),
              plotOutput("mtm.view3", width = "100%", height = "75px"),
              plotOutput("mtm.view4", width = "100%", height = "75px")
            ),
            tabPanel(
              "Hjorth",
              fluidRow(
                column(1, HTML("Winsorization:")),
                column(2, numericInput("sigsumm.winsor", label = NULL, min = 0, max = 0.4, value = 0.02, step = 0.01)),
                column(2, actionButton("do.sigsumm", "Build"))
              ),
              plotOutput("sigsumm.view1", width = "100%", height = "325px", hover = hoverOpts(id = "sigsumm_hover")),
              hr(col = "white"),
              plotOutput("sigsumm.view2", width = "100%", height = "125px")
            ),
            tabPanel(
              "ExE",
              fluidRow(
                column(2, selectInput("exe.ch", label = h5("Channel(s)"), choices = list(), multiple = F, selectize = F)),
                column(2, numericInput("exe.m", label = h5("m"), NULL, min = 3, max = 7, value = 5, step = 1)),
                column(2, numericInput("exe.t", label = h5("t"), NULL, min = 1, max = 20, value = 1, step = 1)),
                column(2, numericInput("exe.rep", label = h5("Splits"), NULL, min = 1, max = 20, value = 5, step = 1)),
                column(2, numericInput("exe.win", label = h5("Winsorization"), NULL, min = 0, max = 0.4, value = 0.02, step = 0.01)),
                column(2, hr(col = "white"), actionButton("do.exe", "Run ExE"))
              ), hr(),
              fluidRow(
                column(6, plotOutput("exe.view1", width = "100%", height = "75px")),
                column(6, plotOutput("exe.view2", width = "100%", height = "75px"))
              ),
              fluidRow(
                column(
                  6, plotOutput("exe.hypno1", width = "100%", height = "30px"),
                  plotOutput("exe.mat", width = "100%", height = "500px", hover = hoverOpts(id = "exe_hover"))
                ),
                column(
                  6, plotOutput("exe.hypno2", width = "100%", height = "30px"),
                  plotOutput("exe.clst", width = "100%", height = "500px")
                )
              ),
              hr(col = "white"),
            )
          )
        ),
        tabPanel(
          "Manips",
          tabsetPanel(
            tabPanel(
              "Re-reference",
              fluidRow(
                column(4, selectInput("reref1", label = h5("Channel(s)"), choices = list(), multiple = T, selectize = F)),
                column(4, selectInput("reref2", label = h5("Reference(s)"), choices = list(), multiple = T, selectize = F)),
                column(2, hr(col = "white"), actionButton("doreref", "Re-reference"), hr(col = "white"), actionButton("do.cmref", "CM reference"))
              )
            ),
            tabPanel(
              "Resample",
              fluidRow(
                column(4, selectInput("resample", label = h5("Channel(s)"), choices = list(), multiple = T, selectize = F)),
                column(4, numericInput("resamplerate", label = h5("Sample rate (Hz)"), min = 10, max = 256, value = 128)),
                column(4, hr(col = "white"), actionButton("doresample", "Resample"))
              )
            ),
            tabPanel(
              "Bandpass filter",
              fluidRow(
                column(4, selectInput("filter", label = h5("Channel(s)"), choices = list(), multiple = T, selectize = F)),
                column(
                  2, numericInput("flwr", label = h5("Lower freq. (Hz)"), min = 0, max = 256, value = 0.3),
                  numericInput("ftw", label = h5("Transition width (Hz()"), min = 0, max = 50, value = 0.5)
                ),
                column(
                  2, numericInput("fupr", label = h5("Upper freq. (Hz)"), min = 0, max = 256, value = 35),
                  numericInput("fripple", label = h5("Ripple"), min = 0, max = 1, value = 0.02)
                ),
                column(4, hr(col = "white"), actionButton("dofilter", "Filter"))
              )
            ),
            tabPanel(
              "Rename",
              fluidRow(
                column(4, selectInput("renameold", label = h5("Channel"), choices = list(), multiple = F, selectize = F)),
                column(4, textInput("renamenew", label = h5("New label"))),
                column(4, hr(col = "white"), actionButton("dorename", "Rename channels"))
              )
            ),
            tabPanel(
              "Drop",
              fluidRow(
                column(4, selectInput("drop", label = h5("Channel(s)"), choices = list(), multiple = T, selectize = F)),
                column(4, hr(col = "white"), actionButton("dodrop", "Drop channels"))
              )
            ),
            tabPanel(
              "Copy",
              fluidRow(
                column(4, selectInput("copyold", label = h5("Channel"), choices = list(), multiple = F, selectize = F)),
                column(4, textInput("copytag", label = h5("New tag"))),
                column(4, hr(col = "white"), actionButton("docopy", "Copy channel"))
              )
            ),
            tabPanel(
              "Transform",
              fluidRow(
                column(4, selectInput("transch", label = h5("Channel"), choices = list(), multiple = F, selectize = F)),
                column(4, textInput("transexp", label = h5("Expression"))),
                column(4, hr(col = "white"), actionButton("dotrans", "Transform"))
              )
            ),
            tabPanel(
              "Mask",
              fluidRow(
                column(1, radioButtons("mask.inc", "Mask", c("Include" = "1", "Exclude" = "0"))),
                column(4, selectInput("mask.annots", label = h5("Annotations"), choices = list(), multiple = T, selectize = F)),
                column(4, textInput("mask.expr", label = h5("Expression"), width = "100%")),
                column(
                  3, hr(col = "white"), actionButton("domask", "Set"),
                  actionButton("flipmask", "Flip"),
                  actionButton("clearmask", "Clear")
                )
              )
            ),
            tabPanel(
              "Map channels", hr(col = "white"),
              fluidRow(
                column(9, textAreaInput("canonical", NULL,
                  width = "100%", height = "250px", resize = "none",
                  placeholder = "(Enter CANONCAL mappings here, or insert NSRR defaults)"
                )),
                column(3, actionButton("mapchs", "Map"), actionButton("addnsrr", "Insert NSRR defaults"))
              ),
              fluidRow(column(6, DT::dataTableOutput("csmappings")), column(6, DT::dataTableOutput("chmappings"))),
              hr(col = "white")
            ),
            tabPanel(
              "Map annots", hr(col = "white"),
              fluidRow(
                column(9, textAreaInput("remaps", NULL,
                  width = "100%", height = "250px", resize = "none",
                  placeholder = "(Enter annotation remappings here, or insert NSRR defaults)"
                )),
                column(3, actionButton("mapanns", "Map"), actionButton("addnsrr_anns", "Insert NSRR defaults"))
              ),
              DT::dataTableOutput("annmappings"),
              hr(col = "white")
            ),
          ),
          verbatimTextOutput("manipout", placeholder = T),
          tags$head(tags$style("#manipout{color:black; font-size:9px;
                                        overflow-y:scroll; height: 150px; background: ghostwhite;}")),
          tags$head(tags$style("#reref1{height: 175px; width: 175px; ")),
          tags$head(tags$style("#reref2{height: 175px; width: 175px; ")),
          tags$head(tags$style("#drop{height: 175px; width: 175px; ")),
          tags$head(tags$style("#resample{height: 175px; width: 175px; "))
        ),
        tabPanel(
          "Models",
          tabsetPanel(
            tabPanel(
              "Adult age prediction",
              fluidRow(
                column(
                  6,
                  textOutput("mod1.lab", inline = FALSE),
                  hr(col = "white"),
                  textOutput("mod1.inp", inline = FALSE),
                  textOutput("mod1.out", inline = FALSE),
                  hr(col = "white"),
                  textOutput("mod1.notes", inline = FALSE)
                ),
                column(
                  6,
                  fluidRow(
                    column(6, selectInput("mod1.ch", label = h5("Central channel(s)"), choices = list(), multiple = T, selectize = F)),
                    column(
                      5, numericInput("mod1.age", label = h5("Age (years)"), min = 40, max = 90, value = 50),
                      numericInput("mod1.th", label = h5("Missing threshold (SD)"), min = 0, max = 10, value = 5),
                      actionButton("do.mod1", "Predict")
                    )
                  )
                )
              ),
              hr(col = "white"),
              DT::dataTableOutput("mod1.out1", width = "100%"),
              hr(col = "white"),
              DT::dataTableOutput("mod1.out2", width = "100%")
            ),
            tabPanel(
              "Norms",
              textOutput("norm.lab", inline = FALSE), hr(col = "white"),
              fluidRow(
                column(
                  2, numericInput("norm.age", label = h5("Age (years)"), min = 0, max = 88, value = 40),
                  radioButtons("norm.sex", "Sex", c("Male" = "M", "Female" = "F")),
                  selectInput("norm.eegF", label = h5("Frontal"), choices = list(), multiple = F, selectize = T),
                  selectInput("norm.eegC", label = h5("Central"), choices = list(), multiple = F, selectize = T),
                  selectInput("norm.eegO", label = h5("Occipital"), choices = list(), multiple = F, selectize = T)
                ),
                column(10, plotOutput("norm.plots", width = "100%", height = "500px"))
              )
            )
          )
        ),
        tabPanel(
          "Luna",
          tabsetPanel(
            tabPanel(
              "Commands", hr(col = "white"),
              fluidRow(column(1, textOutput("label.luna.sigs")), column(11, textOutput("text.luna.sigs"))),
              fluidRow(column(1, textOutput("label.luna.annots")), column(11, textOutput("text.luna.annots"))),
              hr(col = "white"),
              fluidRow(
                column(9, textAreaInput("eval", NULL,
                  width = "100%", height = "60px", resize = "none",
                  placeholder = "(Enter Luna commands here)"
                )),
                column(1, actionButton("go", "Execute"))
              ),
              fluidRow(
                column(9, verbatimTextOutput("evalout", placeholder = T)),
                column(3, selectInput("evalsel", label = h5("Outputs"), choices = list(), multiple = F, selectize = F))
              ),
              tags$head(tags$style("#evalout{color:black; font-size:9px;
                                        overflow-y:scroll; height: 150px; background: ghostwhite;}")),
              DT::dataTableOutput("evaltab", width = "100%")
            ),
            tabPanel(
              "Plots",
              hr(col = "white"),
              fluidRow(
                column(4, selectInput("plotT", label = h5("Tables"), choices = list(), multiple = F, selectize = F)),
                column(2, selectInput("plotX", label = h5("X-axis"), choices = list(), multiple = F, selectize = F)),
                column(2, selectInput("plotY", label = h5("Y-axis"), choices = list(), multiple = F, selectize = F)),
                column(2, selectInput("plotZ", label = h5("Stratifier"), choices = list(), multiple = T, selectize = F))
              ),
              uiOutput("ui_plotter")
            )
          )
        )
      )
    )
  )
)
