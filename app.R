
# POPS models do not swap expected # of predictors.
# canonical page

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

# source("app.R"); shinyApp(ui, server)

library(shiny)
library(luna)
library(shinybusy)
library(shinythemes)
library(DT)

# ------------------------------------------------------------
# Options

# Max EDF file size ( default = 200Mb ) here --> 3G
options(shiny.maxRequestSize = 3000 * 1024^2)

# set error handler for lunaR
lmoonlight_mode()

pops.path     = "./pops"
pops.libs     = c( "s1" , "v1" )
pops.versions = c( "11-Jan-2023", "31-Dec-2022" )

# canonical file

canonical.sigs    <- "./canonical/signals.txt"
canonical.annots  <- "./canonical/annots.txt"


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



# ------------------------------------------------------------
# Define UI 

ui <- fluidPage( #theme = shinytheme("yeti"),

# App title ----
#  titlePanel( h3( "Moonlight/Luna" ) ),
   
   add_busy_spinner(spin = "fading-circle") , 

# Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel( width = 2 , 

      # File input
      fileInput( "files" ,
      		 "Moonlight/Luna", 
                 multiple = T ,
		 accept = c( ".edf", ".edfz", ".gz", ".idx" , ".xml" , ".annot" , ".eannot" ) ),

      textOutput( "text.header1a" ),

      # Select channels
      selectInput( "channels",
      		   label = h5("Channels"), 
    		   choices = list(),
		   multiple=T,
		   selectize=T ),

      # Select annotations
      selectInput( "annots",
      		   label = h5("Annotations"), 
    		   choices = list(),
		   multiple=T,
		   selectize=T ),

      selectInput( "psd.ch" , 
                    label = h5("Spectrogram"),
                    choices = list(), multiple=F, selectize=F ),                    

      selectInput("disp.ann",
                  h5("Listed annotations"), list(),
		  multiple = TRUE, selectize = TRUE),

      selectInput("sel.inst", h5("Instances"), list(),
                  multiple = TRUE, selectize = FALSE),

      fluidRow( column(6,actionButton("reepoch", "Re-epoch") ),
                column(6,actionButton("reset", "Refresh") ) )


    ),


    #
    # Main panel for displaying outputs ----
    #

    mainPanel( width=10, 
     br(),
     plotOutput( "psd.plot" , width = "100%", height = "80px"),
     plotOutput( "hypnogram" , width = "100%", height = "40px",
                  dblclick = "hypno_dblclick",
                  brush = brushOpts(id = "hypno_brush", direction = "x", resetOnNew = F)) ,
     plotOutput( "mask.plot" , width = "100%", height = "15px") ,
     hr(style = "border-top: 1px solid #000000;"),

     tabsetPanel(

        tabPanel("Header",
	  textOutput( "text.header1b" ) ,
          hr(),
          DT::dataTableOutput( "table.header2" ) 
	 ), 

        tabPanel("Harmonize",
          tabsetPanel( 
            tabPanel("Channels", hr( col="white" ) , 
                  fluidRow( column( 9 , textAreaInput("canonical" , NULL , width = '100%' , height = '250px' , resize='none' ,
					placeholder = "(Enter additional CANONCAL mappings here, or insert NSRR defaults)" ) )  ,
                            column( 3 , actionButton("mapchs", "Map") , actionButton( "addnsrr" , "Insert NSRR defaults" ) ) ) ,
		  fluidRow( column( 6 , DT::dataTableOutput( "csmappings" ) ) , column( 6 , DT::dataTableOutput( "chmappings" ) ) ) 		  
	   ) , 
           tabPanel("Annotations",
	   )
	   )
        ),

        tabPanel("Segments",
          textOutput( "text.segments" ),
	  plotOutput( "plot.segments" , width='100%' , height='150px' ),
          DT::dataTableOutput( "table.segments" )
          ),

        tabPanel("Epochs",
           hr( col="white" ),
	   fluidRow( column( 4 , textOutput( "basic.ecount" ) , DT::dataTableOutput("epoch.table1") ) ,
                     column( 4 , textOutput( "aligned.ecount" ) , DT::dataTableOutput("epoch.table2") ) ,
		     column( 4 , textOutput( "selected.ecount" ) , DT::dataTableOutput("epoch.table3") ) )
	),
	
        tabPanel("Hypnogram",
          tabsetPanel(
           tabPanel("Summaries", DT::dataTableOutput( "table.hypno" , width='100%' ) ) ,
	   tabPanel("Times", DT::dataTableOutput( "table.hypno.times" , width='100%' ) ) ,
	   tabPanel("Stages", DT::dataTableOutput( "table.hypno.stages"  ) ) ,
	   tabPanel("Cycles",DT::dataTableOutput( "table.hypno.cycles"  ) ) ,
	   tabPanel("Epochs",DT::dataTableOutput( "table.hypno.epochs" ) ) )
          ),
            
        tabPanel( "SOAP",
	  br(),
	  fluidRow( column(3,selectInput( "soap.ch" , label = h5("Channel"), choices = list(), multiple=F,selectize=F ) ),
                    column(1,hr(),actionButton("soap.run", "Run SOAP" ) ) ),
          plotOutput( "plot.soap" , width="100%" , height = "125px"  ), 
	  fluidRow( column(6, DT::dataTableOutput( "table.soap" , width="95%" ) ) ,
                    column(6, DT::dataTableOutput( "table.soap.stages" , width="95%" ) ) ) ) ,

        tabPanel( "POPS",

          fluidRow(

           column( 9 ,
	     tabsetPanel( id = "popstabs",

             tabPanel( "M1" ,
	       fluidRow(
	        column(4, selectInput("pops.m1.eeg1", label=h5("EEG (C4-M1)"), choices=list(),multiple=F,selectize=F ) ) ) ) ,

             tabPanel( "M2" ,
	       fluidRow(
	        column(4, selectInput("pops.m2.eeg1", label=h5("EEG1 (C3-M2)"), choices=list(),multiple=F,selectize=F )),
	        column(4, selectInput("pops.m2.eeg2", label=h5("EEG2 (C4-M1)"), choices=list(),multiple=F,selectize=F )),
	        column(4, selectInput("pops.m2.eog",  label=h5("EOG (E1-M2)"), choices=list(),multiple=F,selectize=F ))) ) 

                 )
                ) , 

          column(3, hr(), actionButton("pops.run", "Run POPS" ) , checkboxInput("popsshap", label = "SHAP", value = F) ) ) , 
		    
          tabsetPanel( 
  	   tabPanel( "Summaries" , plotOutput( "plot.pops" , width="100%" , height = "150px"  ),
                                   fluidRow( column(6, DT::dataTableOutput( "table.pops" ) ) ,
				             column(6, DT::dataTableOutput( "table.pops.stages" ) ) ) ) ,
	   tabPanel( "Epochs"    , DT::dataTableOutput( "table.pops.epochs" ) ) , 

           tabPanel( "Features"  , fluidRow( column(3, selectInput("sel.pops.features2", h5("Features"), list(),multiple = T, selectize = F ) ),
                                             column(8, hr(col="white"),plotOutput( "plot.pops.features" , width="100%" , height = "150px" ) ) ) , 
                                   DT::dataTableOutput( "sel.pops.features" ) ) ,
           tags$head(tags$style("#sel.pops.features2{height: 800px; width: 20px; font-size: 100px;")) )
         ),

        tabPanel("Annots",
          plotOutput("annot.view", width = "100%", height = "175px"),
          br(),
         tabsetPanel(
	   tabPanel("Summary", dataTableOutput("annot.summary")) ,
	   tabPanel("Instances", dataTableOutput("annot.table")))
          ),

         tabPanel("Signals",
          fluidRow(
            column(width = 1, offset = 0, actionButton("button_epoch_prv", " < Prev", width = "100%")),
            column(width = 1, actionButton("button_epoch_nxt", "Next > ", width = "100%")),
            column(width = 1,  actionButton("winin" , "In" , width='100%')),
	    column(width = 1,  actionButton("winex" , "Out" , width='100%')),
	    column(width = 1, offset = 0, actionButton("entire.record", "All", width = "100%")),
	    column(width = 1, offset = 0, actionButton("bandpass", "Filter", width = "100%")),
            column(width = 3, offset = 0, sliderInput("flt.freq", NULL, width = "100%",
       	                                  min = 0, max = 100, step = 0.25, value = c(0.3, 35)) ),
            column(width = 3, textOutput("info2") )
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


       tabPanel("Manips",
         tabsetPanel(
           tabPanel("Re-reference" , 
	            fluidRow(
		     column( 4 , selectInput( "reref1" , label = h5("Channel(s)"), choices = list(), multiple=T, selectize=F ) ) ,
		     column( 4 , selectInput( "reref2" , label = h5("Reference(s)"), choices = list(), multiple=T, selectize=F ) ) ,
		     column( 4 , hr(col="white"), actionButton("doreref", "Re-reference" ) ) ) ) ,
           tabPanel("Resample" ,
                    fluidRow(
                     column( 4 , selectInput( "resample" , label = h5("Channel(s)"), choices = list(), multiple=T, selectize=F ) ) ,
                     column( 4 , numericInput("resamplerate", label = h5("Sample rate (Hz)"), min = 10 , max = 256 , value = 128 ) ) ,
                     column( 4 , hr(col="white"), actionButton("doresample", "Resample" ) ) ) ) ,
	   tabPanel("Rename" ,
	            fluidRow(
                     column( 4 , selectInput( "renameold" , label = h5("Channel"), choices = list(), multiple=F, selectize=F ) ) ,
                     column( 4 , textInput("renamenew", label = h5("New label") ) ) ,
                     column( 4 , hr(col="white"), actionButton("dorename", "Rename channels" ) ) ) ) ,
           tabPanel("Drop" ,
                    fluidRow(
                     column( 4 , selectInput( "drop" , label = h5("Channel(s)"), choices = list(), multiple=T, selectize=F ) ) ,
                     column( 4 , hr(col="white"), actionButton("dodrop", "Drop channels" ) ) ) ) ,
           tabPanel("Copy" ,
	   	    fluidRow(
                     column( 4 , selectInput( "copyold" , label = h5("Channel"), choices = list(), multiple=F, selectize=F ) ) ,
                     column( 4 , textInput("copytag", label = h5("New tag") ) ),
                     column( 4 , hr(col="white"), actionButton("docopy", "Copy channel" ) ) ) ) ,
	   tabPanel("Transform" ,
	            fluidRow(
                     column( 4 , selectInput( "transch" , label = h5("Channel"), choices = list(), multiple=F, selectize=F ) ) ,
                     column( 4 , textInput("transexp", label = h5("Expression") ) ),
                     column( 4 , hr(col="white"), actionButton("dotrans", "Transform" ) ) ) ) 
	   ),
        tags$head(tags$style("#reref1{height: 270px; width: 175px; ") ) ,
        tags$head(tags$style("#reref2{height: 270px; width: 175px; ") ) ,
        tags$head(tags$style("#drop{height: 270px; width: 175px; ") ) , 
        tags$head(tags$style("#resample{height: 270px; width: 175px; ") ) 
       ) , 


       tabPanel("Luna", textOutput( "text.luna.sigs" ), textOutput( "text.luna.annots" ),  
 		  fluidRow( column( 9 , textAreaInput("eval" , NULL , width = '100%' , height = '60px' , resize='none' ,
		                        placeholder = "(Enter Luna commands here)" ) )  ,
			    column( 1 , actionButton("go", "Execute") ) ) , 			    
	           fluidRow( column( 9 , verbatimTextOutput( "evalout" , placeholder= T ) ),    
		   	     column( 3 , selectInput( "evalsel" , label = h5("Outputs"),choices = list(),multiple=F,selectize=F ))),
		  tags$head(tags$style("#evalout{color:black; font-size:9px; 
                                        overflow-y:scroll; height: 150px; background: ghostwhite;}")),    
		  DT::dataTableOutput( "evaltab" , width='100%' ) ) ,
		  
       tabPanel("Plots",
            hr( col="white" ) , 
	    fluidRow( column( 4 , selectInput( "plotT" , label = h5("Table"), choices = list(), multiple=F, selectize=F ) ) ,
	              column( 2 , selectInput( "plotX" , label = h5("X-axis"), choices = list(), multiple=F, selectize=F ) ) ,  
                      column( 2 , selectInput( "plotY" , label = h5("Y-axis"), choices = list(), multiple=F, selectize=F ) ) ,
		      column( 2 , selectInput( "plotZ" , label = h5("Stratify"), choices = list(), multiple=T, selectize=F ) ) ) ,
		      uiOutput("ui_plotter") )
    )
  )
 )
)


# ------------------------------------------------------------
#
# Implementation
#
# ------------------------------------------------------------

server <- function(input, output, session ) {

# ------------------------------------------------------------
# main store

values <- reactiveValues( opt = list() )

observeEvent( input$files , {

#cat( file=stderr(), "in file reader...\n" )

# clear all
 values$opt  <- NULL
 values$soap <- NULL
 values$pops <- NULL
 values$view <- NULL
 values$evalout <- NULL
 values$nz <- 1
 values$canonical <- NULL
 values$LOFF <- values$LON <- "." 
 
# other cleares
    updateSelectInput(
       session,
       "evalsel",
       choices = "" , 
       label = "" , 
       selected = 0
      )

    updateSelectInput(
       session,
       "plotT",
       choices = "" , 
       label = NULL , 
       selected = 0
      )

# EDFs (pick first if >1)
idx <- grep( ".edf" , ignore.case = T , input$files$name )
values$hasedf <- length(idx) >= 1
if ( values$hasedf ) {
 values$opt[[ "edfname" ]] = input$files$name[ idx[1] ]
 values$opt[[ "edfpath" ]] = input$files$datapath[ idx[1] ] 
}

# annotations
idx <- c( grep( ".xml" , ignore.case = T , input$files$name ) ,
          grep( ".annot" , ignore.case = T , input$files$name ) ,
	  grep( ".eannot" , ignore.case = T , input$files$name ) )
values$hasannots <- length(idx) >= 1
if ( values$hasannots ) { 
 values$opt[[ "annotnames" ]] = input$files$name[ idx ]
 values$opt[[ "annotpaths" ]] = input$files$datapath[ idx ]
} 
values$hasdata <- values$hasedf | values$hasannots

cat( " has data?" , values$hasdata , "\n" )
cat( " has edf?" , values$hasedf , "\n" )
cat( " has annotations?" , values$hasannots , "\n" )

# process EDF
if ( values$hasedf) {

# attach EDF
ledf( values$opt[[ "edfpath" ]] )

# and any annotations
for (a in values$opt[[ "annotpaths" ]] )
    ladd.annot.file( a )

# initial analysis
   init()
cat("done init\n")
# update channels & annots
   update()
cat("done update\n")
}

})


# ------------------------------------------------------------
# Initial analysis (done once on load only)
#

init <- function() {



# epoch recording & SEGMENTS
  cat( "init raw EPOCHs and SEGMENTS\n" )
  ret <- leval( "EPOCH verbose & SEGMENTS" )
#  print( ret$EPOCH )
#  print( ret$EPOCH$E )
  
  values$opt[[ "init.epochs" ]] <- ret$EPOCH$E
  values$opt[[ "ne" ]] <- dim(ret$EPOCH$E)[1]
  values$opt[[ "init.segidx" ]] <- ret$SEGMENTS$SEG[ , c("START","STOP") ]
  session$resetBrush("hypno_brush")

# get stage-aligned epochs and hypnogram
  cat( "init hypnogram\n" )
    ret <- leval( "EPOCH align verbose" )
    values$opt[[ "ne.aligned" ]] <- dim(ret$EPOCH$E)[1]
    values$opt[[ "init.epochs.aligned" ]] <- ret$EPOCH$E

    ret <- leval( "HYPNO epoch" )
    values$hasstaging <- !is.null(lstages())
    if ( values$hasstaging ) { 
     values$opt[[ "hypno.stats" ]]  <- ret$HYPNO$BL
     values$opt[[ "hypno.epochs" ]] <- ret$HYPNO$E
     values$opt[[ "all.hypno.epochs" ]] <- ret$HYPNO$E
     values$opt[[ "ss" ]]           <- ret$HYPNO$E[ , c("E","STAGE") ] 
     values$opt[[ "hypno.cycles" ]] <- ret$HYPNO$C
     values$opt[[ "hypno.stages" ]] <- ret$HYPNO$SS
#     print( head( values$opt[[ "hypno.epochs" ]] ) ) 
  }
   
# signal view parameter defaults
      values$view[[ "epochs" ]]      <- c(1, 1)
      values$view[[ "zoom" ]]        <- NULL
      values$view[[ "raw.signals" ]] <- T
      values$view[[ "bandpass" ]]    <- F
      values$view[[ "bpflt" ]]       <- c(0.3,35)

# report
cat( "from init()\n" )
cat( " # epochs (raw)" , values$opt[[ "ne" ]] , "\n" )
cat( " # epochs (stage-aligned)" , values$opt[[ "ne.aligned" ]] , "\n" )
cat( " has-staging?" , values$hasstaging , "\n" )

}


# ------------------------------------------------------------
# update hypnogram

update.hypnogram <- function() {
    req( values$hasstaging  )
    ret <- leval( paste( "HYPNO epoch lights-off=" , values$LOFF , " lights-on=" , values$LON , sep="" ) )
    values$opt[[ "hypno.stats" ]]  <- ret$HYPNO$BL
    values$opt[[ "hypno.epochs" ]] <- ret$HYPNO$E
    values$opt[[ "all.hypno.epochs" ]] <- ret$HYPNO$E
    values$opt[[ "ss" ]]           <- ret$HYPNO$E[ , c("E","STAGE") ] 
    values$opt[[ "hypno.cycles" ]] <- ret$HYPNO$C
    values$opt[[ "hypno.stages" ]] <- ret$HYPNO$SS
}

# ------------------------------------------------------------
# update channels/annots, etc
# called at init, but also called after functions
#  e.g. as they may have added a new channel, etc

update <- function() { 

isolate( {

  cat( "in update()\n..." )

# get HEADERS/ANNOTS (raw eppochs)
    ret <- leval( "SEGMENTS & HEADERS & ANNOTS & DUMP-MASK" )
#print( ret$HEADERS$BL  )

    # check records set? 
    if ( is.null( ret$HEADERS$BL  ) )
    {
     showModal(modalDialog(
       title = "No unmasked records left" , 
      "Please Refresh or reload a valid EDF" , 
      easyClose = TRUE
    ))
  req( ! is.null( ret$HEADERS$BL  ) )
}

    values$opt[[ "header1" ]] <- ret$HEADERS$BL
    values$opt[[ "header1" ]]$EPOCH <- values$opt[[ "header1" ]]$TOT_DUR_SEC / 30.0

    values$opt[[ "header2" ]] <- ret$HEADERS$CH
    values$opt[[ "header2" ]] <- values$opt[[ "header2" ]][, c("CH","PDIM","SR","PMIN","PMAX","TRANS") ]
    names( values$opt[[ "header2" ]] ) <- c("Channel","Unit","Sample-rate","Min","Max","Transducer")    

# segments
   values$opt[[ "curr.segsumm" ]]  <- ret$SEGMENTS$BL
   values$opt[[ "curr.segidx" ]] <- ret$SEGMENTS$SEG[ , c("START","STOP") ]
   values$opt[[ "curr.segments" ]] <- ret$SEGMENTS$SEG[ , c("SEG","START_HMS","STOP_HMS","DUR_MIN","DUR_SEC","START","STOP") ]
   values$opt[[ "curr.segments" ]]$SEG <- paste( "Seg", values$opt[[ "curr.segments" ]]$SEG )
   if ( ret$SEGMENTS$BL$NGAPS > 0 ) { 
     t2 <- ret$SEGMENTS$GAP[ , c("GAP","START_HMS","STOP_HMS","DUR_MIN","DUR_SEC","START","STOP") ]
     t2$GAP <- paste( "Gap", t2$GAP, sep=" " )
     names(t2)[1] <- "SEG"
     values$opt[[ "curr.segments" ]] <- rbind( values$opt[[ "curr.segments" ]] , t2 )
     values$opt[[ "curr.segments" ]] <- values$opt[[ "curr.segments" ]][ order( values$opt[[ "curr.segments" ]]$START ) , ]
          
   }
  values$opt[[ "curr.segments" ]]$START <- round( values$opt[[ "curr.segments" ]]$START , 2 )
  values$opt[[ "curr.segments" ]]$STOP <- round( values$opt[[ "curr.segments"]]$STOP , 2 )
  values$opt[[ "curr.segments" ]]$DUR_SEC <- round( values$opt[[ "curr.segments"]]$DUR_SEC , 2 )
  values$opt[[ "curr.segments" ]]$DUR_MIN <- round( values$opt[[ "curr.segments"]]$DUR_MIN , 2 )
  names( values$opt[[ "curr.segments" ]] ) <- c( "Segment" , "Start", "Stop", "Duration(m)", "Duration(s)" , "Start(s)", "Stop(s)"  )
   
# Channels
    values$opt[[ "chs" ]] <- ret$HEADERS$CH$CH

# Sample rates
    values$opt[[ "sr" ]] <- ret$HEADERS$CH$SR
    names(values$opt[[ "sr" ]]) <- as.character(ret$HEADERS$CH$CH)

# Channel units
    values$opt[[ "units" ]] <- ret$HEADERS$CH$PDIM
    names(values$opt[[ "units" ]]) <- as.character(ret$HEADERS$CH$CH)
    
# Channel type
    values$opt[[ "type" ]] <- ret$HEADERS$CH$TYPE

# Annots
#  (skip 'SleepStage' and special annots)
    skips <- c( "SleepStage","duration_hms","duration_sec","epoch_sec","start_hms" ) 
    values$opt[[ "annots" ]]      <- ret$ANNOTS$ANNOT$ANNOT[ ! ret$ANNOTS$ANNOT$ANNOT %in% skips ]
    values$hasannots              <- length( values$opt[[ "annots" ]] ) > 0
    values$opt[[ "annots.summ" ]] <- ret$ANNOTS$ANNOT[ ret$ANNOTS$ANNOT$ANNOT != "SleepStage" , ] 
    values$opt[[ "annots.inst" ]] <- ret$ANNOTS$ANNOT_INST_T1_T2[ ret$ANNOTS$ANNOT_INST_T1_T2$ANNOT != "SleepStage" , ] 

# Update channel lists
    updateSelectInput(
       session,
       "channels",
       choices = values$opt[[ "chs" ]],
       label = paste(length(values$opt[[ "chs" ]]), "channels"),
       selected = 0
      )

    updateSelectInput(
       session,
       "annots",
       choices = values$opt[[ "annots" ]],
       label = paste(length(values$opt[[ "annots" ]]), "annotations"),
       selected = 0
      )

      updateSelectInput(
        session,
        "disp.ann",
        choices = values$opt[[ "annots" ]],
        label = paste(length(values$opt[[ "annots" ]]), "listed annotations"),
        selected = 0
      )

# manips

    updateSelectInput( session, "reref1",   choices = values$opt[[ "chs" ]] , label = NULL , selected = 0 )
    updateSelectInput( session, "reref2",   choices = values$opt[[ "chs" ]] , label = NULL , selected = 0 )
    updateSelectInput( session, "resample", choices = values$opt[[ "chs" ]] , label = NULL , selected = 0 )
    updateSelectInput( session, "drop",      choices = values$opt[[ "chs" ]] , label = NULL , selected = 0 )
    updateSelectInput( session, "transch",   choices = values$opt[[ "chs" ]] , label = NULL , selected = 0 )
    updateSelectInput( session, "copyold",   choices = values$opt[[ "chs" ]] , label = NULL , selected = 0 )    
    updateSelectInput( session, "renameold", choices = values$opt[[ "chs" ]] , label = NULL , selected = 0 )

# others



      clear_sel_inst()

    s50 <- values$opt[[ "chs" ]][ values$opt[[ "sr" ]] >= 50 ]
    t50 <- values$opt[[ "type" ]][ values$opt[[ "sr" ]] >= 50 ]
    first.eeg <- which( t50 == "EEG" )[1]

    updateSelectInput(
       session,
       "psd.ch",
       choices = s50 , 
       selected = ifelse( is.na( first.eeg ) , 0 , s50[ first.eeg ]  ) 
      )

    updateSelectInput(
       session,
       "soap.ch",
       choices = s50 , 
       selected = 0
      )

    updateSelectInput( session, "pops.m1.eeg1", choices = s50, selected = 0 )
    
    updateSelectInput( session, "pops.m2.eeg1", choices = s50, selected = 0 )
    updateSelectInput( session, "pops.m2.eeg2", choices = s50, selected = 0 )
    updateSelectInput( session, "pops.m2.eog", choices = s50, selected = 0 )

    # Get mask
    values$opt[[ "unmasked" ]] <- ret$DUMP_MASK$E$E[ ret$DUMP_MASK$E$EMASK == 0 ]
    values$opt[[ "included" ]] <- ret$DUMP_MASK$E$E
})


}




# ------------------------------------------------------------
# Process an arbitary Luna command

  observeEvent(input$go, {
     req( values$hasedf )

     values$evalout <- c( input$eval , "\n" , capture.output( values$opt[[ "k" ]] <- leval( input$eval ) ) , "\n" )

     l1 <- character()
     for (i in names(values$opt[[ "k" ]]))
      for (j in names(values$opt[[ "k" ]][[i]]) )
       l1 <- c( l1 , paste( i , j , sep=" : " ) )

     updateSelectInput(
          session,
          "evalsel", 
          choices = l1 , 
          label = paste(length(l1), " tables") 
        )

     updateSelectInput(
          session,
          "plotT", 
          choices = l1 , 
          label = h5( "Luna tables" )
        )

updateTextAreaInput(
     session,
     "eval",
     label = NULL,
     value = "",
     placeholder = "(Enter Luna commands here)" )

   # update channels, annots, etc
   update()
 
  })


# ------------------------------------------------------------
# Populate tabular output, depends on evalsel

 output$text.luna.sigs <- renderText({
     req( values$hasedf )
     paste( "Channels:" , paste( values$opt[[ "chs" ]] , collapse="  " ) )
  })

 output$text.luna.annots <- renderText({
     req( values$hasedf )
     paste( "Annotations:" , paste( values$opt[[ "annots" ]] , collapse="  " ) )
  })

 output$evaltab <- DT::renderDataTable({
   req( values$hasedf , input$evalsel , values$opt[[ "k" ]] )
   tok <- unlist( strsplit( input$evalsel , " : " ) ) 
   df <- values$opt[[ "k" ]][[ tok[1] ]][[ tok[2] ]]
   df$ID <- NULL
   df[ is.na( df ) ] <- "." 

   DT::datatable( df, 
                   options = list(scrollY = '380px' ,
		               scrollX = '100%' , 
                               dom="tB" ,
			       buttons = list( list(extend = "copy", text = "Copy") ) ,
                               paging = F , ordering=F,
                               info = FALSE ,
                               searching = FALSE ,
                               columnDefs = list(list(className = "dt-center", targets = "_all"))),
                 rownames= FALSE )

  })


 output$evalout <- renderText({
  req(  values$evalout )
  paste0( values$evalout , sep="\n" )
 })


# ------------------------------------------------------------
# List EDF headers

output$text.header1a <- renderText({
    req( values$hasedf )
    values$opt[[ "edfname" ]] 
})

output$text.header1b <- renderText({
    req( values$hasedf )    
c( values$opt[[ "header1" ]]$EDF_TYPE, "with" , 
   values$opt[[ "header1" ]]$NS , "signals |" ,
   "start date" , values$opt[[ "header1" ]]$START_DATE ,
   "| clocktime", values$opt[[ "header1" ]]$START_TIME ,
   "-" , values$opt[[ "header1" ]]$STOP_TIME , "| duration" , 
   values$opt[[ "header1" ]]$TOT_DUR_HMS , "|" , values$opt[[ "header1" ]]$TOT_DUR_SEC , " secs |",
   floor( values$opt[[ "header1" ]]$TOT_DUR_SEC/30.0 ) , " epochs" )
} )


# ------------------------------------------------------------
# Channel-wise EDF headers output

  output$table.header2 <- DT::renderDataTable({

    req( values$hasedf )

    DT::datatable( values$opt[[ "header2" ]] ,
                   extensions = c('Buttons'),
                   options = list(scrollY = '300px' , 
		                  paging = F ,
				  info = FALSE , 
                                  searching = FALSE,
				  dom = 'tB', buttons = list( list(extend = "copy", text = "Copy") ) , 
				  columnDefs = list(list(className = "dt-center", targets = "_all"))),
                   rownames= FALSE )
  })


 output$basic.ecount <- renderText( {
   req( values$hasedf )
   paste( values$opt[[ "ne" ]] , "original epochs" )   
  })

 output$aligned.ecount <- renderText( {
   req( values$hasedf )
   paste( values$opt[[ "ne.aligned" ]] , "stage-aligned original epochs" )   
  })

 output$selected.ecount <- renderText( {
   req( values$hasedf , values$opt[[ "curr.ne" ]] != 0 )
   paste( values$opt[[ "curr.ne" ]] , "stage-aligned current epochs" )
  })


 # ------------------------------------------------------------
 # Segments (EDF+D info)
 # 

 output$text.segments <- renderText( {
   req( values$hasedf )
   paste( "Selected data comprise",
           values$opt[[ "curr.segsumm"]]$NSEGS ,
	   "segment(s) and" , values$opt[[ "curr.segsumm" ]]$NGAPS , "gap(s)" )
  })

 output$plot.segments <- renderPlot( {
   req( values$hasedf )
   par(mar=c(0,0,0,0))
   plot(c(0,0),c(1,1),type="l")   
   #init.epochs
   #curr.epochs
   #mns <- min( values$opt[[ "init.epochs" ]]$START )
   mns <- 0
   mxs <- max( values$opt[[ "init.epochs" ]]$STOP )
   plot( c( mns , mxs ) , c( 0 , 1 ) , type="n" , xlim=c(mns,mxs) , xaxs="i")

   # dur of extracted
   nsec <- sum( values$opt[[ "curr.segidx" ]]$STOP - values$opt[[ "curr.segidx" ]]$START )
   p1 <- mns + ( mxs - mns )/2 - nsec/2
   p2 <- mns + ( mxs - mns )/2 + nsec/2

   # concatenated current, w/ lines
   for (s in 1:length(values$opt[[ "curr.segidx"]]$START) ) {
    o1 = values$opt[[ "curr.segidx"]]$START[s]
    o2 = values$opt[[ "curr.segidx"]]$STOP[s]
    n1 = p1
    n2 = p1 + ( o2 - o1 )
    rect( n1 , 0.15 , n2 , 0.25 , col= ifelse( s %% 2 , 1 , 2 ) )
    lines( c(o1,o1) , c(0.55,0.75) , col="gray" )
    lines( c(o2,o2) , c(0.55,0.75) , col="gray" )
    lines( c(o1,n1) , c(0.45,0.25) , col="gray" )
    lines( c(o2,n2) , c(0.45,0.25) , col="gray" )    
    p1 = n2    
  }
  # original
  for (s in 1:length(values$opt[[ "init.segidx"]]$START) )
   rect( values$opt[[ "init.segidx"]]$START[s] , 0.75 ,
         values$opt[[ "init.segidx"]]$STOP[s] , 0.85 ,
	 col= ifelse( s %% 2 , 3 , 4 ) )

  # curr
  for (s in 1:length(values$opt[[ "curr.segidx"]]$START) )
    rect( values$opt[[ "curr.segidx"]]$START[s] , 0.45 , 
          values$opt[[ "curr.segidx"]]$STOP[s] , 0.55 ,
	  col= ifelse( s %% 2 , 5 , 6 ) )

  # labels
  h1 <- round( sum( values$opt[[ "init.segidx"]]$STOP - values$opt[[ "init.segidx"]]$START ) / 3600 , 2 )
  h2 <- round( sum( values$opt[[ "curr.segidx"]]$STOP - values$opt[[ "curr.segidx"]]$START ) / 3600 , 2 )
  text( mns + ( mxs - mns )/2 , 0.9 , paste( "Original (" , h1 , " hrs)",sep="") )
  text( mns + ( mxs - mns )/2 , 0.6 , paste( "Masked (" , h2 , " hrs)",sep="") )
  text( mns + ( mxs - mns )/2 , 0.10 , "Masked, concatenated" )
  })



  output$table.segments <- DT::renderDataTable({
    req( values$hasedf )
    DT::datatable( values$opt[[ "curr.segments"]] ,
                   options = list(scrollY = '250px' ,		                  
                                  info = FALSE ,
                                  searching = FALSE,
				  dom = 'tB', buttons = list( list(extend = "copy", text = "Copy") ) ,
	                          columnDefs = list(list(className = "dt-center", targets = "_all"))),
                   rownames= FALSE)
  })


 # ------------------------------------------------------------
 # Hypnogram
 # 

 output$hypnogram <- renderPlot( {
   req( values$hasedf , values$hasstaging )
   par(mar=c(0,0,0,0))
   lhypno( values$opt[[ "hypno.epochs"]]$STAGE ,
   	   cycles = values$opt[[ "hypno.epochs"]]$CYCLE ,
	   times = values$opt[[ "init.epochs.aligned" ]]$START )
  })


  observeEvent(input$hypno_dblclick, {
      session$resetBrush("hypno_brush")
      values$opt[[ "hypno.epochs" ]] <- values$opt[[ "all.hypno.epochs" ]]
       values$LOFF <- values$LON <- "."
       update.hypnogram()
    })

    observeEvent(input$hypno_brush, {
      brush <- input$hypno_brush
      # x axis is in hrs
      ne <- values$opt[[ "ne" ]]
      if ( ! is.null( brush ) )
      {
        # 120=60^2/30
        mine <- floor( brush$xmin * 120 ) + 1
	maxe <- ceiling( brush$xmax * 120 ) + 1
	cat( mine, maxe, "is min/max E\n" )
	values$opt[[ "hypno.epochs" ]]$STAGE <- rep( "L" , ne )
        print( values$opt[[ "hypno.epochs" ]] )
        values$opt[[ "hypno.epochs" ]]$STAGE[ mine:maxe ] <- values$opt[[ "all.hypno.epochs" ]]$STAGE[ mine:maxe ]
        print( values$opt[[ "hypno.epochs" ]] )
	# set LON / LOFF in seconds
	values$LOFF <- brush$xmin * 3600
	values$LON <- brush$xmax * 3600
	update.hypnogram()
      }
    })


 # ------------------------------------------------------------
 # Hypnogram statistics
 # 

  output$table.hypno <- DT::renderDataTable({
    req( values$hasedf , values$hasstaging )

 m <- as.data.frame( matrix( 
  c( "TRT","Total Recording Time, based on scored epochs (T0 – T6) (mins)",
     "TIB","Time In Bed: Lights Off to Lights On (mins) (T1 – T5) (mins)",
     "SPT","Sleep period time: Sleep Onset to Final Wake (T2 – T4) (mins)",
     "SPT_PER","Persistent Sleep Period time: Persistent Sleep Onset to Final Wake (mins)",
     "LOT","Lights On Time (mins)",
     "TWT","Total Wake time during Lights Off = SLP_LAT + WASO + FWT (mins)",
     "TST", "Total Sleep Time (mins)" ,
     "TST_PER", "Total Persistent Sleep Time (mins)" ,
     " " , "" , 
     "SME", "Sleep maintenance efficiency, TST / SPT (denom. = T2 – T4)",
     "SE",  "Sleep efficiency, TST / TIB (denom. = T1 – T5)",   
     "WASO","Wake time between sleep onset and final wake onset (T2 – T4)",
     "FWT", "Duration of wake from final wake onset to Lights on (T4 – T5)",
     "SOL", "Sleep latency (T1 – T2)",
     "SOL_PER","Persistent sleep latency (T1 to onset of persistent sleep)",
     "REM_LAT","REM latency (sleep onset T2 – first REM epoch)",
     "REM_LAT2","REM latency excluding W, i.e. elapsed NR at REM onset",
     "NREMC","Number of NREM cycles",
     "NREMC_MINS","Mean NREM cycle duration (mins)",
     "  ", "" , 
     "CONF","Number of epochs w/ conflicting stages (should be 0)",
     "SINS","Recording starts in sleep (0=N, 1=Y)",
     "EINS","Recording ends in sleep (0=N, 1=Y)",
     "OTHR","Duration of non-sleep/non-wake annotations (unknown/movement)",  
     "FIXED_WAKE",   "Excessive leading/trailing Wake epochs set to L",
     "FIXED_LIGHTS", "Number of epochs set or L before/after lights out/on",
     "FIXED_SLEEP",  "Sleep epochs set to L due to extreme WASO intervals",
     "LOT" , "Lights On duration (mins)",
     "LOST","Lights On Sleep duration (mins) : sleep set to L (should be 0)",   
     "   " , "" , 
     "SFI","Sleep Fragmentation Index: Sleep/W transition count / TST" ,
     "TI_S","Stage Transition Index (excludes W): N1/N2/N3/R transition count / TST",
     "TI_S3","3-class Stage Transition Index: NR/R/W transition count / SPT" ,
     "TI_RNR","REM/NREM Transition Index, NR/R transition count / TST",  
     "LZW","LZW complexity index" ), 
      ncol = 2 , byrow = T ) )

# spacers
values$opt[[ "hypno.stats" ]][ , " " ] <- NA
values$opt[[ "hypno.stats" ]][ , "  " ] <- NA
values$opt[[ "hypno.stats" ]][ , "   " ] <- NA 

# add in values
m$VALUE <- round( as.numeric( values$opt[[ "hypno.stats"]][ , m[,1] ]  )  , 3 ) 

 DT::datatable( m ,
                options = list(scrollY = '380px' ,
                               dom="tB" ,  buttons = list( list(extend = "copy", text = "Copy") ) ,
		               ordering=F, paging = F , 
		               info = FALSE , 
                               searching = FALSE ),
                 rownames= FALSE , colnames = rep("", 3) )  
  })


 output$table.hypno.times <- DT::renderDataTable({
    req( values$hasedf , values$hasstaging )
m <- as.data.frame( matrix( 
 c("X0_START" ,    "Study Start" ,
 "X1_LIGHTS_OFF" , "Lights Off time (or start of recording)" ,
 "X2_SLEEP_ONSET", "Sleep Onset time" ,
 "X3_SLEEP_MIDPOINT" , "Mid-point of T2 & T4" ,
 "X4_FINAL_WAKE","Final Wake Onset time",
 "X5_LIGHTS_ON","Lights On time (or end of recording)",
 "X6_STOP","Study Stop"),
      ncol = 2 , byrow = T ) )
m$HMS <- as.character( values$opt[[ "hypno.stats"]][ , gsub("X","HMS",m[,1] ) ] )
m$E   <- round( as.numeric( values$opt[[ "hypno.stats"]][ , gsub("X","E", m[,1] ) ] )  , 3 )
m$T   <- round( as.numeric( values$opt[[ "hypno.stats"]][ , gsub("X","T", m[,1] ) ] )  , 3 )

DT::datatable( m ,
                options = list(ordering=F, pageLength = dim(m)[1] ,
                               buttons = list( list(extend = "copy", text = "Copy") ) ,
			       lengthChange = FALSE ,
		               dom = "tB"  , info = FALSE , 
                               searching = FALSE ,
			       columnDefs = list(list(className = "dt-left", targets = "_all"))),
                 rownames= FALSE ,
		 colnames = c("Time-point","Description", "Clock-time", "Elapsed (mins)", "Time past prior midnight (hrs)" ) )
		         
  })




 # ------------------------------------------------------------
 # Hypnogram stage statistics
 #
 
  output$table.hypno.stages <- DT::renderDataTable({

    req( values$hasedf , values$hasstaging )

    dt <- values$opt[[ "hypno.stages" ]]
    dt <- dt[ dt$SS %in% c("?","N1","N2","N3","R","S","W","WASO") , ]
    dt <- dt[ , c("SS","MINS","PCT","BOUT_MD","BOUT_N") ]
    dt$PCT <- round( 100 * dt$PCT , 2 )
    names(dt) <- c( "Stage","Duration(m)","Duration(%)","Median-bout(m)","N-bouts")
    DT::datatable( dt , 
                   options = list( scrollY = '300px',
		                   dom = 'tB', buttons = list( list(extend = "copy", text = "Copy") ) ,
                                   info = FALSE ,
                                   searching = FALSE,
                                   columnDefs = list(list(className = "dt-center", targets = "_all"))),
                   rownames= FALSE)
  })


 # ------------------------------------------------------------
 # Hypnogram cycle statistics
 #

  output$table.hypno.cycles <- DT::renderDataTable({

    req( values$hasedf , values$hasstaging )
    dt <- values$opt[[ "hypno.cycles"]]
    dt$NUM <- 1:(dim(dt)[1])
    dt <- dt[ , c( "NUM", "NREMC_START" , "NREMC_N" , "NREMC_MINS", "NREMC_NREM_MINS", "NREMC_REM_MINS" ) ]
    names(dt) <- c( "Cycle" , "Start(E)" , "Duration(E)", "Duration(m)" , "NREM-duration(m)", "REM-duration(m)" )
    DT::datatable( dt , 
                   options = list(scrollY = '300px',
                                  dom = 'tB', buttons = list( list(extend = "copy", text = "Copy") ) ,
				  info = FALSE ,
                                  searching = FALSE,
                                  columnDefs = list(list(className = "dt-center", targets = "_all"))),
                   rownames= FALSE)
  })


 # ------------------------------------------------------------
 # Hypnogram epoch-statistics
 #
 
  output$table.hypno.epochs <- DT::renderDataTable({

    req( values$hasedf , values$hasstaging )

    dt <- values$opt[[ "hypno.epochs" ]]
    dt <- dt[ , c("E","CLOCK_TIME","MINS","STAGE","CYCLE","PERSISTENT_SLEEP","WASO","E_N1","E_N2","E_N3","E_REM","E_SLEEP","E_WASO") ]
    names(dt) <- c("E","Clock","Mins","Stage","Cycle","Per-Sleep","WASO","E(N1)","E(N2)","E(N3)","E(REM)","E(S)","E(WASO)" )
#    dt$PCT <- round( dt$PCT , 2 )
    DT::datatable( dt , 
                   options = list(scrollY = '350px',
                                  dom = 'tB', buttons = list( list(extend = "copy", text = "Copy") ) ,
                                  info = FALSE , paging = F , 
                                  searching = FALSE,
                                  columnDefs = list(list(className = "dt-center", targets = "_all"))),
                   rownames= FALSE)
  })


# ------------------------------------------------------------
 # PSD analysis
 #

 output$psd.plot <- renderPlot( {
    req( input$psd.ch )
    cmd <- paste( "PSD epoch-spectrum max=25 dB sig" , input$psd.ch , sep="=" )
    ret <- leval( cmd )
    par(mar=c(0,0,0,0))
    lheatmap( ret$PSD$CH_E_F$E , ret$PSD$CH_E_F$F , ret$PSD$CH_E_F$PSD , win=0.05)
 })

 # ------------------------------------------------------------
 # Mask plot
 #

 output$mask.plot <- renderPlot( {
    req( values$hasedf )
    inc <- 1:values$opt[[ "ne" ]] %in% values$opt[[ "included" ]]
    unmsk <- 1:values$opt[[ "ne" ]] %in% values$opt[[ "unmasked" ]]
    col <- "white"
    col[ inc ] <- "ivory3"
    col[ unmsk ] <- "orange2"
    par(mar=c(0,0,0,0))
    plot( 1:values$opt[[ "ne" ]] ,
          rep(1,values$opt[[ "ne" ]]), pch="|" ,
          xaxt="n", yaxt="n", xaxs="i", 
          col = col )
})


# ------------------------------------------------------------
# Epoch tables

   # raw epochs
   output$epoch.table1 <- DT::renderDataTable({
      req( values$hasdata )
      df <- values$opt[[ "init.epochs" ]][ , c( "E", "HMS" , "INTERVAL" ) ] 
      names(df) <- c("Epoch","Time","Interval(secs)" )
      DT::datatable( df ,
                   options = list(scrollY = '300px' , 
				  dom = 'tB', buttons = list( list(extend = "copy", text = "Copy") ) ,
                                  paging = F , info = F , searching = F,
                                  columnDefs = list(list(className = "dt-center", targets = "_all"))),
                   rownames= FALSE)      
    })

   # stage-aligned epochs
   output$epoch.table2 <- DT::renderDataTable({
      req( values$hasdata )
      df <- values$opt[[ "init.epochs.aligned"]][ , c( "E", "HMS" , "INTERVAL" ) ] 
      names(df) <- c("Epoch","Time","Interval(secs)" )
      DT::datatable( df ,
                   options = list(scrollY = '300px' ,
		                  dom = 'tB', buttons = list( list(extend = "copy", text = "Copy") ) ,
                                  paging = F , info = F , searching = F,
                                  columnDefs = list(list(className = "dt-center", targets = "_all"))),
                   rownames= FALSE)
   })

   # selected, stage-aligned epochs
   output$epoch.table3 <- DT::renderDataTable({
      req( values$hasdata , values$opt[[ "curr.ne"]] != 0 )
      df <- values$opt[[ "curr.epochs"]][ , c( "E", "HMS" ) ]
      df$EMASK <- as.integer( ! df$E %in% values$opt[[ "unmasked" ]] ) 
      df$E <- paste( df$E , " (" , 1:length(df$E) , ")" , sep="" ) 
      names(df) <- c("Epoch","Time","Masked" )
      DT::datatable( df ,
                   options = list(scrollY = '300px' ,
		                  dom = 'tB', buttons = list( list(extend = "copy", text = "Copy") ) ,
                                  paging = F , info = F , searching = F,
                                  columnDefs = list(list(className = "dt-center", targets = "_all"))),
                   rownames= FALSE)
   })


# ------------------------------------------------------------
# Annotations

    output$annot.view <- renderPlot({
      req( values$hasdata  )
      # get annotationss
      df <- values$opt[[ "annots.inst" ]][, c("ANNOT", "START", "STOP")]
      df <- df[df$ANNOT %in% input$annots , ]
      df$START <- df$START / 3600
      df$STOP <- df$STOP / 3600
      # ensure 0-dur annots get plotted
      # --> make 1-sec arb
      df$STOP[ df$STOP == df$START ] <- df$STOP[ df$STOP == df$START ] + ( 1 / 3600 ) 
      na <- length(unique(df$ANNOT))

      # length of recording
      recdur.hrs <- values$opt[[ "header1" ]]$TOT_DUR_SEC / 3600
      # main plot (-3600 puts 2 hr of time in the left axis for labels)
      par(mar = c(2.2, 0, 0, 0))
      plot(c(-2, recdur.hrs), c(0, 1), type = "n", axes = F, ylim = c(0, 1), ylab = "", xlab = "")
      axis(1, 0:round(recdur.hrs))
      # plot each annot (0.5 is spacer for top/bottom)
      py <- yinc <- 1 / (length(input$annots) + 0.5)
      yidx <- 1
      for (ann in input$annots) {
        cidx <- 1 + (yidx %% 10)
        flt <- df$ANNOT == ann
        for (aa in which(flt)) {
          rect(df$START[aa], 1 - py - 0.5 * yinc,
	       df$STOP[aa], 1 - py + 0.5 * yinc,
	       col = NA , border = pal10[cidx] )
	       #col = pal10[cidx], border = NA)
        }
        text(-2, 1 - py, ann, col = pal10[cidx], pos = 4)
        py <- py + yinc
        yidx <- yidx + 1
      }
    })


# ------------------------------------------------------------
# Annotation tables

  output$annot.summary <- renderDataTable(
    {
      req( values$hasannots )
      df <- values$opt[[ "annots.summ" ]]
      df$ID <- NULL
      df$AVG <- df[, 3] / df[, 2]
      df[, 2] <- as.integer(df[, 2])
      names(df) <- c("Annotation", "Count", "Total-duration(s)", "Average-duration(s)")

      DT::datatable( df ,
                     options = list(scrollY = '300px' ,
                                   dom = 'tB', buttons = list( list(extend = "copy", text = "Copy") ) ,
                                   paging = F , info = F , searching = F,
                                   columnDefs = list(list(className = "dt-center", targets = "_all"))),
                   rownames= FALSE)
    })

    output$annot.table <- renderDataTable({
      req( values$hasannots )
      df <-
        values$opt[[ "annots.inst" ]][, c("ANNOT", "INST", "START_HMS","STOP_HMS","START", "STOP")]
      df <- df[df$ANNOT %in% input$annots , ]
      df$DUR <- round(df$STOP - df$START, 3)      
      names(df) <-
        c("Annotation", "Instance", "Start" , "Stop", "Start(s)", "Stop(s)", "Duration(s)")

      DT::datatable( df ,
                     options = list(scrollY = '300px' ,
                                   dom = 'tB', buttons = list( list(extend = "copy", text = "Copy") ) ,
                                   paging = F , info = F , searching = F,
                                   columnDefs = list(list(className = "dt-center", targets = "_all"))),
                   rownames= FALSE)
    })


    #
    # annot-instance list selector
    #

    observe({
      req(values$opt[[ "annots.inst" ]])
      flt <- values$opt[[ "annots.inst" ]]$ANNOT %in% input$disp.ann
      if (sum(flt) > 0) {
        secs1 <- values$opt[[ "annots.inst"]]$START[flt]
        secs2 <- values$opt[[ "annots.inst"]]$STOP[flt]
        annot <- values$opt[[ "annots.inst"]]$ANNOT[flt]
        #      inst <- values$opt[[ "annots.inst"]]$INST[ flt ]
        vals <- paste(annot, secs1, sep = ": ")
        inst <- as.list(paste(secs1, secs2))
        names(inst) <- vals
        if (length(secs1) > 0) inst <- inst[order(secs2)]
        updateSelectInput(
          session,
          "sel.inst",
          choices = inst,
          label = paste(length(secs1), " instances,", length(input$disp.ann), "annotations"),
          selected = 0
        )
      }
    })



 # ------------------------------------------------------------
 # SOAP
 #

observeEvent( input$soap.run , {
  req( values$hasdata , input$soap.ch )
  req( length( unique( values$opt[[ "ss" ]]$STAGE[ values$opt[[ "ss" ]]$E %in% values$opt[[ "included" ]] ] ) ) >= 2 ) 
  cmd <- paste( "SOAP force-reload epoch sig=" , input$soap.ch , sep="" )
  cat( "cmd|", cmd , "\n" )
  ret <- leval( cmd )   
  values$soap[[ "soap" ]] <- ret$SOAP$BL
  values$soap[[ "soap.stages" ]] <- ret$SOAP$SS
  df <- ret$SOAP$E
  if ( !any( names( df ) == "PP_N1" ) ) df$PP_N1 <- 0
  if ( !any( names( df ) == "PP_N2" ) ) df$PP_N2 <- 0
  if ( !any( names( df ) == "PP_N3" ) ) df$PP_N3 <- 0	
  if ( !any( names( df ) == "PP_R"  ) ) df$PP_R  <- 0
  if ( !any( names( df ) == "PP_W"  ) ) df$PP_W  <- 0
  df <- df[ , c("E","PRED","PRIOR","PP_N1","PP_N2","PP_N3","PP_R","PP_W") ]
  df$FLAG <- 0
  values$soap[[ "soap.epochs" ]] <- df 
})


output$plot.soap <- renderPlot({
  req( values$soap )
  par(mar=c(0,0,0,0))
  lpp2( values$soap[[ "soap.epochs" ]] )
})


output$table.soap <- DT::renderDataTable({
  req( values$hasdata , values$hasstaging , values$soap )

  df <- values$soap[[ "soap" ]]
  df$ID <- NULL
  df <- df[ , c("K","K3","ACC","ACC3","MCC","MCC3","F1","F13") ] 
  df2 <- as.data.frame( t(df) )
  names(df2) <- "VALUE"
  df2$VAR <- names(df)
  df2$DESC <- c( "Kappa", "3-class Kappa", "Accuracy","3-class Accuracy", 
                 "Matthews correlation coefficient", "3-class Matthews correlation coefficient",
		 "F1 statistic" , "3-class F1 statistic" )                  
  df2 <- df2[ , c("VAR","DESC","VALUE") ]
  df2$VALUE <- round( df2$VALUE, 2 ) 
  DT::datatable( df2, 
                   extensions = c('Buttons'),
                   options = list(scrollY = '200px' ,
                                  paging = F ,
                                  info = FALSE ,
                                  searching = FALSE,
                                  dom = 'tB', buttons = list( list(extend = "copy", text = "Copy") ) ) ,                                  
                   rownames= FALSE ,  colnames = c( "Metric","Description","Value" ) )

})


output$table.soap.stages <- DT::renderDataTable({
  req( values$hasdata , values$hasstaging , values$soap )

  df <- values$soap[[ "soap.stages" ]]
  df <- df[ , c( "SS" , "DUR_OBS" , "DUR_PRD" , "F1" ) ] 
  df <- df[ df$SS != "?" , ]
  df$DUR_OBS <- round( df$DUR_OBS , 2 )
  df$DUR_PRD <- round( df$DUR_PRD , 2 )
  df$F1 <- round( df$F1 , 2 )
  df$F1[ df$DUR_OBS == 0 ] <- NA 
  names(df) <- c( "Stage", "Obs.duration(m)" , "Pred.duration(m)" , "F1" ) 
  
  DT::datatable( df, 
                   extensions = c('Buttons'),
                   options = list(scrollY = '200px' ,
                                  paging = F ,
                                  info = FALSE ,
                                  searching = FALSE,
                                  dom = 'tB', buttons = list( list(extend = "copy", text = "Copy") ) ) ,                                  
                   rownames= FALSE )

})


# ------------------------------------------------------------
# POPS

observeEvent( input$pops.run , {
  req( values$hasdata )

  if ( input$popstabs == "M1" )
  {
    req( input$pops.m1.eeg1 )
    pops.lib <- pops.libs[1]
    aliases <- paste( "alias=CEN|" , input$pops.m1.eeg1 , sep="" )   
  }
  else if ( input$popstabs == "M2" )
  {
   req( input$pops.m2.eog , input$pops.m2.eeg1 , input$pops.m2.eeg2 ) 
   pops.lib <- pops.libs[2]
   aliases <- paste( "alias=EOG|" , input$pops.m2.eog , 
                        ",C3_M2|" , input$pops.m2.eeg1 ,
                        ",C4_M1|" , input$pops.m2.eeg2 , sep="" )
  }
		    
  cmd <- paste( "POPS force-reload output-features path=" , pops.path ,
                " lib=" , pops.lib , " " ,
		ifelse( input$popsshap , " SHAP " , "" ) , 
		aliases ,
		" lights-off=" , values$LOFF ,
	        " lights-on=" , values$LON , 
		sep="" )
 
  ret <- leval( cmd )  

  values$pops[[ "pops" ]] <- ret$POPS$BL
  values$pops[[ "pops.epochs" ]] <- ret$POPS$E
  values$pops[[ "pops.features" ]] <- ret$POPS$E_FTR
  values$pops[[ "pops.stages" ]] <- ret$POPS$SS

  if ( input$popsshap ) { 
   values$pops[[ "pops.SHAP" ]] <- ret$POPS$FTR_SS
   values$pops[[ "SHAP" ]] <- 1
  } else { 
  values$pops[[ "SHAP" ]] <- 0
  }

# feature list
  updateSelectInput(
      session,
      "sel.pops.features2",
      choices = unique( ret$POPS$E_FTR$FTR ),
      selected = 0 )
	
  # update channels, annots, etc
  update()
})

output$plot.pops <- renderPlot({
  req( values$hasdata , values$pops )
  req( dim( values$pops[[ "pops.epochs" ]]  )[1] != 0 ) 
  par(mar=c(0,0,0,0))
  lpp2( values$pops[[ "pops.epochs" ]] )
})

output$table.pops <- DT::renderDataTable({
  req( values$hasdata , values$hasstaging, values$pops )
  df <- values$pops[[ "pops" ]]
  df$ID <- NULL
  df <- df[ , c("K","K3","ACC","ACC3" ) ]
  df2 <- as.data.frame( t(df) )
  names(df2) <- "VALUE"
  df2$VAR <- names(df)
  df2$DESC <- c( "Kappa", "3-class Kappa", "Accuracy","3-class Accuracy" )

  df2 <- df2[ , c("VAR","DESC","VALUE") ]
  df2$VALUE <- round( df2$VALUE, 2 )
  DT::datatable( df2,
                   options = list( paging = F ,
                                   info = FALSE ,
                                   searching = FALSE,
                                   dom = 't' ),
                   rownames= FALSE ,  colnames = c("Metric","","Value") )
})

output$table.pops.stages <- DT::renderDataTable({
  req( values$hasdata , values$hasstaging , values$pops )

  df <- values$pops[[ "pops.stages" ]]
  df <- df[ , c( "SS" , "ORIG" , "PRF" , "F1" ) ] 
  df <- df[ df$SS != "?" , ]
  df$ORIG <- round( df$ORIG , 2 )
  df$PRF <- round( df$PRF , 2 )
  df$F1 <- round( df$F1 , 2 )
  df$F1[ df$ORIG == 0 ] <- NA 
  names(df) <- c( "Stage", "Obs.duration(m)" , "Pred.duration(m)" , "F1" ) 
  
  DT::datatable( df,                    
                   options = list(scrollY = '180px' ,
                                  paging = F ,
                                  info = FALSE ,
                                  searching = FALSE,
                                  dom = 't' ),
                   rownames= FALSE )

})


output$table.pops.epochs <- DT::renderDataTable({
  req( values$hasdata , values$pops )
  req( dim( values$pops[[ "pops.epochs" ]]  )[1] != 0 )

  df <- values$pops[[ "pops.epochs" ]]
  df$ID <- NULL
  df$CONF <- round( df$CONF , 2 )
  df$PP_N1 <- round( df$PP_N1 , 2 )
  df$PP_N2 <- round( df$PP_N2 , 2 )
  df$PP_N3 <- round( df$PP_N3 , 2 )
  df$PP_R <- round( df$PP_R , 2 )
  df$PP_W <- round( df$PP_W , 2 )
  
  DT::datatable( df,
                   options = list(buttons = list( list(extend = "copy", text = "Copy") ) ,
		                  scrollY = '300px' ,
                                  paging = F , 
                                  info = FALSE ,
                                  searching = FALSE,
                                  dom = 'tB' ),
                   rownames= FALSE )

})


#output$table.pops.features <- DT::renderDataTable({
#  req( values$hasdata , values$hasstaging , values$pops )
#  req( dim( values$pops[[ "pops.features" ]]  )[1] != 0 )
#  df <- values$pops[[ "pops.features" ]] 
#  ftrs <- unique( df$FTR  )
#  print(df)
#  df
#})


output$sel.pops.features <- DT::renderDataTable({
  req( values$pops[[ "SHAP" ]] == 1 )
  ftrs <- values$pops[[ "pops.SHAP" ]]

  df <- data.frame( FTR = ftrs$FTR[ ftrs$SS == "N1" ],
                    N1 = round( ftrs$SHAP[ ftrs$SS == "N1" ] , 2) ,
		    N2 = round( ftrs$SHAP[ ftrs$SS == "N2" ] , 2) ,	
		    N3 = round( ftrs$SHAP[ ftrs$SS == "N3" ] , 2) ,
		    R  = round( ftrs$SHAP[ ftrs$SS == "R" ] , 2 ) ,
		    W  = round( ftrs$SHAP[ ftrs$SS == "W" ] , 2 ) )
  df$MX <- apply( df[,-1] , 1 , max )
  
  DT::datatable( df,
                 options = list( scrollY = '200px' ,
		                 paging = F ,
                                 info = FALSE ,
                                 searching = FALSE,
                                 dom = 't' ),
                   rownames= FALSE , selection = 'single' )
})



output$plot.pops.features <- renderPlot({
#  req( values$pops[[ "SHAP" ]] == 1 )
#  req( input$sel.pops.features_rows_selected )
#  ftrs <- unique( values$pops[[ "pops.features" ]]$FTR )
#  ftr <- ftrs[ input$sel.pops.features_rows_selected ]

  req( input$sel.pops.features2 )
  ftr <- input$sel.pops.features2   
  par(mar=c(0,0,0,0))
  df <- values$pops[[ "pops.features" ]][ values$pops[[ "pops.features" ]]$FTR == ftr , ]
  df$X <- zf( df$X )
  eps <- unique( df$E )
  preds <- values$pops[[ "pops.epochs" ]]$PRED[ values$pops[[ "pops.epochs" ]]$FLAG != -1 ]
  plot( eps ,  df$X , ylim = range( df$X , na.rm=T ) , xlab="", ylab="" , main="" , col = lstgcols( preds ) , pch=20 )
})


#output$plot2.pops <- renderPlot({
#  req( values$hasdata , values$hasstaging , values$pops )
#  req( dim( values$pops[[ "pops.epochs" ]]  )[1] != 0 )
#  par(mar=c(0,0,0,0))
#  df <- values$pops[[ "pops.features" ]]
#  ftrs <- unique( df$FTR )
#  for (f in ftrs) df$X[ df$FTR == f ] <- zf( df$X[ df$FTR == f ] )   
#  lheatmap( df$E , df$FTR , df$X , win=0.05 ) 
#})



# ------------------------------------------------------------
# Signal views

    output$signal.master <- renderPlot({

      req( values$hasdata )

      session$resetBrush( "master_brush" )

      # hypnogram image used to select from the above
      par(mar = c(0, 0, 0, 0))

      # masked out
      inc <- values$opt[[ "ss" ]]$E %in% values$opt[[ "included" ]]

      # use staging, if available
      if (values$hasstaging ) {
        plot(values$opt[[ "ss" ]]$E[inc],
	     rep(0.5, length(values$opt[[ "ss" ]]$E[inc])),
             col = lstgcols(values$opt[[ "ss" ]]$STAGE[inc]),
	     axes = F, ylim = c(0, 1), pch = "|", ylab = "", xaxs = "i", yaxs = "i", xlim=c(1,values$opt[["ne"]] ) 
          )
        } else {
        # just fill in blank
        plot(seq(1, values$opt[[ "ne" ]]),
	     rep(0.5, values$opt[[ "ne" ]]),
	     axes = F, ylim = c(0, 1), pch = "|", ylab = "", xaxs = "i", yaxs = "i")
       }

    })

    output$signal.master2 <- renderPlot({
      req( values$hasdata )
      par( mar = c(0, 0, 0, 0) )
      plot(values$view[[ "epochs" ]], c(0.5, 0.5),
           col = "black", lwd = 5, type = "l", axes = F, ylab = "", xlab = "",
	   ylim = c(0, 1), xlim = c(1, values$opt[[ "ne" ]] ), xaxs = "i", yaxs = "i")
    })


# ------------------------------------------------------------
# primary signal plot

    output$signal.view <- renderPlot(
      {
        req( values$hasdata , c(input$channels, input$annots ) )

	# all epochs
	dfe <- values$opt[[ "init.epochs" ]][ , c( "START" , "STOP" ) ]
	
        epochs <- values$view[[ "epochs" ]]
	zoom   <- values$view[[ "zoom" ]]
        bp     <- values$view[[ "bandpass" ]]
        bpflt  <- values$view[[ "bpflt" ]]

        isolate({

          #    cat( "\nin renderPlot()\n" )

          # epochs are the (30-second) spanning epochs which are fetched (that always)
          # if zoom is defined, then back calculate

          # should not happen, but if for some reason nothing is defined,
          # display the first epoch:
          if (is.null(epochs) & is.null(zoom)) {
            epochs <- c(1, 1)
            zoom <- c(0, 30)
            values$view[[ "raw.signals" ]] <- T
          } else {
            if (is.null(epochs)) {
              epochs <- c(floor((zoom[1] / 30) + 1), floor((zoom[2] / 30) + 1))
            }

            if (is.null(zoom)) {
              zoom <- c((epochs[1] - 1) * 30, epochs[2] * 30)
            }

            epochs <- c(floor(epochs[1]), ceiling(epochs[2]))
          }

          # compile final values: epochs and seconds (always round to nearest whole second)
          secs <- c(floor(zoom[1]), ceiling(zoom[2]))

          # we should now have a) the spanning epochs (for ldata() ) in values$view[[ "epochs" ]]
          # and the range to display in values$view[[ "zoom" ]] (in seconds)

#              cat( "\n\nepochs : " , epochs , "\n" )
#              cat( "seconds: " , secs , "\n" )

          # update raw signals status as needed: if more than 5 mins, use summary stats
	  # 1 / 3 / 5 / 7 / 9 
          values$view[[ "raw.signals" ]] <- (zoom[2] / 30 - zoom[1] / 30) < 10

          annots <- input$annots
          chs <- input$channels
          na <- length(annots)
          nc <- length(chs)


          #
          # Plot parameters
          #

          # room for text on left (but w/in plot),
          # is 20% of main span
          x0 <- secs[1] - (secs[2] - secs[1]) * 0.05
          xr <- range(x0, secs[2])

          # y-axis
          cfac <- 3 # channel : annotation y-expansion factor
          sfac <- 1.5 #           spanning factor (only for raw signals, not summ stats)

          # i.e. give chs x3 vertical space; +1 is spacer
          yinc <- 1.0 / (cfac * nc + na + 1)

          # width of y-range (might be > yinc, i.e. for partial overlap)
          yspan <- yinc * sfac

          # initiate y-poinyter (half an increment up)
          #      yp <- yinc * 0.5
          yp <- 0
          yidx <- 1

          # initiate plot
          par(mar = c(2.2, 0, 0, 0)) 

          plot(c(0, 1),
               type = "n",
               ylim = c(0, 1),
               xlim = xr, xaxt = "n", yaxt = "n", axes = T,
               xlab = "", ylab = ""
          )

          axis(1, at = c( secs[1] , secs[2] ) , 
	          labels= c( paste( secs[1] , "s" , sep="" ) ,
		             paste( secs[2] , "s" , sep="" ) ) , tick = F )


          #
          # Zoomed-in hypnogram at top
          #

          stgs <- values$opt[[ "ss" ]]$STAGE
          enum <- values$opt[[ "ss" ]]$E

          for (e in epochs[1]:epochs[2]) {
            s <- secs[1] + (e - epochs[1]) * 30
            if (s < secs[2]) {
              s_end <- s + 30
              if (s_end > secs[2]) {
                s_end <- secs[2]
              }
              rect(s, 0.99, s_end, 1.00,
                   col = lstgcols(stgs[enum == e]),
                   border = NA
              )
            }
          }


          #
          # Signals
          #

          if (nc) {

            #
            # For short intervals, plot original data
            #

            if (values$view[[ "raw.signals" ]]) {

              #
              # Pull raw signal data
              #

              yidx <- 0
              for (ch in rev(chs)) {
                req(epochs[1] >= 1, epochs[2] <= values$opt[[ "ne" ]] )

#               cat( "ch",ch,"\n")
#               cat( "ep" , epochs , "\n")
#	       cat( "secs" , secs , "\n" )
	       
#                dat <- ldata(epochs[1]:epochs[2], chs = ch)
                qry <- list( range( dfe$START[ epochs[1] ] , dfe$STOP[ epochs[2] ] ) )
		dat <- ldata.intervals( qry , chs = ch)
                dat <- dat[dat$SEC >= secs[1] & dat$SEC <= secs[2], ]
                ts <- dat$SEC
		empty <- length(ts) == 0
		
		if ( empty )
		{
                 # no data - nothing to draw
		 cidx <- yidx %% 10 + 1
		 text( secs[1], yp + (yinc * cfac) / 2,
                     paste(ch, "\n**no data**"),
                     adj = 1, col = pal10[cidx], cex = 0.9 )
                }
		 else
		{ 

                # draw the actual signal
                dy <- dat[, 3]
		# flat?
		yr <- range(dy,na.rm=T)

		# filter?
                if (diff(yr) > 0 & values$view[[ "bandpass" ]] ) {
                  dy <- ldetrend(dy)
                  dy <- lfilter(dy, values$opt[[ "sr" ]][ch], values$view[[ "bpflt" ]][1] , values$view[[ "bpflt" ]][2] , 5, 0.05 )
                }
                yr <- range(dy,na.rm=T)
                # zero-centered signal?
                zc <- yr[1] < 0 & yr[2] > 0
                # mean center
                dy <- dy - mean(dy)
                # max absolute value
                yrmx <- max(abs(range(dy)))
		if ( yrmx == 0 ) yrmx = 1 
                # if +/- signal, scale to -1, +1 ( 0 .. 1 ) based on max( |x| )
                dy <- dy / (2 * yrmx)
                # convert to plot co-oords
                dy <- (yp + (yinc * cfac) / 2) + dy * yspan * cfac
                # plot
                cidx <- yidx %% 10 + 1
                lines(ts, dy, lwd = 0.75, col = pal10[cidx] )
                # 0 line?
		if ( zc ) lines(ts, rep(yp + (yinc * cfac) / 2 , length(ts) ) , col = "gray" ) 
                # labels
		text(secs[1], yp + (yinc * cfac) / 2,
                     paste(ch, "\n(",
		           signif(yr[1], 3), "..", signif(yr[2], 3),
		           ifelse( values$view[[ "units" ]][ch] != "." , values$view[[ "units"]][ch], "" ), ")"),
                     adj = 1, col = pal10[cidx], cex = 0.9
                )
	       }

               # drop down to next channel
               yp <- yp + yinc * cfac
               yidx <- yidx + 1
              }	     
            }

            else {

              #
              # otherwise, print message about data not present (i.e. no summary data )
              #

              yidx <- 0
              cidx <- 0

              for (ch in rev(chs)) {
                cidx <- yidx %% 10 + 1

                text(x0 + 0.1 * (xr[2] - xr[1]), yp + (yinc * cfac) / 2,
                     "... no summary values available ... \n... select a smaller region to view this signal ... ",
                     pos = 4, col = pal10[cidx], cex = 1.0
                )

                # labels
                text(x0, yp + (yinc * cfac) / 2,
                     ch,
                     pos = 4, col = pal10[cidx], cex = 0.9
                )

                # drop down to next channel
                yp <- yp + yinc * cfac
                yidx <- yidx + 1
              }
            }
      } # end of 'if-channels'

          #
          # Annotations (these are pre-loaded) [ will be plotted at top ]
          #

          if (na) {
            df <- values$opt[[ "annots.inst"]][, c("ANNOT", "START", "STOP")]
            df <- df[df$ANNOT %in% annots, ]
            df <- df[(df$START <= secs[2] & df$STOP >= secs[1]), ]
            # left/right censor
            df$START[df$START < secs[1]] <- secs[1]
            df$STOP[df$STOP > secs[2]] <- secs[2]

            for (annot in rev(annots)) {
              # color
              cidx <- 1 + (yidx %% 10)
              flt <- which(df$ANNOT == annot)

              for (a in flt) {
                rect(df$START[flt], yp + yinc / 2 + yinc / 4 ,
                     df$STOP[flt], yp + yinc / 2 - yinc / 4 ,
                     border = pal10[cidx], col = NA )
                 #  col = pal10[cidx], border = NA )
              }
              # labels
              legend(x0, yp + yinc / 2, annot, yjust = 0.5, fill = pal10[cidx], cex = 0.9, border = NA)

              # drop down to next annotation
              yp <- yp + yinc
              yidx <- yidx + 1
            }
          } # end if annots

          session$resetBrush("zoom_brush")
        }) # isolate
      },
      height = "auto"
    )


# plot helper functions




    clear_sel_inst <- function() {
      if (!is.null(input$sel.inst)) {
        updateSelectInput(session, "sel.inst", selected = "")
      }
    }

    # single-click jumps to a single epoch
    observeEvent(input$master_click, {
      if (is.null(input$master_brush)) {
        clear_sel_inst()
        values$view[[ "epochs" ]] <- c(floor(input$master_click$x), floor(input$master_click$x))
        values$view[[ "zoom" ]] <- NULL
      }
    })

    # double-click clears all
    observeEvent(input$master_dblclick, {
      clear_sel_inst()
      values$view[[ "epochs" ]] <- c(1, 1)
      values$view[[ "zoom" ]] <- NULL
    })

    # brush will zoom in to range of epochs
    observeEvent(input$master_brush, {
      clear_sel_inst()
      brush <- input$master_brush
      if (!is.null(brush)) {
        if (brush$xmin < 1 || brush$xmax > values$opt[[ "ne" ]]) {
          session$resetBrush("master_brush")
        } else {
          values$view[[ "epochs" ]] <- c(brush$xmin, brush$xmax)
          values$view[[ "zoom" ]] <- NULL
        }
      } else {
        values$view[[ "epochs" ]] <- values$view[[ "zoom" ]] <- NULL
      }
    })

    # Full length selection
    observeEvent(input$entire.record, {
      req(values$hasdata )
      session$resetBrush("master_brush")
      clear_sel_inst()
      values$view[[ "epochs" ]] <- c(1, values$opt[[ "ne" ]])
      values$view[[ "zoom" ]] <- NULL
    })

    # shrink view 
    observeEvent(input$winin, {
      req(values$hasdata )
      session$resetBrush("master_brush")
      clear_sel_inst()      
      sz <- diff( values$view[[ "epochs" ]] )
      if ( sz > 1 ) values$view[[ "epochs" ]] <- c( values$view[[ "epochs"]][1]+1 , values$view[[ "epochs"]][2] -1 )
      if ( sz == 1 ) values$view[[ "epochs" ]] <- c( values$view[[ "epochs"]][1] , values$view[[ "epochs"]][2] - 1 )
      values$view[[ "zoom" ]] <- NULL
    })

    # expand view 1epoch either side
    observeEvent(input$winex, {
      req(values$hasdata )
      session$resetBrush("master_brush")
      clear_sel_inst()      
      values$view[[ "epochs" ]] <- c(max( 1 , values$view[[ "epochs" ]][1]-1 ) ,
                                     min( values$opt[[ "ne" ]] , values$view[[ "epochs" ]][2]+1 ) )
      values$view[[ "zoom" ]] <- NULL
    })

    # Apply bandpass filter to all signals
    observeEvent(input$bandpass, {
      req(values$hasdata )
      values$view[[ "bandpass" ]] <- !values$view[[ "bandpass" ]]
      values$view[[ "bpflt" ]] <- input$flt.freq
    })

    # Apply bandpass filter to all signals
    observeEvent(input$flt.freq, {
      req(values$hasdata )
      values$view[[ "bpflt" ]] <- input$flt.freq
    })

# drive by annotation instance box
    observeEvent(input$sel.inst, {

      xx <- range(as.numeric(unlist(strsplit(input$sel.inst, " "))))
      sz <- diff( xx )
      xx <- c( floor(xx[1] / 30) + 1, ceiling(xx[2] / 30) )
      # if sz > 10 seconds, expand to 3 epochs if would
      # otherwise have been a one-second view 
      if ( sz > 10 & xx[1] == xx[2] )
       xx <- c( max( 1 , xx[1] - 1 ) , min( values$opt[[ "ne" ]] , xx[2] + 1) )
      values$view[[ "epochs" ]] <- xx
      values$view[[ "zoom" ]] <- NULL
      session$resetBrush("master_brush")
      session$resetBrush("zoom_brush")
      #    session$setBrush(
      #      brushId = "master_brush",
      #      coords = list(xmin=xx[1], xmax=xx[2]) )
      #      panel = 1
    })

    observeEvent(input$zoom_dblclick, {
      session$resetBrush("zoom_brush")
      #    session$resetBrush( "master_brush" )
      #    values$opt[[ "epochs" ]] = NULL
      values$view[[ "zoom" ]] <- NULL
    })

    observeEvent(input$zoom_brush, {
      brush <- input$zoom_brush
      epochs <- values$view[[ "epochs" ]]
      if (!is.null(brush) && !(brush$xmin < (epochs[1] - 1) * 30 || brush$xmax > epochs[2] * 30)) {
        values$view[[ "zoom" ]] <- c(brush$xmin, brush$xmax)
      } else {
        values$view[[ "zoom" ]] <- NULL
      }
    })

    observeEvent(input$button_epoch_prv, {
      req(values$hasdata )
      clear_sel_inst()
      curr_epochs <- values$view[[ "epochs" ]]
      values$view[[ "zoom" ]] <- NULL
      session$resetBrush("master_brush")
      shft <- values$view[[ "epochs" ]][2] - values$view[[ "epochs" ]][1] + 1
      values$view[[ "epochs" ]] <- values$view[[ "epochs" ]] - shft
      values$view[[ "epochs" ]][ values$view[[ "epochs" ]] < 1 ] <- 1      
    })

    observeEvent(input$button_epoch_nxt, {
      req(values$hasdata )
      clear_sel_inst()
      curr_epochs <- values$view[[ "epochs" ]]
      values$view[[ "zoom" ]] <- NULL
      session$resetBrush("master_brush")
      shft <- values$view[[ "epochs" ]][2] - values$view[[ "epochs" ]][1] + 1
      values$view[[ "epochs" ]] <- values$view[[ "epochs" ]] + shft
      values$view[[ "epochs" ]][ values$view[[ "epochs" ]] > values$opt[[ "ne" ]] ] <- values$opt[[ "ne" ]]
    })

    output$info2 <-
      renderText({
        req(values$hasdata )

        # zoom info to display?

        zoom_info <- NULL

        if (!is.null(values$view[[ "zoom" ]])) {
          brush <- input$zoom_brush
          zoom_info <- paste0(". Zoomed in epoch range is: ",
	                      floor(values$view[[ "zoom" ]][1] / 30), " to ",
			      ceiling(values$view[[ "zoom"]][2] / 30))
        }

        epochs <- values$view[[ "epochs" ]]
        if (is.null(epochs)) epochs <- c(1, 1)
        hrs <- ((epochs[1] - 1) * 30) / 3600

        all_good <- TRUE
        max_epoch <- values$opt[[ "ne" ]]
        if ((epochs[1] < 1 || epochs[2] > max_epoch) && is.null(input$master_brush)) {
          all_good <- FALSE
        }
        if ((epochs[1] < 1 || epochs[1] > max_epoch) && (epochs[2] < 1 || epochs[2] > max_epoch) && !is.null(input$master_brush)) {
          all_good <- FALSE
        }

        if (all_good) {
          paste0(
            "Epoch ", floor(epochs[1]), " to ", ceiling(epochs[2]),
            " (", (ceiling(epochs[2]) - floor(epochs[1]) + 1) * 0.5, " minutes)",
            " ", signif(hrs, 2), " hours from start", zoom_info,
            ifelse(values$view[[ "bandpass" ]], paste( " (w/" ,
	           values$view[[ "bpflt" ]][1] , "-" ,
		   values$view[[ "bpflt" ]][2] , "Hz filter)"), " (unfiltered)")
          )
        } else {
          paste0("Selected value is out of range")
        }
      })



 # ------------------------------------------------------------
 # Manip functions

 observeEvent( input$doreref , { 
   req( input$reref1 , input$reref2 )
   pris <- paste( input$reref1 , collapse="," )
   refs <- paste( input$reref2 , collapse="," )
   leval( paste( "REFERENCE sig=" , pris , " ref=" , refs , sep="" ) )
   update()   
   } )

 observeEvent( input$doresample , {
  req( input$resample , input$resamplerate )
  pris <- paste( input$resample , collapse="," )
  leval( paste( "RESAMPLE sig=" , pris , " sr=" , input$resamplerate , sep="" ) )
  update()
 } )

 observeEvent( input$dorename , {
  req( input$renameold , input$renamenew )
  leval( paste( "RENAME sig=" , input$renameold , " new=" , input$renamenew , sep="" ) )
  update()
 } )

 observeEvent( input$dodrop , {
  req( input$drop )
  leval( paste( "DROP sig=" , input$drop , sep="" ) )
  update()
 } )

 observeEvent( input$docopy , {
  req( input$copyold , input$copytag )
  leval( paste( "COPY sig=" , input$copyold , " tag=" , input$copytag , sep="" ) ) 
  update()
 } )


 observeEvent( input$dotrans , {
  req( input$transch , input$transexp )
  cmd <- paste( "TRANS sig=" , input$transch , " expr=\" " , input$transexp , "\"" , sep="" )
  cat( "trans[" , cmd , "]\n" )
  leval( cmd )
  update()
 } )




 # ------------------------------------------------------------
 # Reload/refresh/reepoch an EDF
 #

 observeEvent( input$reset , {

   req( values$hasdata )
   leval( "MASK clear" )   
   lrefresh()
   init()
   update()

} )

 observeEvent( input$reepoch , {
   req( values$hasdata )
   # note, this will destory any current MASK
   isolate({
    ret <- leval( "RE & EPOCH align verbose & DUMP-MASK " )    
    values$opt[[ "curr.epochs" ]]   <- ret$EPOCH$E
    values$opt[[ "curr.ne" ]] <- dim(ret$EPOCH$E)[1]    
    values$opt[[ "unmasked" ]] <- ret$DUMP_MASK$E$E[ ret$DUMP_MASK$E$EMASK == 0 ]
    values$opt[[ "included" ]] <- ret$DUMP_MASK$E$E
   } )
  update()
} )



 # ------------------------------------------------------------
 # Harmonization of channels & labels
 #

 observeEvent( input$mapchs , {
    req( values$hasdata )

    # save as tempfile
    tfile <- tempfile()
    write( input$canonical , file = tfile )

    # run CANONICAL
    ret <- leval( paste( "CANONICAL file=" , tfile , sep="" ) )

    # details
    values$canonical[[ "ch" ]] <- ret$CANONICAL$CH
    values$canonical[[ "cs" ]] <- ret$CANONICAL$CS

    # update tables
    update()
    
    # clean up
    on.exit( unlink( tfile ) , add = T ) 
 } )


# add NSRR defaults

  observeEvent( input$addnsrr , {	
        updateTextAreaInput(session, "canonical",
     value = paste( scan( canonical.sigs , what = as.character() , sep = "\n" , blank.lines.skip = F ) ,
                    collapse = "\n" ) )    
 } )



 output$csmappings <- DT::renderDataTable({
   req( values$canonical )
   df <- values$canonical[[ "cs" ]]
   df$ID <- NULL
   df <- df[ order( df$DEFINED , decreasing = T ) , ]
   
   DT::datatable( df,
                   options = list(scrollY = '380px' ,
                               scrollX = '100%' ,
                               dom="tB" ,
                               buttons = list( list(extend = "copy", text = "Copy") ) ,
                               paging = F , ordering=F,
                               info = FALSE ,
                               searching = FALSE ,
                               columnDefs = list(list(className = "dt-center", targets = "_all"))),
                 rownames= FALSE )

  })

 output$chmappings <- DT::renderDataTable({
   req( values$canonical )
   df <- values$canonical[[ "ch" ]] 
   df$ID <- NULL
   df <- df[ order( df$USED , decreasing = T ) , ]
   DT::datatable( df,
                   options = list(scrollY = '380px' ,
                               scrollX = '100%' ,
                               dom="tB" ,
                               buttons = list( list(extend = "copy", text = "Copy") ) ,
                               paging = F , ordering=F,
                               info = FALSE ,
                               searching = FALSE ,
                               columnDefs = list(list(className = "dt-center", targets = "_all"))),
                 rownames= FALSE )

  })



 # ------------------------------------------------------------
 # Generic table plotter
 #

 observe({
   req( input$plotT )

   tok <- unlist( strsplit( input$plotT , " : " ) )
   df <- values$opt[[ "k" ]][[ tok[1] ]][[ tok[2] ]]
   df$ID <- NULL 
   vars <- c( "." , names(df) )

   updateSelectInput(
       session,
       "plotX",
       choices = vars , 
       label = NULL 
     )

   updateSelectInput(
       session,
       "plotY",
       choices = vars , 
       label = NULL 
     )

   updateSelectInput(
       session,
       "plotZ",
       choices = vars , 
       label = NULL
     )

})
   
 pdata <- reactive({
   tok <- unlist( strsplit( input$plotT , " : " ) )
   df <- values$opt[[ "k" ]][[ tok[1] ]][[ tok[2] ]]
   hasX <- any( input$plotX %in% names(df) )
   hasY <- any( input$plotY %in% names(df) )
   hasZ <- all( input$plotZ %in% names(df) ) 
   req( hasX )
   if ( hasY )
   {
   if ( hasZ ) return( df[, c( input$plotX , input$plotY , input$plotZ ) ] )
   else return( df[, c( input$plotX , input$plotY ) ] )
   } else {
   if ( hasZ ) return( df[, c( input$plotX , input$plotZ ) ] )
   else return( df[, c( input$plotX ) ] )
   }
   return(NULL)
})
 

 # helper function to set height of PSD plots
 plotter_height <- function() {
#    values$plotter.height <- 250 * max(1, ceiling(length(input$sel.ch) / 2))
#    return(values$plotter.height )
    return( ceiling(values$nz/2) * 240 )
  }


 # wrap plotOutput in renderUI to alter height of plot(s)
  output$ui_plotter <- renderUI({
   req( values$hasedf , input$plotT , values$opt[[ "k" ]] )
      plotOutput("plotter", height = plotter_height(), width = "100%")
    })



 output$plotter <- renderPlot({
   req( input$plotT , input$plotX, values$opt[[ "k" ]] )

   # get data
   df <- pdata()

   # require at least one plot
   hasX	<- input$plotX	%in% names(df)
   req( hasX )
   req( is.numeric( df[,input$plotX] ) )
   
   # will always have an X names
   x <- df[,input$plotX]

   hasY <- input$plotY %in% names(df)

   hasZ <- all( input$plotZ %in% names(df) ) 
   z <- rep("" , dim(df)[1])
   zfac <- ""
   if ( hasZ ) {
     zidx <- which( names(df) %in% input$plotZ )
     z <- apply( df , 1 , function(x,y) { paste(x[y],collapse=' ')  } , zidx )
     zfac <- unique( z )
     print( zfac )
   }
   nz <- length( zfac )
   req( nz < 100 )
   values$nz <- nz
   
   par(mfrow = c(ceiling(nz / 2), 2), mar = c(4, 4, 2, 1))

   for (zi in 1:nz)
   {
      inc <- z == zfac[zi]
      if ( hasY ) plot( df[inc,input$plotX] ,df[inc,input$plotY] , main=zfac[zi] , xlab = input$plotX , ylab = input$plotY , pch=20, col="blue" , type="b" ) 
      else hist( df[inc,input$plotX] , main=zfac[zi] , xlab = input$plotX , breaks=20, col="blue" )
   }
 
 })


}



# Run the app ----
shinyApp(ui, server)
