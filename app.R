# journal rank R Shiny app
# Corey Bradshaw
# Flinders University

## remove everything
rm(list = ls())

# load libraries
library(shiny)
library(shinybusy)
library(Hmisc)
library(cluster)
library(bootstrap)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(forcats)
library(dplyr)

## call functions
source(file.path("./functions/", "setBackgroundColor.R"), local=T)
source(file.path("./functions/", "jRnkBootFunc.R"), local=T)

ui <- fluidPage(
  
  tags$head(
    tags$meta(name="google-site-verification", content="bVQ54bgpekDeCqy30pOSnGOMgbNXXV1AdwIoMRXSAAI")
  ),
  
  # title of app
  titlePanel("JournalRankShiny: a multi-index bootstrap function to rank a sample of peer-reviewed journals"),
  
  wellPanel(style = "background: azure",
    tags$a(href="https://github.com/cjabradshaw/JournalRankShiny", tags$img(height = 150, src = "race.png", style="float:right")),
    tags$p(style="font-family:Avenir", tags$i(class="fab fa-r-project", title="R Project"),"Shiny App by", tags$a(href="https://globalecologyflinders.com/people/#CJAB", "Corey Bradshaw "),
           tags$a(href = "mailto:corey.bradshaw@flinders.edu.au","(",tags$i(class="far fa-envelope"),"e-mail"),";",
           tags$a(href = "https://github.com/cjabradshaw", tags$i(class="fab fa-github",title="Github"),"Github)")),
    tags$h4(style="font-family:Avenir", "Preamble"),
    tags$p(style="font-family:Avenir", "There are many methods available to assess the relative citation performance of peer-reviewed journals.
           Regardless of their individual faults and advantages, citation-based metrics are used by researchers to maximise the citation potential
           of their articles, and by employers to rank academic track records. The absolute value of any particular index is arguably meaningless
           unless compared to other journals, and different metrics result in divergent rankings. To provide a simple yet more objective way to rank
           journals within and among disciplines, this app provides a κ-resampled composite journal rank incorporating six user-supplied citation indices:",
           tags$a(href="http://help.incites.clarivate.com/incitesLiveJCR/glossaryAZgroup/g8/4346-TRS.html", "Impact Factor"), "(IF),",
           tags$a(href="http://help.incites.clarivate.com/incitesLiveJCR/glossaryAZgroup/g7/7751-TRS.html", "Immediacy Index"), "(IM),",
           tags$a(href="https://scholar.google.com/intl/en/scholar/metrics.html#metrics", "Google 5-year h-index"), "(h5),",
           tags$a(href="https://service.elsevier.com/app/answers/detail/a_id/30562/supporthub/scopus/", "CiteScore"), "(CS),",
           tags$a(href="https://blog.scopus.com/posts/journal-metrics-in-scopus-source-normalized-impact-per-paper-snip", "Source-Normalized Impact Per Paper"),
           "(SNIP), and", tags$a(href="https://www.scopusjournals.com/2019/02/scimago-journal-rank.html", "SCImago Journal Rank"), "(SJR).
           The output gives an index of relative rank uncertainty for all sample journals provided by the user. This", tags$i(class="fab fa-github"),
           "Github ", tags$a(href = "https://github.com/cjabradshaw/JournalRankShiny", "repository"),
           "provides all the 'under-the-bonnet'",tags$i(class="fab fa-r-project"),"code for the app. Read the related",
           tags$a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0149852", "paper", tags$i(class="far fa-file")),
           " and/or", tags$a(href="https://conservationbytes.com/2016/02/18/how-to-rank-journals/",
                                        "blog post", tags$i(class="fas fa-blog"), ".")),
    tags$h4(style="font-family:Avenir", "Instructions"),
    tags$ol(tags$li(tags$p(style="font-family:Avenir", "Create a delimited text file of", tags$strong("exactly the same format"), "as the example file in this",
           tags$a(href="https://github.com/cjabradshaw/JournalRankShiny/blob/main/Jsamp2019.csv","repository", tags$i(class="far fa-file")), ",
           although you can specify the delimit character (", tags$em("comma"),", ", tags$em("space"),", ", tags$em("tab"),").")),
           tags$li(tags$p(style="font-family:Avenir", "Load your delimited text file in the app by clicking the",tags$i(class="fas fa-file-import"),
           tags$strong("choose file"), "button.")),
           tags$li(tags$p(style="font-family:Avenir", "Select the number of bootstrap iterations", tags$i(class="fas fa-sort-amount-down-alt"), 
           "you desire (1000, 10000, or 100000) — more iterations will provide better estimates of the uncertainty bounds, but will take longer to calculate.")),
           tags$a(href="https://globalecologyflinders.com/", tags$img(height = 100, src = "GEL Logo Kaurna transparent.png", style="float:right",
                                                               title="Global Ecology @ Flinders University")),
           tags$li(tags$p(style="font-family:Avenir", "Set the", tags$em("κ"), "'clipping'",tags$i(class="fas fa-cut"), "limitation and number of repeats",
                          tags$em("n"), tags$i(class="fas fa-redo"), "to reduce the influence of outliers on the  uncertainty bounds.")),
           tags$li(tags$p(style="font-family:Avenir", "Click the", tags$i(class="fas fa-calculator"), tags$strong("calculate ranks"), "button.")),
           tags$li(tags$p(style="font-family:Avenir", "Download the results table as a", tags$i(class="fas fa-file-csv"), "file by clicking the", tags$i(class="fas fa-download"),
           tags$strong("download"), "button.")))
  ), # end wellPanel
  
  tabsetPanel(id="tabs",
              tabPanel(value="tab1", title="user-collated journal metrics",
                       
                       sidebarLayout(
                         sidebarPanel(
                           wellPanel(style = "background: LightCyan",
                             fileInput("file1", label=tags$p(tags$i(class='fas fa-file-import'),"choose delimited file with index data (9 columns)"),
                                       multiple=F, buttonLabel="choose file", placeholder="no file selected"),
                             tags$hr(),
                             radioButtons("sep",label=tags$p(tags$i(class="fas fa-file-csv"),"separator"),choices=c(comma=',',space="",tab="\t"), inline=T),
                             checkboxInput("header1", "header?", TRUE),
                             tags$hr(),
                             radioButtons("iter", label=tags$p(tags$i(class='fas fa-sort-amount-down-alt'), "select number of bootstrap iterations"), inline=T,
                                          choiceNames = list("1000", "10000", "100000"), choiceValues = list(1000,10000,100000)),
                             tags$hr(),
                             radioButtons("kappa", label=tags$p(tags$i(class='fas fa-cut'), "choose clipping value", tags$em("κ")), inline=T,
                                         c("1.5"="1.5","2"="2","3"="3","4"="4"), selected="2"),
                             tags$hr(),
                             radioButtons("nK", label=tags$p(tags$i(class='fas fa-redo'), "choose number of clipping repeats", tags$em("n")), inline=T,
                                          c("2"="2","5"="5","10"="10"), selected="5"),
                             tags$hr(),
                             actionButton("calcButton", label="calculate ranks",icon=shiny::icon("fas fa-calculator")),
                             br(),
                             tags$small(style="font-family:Avenir", "(refresh page to clear data)"),
                             tags$hr(),
                             downloadButton('downloadData', 'download',icon = shiny::icon("download"))
                           ),
                         ),
                         
                         # open main panel
                         mainPanel(style = "background: MintCream",
                           
                           fluidRow(
                             tags$div(id = "firstOutput", 
                                      h3("input data"),
                                      add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                                      dataTableOutput("table1")) 
                           ),
                           
                           fluidRow(
                             add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                             tags$div(id = "placeholder") # the dynamic UI will be inserted relative to this placeholder
                           ),
                           
                         ) # close mainPanel
                         
                       ) # sidebarLayout
              ), # end tab1
              
  
              tabPanel(value="tab2", title=tags$strong("rank plots"), style = "background: MintCream",
                       
                       tags$br(),
                       tags$p(style="font-family:Avenir", "The following plot show the relative ranks (± 95% bootstrapped confidence intervals)
                              for the sample of journals provided according to:"),
                       tags$ul(tags$li(tags$p(style="font-family:Avenir", tags$strong("A.", tags$em("κ"),"-clipped bootstrap rank"))),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("B. bootstrap rank"))),
                       ), # end ul
                       
                       mainPanel(
                         tags$br(),
                         add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                         plotOutput(height="1200px", width="150%","rankPlots")
                       ) # end mainPanel
                       
              ), # end tab2
              
              tabPanel(value="tab3", title=tags$strong("Impact Factor", tags$em("vs."), "rank"), style = "background: MintCream",
                       
                       tags$br(),
                       tags$p(style="font-family:Avenir", "The following plot shows the relationship between log",tags$sub(tags$em("e")), " journal Impact Factor and
                              its relative",tags$em("κ"),"-clipped rank:"),

                       mainPanel(
                         tags$br(),
                         tags$p(style="font-family:Avenir","The loess trend is indicated by the blue line."),
                         tags$br(),
                         add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                         plotOutput(height="800px", width="150%", "cMYPlots")
                       ) # end mainPanel
                       
              ), # end tab3
              
              tabPanel(value="tab4", title=tags$strong("input/output column descriptors"), style = "background: MintCream",
                       tags$h2(style="font-family:Avenir", "Column descriptors"),
                       tags$a(href="https://flinders.edu.au/", tags$img(height = 100, src = "F_V_CMYK.png", style="float:right",title="Flinders University")),
                       tags$h3(style="font-family:Avenir", "Input data file requirements"),
                       tags$ol(tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 1"),": ", tags$em("Journal")," — abbreviated journal name (see abbreviations
                                              list", tags$a(href="https://www.library.caltech.edu/journal-title-abbreviations", "here"),")")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 2"),": ", tags$em("n")," — number of citable items that year (available
                                              from Web of Science™", tags$a(href="https://clarivate.com/webofsciencegroup/solutions/journal-citation-reports/",
                                                                            "Journal Citation Reports"),")")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 3"),": ", tags$em("cites")," — total number of citations that year
                                              (available from Web of Science™", tags$a(href="https://clarivate.com/webofsciencegroup/solutions/journal-citation-reports/",
                                                                            "Journal Citation Reports"),")")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 4"),": ", tags$em("h5")," — five-year h-index
                                              (available from", tags$a(href="https://scholar.google.com.au/citations?view_op=top_venues&hl=en",
                                                                                       "Google Scholar"),")")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 5"),": ", tags$em("IF")," — journal Impact Factor
                                              (available from Web of Science™", tags$a(href="https://clarivate.com/webofsciencegroup/solutions/journal-citation-reports/",
                                                                                       "Journal Citation Reports"),")")),
                       tags$a(href="https://epicaustralia.org.au/", tags$img(height = 150, src = "CABAHlogo.png",
                                                                             style="float:right", title="ARC Centre of Excellence for Australian Biodiversity and Heritage")),
                       tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 6"),": ", tags$em("IM")," — Immediacy Index
                                              (available from Web of Science™", tags$a(href="https://clarivate.com/webofsciencegroup/solutions/journal-citation-reports/",
                                                                                       "Journal Citation Reports"),")")),
                       tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 7"),": ", tags$em("CS")," — CiteScore
                                              (available from Elsevier™", tags$a(href="https://www.scopus.com/sources",
                                                                                       "Scopus"),")")),
                       tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 8"),": ", tags$em("SNIP")," — Source-Normalized Impact Per Paper
                                              (available from Elsevier™", tags$a(href="https://www.scopus.com/sources",
                                                                                 "Scopus"),")")),
                       tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 9"),": ", tags$em("SJR")," — SCImago Journal Rank
                                              (available from Elsevier™", tags$a(href="https://www.scopus.com/sources",
                                                                                 "Scopus"),")"))),
                       
                       tags$h3(style="font-family:Avenir", "rank output"),
                       tags$ol(tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 1"),": ", tags$em("journal")," — abbreviated journal name (see abbreviations
                                              list", tags$a(href="https://www.library.caltech.edu/journal-title-abbreviations", "here"),")")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 2"),": ", tags$em("IF")," — journal Impact Factor")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 3"),": ", tags$em("avgObsRnk")," — average rank across 6 metrics (not bootstrapped)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 4"),": ", tags$em("rnkLo")," — lower 95% confidence bound of bootstrapped rank")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 5"),": ", tags$em("rnkMed")," — median bootstrapped rank")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 6"),": ", tags$em("rnkUp")," — upper 95% confidence bound of bootstrapped rank")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 7"),": ", tags$em("rnkKaplo")," —", tags$em("κ"), "-clipped lower 95% confidence bound of bootstrapped rank")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 8"),": ", tags$em("rnkKapmed")," —", tags$em("κ"), "-clipped median bootstrapped rank")),
                               tags$a(href="https://github.com/cjabradshaw/JournalRankShiny/blob/main/LICENSE", tags$img(height = 50, src = "GNU GPL3.png", style="float:right", title="GNU General Public Licence v3.0")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 9"),": ", tags$em("rnkKapup")," —", tags$em("κ"), "-clipped upper 95% confidence bound of bootstrapped rank"))),
                               tags$br()
              ) # end tab4

  ) # end tabsetPanel
  
) # close fluidPage


server <- function(input, output, session) {
  
  observeEvent(input$file1, {
    
    if(input$tabs == "tab1"){
      
      output$table1 <- renderDataTable({
        file_to_read = input$file1
        if(is.null(file_to_read)){
          return()
        }
        read.table(file_to_read$datapath, sep=input$sep, header=input$header1, quote=NULL)
      }) # end output table1
      
      datin <- reactive({
        fileinp <- input$file1
        if(is.null(fileinp)){return()}
        inpdat <- data.frame(read.table(fileinp$datapath, sep=input$sep, header = input$header1, quote=NULL))
        return(inpdat)
      }) # end datin
      
      KappaDat <- reactiveValues()
      observe({
        KappaDat$iter <- as.numeric(input$iter)
        KappaDat$kappa <- as.numeric(input$kappa)
        KappaDat$nK <- as.numeric(input$nK)
      })
      
      # when action button pressed ...
      observeEvent(input$calcButton, {
        removeUI("div:has(>#firstOutput)")
        insertUI(
          selector = "#placeholder",
          where = "afterEnd", # inserts UI after the end of the placeholder element
          ui = fluidRow(
            h3("calculating bootstrapped ranks ... (this can take some time depending on the number of journals in your sample and the bootstrap settings)"),
            output$ranktable <- renderDataTable({
              if(is.null(datin())){return ()}
              results <<- jRnkBootFunc(datsamp=(datin()), iter=KappaDat$iter, kap=KappaDat$kappa, kN=KappaDat$nK)
            })))
      }) # end observeEvent
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("journalRanksOut", "csv", sep = ".")
        },
        
        content = function(file) {
          sep <- ","
          
          write.table(results, file, sep=sep, row.names = F)
        }
      )
    } # end if for tab1
    
  }) # end Events

  observeEvent(input$tabs, {
    
    if(input$tabs == "tab2"){
      
        output$rankPlots <- renderPlot({
          input$rankPlots
          
          Ctheme1 = theme(
            axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 16),
            axis.text.y = element_text(size = 8, face="italic")
            )

          kappaRnk <- results %>%
            mutate(journal = fct_reorder(journal, desc(rnkKapmed))) %>%
            ggplot( aes(x=rnkKapmed, y=journal, xmin = rnkKaplo, xmax = rnkKapup)) +
            geom_point(size=1) +
            xlim(1, max(results$rnkKapup)) +
            geom_errorbarh() +
            labs(x="(← higher)      rank      (lower →)", y=NULL) +
            Ctheme1

          bootRnk <- results %>%
            mutate(journal = fct_reorder(journal, desc(rnkMed))) %>%
            ggplot( aes(x=rnkKapmed, y=journal, xmin = rnkLo, xmax = rnkUp)) +
            geom_point(size=1) +
            xlim(1, max(results$rnkUp)) +
            geom_errorbarh() +
            labs(x=NULL, y=NULL) +
            Ctheme1
          
          ggarrange(kappaRnk, bootRnk,
                    labels=c("A. κ-clipped bootstrap rank", "B. bootstrap rank"),
                    hjust=c(-1,-1.6),
                    label.y=c(0.11, 0.08),
                    ncol=1, nrow=2)
        })
      
    } # end if for tab2
    
    if(input$tabs == "tab3"){

        output$cMYPlots <- renderPlot({
          input$cMYPlots
          
          Ctheme = theme(
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14))
          
          IFkapRnk <- ggplot(data=results, aes(x=rnkKapmed, y=log(IF), xmin=rnkKaplo, xmax=rnkKapup)) + 
            geom_point() +
            geom_errorbar(linetype=2, position=position_dodge(0.4)) +
            geom_smooth() +
            labs(x="κ rank", y="log Impact Factor") +
            geom_label_repel(aes(label = journal),
                             box.padding = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            Ctheme
          IFkapRnk

          ggarrange(IFkapRnk,
                    labels=NULL,
                    ncol=1, nrow=1)
        })

    } # end if for tab3
    
  }) # end tab Events
  
  session$onSessionEnded(stopApp)
  
} # end server

shinyApp(ui, server)
