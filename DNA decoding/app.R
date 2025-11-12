library(shiny)
library(shinythemes)

source(file = "src/decode_functions.R")

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  
  # Application title
  titlePanel(title=div(img(src="myneotx_logo.jpeg", height = '40', width = '40'), "myNEO Therapeutics"),windowTitle ="myNEO Therapeutics" ),
  #Add area to paste input
  tabsetPanel(
    tabPanel("DNA decoding",
             sidebarLayout(sidebarPanel(width = 3,
               textAreaInput("input", "Insert valid DNA sequence", rows = 8,
                             value = "CGTACGTACGTACGTACGTAGTCGCACTACACAGTCGACTACGCTGTACTGCAGAGTGCTGTCTCACGTGATGACGTGCTGCATGATATCTACAGTCATCGTCTATCGAGATACGCTACGTACGT"),
               actionButton("decode_button", "Decode DNA", class = "btn-success"),
               ),
               
    
    mainPanel(plotOutput("decoded_message"))))))

server <- function(input, output, session) {
    output$decoded_message <- renderPlot({
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    input$decode_button
    
    # Use isolate() to avoid dependency on input$obs
    message_decoded <- isolate(decode_dna(input$input))
    ggplot(message_decoded,aes(x=position,y=Code))+
      geom_tile(aes(fill = Code),color="black")+ #width=2, height=1
      geom_text(aes(size= Code,label = Value))+
      scale_size_manual(values=c(3,3,6))+
      scale_fill_manual(values=c("lightgreen","#FF8488","lightblue")) +
      theme_minimal() +
      xlab("")+ylab("")+
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = "none",
            strip.background = element_blank(),
            strip.text.x = element_blank())+
      facet_wrap(~bin,scales = "free",ncol = 1)
    
    
  })
}

shinyApp(ui, server)