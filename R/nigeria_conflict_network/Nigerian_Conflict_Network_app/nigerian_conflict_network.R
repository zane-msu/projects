#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(igraph)

load("nigeriaData")

# create edges df with renamed variables
ng_edges <- nigeriaData %>%
    rename(source = Var1, target = Var2, conflict = value, year = L1)


# get unique source and target actors 
uniq_s <- unique(ng_edges$source)
uniq_t <- unique(ng_edges$target)
# are they equal to each other? (result is TRUE, they are equal to each other) 
# all_equal(uniq_s, uniq_t)
uniq_orgs <- unique(ng_edges$source)

id <- 1:length(uniq_s)

# create nodes df
ng_nodes <- as.data.frame(uniq_orgs)
ng_nodes <- cbind(ng_nodes, id) # add id variable
names(ng_nodes) <- c("actor", "ID") # rename nodes column headers 

# for each source of conflict their total years conflicting with each target
ng_edges_weight <- ng_edges %>%
    select(source, target, conflict) %>%
    group_by(source, target) %>%
    summarise_all(sum)
# filter out pairs of sources and targets that had zero conflicts
ng_edges_weight <- ng_edges_weight %>%
    filter(conflict !=0)

# size of nodes, can be based on the total number of conflicts occurred with an organization (could be source or target of conflict) 
# need to get the total number of years with conflict for each organization regardless of source/target
# create two summary tables one grouped by source, and one by target, with total conflict of each organization
source_conf <- ng_edges_weight %>%
    group_by(source) %>%
    summarize(
        source_conflicts = sum(conflict)
    ) %>%
    rename(organization = source)

target_conf <- ng_edges_weight %>%
    group_by(target) %>%
    summarize(
        target_conflicts = sum(conflict)
    ) %>%
    rename(organization = target)

# want to join these two dataframes to get the true total conflicts, regardless of source/target position
# full_join() puts NA for orgs that don't appear in one of the datasets as their source/target value
# change NAs to zeros to ensure the total column adds up correctly
conflicts <- full_join(source_conf, target_conf, by = "organization", keep = TRUE)
conflicts$source_conflicts[is.na(conflicts$source_conflicts)] <- 0
conflicts$target_conflicts[is.na(conflicts$target_conflicts)] <- 0

# create a new column in conflicts, that sums the source and target conflicts to get a count of total conflicts for each organization
conflicts <- conflicts %>%
    mutate(
        total_conflicts = source_conflicts + target_conflicts
    )

# need to get orgs and total conflicts into named vector format to add it as a column for V()
confs <- as.numeric(conflicts$total_conflicts)
names(confs) <- conflicts$organization
confs <- log(confs +1)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Nigerian Conflict"),

    # drop-down with which centrality statistic you want to select
    sidebarLayout(
        sidebarPanel(
            selectInput("centStat",
                        "Choose a centrality statistic:",
                        choices=c(
                            "degree",
                            "betweenness", 
                            "closeness",
                            "eigenvector"
                        )
            )
        ),

        # Show plot that changes with user choice, table, and custom plot
        mainPanel(
           plotOutput("ng_netPlot"), tableOutput("org_table"), plotOutput("customCent_ng_netPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    prep_ng <- reactive({
        
        # graph from the df, directed is TRUE because the conflict has sources and targets
        ng_g <- graph_from_data_frame(d = ng_edges_weight, vertices = ng_nodes, directed = TRUE)
        # set weights manually
        ng_g <- set_edge_attr(ng_g, "weight", value = ng_edges_weight$conflict)
        
        # calculate centrality statistic
        if(input$centStat=='degree'){
            V(ng_g)$size = degree(ng_g) }
        if(input$centStat=='betweenness'){
            V(ng_g)$size = betweenness(ng_g) }
        if(input$centStat=='closeness'){
            V(ng_g)$size = closeness(ng_g) }
        if(input$centStat=='eigenvector'){
            V(ng_g)$size = eigen_centrality(ng_g)$vector } 
        
        # set other attributes
        # only put organization name on the node if they had 5+ years with conflicts since 1997
        V(ng_g)$label = ifelse(
            confs > 2,
            V(ng_g)$name,
            NA
        )
        
        # add width of edges
        E(ng_g)$width = log(E(ng_g)$weight + 1)
        
        
        # set color of nodes that are primarily sources red, primarily targets blue, undecided or other as black
        # need to do manually
        # red = primarily source
        # blue = primarily target
        # green = Nigerian police/military
        # grey = neutral 
        # category is determined by a >= 3 difference in either direction makes you primarily that side
        # is less than 3 you are neutral, unless you have a 0 in one side then youre categorized as the one with values
        
        V(ng_g)$color = c('grey', 'blue', 'grey', 'red', 'grey', 'red' ,'blue' ,'blue', 'red', 'green', 'red', 'green',
                          'grey', 'blue', 'grey', 'grey', 'grey', 'grey', 'red', 'grey', 'grey', 'grey', 'grey', 'blue',
                          'red', 'grey', 'grey', 'grey', 'grey', 'blue', 'red', 'grey', 'grey', 'blue', 'blue', 'blue',
                          'blue')
        return(ng_g)
    })

    output$ng_netPlot <- renderPlot({
        ng_g = prep_ng()
        
        # rescale for visualization
        V(ng_g)$nodeSize = rescale(V(ng_g)$size, to=c(1, 25))
        
        # plot
        set.seed(23)
        plot.igraph(
            ng_g,
            layout = layout_with_fr,
            vertex.color = V(ng_g)$color, # changes the color of the nodes
            vertex.size = V(ng_g)$nodeSize, # controls the sizing of the nodes
            vertex.label = V(ng_g)$label, # controls what text is displayed on the nodes
            vertex.label.color = 'black', # changes the color of the label text
            vertex.label.cex = .35, # change size of labels to 50% of original scale
            edge.curved = .25, # controls level of curving to dataset (25%)
            edge.color = 'grey20',
            edge.width = E(ng_g)$width, # assigns edge width
            edge.arrow.size = .18, # controls arrow size (18%)
            main = "Nigerian Conflict",
            xlim = c(0,.25),
            ylim = c(-.9,.9)
        )
    })
    
    output$org_table <- renderTable({
        tab = conflicts
        
        return(tab)
    })
    
    
    
    output$customCent_ng_netPlot <- renderPlot({
        
        ng_g = prep_ng()
        
        # override centrality statistics with custom sizing
        V(ng_g)$size = confs
        V(ng_g)$nodeSize = rescale(V(ng_g)$size, to=c(1, 25))
        
        # plot
        set.seed(23)
        plot.igraph(
            ng_g,
            layout = layout_with_fr,
            vertex.color = V(ng_g)$color, # changes the color of the nodes
            vertex.size = V(ng_g)$nodeSize, # controls the sizing of the nodes
            vertex.label = V(ng_g)$label, # controls what text is displayed on the nodes
            vertex.label.color = 'black', # changes the color of the label text
            vertex.label.cex = .35, # change size of labels to 50% of original scale
            edge.curved = .25, # controls level of curving to dataset (25%)
            edge.color = 'grey20',
            edge.width = E(ng_g)$width, # assigns edge width
            edge.arrow.size = .18, # controls arrow size (18%)
            main = "Nigerian Conflict",
            xlim = c(0,.25),
            ylim = c(-.9,.9)
        )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
