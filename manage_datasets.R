## @knitr manage_datasetss
## author             = Michael Flanagin
## date started       = 1 Jun 2018
## date last modified = 3 Jun 2018
## advisor            = Amalia Magaret
## objective          = Analyze results of analysis


#' [ step 0: set output directory for simulation results ]
dir_of_dirs <- "~/Downloads/MSThesis/expanded_datasets/"
dirs <- sapply( dir(), function( .dir ){ paste0(dir_of_dirs, .dir, "/results/", collapse="/")})

sink( file = "contY-binX-big-description.txt", append = TRUE )
describe( "~/Downloads/MSThesis/expanded_datasets/contY-binX-big/results/" )
sink()

sink( file = "howmany-description.txt", append = TRUE )
describe( "~/Downloads/MSThesis/expanded_datasets/howmany/results" ) #' 48mb 288 models no draws
sink()

sink( file = "sim-alloc-contY-binX-description.txt", append = TRUE )
describe( "~/Downloads/MSThesis/expanded_datasets/sim-alloc-contY-binX/results/" )
sink()

sink( file = "sim-study-description.txt", append = TRUE )
describe( "~/Downloads/MSThesis/expanded_datasets/sim-study/results/" )
sink()

