## bar plots MPA data

# load libs
library(ggplot2)
library(ggthemr)

# 1 --------------------set colour palette with ggthemr() -------------------

dark_cols <- c("orangered4", "firebrick", "red", "orange1", "darkorange3", "blue4", 
               "lightslateblue", "darkgreen", "seagreen1", "indianred1", 'lightgoldenrod1', 
               "chartreuse2", "mediumturquoise", "yellow1", "orchid", "deeppink2", 
               "magenta4", "black", "pink", "green", "grey", "blue")

DarkCols1 <- c("#555555", dark_cols)
# remove previous effects:
ggthemr_reset()
# Define colours for your figures with define_palette
darkCols <- define_palette(
  swatch = DarkCols1, # colours for plotting points and bars
  gradient = c(lower = DarkCols1[1L], upper = DarkCols1[2L]), #upper and lower colours for continuous colours
  background = "white" #defining a grey-ish background 
)
# set the theme for your figures:
ggthemr(darkCols)



# 2 -------------------- run setFactorOrder function   -------------------

# run this function to set factor order
# from # https://stackoverflow.com/questions/19384304/is-there-a-way-to-order-the-ggplot2-facet-grid-subplots

setFactorOrder <- function(x, order=sort(levels(x))) { 
  # Returns a factor ordered by `order`.  
  # If order is missing, defaults to `levels(x)` if available, else to `sort(unique(x))`
  # Useful for ggplot and elsewhere were ordering is based on the order of the levels
  
  if (!is.factor(x)) {
    warning("`x` is not a factor. Will coerce.")
    levs <- sort(unique(x))
    if (missing(order))
      order <- levs
  } else {
    levs <- levels(x)
  }
  
  # any values in order, not in levels(x)
  NotInx <- setdiff(order, levs)
  
  if (length(NotInx)) {
    warning ("Some values not in x:\n", paste(NotInx, collapse=", "))
  }
  
  # levels(x) not explicitly named in order
  Remaining <-  setdiff(levs, order)
  
  order <- c(setdiff(order, NotInx), Remaining)
  
  factor(x, level=order)
}

# 3 --------------------read in data  -------------------


dat <- read.table("CellcompTrimmed.txt", header = TRUE)
## table of cellular compononet ontology and how many proteins 
# assigned to each, as a percentage of total. 
# as output from MPA (where it's usually plotted as a pie)

# set factor order using function 

dat[["CellularComp"]] <- setFactorOrder(dat[["CellularComp"]], c("Unknown", "Cytoplasm", "Membrane", "CellMembrane", "CF1", "CellinnerMembrane", 
                                                                 "Nucleus", "Secreted", "Plastid", "Chloroplast", "Mitochondrion", "Others"))





# 4 -------------------- Plotting   -------------------

p <- ggplot(dat, aes(x=Sample, y=Abund, fill=CellularComp), show_guide=FALSE) + 
  geom_bar(stat="identity") + 
  guides(fill = guide_legend(override.aes = list(size=14))) +
  theme_classic() +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(size=16, colour="black")) +
  labs(y = "Percentage") +
  theme(axis.text.y=element_text(size=15, colour="black")) +
  theme(axis.title.y=element_text(size=18, colour="black")) +
  #theme(axis.text.y=element_text(size=9)) +
  theme(legend.title = element_text(size=21)) +
  theme(legend.text=element_text(size=16)) +
  labs(fill="      Cellular\n   Component\n") +
  scale_fill_discrete(labels = c("Unknown", "Cytoplasm", "Membrane", "Cell Membrane", "CF1", "Cell Inner Membrane", 
                                 "Nucleus", "Secreted", "Plastid", "Chloroplast", "Mitochondrion", "Others"))

p


# save as pdf


# -------------------- Do the same for molecular function plots   -------------------

# read in data, as output from MPA

dat <- read.table("MolecOntol.txt", header = TRUE)

## set factor order 

dat[["MolFunc"]] <- setFactorOrder(dat[["MolFunc"]], c("Unknown", "Transferase", "Hydrolase", "Oxidoreductase", "Ligase", "Chaperone",  "Kinase", "Lyase", "Protease", "Aminoacyl-tRNAsynthetase",
                                                       "Ribonucleoprotein", "Nucleotidyltransferase", "ElongationFactor", "Helicase", "RibosomalProtein","Isomerase", "Others"))             


## plotting

Pm <- ggplot(dat, aes(x=Sample, y=Abund, fill=MolFunc), show_guide=FALSE) + 
  geom_bar(stat="identity") + 
  guides(fill = guide_legend(override.aes = list(size=14))) +
  theme_classic() +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(size=16, colour="black")) +
  labs(y = "Percentage") +
  theme(axis.text.y=element_text(size=15, colour="black")) +
  theme(axis.title.y=element_text(size=18, colour="black")) +
  #theme(axis.text.y=element_text(size=9)) +
  theme(legend.title = element_text(size=21)) +
  theme(legend.text=element_text(size=16)) +
  labs(fill="      Molecular Function Ontology\n") +
  scale_fill_discrete(labels = c("Unknown", "Transferase", "Hydrolase", "Oxidoreductase", "Ligase", "Chaperone",  "Kinase", "Lyase", "Protease", "Aminoacyl-tRNA synthetase",
                                 "Ribonucleoprotein", "Nucleotidyltransferase", "Elongation Factor", "Helicase", "Ribosomal Protein","Isomerase", "Others"))     


# fix legend. 

Pt <- Pm + guides(fill = guide_legend(override.aes = list(size=14), ncol=2))

Pt

# save as pdf

