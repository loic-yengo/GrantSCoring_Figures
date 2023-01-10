This folder contains R scripts to generate figures for
Visscher & Yengo (2023), "The effect of the scale of grant scoring on ranking accuracy"


-- CorrEquiSpaced.R: an R script defining the function "CorrEquiSpaced(K)", which input is the number of score categories and output is a list containing (1) the expected correlation between the continuous and discretised scoring scales, (2) the expected proprtions of scores within each category, (3) the thresholds used to discretize the continuous scale, and (4) the spacing between thresholds.
-- Figure1.R : Uses CorrEquiSpaced.R to reproduce Figure 1.
-- Figure2.R : Uses CorrEquiSpaced.R to reproduce Figure 2.
-- Figure3.R : Uses CorrEquiSpaced.R to reproduce Figure 3.
-- Figure4.R : Uses CorrEquiSpaced.R to reproduce Figure 4.
