##README for Oak sapling dataset
##e321
##Chad Zirbel, Forest Isbell, David Tilman
##Contact: crzirbel@gmail.com, isbell@umn.edu

##EXPERIMENTAL SETUP##

#Fencing
In the fall of 2017 fencing was installed around a 210 acre section of the oak savanna at Cedar Creek. See figure x for a map of the fenced area.

#Plot fences
In the spring of 2018 42 plots were established. These consit of 14 sets of plots. One plot outside the bison fence and two plots inside the fence.
The plots inside the fence are imediately adacent to each other. One plot allows grazing by bison while the other is surround by cattle pannel and
excludes grazing by bison. There are 2 sets of plots in each of 7 burn units. Each plot is 7m by 7m.

#Tree planting
In the spring of 2018 oak saplings from two species, Quercus macrocarpa and Quercus ellipsoidalis, were planted into 30 of the 42 plots.
Trees were not planted into plots 3,4,19,20,21,22,23,24,25,26,39,40. Four groups of 5 trees were planted into each plot (10 from each species). These four groups consist of four
different planting types. A Q. macrocarpa monoculture, a Q. ellipsoidalis monoculture, four Q. macrocarpa with a Q. ellipsoidalis in the center, and
four Q. ellipsoidalis with a Q. macrocarpa in the center. Two additional Q. macrocarpa were planted in each plot as well. This resulted in a total of
660 trees. After planting we watered each tree with 2 gallons of water twice per week for 1 month unless there was sufficent rain.
During the spring tree survey we replanted any saplings that did not survive the initial planting shock. These are recored in the notes section of oak_seedling_data.csv
See figure XX for a visual represntation how trees were planted into each plot.

#Bison grazing
In 2018 32 two year old male bison were allowed to graze freely within the fenced area from June 13th until September 14th.
In 2019 17 two year old male bison were allowed to graze freely within the fenced area from May 25th until October 17th.

#Tree measurements
In the spring and Fall of 2018 and we measured all planted trees. For each tree we recoreded the height of the tallest bud, the diameter of the root collar
and whether or not each sapling had leaves.

##Metadata for oak_seedling_data.csv

#variable	units	description
plot		NA		the plot each tree was planted in.
group		NA		Is the tree planted in the NW, NE, SE, C, or SW group
subplot		NA		Which sub-plot is each tree planted in (combination of the plot and group codes)
tree		NA		which position in the sub-plot is each tree planted in. 1-5 for the four groups and N or S for the C group
tree.id		NA		unique tree id for each individual in the experiment
height		cm		oak sapling height from the ground to the highest bud
diameter	mm		oak sapling root collar diameter where the stem meets the ground
leaves		NA		Did the sapling have leaves at the time of sampling: Yes or No
tag_number	NA		physical tag number placed on each sapling in the field
notes		NA		notes about the sapling
date		NA		date the measurements were taken
year		NA		The year the measurements were taken in
season		NA		The season the measurements were taken in

##Metadata for oak_sp_planting_list
Below each sub-plot has one of the following four planting types. e.mono= Q. ellipsoidalis monoculture, m.mono= Q. macrocarpa monoculture,
e.mix= four Q. macrocarpa with a Q. ellipsoidalis in the center, and m.mix= four Q. ellipsoidalis with a Q. macrocarpa in the center. 

#variable	units	description
plot		NA		the plot number
NW			NA		Planting type for the NW sub-plot
NE			NA		Planting type for the NE sub-plot
SW			NA		Planting type for the SW sub-plot
SE			NA		Planting type for the SE sub-plot

##Metadata for e321_site_data.csv

#variable		units		description
plot			NA			the plot number
burn.unit		NA			burn unit number each plot is located in
fire.frequency	burns/years	How frequently does each plot burn
cover.type		NA			whether the dominant cover type of the plot is grass, shrub, or forest
old.field		NA			Is the plot in an old field (perviously row crops) or not: Yes or No
grazed			NA			Is the plot open to grazing: Yes or No
bison_fence		NA			Is the plot within the bison enclosure: Yes or No 
fence_type		NA			Three plot types: outside the fence and open to grazing, inside the fence and open to grazing, and inside the fence with no grazing
tree.planted	NA			Whether or not a tree was planted in the plot: Yes or No