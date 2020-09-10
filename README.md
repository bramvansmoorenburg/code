# simple code
general code which can be contained in 1 document

##  15 digits
several functions which can format numbers into 'super numbers', which are numbers that can have more than 15 digits, the maximum allowed by R. these functions can also add, subtract and multiply these numbers. supernumbers are formatted in several chunks of 15 numbers. the first chunk is not part of the number, but indicates the location of the comma in the number. so a first chunk value of 20 means that the comma is located after the 20th digit, or digit 5 of chunk 2 of the actual number.
![alt text](https://github.com/bramvansmoorenburg/images/blob/master/15%20digits.png)

##  CVsteppin
in a linear model, one has a response variable and several explanatory variable. but which explanatory variables are the best? how many explanatory variables should one use? R has a built in 'step' function which can determine for each total number of explanatory variables which combination of explanatory variables is best. the CVsteppin function builds on top of this existing step function and uses cross-validated sums of squared error to determine how many explanatory variables one should use.
![alt text](https://github.com/bramvansmoorenburg/images/blob/master/CVsteppin.png)

##  Formatie - voorkeur (sum)
program which can be used following an election to determine which combination of parties could most easily form a government. takes as input an excel file with for each party the outcome in terms of parliament and senate seats as well as a preference list for each party and gives as outcome a sorted list with most likely combinations.
![alt text](https://github.com/bramvansmoorenburg/images/blob/master/formeren.png)

##  installed packages
program which can record which R packages are installed on your computer.

##  Tournament
a series of experimental simulations to determine which type of competition can best rank a set of contestants from best to worst. For example, if we have 64 contestants and can play 388 games, who should we let compete against who to be able to determine the ranking? the criterion to determine between different types of competitions is called 'loss', and it means how many swaps have to be made in the ranking on average to obtain the true ranking.
![alt text](https://github.com/bramvansmoorenburg/images/blob/master/tournament.png)

##  travelling salesman
set of experimental simulations to determine which path a travelling salesman should take if he wants to take the least amount of time to reach a number of cities. uses package ggplot to draw graphs of the path.
![alt text](https://github.com/bramvansmoorenburg/images/blob/master/travelling%20salesman.png)

##  automatic text generation
text mining from a large txt file. this program creates a dictionary with common letter combinations found in the txt file, then uses this dictionary to automatically generate new text similar to the original file. is not very useful but quite funny.

##  folder size
program which can determine which sub-folders in a particular folder take up the most memory space.

##  installing packages
function which can install a list of packages all at once. can be used in combination with the program for 'installed packages' above to install all packages which you previously recorded.

##  parallel computing
set of experiments which can show the benefits of parallel computing in R using package parallel. if applied properly, it works faster than regular computing.

##  Risk in R
function used for the classic board game risk. one of the earliest programs I wrote for R. determines for a given number of attacking armies and a given number of defending armies how many armies you are likely to lose in conquering the territory.
![alt text](https://github.com/bramvansmoorenburg/images/blob/master/risk%20dice.png)

##  Risk map
program which takes as input a file showing all territories and connections between them and computes the distance from each territory to each other territory, tries to reconstruct the map using multidimensional scaling and determines which territories are most isolated.
![alt text](https://github.com/bramvansmoorenburg/images/blob/master/risk.png)

##  spybots the nightfall incident
program which creates an app using the R package shiny to to re-create the old flash game spybots - the nightfall incident.
![alt text](https://github.com/bramvansmoorenburg/images/blob/master/spybots.png)

##  viswijzer
program which scapes data from the website goedevis.nl to determine which fish is in season and is caught sustainably.
![alt text](https://github.com/bramvansmoorenburg/images/blob/master/viswijzer.png)
