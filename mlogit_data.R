https://cran.r-project.org/web/packages/mlogit/refman/mlogit.html#mlogit-deprecated

choice	
the variable indicating the choice made: it can be either a logical vector, 
a numerical vector with 0 where the alternative is not chosen, a factor with
level 'yes' when the alternative is chosen


shape	
the shape of the 'data.frame': whether 'long' if each row is an alternative 
or 'wide' if each row is an observation,

varying	
the indexes of the variables that are alternative specific,

sep	
the seperator of the variable name and the alternative name (only relevant 
for a 'wide' 'data.frame'),


alt.var	
the name of the variable that contains the alternative index (for a 'long'
 'data.frame' only) or the name under which the alternative index will be
 stored (the default name is 'alt'),

chid.var	
the name of the variable that contains the choice index or the name under
 which the choice index will be stored,


alt.levels	
the name of the alternatives: if null, for a 'wide' data.frame, they are 
guessed from the variable names and the choice variable (both should be the 
same), for a 'long' 'data.frame', they are guessed from the 'alt.var' argument,


