# GerNameR
Collection of names and associations to names for German

## Usage

### Installation

The software can be installed directly from github using the devtools package. Just type

```R
devtools::install_github("aggloeckner/GerNameR")
```

at the R prompt to install the software. If you don't have devtools installed currently, please install the devtools package via the
command

```R
install.packages("devtools")
```

### Selecting Names

Names can be selected and grouped using a variety of commands. The simples method is to filter based on attributes.
You can use the method "filter.names()" for this. E.g. 
```R
filter.names(Intelligence > 0.8)
```
will select all names where Intelligence is above the 80th percentile.

### Grouping names

To create stimulus materials it is often necesary to group names into pairs of most similar names. To do this, select 
sets to be paired (if you do not have any criterion to split the names on, e.g. to compare male and female names,
you can select random sets) and perform the grouping using "match.split()". E.g.
```
# Create a split based on Sex
s <- partition.names(Sex)

# Match pairs of male and female names
m <- match.split( s )
```
will create two groups of male and female names and then create pairs of names with each pair consisting of one male
and one female name.
