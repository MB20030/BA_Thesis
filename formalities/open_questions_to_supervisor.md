# Open Questions

## Sources

1. How should I cite the Datasources, in the Coding Part?
My current approach is:
Before I load the file into R, I do a intext APA 7 citation of the source.
e.g. 
 # Import raw Aargau 2009 dataset
  # (Statistisches Amt Kanton Aargau, n.d.)

The longform/Bibliography Citation of the sources is then found in the Bibliography of the 
Thesis.qmd in the section "Data Sources"

Is this approach correct? 


2. How should I handle the special case of the Canton of Valais?

My current approach is:

Because the upper-tier apportionment in Valais is conducted separately within six electoral districts rather than canton-wide, I keep the list groups as they actually enter the seat-allocation process.

For example, I treat SP_GAUCHE_CITOYENNE and SP_GRUENE_UNABHAENGIGE as separate electoral groupings instead of merging them into one broader canton-wide party family. Since then It's unclear wether to distribute these seats to the SP or Greens. 

I plan to do my core-analysis ofer all elections, once with and once without Valais.

An alternative would be to calculate Valais twice:

once based on the legally relevant district-level list groups, as I currently do;
once using broader canton-wide party families, similar to how SRF presents the election results.

The problem with the second approach is that I have not found an official source from the Canton of Valais that provides such a canton-wide party aggregation.

Is my current approach methodologically appropriate, or would it be better to present both versions for Valais?