## Star Wars Characters

Number of total characters: {nrow(dat)}

Data collected from the films:
#< collapse ', '
{films}
#>

#< use dat; group_by species; collapse <<newline>>

### Characters of species {first(species)} ({nrow(.DATA)})

{md_table(.DATA, cols)}

#>

