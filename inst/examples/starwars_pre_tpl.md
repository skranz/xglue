## Star Wars Characters By Species

Number of total characters: {nrow(dat)}

#< use dat; group_by species; collapse <<newline>>

### Characters of species {first(species)} ({nrow(.DATA)})


#< pre
|
#< collapse
{fill('<<cols>>',<<ns>>,' ')}|
#>

|
#< collapse
{fill('',<<ns>>,'-')}|
#>
#>

#< collapse <<newline>>

#< pre
|
#< collapse
{fill(format.na(<<cols>>),<<ns>>,' ')}|
#>
#>
#>
#>
