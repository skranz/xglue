## Star Wars Characters By Species

Number of total characters: {nrow(dat)}

#< use dat; group_by species; collapse <<newline>>

### Characters of species {first(species)} ({nrow(.DATA)})

#< ignore
A simple Markdown table looks as follows
| Col1 | Col2 |
|------|------|
| x1   | y1   |
| x2   | y2   |
#>
|{fill('Name',20,' ')}|{fill('Homeworld',20,' ')}|
|{fill('',20,'-')}|{fill('',20,'-')}|

#< collapse <<newline>>
|{fill(name,20,' ')}|{fill(homeworld,20,' ')}|
#>
#>

