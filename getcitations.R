#    ---- BibTeX Citation Organizer ----
# Garrick Aden-Buie: <http://garrickadenbuie.com>
#
# Searches through Pandoc-styled markdown documents for citation keys
# and pulls the corresponding citation from a central `.bib` file.
# For me, this file comes from Mendeley and automatically contains every
# article in my database. But Mendeley adds lots of unnecessary entries; thus 
# I keep only the minimum-required entries. Add keywords to the `keep` variable
# to suite your needs.
#
# Usage: > Rscript getcitations.R <input doc file> <output bib file>
# WARNING! I assume all synchronization occurs through the central file, so
#          the output file is blindly overwritten.
#
# The script looks for citations like: `@CitationKey`
# Citations start with `[`, a space or at the begining of a line,
# followed by an `@`, the citation key with any punctuation + alphanum chars
# and end with a space, `]`, `;`, `,` or at the end of a line.
#
# BibTeX entries start on lines with `@<type>{CitationKey,`
# and end with a line with a single closing bracket `}`.


#          ---- User Variables ----

# Set Master BibTeX file location
MasterBibtexFile <- "~/.pandoc/library.bib"

# Citation entries to keep, all others are skipped
keep <- c('author', 'title', 'journal', 'year', 'volume', 'number',
          'pages', 'month', 'editor', 'publisher', 'series', 'edition',
          'adress', 'booktitle', 'organization', 'chapter',
          'school', 'doi', 'url', 'howpublished', 'institution')


#             ---- Set Up ----

require(stringr, quietly=TRUE)
args <- commandArgs(trailingOnly = TRUE)
infile <- args[1]
outfile <- args[2]
unlink(outfile)


#    ---- Find citation keys in markdown ----

stripCitekeys <- function(infile){
  indoc <- file(infile, open='r')
  cat(paste('Finding citations in:',infile))
  citations <- c()
  for(line in readLines(indoc, warn=F)){
    if(str_detect(line, '@')) {
      candidate <- str_match_all(line, "([[[:space:]]|^)@([[:alnum:]:&!~=_+-]+)([]\\)\\;\\,\\s[:space:]\\.]|$)")[[1]]
      if(length(candidate) != 0){
        for(cite in candidate[,3]){
          citations <- c(citations, cite)
        } 
      } else {
        candidates <- str_match_all(line, "[[:space:]|^][[:alnum:][:punct:]]+@[[:alnum:][:punct:]]+")[[1]]
        cat('\nTrouble match: ', candidates)
      }
    }
  }
  close(indoc)
  return(unique(citations))
}

citations <- stripCitekeys(infile)


#    ---- Find and write BibTeX entries ----

cat(paste('\n\nWriting BibTeX entries to:',args[2],'\n'))
pb <- txtProgressBar(min=0, max=length(citations), initial=0, style=3)

keyfound <- FALSE
found <- c()
biblib <- file(MasterBibtexFile, open='r')
for(line in readLines(biblib, warn=F)){
  if(keyfound){
    # Key is found, so print any entries we want to keep.
    if(str_match(line, "(\\w+)[[:blank:]][=]")[1,2] %in% keep){
      cat(paste0('    ',line), file=outfile, append=TRUE, sep='\n')
    }
    if(line == '}'){
      # Print last line and turn off printing until next key is found.
      cat('}', file=outfile, append=TRUE, sep='\n')
      keyfound <- FALSE
    }
  }
  
  # Find lines with keys, print the first line and turn printing on
  if(str_detect(line, '@')){
    key <- str_match(line, "@\\w+[\\{]([^\\,]+)[\\,]")[1,2]
    if(key %in% citations){
      keyfound <- TRUE
      if(key %in% found){
        warning(paste0("Found more than one entry for citation key @", key))
      } else {
        found <- c(found, key)
      }
      cat(paste0('\n',line), file=outfile, append=TRUE, sep='\n')
    }
  }
  setTxtProgressBar(pb, value=length(found))
}

close(biblib)
cat('\n')


#      ---- Final output checks ----

# Warn if entries aren't found in BibTeX file
if(length(found) != length(citations)){
  warning('BibTeX entries were not found for all citations.')
  for(cite in citations){
    if(!(cite %in% found)){
      cat(paste0('Bibtex entry not found for @',cite),sep='\n')
    }
  }
}

# Print citations found, provides early warning if pandoc-citeproc will fail
cat(paste(length(citations), 'citations found in doc,', length(found),
          'in', MasterBibtexFile, '\n\n', sep=' ')) 

# cat(citations, sep='\n')
