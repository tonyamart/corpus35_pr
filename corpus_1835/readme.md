### Corpus of Russian poetry 1835-1840  
  
This part of repository includes the corpus of poetry printed between 1835 and 1840 in Russian. The corpus is based on digitised historical sources and annotated by me.  
Online version of the corpus is available as a [shiny app](https://tonyamart.shinyapps.io/corpus_1835/).  
  
Folder structure:  
- `corpus_1835.Rds`: a short version of the corpus, an `.Rds` file ready to be imported to R;  
- `csv/`: related csv files (database-like) with full information gathered for each text; the structure of the database is shown below.
  
![schema]()
  
`csv/` **tables description**
- `texts_metadata`: contains information about each text, both bibliographical (author, source, title, pages) and formal (number of lines, meter, and feet);  
- `texts`: table with different types of texts (raw, cleaned, lemmatised, accented); to be connected with `texts_metadata` via *text_id*;  
- `sources`: description of used sources, periodicals and poetry books can be filtered via *type*  column; each source has a *source_id* which connects it to the `texts_metadata` table;  
- `reviews`: reviews gathered for poetry books, connected with `sources` via *source_id*;
- `authors`: some information about known authors; to be connected with `texts_metadata` via *A_ID* key;  
- `rhyme_pairs`: a list of rhyme pairs found in each text, to be connected with `texts_metadata` via *text_id*;
- `rhyme_words`: each rhyme word annotated with stress, POS and grammatical features, closure type, etc. Can be connected to `rhyme_pairs` to *from* or *to* columns.  