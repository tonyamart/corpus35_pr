## Metadata
This folder contains tables with bibliographical, biographical, and text-level metadata used for the corpus building and analysis. 

## Book-level metadata
### bibliography_1830-1850.tsv
The bibliography of poetry books printed between 1830 and 1850. 
- `poetry_prose`: column indicating if a book contains some prose (pr) or only verses (v);  
- `book_type`: one of the three types of books in bibliography: almanacs, collections, separate editions;  
- `genre`: more specific description of a book's contents: lyrical or narrative poetry; *this category is available only for collections (col) and separate editions (sep), empty in case of almanacs*;   
- `special_tag`: some notes on the genre of poems (elegy, songs, orthodox poetry); `double` stays for the 2nd & 3rd editions of the same book; `unknown` mentioned in the cases there a book is not found in the modern collections (source is either archival or 19-c. catalogue);  
- `author_sign` - author's name as stated in the book; `author`, `author_full_name` - a real author's name, if available (otherwise same as the `author_sign`);  
- `title`, `city`, `publisher`, `year` - bibliographical data regarding the book;  
- `pages`, `size_fold`, `size_cm` - physical description of the book (number of pages, folding, size in cm);  
- from `smirdin_1832` to `MU_1838`: data on the book's availability and price in the 19th-c. catalogues; the books id in the catalogue is used if available;   
- from `CZ_SLK` to `SmSok`: data on the book's availability in the modern collections; the books id in the catalogue is used if available;  
- `digital_copy`, `digital_copy_URL`: data on the book's digitized version;  
- rest of the columns: comments and references to the archival sources regarding the book in the censorship.  

## Text-level metadata
### texts-meta_collections.csv
The metadata for each text available from the books in the bibliography. To be merged with `collections_1835-1840.csv` via `id`.  


## working_files/
A folder with additional tables used for merging different tables together. 
- `authors_cols.csv`: author name as stated in the metadata (bibliography_1830-1850) to A_ID;