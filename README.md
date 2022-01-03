# Global Study of Scientific Expertise

## A cross-cultural database containing ethnographic descriptions of scientific and conceptual expertise

This R data package includes many variables that were coded on 547 text records from ethnographic texts about 55 cultures in the Human Relations Area Files. Uses of cleaner and simplified versions of these datasets are featured in my other repos and their linked papers, such as [our paper on knowledge specialists](https://github.com/alightner/conceptualExpertsHRAF) and [our paper on ethnomedical specialists](https://github.com/alightner/ethnomedicine-magic). This data package also includes variables at the culture level from the Standard Cross Cultural Sample, which we pulled and integrated from the excellent [D-PLACE](https://d-place.org) project.

This is a working repo while we prepare a large exploratory analysis for publication, but if you are interested in the work we are doing here, then please direct any questions, concerns, and suggestions to Aaron Lightner, at adlightner at cas dot au.dk.

## Getting additional text data wrangled together for analysis

The raw text has already been compiled into a single tsv file, called `data-raw/raw-text-dataset.tsv`. An Rdata object called `data/text_data.rda` is also included, which can be loaded into an R session if needed.

If you want to re-compile the raw text into a new csv file for some reason (e.g., perhaps you added some text records to the dataset and want to update the dataset for a new analysis), then you can use the python script, `data-raw/pull-text.py` to do so.

Here is how I used the python script to pull together my own raw text records into a csv file:

1. All raw text files are kept in the `data-raw/raw-text` directory, and are kept in a consistent format: for example, `raw-text-file--first-pass.txt` (and `second-pass`, `third-pass`, and `fourth-pass`). Each "pass" refers to an added query that was contributed to the raw text, and this format in general is meant to allow the dataset to be indefinitely extensible. For example, if one wanted to add a fifth search to the raw text database, they could do so (keeping the format consistent with the first, second, third, and fourth passes, and store the text file as `raw-text/raw-text-file--fifth-pass.txt`.
2. From the main directory and in the command line, you can then pull some or all the text files in `data-raw/raw-data` by running `python3 data-raw/pull-text.py`. You will be asked to specify which files you want to pull, and should only type which pass(es) you wish to compile. To pull all of them, include `first-pass`, `second-pass`, `third-pass`, and `fourth-pass`. When you are finished, enter a blank entry to exit the loop.
3. You will be prompted to name the file to which the data are written, e.g., `data-raw/raw-text-dataset.csv`.
4. That's it! You pulled all the raw text into a single csv file in the `data-raw` directory. To write it to an Rdata object, you can re-build the package using devtools.
