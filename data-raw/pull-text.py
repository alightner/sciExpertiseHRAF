import re
import string
import numpy as np
import pandas as pd

# filepath = "raw-text/raw-text-file--third-pass.txt"
# filepath = "raw-text-file.txt"
cstr = "--C: "
idstr = "--d"
citestr = "--x"
parstr = "--p"
ocmstr = "--cm"

def filepath_entry():
    rootstr = "raw-text/raw-text-file--"
    stemstr = ".txt"
    filesout = []
    while(True):
        tmp = input("file: ")
        if tmp == "":
            break
        addfile = rootstr+tmp+stemstr
        filesout.append(addfile)
    return(filesout)

def pull_paragraphs():
    """creates a csv file df with paragraph text"""
    filepaths = filepath_entry()
    outfile = input("csv file write: ")
    df = pd.DataFrame(columns=['culture','id','citation','ocm'])
    for files in filepaths:
        with open(files, 'r') as f:
            for line in f:
                if line.startswith(cstr):
                    culture = (line.replace(cstr, '')).rstrip()
                if line.startswith(idstr):
                    idn = next(f).rstrip()
                if line.startswith(citestr):
                    cited = next(f).rstrip()
                if line.startswith(parstr):
                    addtxt = []
                    for line in f:
                        if line =='\n':
                            break
                        else:
                            addtxt.append(line.strip())
                    d = {'culture':culture, 'id':idn, 'citation':cited, 'paragraph':addtxt}
                    xdf = pd.DataFrame(d)
                    df = df.append(xdf)
    df.to_csv(outfile, index=False)
    return(df)

"""
def ocm_edges():
    df = pd.DataFrame(columns=['ego','alter'])
    with open(filepath, 'r') as f:
        for line in f:
            if line.startswith(ocmstr):
                ego = next(f).rstrip()
                #alter = []
                for line in f:
                    if line =='\n':
                        break
                    else:
                        alter = line.strip()
                        #alter.append(line.strip())
                        d = {'ego':ego, 'alter':alter}
                        xdf = pd.DataFrame(d, index=[0])
                        df = df.append(xdf)

    outfile = "ocmedges.xlsx"
    df.to_excel(outfile, index=False)
    
    return(df)
"""

pull_paragraphs()
# ocm_edges()






