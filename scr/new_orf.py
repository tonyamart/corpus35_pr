import os
import re
from russpelling import *
import pandas as pd

try:
    folder = input('Write the folder name: ')
    os.chdir(folder)

    # test if all texts are covered by 'if file.endswith('.txt')
    # or change the if condition
    for file in os.listdir():
        if file.endswith('.txt'):
            print(file)
except:
    print('Sth is wrong with folder settings')

# compare to the files inside the folder:
# print(os.listdir())

try: 
    for file in os.listdir(): # for each file in the folder
        if file.endswith('.txt'): # which is .txt
            # make a new filename from the exising one by appending '_nf.txt'
            result_name = file.replace('.txt', '_nf.txt')
            with open(file, 'r') as text: # read exising file
                with open(result_name, 'w') as new_file: # open empty file_new-orf.txt in a writing mode
                     for line in text: # input to the function is a string
                        new = None # empty variables to store results
                        new_orf = []
                        
                        new = normalize(line) # new orthography function
                        new_orf.append(new + os.linesep) # add all lines from the file separated by newline (could be '\n' but I put os-specific line separator)

                        result = ''.join(new_orf) # paste it as strings separated by the new lines
                        new_file.write(result) # write the result to the new file
                        print('Text in the modern orthography is written in', result_name)
except:
    print('Sth went wrong during rewriting')                    

