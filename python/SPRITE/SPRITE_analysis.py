import pandas as pd


# Input
data_file = "Z:/Nathan/Code/TFL/R/data/SPRITE/SPRITE.txt"

# Delimiter
data_file_delimiter = '\t'

# The max column count a line in the file could have
largest_column_count = 0

# Loop the data lines
with open(data_file, 'r') as temp_f:
    # Read the lines
    lines = temp_f.readlines(100000000)
    
    for l in lines:
        # Count the column count for the current line
        column_count = len(l.split(data_file_delimiter)) + 1
        # Set the new most column count
        largest_column_count = column_count if largest_column_count < column_count else largest_column_count

# Generate column names (will be 0, 1, 2, ..., largest_column_count - 1)
column_names = [i for i in range(0, largest_column_count)]

# Read csv
df = pd.read_csv(data_file, header=None, delimiter=data_file_delimiter, names=column_names, nrows = 50)

print(df.head())
# print(df)



"""
path_sprite = "Z:/Nathan/Code/TFL/R/data/SPRITE/SPRITE.txt"
size = 1

df_sprite = pd.read_csv(path_sprite,
                        sep = "\t",
                        nrows = size,
                        header = None)

print(df_sprite.head())
"""
