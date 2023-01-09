"""
DESCRIPTION:
Program designed to take a fasta file and extract all the GATC digestion fragments in this genome

INPUTS:
--genome takes the path to the genome from which you want to extract the GATC fragments from

OUTPUTS:
bed file of the following format: chrom_id   start   stop
gff file containing the same informations
tsv file containing the kallisto target_id to 'gene_id' conversion file 
    containing the chrom start and stop in the geneID

OTHER OPTIONS:
--salmon option will add an identifier in place of the chromosome name to the bed file
--overlap option will cause the script to count the whole GATC site in the fragment
--overlap_size option allows the user to input a custom overlap to the fragments


DISCLAIMER:
This program is designed to be used as a part of a nextflow pipeline, so it will 
output the files in root directory

"""

import argparse
import re

import numpy as np
from Bio import SeqIO


# Defines arguments for command line call
parser = argparse.ArgumentParser()

parser.add_argument("--genome", action="store",
                    help = "<path> Path to the fasta file containing the genome to analyse")

parser.add_argument("--overlap", action="store_true",
                    help = "Defines the fragment with the whole GATC sites")

parser.add_argument("--overlap_size", action="store",
                    help = ("<int> Number of bases to make the sites overlap\n"
                            "(count starts at the last base of the first site and first base of the second site)"))

parser.add_argument("--salmon", action="store_true",
                    help = ("adds a unique identifier to each fragment to fir salmon's format"))

args = parser.parse_args()

# Gets the arguments from the command line
genome_file = args.genome
overlap = args.overlap
overlap_size = args.overlap_size
salmon = args.salmon


# Opening the file to write the positions (bed and GFF)
f_bed = open("sites.bed", "w")
f_gff = open("sites.gff", "w")
f_tsv = open("gene_id_DamID.tsv", "w")

# Writting the first Headers of the tsv file
f_tsv.write("TXNAME\tGENEID")

# Motif we are looking for
motif = "GATC"

i = 0
site = list()
sites_list = list()

# Cycles through the parsed chromosomes from the fasta file
for seq_record in SeqIO.parse(genome_file, "fasta"):
    
    
    chrom_name = seq_record.name
    
    chrom_id = seq_record.id

    # Cycle throught all the motif that are found in the chromosome
    for match in re.finditer(motif, str(seq_record.seq)):
        
        i += 1
        
        start_pos = match.start() +1
        end_pos = match.end() + 1
        
        site.append(chrom_id)
        site.append(start_pos)
        site.append(end_pos)
        site.append(chrom_name)
        
        sites_list.append(site)
        
        site = list()
        

# If overlap is specified, uses a bigger overlap
if overlap == True:
    overlap_value = 4


elif overlap_size is not None:
    
    while True:
        try:
            overlap_value = int(overlap_size)
            break
        except ValueError:
            raise ValueError("Please provide a number to --overlap_size")

    overlap_value = round((int(overlap_size) / 2)) + 2
    
# If not sepcified takes only the whole gatc into account
elif overlap == False:
    overlap_value = 0

# Initializing the unique identifier for salmon
if salmon == True:
    identifier = 0
else:
    identifier = ""

gene_id_list = list()

length_list = list()

for i in range(1, len(sites_list)):
    
    if sites_list[i-1][0] == sites_list[i][0]:
    
        if salmon == True:
            identifier += 1

        # Transform individual sites information into GATC fragments
        bin_chrom = (sites_list[i-1][0])
        bin_start = (sites_list[i-1][2] - overlap_value)
        bin_end = (sites_list[i][1] + overlap_value)
        bin_chrom_name = (sites_list[i-1][3])
        
        # In case a fragment is closer to the end or start than the overlap
        if bin_start < 0:
            bin_start = 0
        
        if bin_end < 0:
            bin_end = 1
        
        # Create the kallisto target_id and associates the DamID geneID
        target_id = f"_{i-1}"
        gene_id = f"{sites_list[i][0]}_{bin_start}_{bin_end}"
        
        f_tsv.write(f"{target_id}\t{gene_id}\n")
        
        length = bin_end - bin_start
        length_list.append(length)
        
        # Writes the position in the .bed file (chro/start/end)
        line_f_bed = (f"{str(identifier)}{bin_chrom}"
                      f"\t{bin_start}"
                      f"\t{bin_end}"
                      f"\t{bin_chrom_name}__{bin_start}__{bin_end}\n")
        f_bed.write(line_f_bed)
        
        # Gets the chromosome name from the file
        chrom_name = re.search("|(.+?)|", sites_list[i][0]).group(1)

        # Writes the same thing but in gff format
        line_f_gff = f"{chrom_name}\tgatc_finder\tgatc_frag\t{bin_start}\t{bin_end}\t.\t.\t.\t\n"
        f_gff.write(line_f_gff)


print(np.mean(length_list))
print(np.std(length_list))

f_bed.close()
f_gff.close()
f_tsv.close()
