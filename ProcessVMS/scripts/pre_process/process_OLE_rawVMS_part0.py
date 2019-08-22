####### Pre-process raw VMS data from OLE #######
#
# This script (1) reads in all OLE files for a single year and (2) checks for VMS data which does not match the date in the file.
#
# For the script to work, the year must be in the same location in each file name.
#
#################################################
## 3/4/2019 - M. Fisher
## Written for Python 3
## If the "interactive" option is chosen, must be run in terminal.
#################################################
################################################# 


# import modules
import argparse 
import glob
import re



# command line arguments
parser = argparse.ArgumentParser(description="check for the incorrect year in raw VMS files from OLE. write any misplaced lines into a separate text file. this will not delete misplaced lines.")

parser.add_argument("-i", "--indir", help="location + prefix of raw ole files - must include the wildcard symbol * at end. type=string")
parser.add_argument("-c", "--column", help="[0,1,2...] index for the date column (note: first column index is 0).type=integer")
parser.add_argument("-s", "--start", help="[0,1,2...] index for the first character of date within column (note: index starts at 0). type=integer")
parser.add_argument("-o", "--outfile", help="location + file name for output. type=string")
parser.add_argument("-v", "--verbose", help="[yes/no] extra reporting on script progress. type=string")

args = parser.parse_args()




# create function to be used on each file
def check_dates (myfile, outfile,col, date_begins, file_year) :
	r=0
	empty=0
	xtra_header=0
	# get start and end indices for date
	date_ends=int(date_begins)+4
	# PART I: Read in File and Check Dates #
	# read in the existing header and create a list
	header = myfile.readline()
	if "," in header:
		header_list = header.strip().split(",")
		split_by=","
	elif "\t" in header:
		header_list = header.strip().split("\t")
		split_by="\t"
	else:
		header_list = header.strip().split("  ")
		split_by="  "
	header_list = [i for i in header_list if i != ""]
	print("make sure that the script has chosen the correct delimiter for your file! does this header list split columns correctly?")
	print(header_list)
	print("\n\n")
	# prep for while loop: read in the first non-header line of the file; set counter
	line = myfile.readline()
	n = 1
	# for each line in the file...
	while line:		
		## if this is not a header line...
		if header_list[1] not in line and header_list[2] not in line:
			if not re.match(r'^\s*$', line) and "-----" not in line:
				linelist = line.strip().split(split_by) #strip away extra spaces at the end of the line, and split by comma
				linelist = [i.strip() for i in linelist if i != ""] #strip away extra spaces associated with each word / number
				if len(linelist) > 1:
					### for integer from 0 to length of the header list...
					tmp_yr = linelist[col][date_begins:date_ends]
					if tmp_yr != file_year:
						outfile.write(line)
						r += 1
						print("line", str(n), " date (", str(tmp_yr), ") does not match file date (", str(file_year),").")
				else:
					empty += 1
					if args.verbose=="yes":
						print("line ", str(n), " was an empty line. could not check date.")
			else:
				empty += 1
				if args.verbose=="yes":
					print("line ", str(n), " was an empty line. could not check date.")
		## if this is an extra header line...
		else:
			xtra_header += 1
			if args.verbose=="yes":
				print("line ", str(n), " was an extra header. could not check date.")
		## add to counter; read next line.
		n += 1
		line = myfile.readline()
	# summarise empty / header lines
	print("number of empty lines in file:", str(empty), "\nnumber of extra headers in file:", str(xtra_header))
	# write out the number of lines written to outfile
	print(str(r), " lines written to output because of mismatched dates.")
	# close file
	myfile.close()


# run function on each file
output = open(args.outfile, "a")
counter = 0
for filename in glob.glob(args.indir):
	counter += 1
	file_prefix = args.indir.replace("*","")
	tmp_year = filename.replace(file_prefix, "")
	tmp_year = tmp_year.strip("_").split("_")
	tmp_year = tmp_year[0]
	print("This file has data from ", str(tmp_year))
	with open(filename, "r") as infile:
		print("working on file: ", filename, ".\n")
		check_dates(myfile=infile, outfile=output, col=int(args.column), date_begins=int(args.start), file_year=tmp_year)
		print("completed file: ", filename, ".\n-----\n")

output.close()







