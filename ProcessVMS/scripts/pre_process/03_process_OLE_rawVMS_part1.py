####### Pre-process raw VMS data from OLE #######
#
# This script (1) reads in all files for a given year, and (2) runs a pre-processing function on them, then (3) writes all of that information to a single file.
#
# Breaking down (2) above: (a) remove all excess spaces at end of each row / around each string on each row, (b) remove any extra header rows sprinkled in the middle of the file, (c) re-order and re-name the columns when writing output.
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
parser = argparse.ArgumentParser(description="clean and combine raw VMS files from OLE for reading into R.")

parser.add_argument("-i", "--indir", help="location + prefix of files for given year (if f = multi, must include the wildcard symbol * at end).")
parser.add_argument("-f", "--file", help="[multi,single] whether a single input file is being entered, or a wildcard.")
parser.add_argument("-y", "--year", help="calendar year of the data.")
parser.add_argument("-o", "--outfile", help="location + file name for output.")
parser.add_argument("-c", "--columns", help="string with column headers for output file, separated by commas.")
parser.add_argument("-skip", "--skip_missing", help="[yes/no] if one or more fields are missing, write out the line as-is (yes) or raise an exception and stop the script (no).")
parser.add_argument("-interact", "--interactive", help="[yes/no] if interactive, script will allow you to manually enter the new column order for each input file. if not interactive, default order will be automatically used for each file.")
parser.add_argument("-v", "--verbose", help="extra reporting on script progress")

args = parser.parse_args()


# create function to be used on each file
def parse_file (myfile, outfile, first = "yes", new_header="UTCDATETIM,LATITUDE,LONGITUDE,VESSEL_NAM,AVG_SPEED,AVG_COURSE,DOCNUM,DECLARATIO\n"):
	# PART I: Read in / Clean up Existing File Information #
	# read in the existing header and create a list
	header = myfile.readline()
	if "UTC_DATE" in header:
		header = header.replace("UTC_DATE", "UTC_DATE_TIME")
	if "DOC_NUM" in header:
		header = header.replace("DOC_NUM", "DOCNUM")
	if "SPEED" in header and "AVG_SPEED" not in header:
		header = header.replace("SPEED", "AVG_SPEED")
		print("WARNING: no avg speed. writing out speed as avg speed.\n")
	if "COURSE" in header and "AVG_COURSE" not in header:
		header = header.replace("COURSE", "AVG_COURSE")
		print("WARNING: no avg course. writing out course as avg course.\n")
	if "," in header:
		header_list = header.strip().split(",")
		split_by = "comma"
	elif "\t" in header: 
		header_list = header.strip().split("\t")
		split_by = "tab"
	else:
		header = header.replace("UTC DATE TIME", "UTC_DATE_TIME")
		header = header.replace("UTC DATE", "UTC_DATE_TIME")
		header = header.replace("VESSEL NAME", "VESSEL_NAME")
		header_list = header.strip().split(" ")
		header_list = [i for i in header_list if i != ""]
		split_by = "space"
	if args.verbose=="yes":
		print(header)
		print(header_list)
		print("Splitting by: ", split_by)
	# get the index of the avg course and avg speed in the header list	
	speed_index = header_list.index("AVG_SPEED")
	course_index = header_list.index("AVG_COURSE")
	# set up dictionary, where keys are the column headers and entries are a list of values
	file_dict = {}
	for i in header_list:
    		file_dict[i] = []
	if args.verbose=="yes":
		print(file_dict.keys())
	# prep for while loop: read in the first non-header line of the file; set counter
	line = myfile.readline()
	n = 1
	# for each line in the file...
	while line:
		if header_list.index("UTC_DATE_TIME") < header_list.index("VESSEL_NAME"):
			## remove special characters / white space from end of line
			line = line.strip()
		## if the line is blank (re.match) or the line is just a series of dashes...
		if re.match(r'^\s*$', line) or list(set(line)) == ["-", " "] or list(set(line)) == [" ", "-"] or list(set(line)) == ["-"] or list(set(line)) == [" ", "-",","] or list(set(line)) == ["-", " ",","] or list(set(line)) == [',', ' ', '-'] or list(set(line)) == [" ", ",","-"] or list(set(line)) == ["-", ","," "] or list(set(line)) == [',', '-', ' ']:
			if args.verbose=="yes":
				print("line", str(n), " was an empty line. not appended to dictionary.")
		## if there is text in the line and it is not a header line...
		elif header_list[1] not in line and header_list[2] not in line:
			line=line.replace(" .","0.")
			line=line.replace(",.",",0.")
			if split_by == "comma":
				linelist = line.strip().split(",") #strip away extra spaces at the end of the line, and split by comma
				linelist = [i.strip() for i in linelist] #strip away extra spaces associated with each word / number
				linelist = ["NA" if i == "" else i for i in linelist] #add in NAs for missing data
			elif split_by == "space":
				linelist = line.strip().split(" ") #strip away extra spaces at the end of the line, and split by space
				linelist = [i.strip() for i in linelist if i != ""] #strip away extra spaces associated with each word / number. remove blank entries (due to spacing, not actually blank data)
			elif split_by == "tab":
				linelist = line.strip().split("\t") #strip away extra spaces at the end of the line, and split by tab
				linelist = ["NA" if i == "" else i for i in linelist] #add in NAs for missing data
				linelist = [i.strip() for i in linelist]

			if len(linelist) < 3: 
				if args.verbose=="yes":
					print("line", str(n), " was an empty line. not appended to dictionary.")
			elif len(linelist) < len(header_list):
				if args.skip_missing == "yes":
					print("WARNING: line ", str(n), " is missing one or more fields.")
					print(line)
					## insert "NAs" where speed / course columns should be in the current line (need to insert the smaller index first)
					if len(linelist) == (len(header_list)-1):
						if split_by=="space":
							### combine the split date (was split into day / time)
							date_index = header_list.index("UTC_DATE_TIME")
							new_date = linelist[date_index] + " " + linelist[date_index + 1]
							linelist[date_index] = new_date
							del linelist[date_index + 1]
						else:
							linelist.insert(speed_index, "NA")
					if len(linelist) < (len(header_list)-1): # assuming that course and speed are missing
						if speed_index < course_index:
							linelist.insert(speed_index, "NA")
							linelist.insert(course_index, "NA")
							if args.verbose == "yes":
								print(linelist)
						else:
							linelist.insert(course_index, "NA")
							linelist.insert(speed_index, "NA")
							if args.verbose == "yes":
								print(linelist)
					### if it's still shorter than the header list... just keep adding NAs at the end
					if len(linelist) < (len(header_list)):
						l = len(linelist)
						short_by = len(header_list) - l
						print(short_by)
						for i in range(0, short_by):
							linelist.append("NA")
					print(linelist)
					### for integer from 0 to length of the header list...
					for i in range(0, len(header_list)):
						tmp_col = header_list[i] #save tmp column name
						tmp_list = file_dict[tmp_col] #pull the existing list of values for that column from the dictionary
						tmp_list.append(linelist[i]) #append the value for the current line
						file_dict[tmp_col] = tmp_list #re-save the list under that column name  
					
				else:
					raise Exception("LINE ", str(n), " IS MISSING ONE OR MORE FIELDS.")
			else:
				# if there are 8 elements in the list, but the avg_speed column is NOT a float or integer or NA, then... 
					#(this should only happen when splitting by space)				
				if len(linelist) == (len(header_list)-1) and not re.match("^[-+]?[0-9]+$",linelist[speed_index]) and not re.match("^\d+?\.\d+?$", linelist[speed_index]) and linelist[speed_index] != "NA":
					### combine the split date (was split into day / time)
					if split_by == "comma":
						print(linelist) # this should not happen
						print("crap, something is wrong")
						print(line)
						print(list(set(line)))
					date_index = header_list.index("UTC_DATE_TIME")
					new_date = linelist[date_index] + " " + linelist[date_index + 1]
					linelist[date_index] = new_date
					del linelist[date_index + 1]
					### combine the split vessel name
					name_index = header_list.index("VESSEL_NAME")
					new_name = linelist[name_index] + " " + linelist[name_index + 1]
					linelist[name_index] = new_name
					del linelist[name_index + 1]
					print("WARNING: line ", str(n), " is missing one or more fields.")
					if speed_index < course_index:
						linelist.insert(speed_index, "NA")
						linelist.insert(course_index, "NA")
						if args.verbose == "yes":
							print(linelist)
					else:
						linelist.insert(course_index, "NA")
						linelist.insert(speed_index, "NA")
						if args.verbose == "yes":
							print(linelist)
				# if there are 9 elements in the list,
				if len(linelist) == (len(header_list)+1):
					### combine the first two columns -- the date was split into day / time
					date_index = header_list.index("UTC_DATE_TIME")
					new_date = linelist[date_index] + " " + linelist[date_index + 1]
					linelist[date_index] = new_date
					del linelist[date_index + 1]
					# if combining the day/time brings the length down to 8 elements, but the speed is not an integer or 0,
					if len(linelist) == (len(header_list)-1) and not re.match("^[-+]?[0-9]+$",linelist[speed_index]) and not re.match("^\d+?\.\d+?$", linelist[speed_index]):
						# combine the split vessel name
						diff = 2
						new_name = linelist[name_index]
						for i in range(1,diff+1):
							new_name += " " + linelist[name_index + i]
						linelist[name_index] = new_name
						for i in range(1,diff+1):
							del linelist[name_index + 1]
						## insert "NAs" for speed and course
						if speed_index < course_index:
							linelist.insert(speed_index, "NA")
							linelist.insert(course_index, "NA")
						else:
							linelist.insert(course_index, "NA")
							linelist.insert(speed_index, "NA")
						print("\n\nThis was a crazy line. Please make sure the list output is correct.")
				# if there are 10 elements in the list,
				elif len(linelist) >= (len(header_list)+2):
					### combine the split date (was split into day / time)
					date_index = header_list.index("UTC_DATE_TIME")
					new_date = linelist[date_index] + " " + linelist[date_index + 1]
					linelist[date_index] = new_date
					del linelist[date_index + 1]
					### combine the split vessel name
					name_index = header_list.index("VESSEL_NAME")
					new_name = linelist[name_index] + " " + linelist[name_index + 1]
					linelist[name_index] = new_name
					del linelist[name_index + 1]	
					### if the line is still longer than the header...
					if len(linelist) > (len(header_list)):
						diff = len(linelist) - (len(header_list)-1)
						for i in range(1,diff+1):
							new_name += " " + linelist[name_index + i]
						linelist[name_index] = new_name
						for i in range(1,diff+1):
							del linelist[name_index + 1]
					print("\n\nThis was a crazy line. Please make sure the list output is correct.")
																
				### for integer from 0 to length of the header list...
				for i in range(0, len(header_list)):
					tmp_col = header_list[i] #save tmp column name
					tmp_list = file_dict[tmp_col] #pull the existing list of values for that column from the dictionary
					tmp_list.append(linelist[i]) #append the value for the current line
					file_dict[tmp_col] = tmp_list #re-save the list under that column name           
		## if this is an extra header line...
		else:
			if args.verbose == "yes":
				print("line ", str(n), " was an extra header. not appended to dictionary.")
		if n%10000 == 0 and args.verbose == "yes":
			print("parsed out ", str(n+1), " lines of file.")
		## add to counter; read next line.
		n += 1
		line = myfile.readline()
	# close file
	myfile.close()

	# PART II: Write out Cleaned Information #
	# open output file for writing (**append**)
	newfile = open(outfile, "a")
	# if this is the first time the file is being written to, add in the header. otherwise, skip.
	if first == "yes":
		newfile.write(new_header)
	# get the new order for the columns
	if args.interactive == "no":
		date_time_index = [i for i, e in enumerate(header_list) if "UTC" in e][0]
		lat_index = [i for i, e in enumerate(header_list) if "LAT" in e][0]
		lon_index = [i for i, e in enumerate(header_list) if "LON" in e][0]
		name_index = [i for i, e in enumerate(header_list) if "VESSEL" in e][0]
		speed_index = header_list.index("AVG_SPEED")
		course_index = header_list.index("AVG_COURSE")
		docnum_index = header_list.index("DOCNUM")
		declo_index = [i for i, e in enumerate(header_list) if "DECLARAT" in e][0]
		new_header_index = [date_time_index, lat_index, lon_index, name_index, speed_index, course_index, docnum_index, declo_index]
	elif args.interactive == "yes":
		print("oh no! this part of the script has not been written yet. please ctrl-c and rerun script.")
	# Check the years associated with each date to make sure that all of them are from the correct year.
	date_col = header_list[date_time_index]
	date_list = file_dict[date_col]
	counter = 0
	for d in date_list:
		counter += 1
		if "." in d:
			tmp_year = d.split(".")[0]
		elif "-" in d:
			tmp_year=d.split("-")[0]
		if tmp_year != args.year:
			print("line ", str(counter), " does not contain info from correct year. Year reported: ", str(tmp_year), "\n")
	print("Checked all dates for year ", args.year, ".\n")		
	# Using the indices to re-order the columns, call row information from across dictionary entries and write out to new file.
	nrows = len(file_dict[header_list[0]])
	for i in range(0,nrows):
		newrow = ""
		for j in new_header_index:
			tmp_col = header_list[j]
			if j != new_header_index[len(new_header_index) -1]:
				newrow += file_dict[tmp_col][i] + ","
			else:
				newrow += file_dict[tmp_col][i] + "\n"
				newfile.write(newrow)
		if i%10000 == 0 and args.verbose=="yes":
			print("completed writing out ", str(i), " out of ", str(nrows-1), " rows.")
	if args.verbose=="yes":
		print("completed writing out ", str(i), " out of ", str(nrows-1), " rows.")
	# close the new file
	newfile.close()
		





# apply function to each file that matches with wildcard prefix
if args.file == "multi":
	counter = 0
	for filename in glob.glob(args.indir):
		counter += 1
		with open(filename, "r") as infile:
			print("working on file: ", filename, ".\n")
			if counter == 1:
				if args.columns == "default":
					print("using default column headers.")
					parse_file(myfile=infile, outfile=args.outfile)
				else:
					parse_file(myfile=infile, outfile=args.outfile, new_header=args.columns)
			else:
				if args.columns == "default":
					print("using default column headers.")
					parse_file(myfile=infile, outfile=args.outfile, first = "no")
				else:
					parse_file(myfile=infile, outfile=args.outfile, first = "no", new_header=args.columns)
			print("completed file: ", filename, ".\n-----\n")
elif args.file == "single":
	parse_file(myfile=infile, outfile=args.outfile)
	print("completed file: ", filename, ".\n-----\n")






