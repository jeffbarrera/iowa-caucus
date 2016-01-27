import csv
from datetime import date, timedelta

from clean_2008_data import get_midpoint

from caucus_dates import iowa_caucus_dates

candidates = {
    2016:['Trump','Cruz','Rubio','Carson','Bush','Christie','Rand Paul','Kasich','Huckabee','Fiorina','Santorum','Gilmore','Graham','Jindal','Pataki','Perry','Walker'],
    2012:['Romney','Gingrich','Santorum','Paul','Cain','Huntsman','Perry']
    }

# file naming convention please, huffpo? 
file_names = {  2012:
                    {'national':'data/2012-national-gop-primary.csv',
                    'iowa':'data/2012-iowa-gop-primary.csv'
                    },
                2016:
                    {'iowa':'data/2016-iowa-presidential-republican-caucus.csv',
                    'national':'data/2016-national-gop-primary.csv'
                    }
            }

def get_date(raw_date):
    if raw_date:
        date_parts = raw_date.strip().split('-')
        return date(int(date_parts[0]), int(date_parts[1]), int(date_parts[2]))
    else: 
        return None
    
if __name__ == '__main__':

    for cycle in [2012,2016]:
        cycle_candidates = candidates[cycle]
        field_names = ['POLLSTER','Start Date','End Date','days_to_caucus','Number of Observations','Population','Mode', 'Pollster URL','Source URL','Partisan','Affiliation'] + cycle_candidates
        files_cycle = file_names[cycle]
        
        caucus_date = iowa_caucus_dates[cycle]
        for this_file_key in files_cycle:
            file_location = files_cycle[this_file_key]
            reader = csv.DictReader(open(file_location, 'Urb'))
            outfilename = "data/cleaned_" + str(cycle) + "_" + this_file_key + ".csv"
            print "Processing file %s %s to %s" % (cycle, this_file_key, outfilename)
            
            outfile = open(outfilename, 'w')
            outfile.write(",".join(field_names) +"\n")
            dw = csv.DictWriter(outfile, fieldnames=field_names, restval='', extrasaction='ignore')
            
            for row in reader:
                row['POLLSTER'] = row['Pollster']
                print row
                
                start_date = get_date(row['Start Date'])
                end_date = get_date(row['End Date'])
                                
                midpoint = get_midpoint(start_date, end_date)
                days_before_caucus_td = caucus_date - midpoint
                days_before_caucus = days_before_caucus_td.days
            
                # add data to row so it gets written
                row['days_to_caucus'] = days_before_caucus  
                print "%s from %s ; %s from %s, midpoint: %s days_to_caucus: %s" % (start_date, row['Start Date'], end_date, row['End Date'],midpoint,  days_before_caucus)
                          
                dw.writerow(row)
                
            outfile.close()
    