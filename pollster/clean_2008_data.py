import csv
from datetime import date, timedelta

from caucus_dates import iowa_caucus_dates

caucus_date_2008 = iowa_caucus_dates[2008]
candidates_2008 = ['GIULIANI' ,'HUCKABEE' ,'MCCAIN' ,'PAUL','ROMNEY', 'THOMPSON', 'UNDECIDED']

# dumping 'Entry Date/Time (ET)'
field_names = ['POLLSTER','Start Date','End Date','days_to_caucus','Number of Observations','Population','Mode', 'Pollster URL','Source URL','Partisan','Affiliation'] + candidates_2008
files_2008 = {'national':'data/2008-national-gop-primary.csv', 'iowa':'data/2008_iowa_gop_primary.csv'}


def parse_poll_dates(date_value):
    # there's a start and an end date provide in several diff formats:
    # 1. 2/24-26/08 <-- M/D-D/Y
    # 2. 1/31-2/2/08 <- M/D-M/D/Y
    # 3. 1/21/08 <-- single day poll, apparently
    # 4. 12/31/07-1/2/08 <- 
    
    # We assume no poll spans two years, which I checked and is true.
    print date_value
    dates = date_value.strip().split('-')
    start_date = None
    end_date = None
    # If more than two dates, it's a problem
    if len(dates) == 2:
        [start_date_raw, end_date_raw] = dates
        year = None
        
        end_date_parts = end_date_raw.strip().split('/')
        start_date_parts = start_date_raw.strip().split('/')
        
        if len(end_date_parts) == 3:
            # case #2 or 4
            
            year = int("20" + end_date_parts[2])
            end_date = date(year, int(end_date_parts[0]), int(end_date_parts[1]))
            
            if len(start_date_parts) == 2:
                # case 2
                
                start_date = date(year, int(start_date_parts[0]), int(start_date_parts[1]))
                #print "2: Got start date %s end date %s from %s" % (start_date, end_date, date_value)
            
            elif len(start_date_parts) == 3:
                # case 4
                print "#4: %s" % (date_value)
                start_date = date(int("20" + start_date_parts[2]), int(start_date_parts[0]), int(start_date_parts[1]))
                
            
                
            else:
                print "unparseable date: %s" % (date_value)
                assert False
            
            
        elif len(end_date_parts) == 2:
            # case 1
            
            end_date_parts = end_date_raw.split('/')
            end_day = int(end_date_parts[0])
            year = int("20" + end_date_parts[1])
            
            
            if len(start_date_parts) == 2:
                
                start_month = int(start_date_parts[0])
                start_day = int(start_date_parts[1])
                
                end_date = date(year, start_month, end_day)
                start_date = date(year,start_month, start_day)
                
                #print "1: Got start date %s end date %s from %s" % (start_date, end_date, date_value)
                
                
            else:
                print "unparseable date: %s" % (date_value)
                assert False    
        
    elif len(dates) == 1:
        # case 3
        
        date_parts = dates[0].strip().split('/')
        
        if len(date_parts)!=3:
            print "unparseable date: %s" % (date_value)
            assert False
            
        
        print "3: %s" % (date_value)
        start_date = date(int("20"+date_parts[2]), int(date_parts[0]), int(date_parts[1]))
        end_date = start_date
        
        #print "3: Got start date %s end date %s from %s" % (start_date, end_date, date_value)
        
        
        
    else:
        print "unparseable date: %s" % (date_value)
        start_date = 1
        end_date = 1
    
    return [start_date, end_date]

def get_midpoint(start_date, end_date):
    # returns middle of polling period--favors the day closer to the end if duration is even. 
    try:    
        poll_duration = end_date - start_date
    
        half_length = poll_duration.days / 2
        midpoint = end_date + timedelta(int(half_length))
        return midpoint
    except TypeError:
        return None
        
def parse_npop(raw_npop):
    npop_parts = raw_npop.strip().split(' ')
    if len(npop_parts) == 2:
        return [npop_parts[0], npop_parts[1]]
    elif len(npop_parts) ==1:
        try:
            voters = int(npop_parts[0])
            return [voters, '']
        except ValueError:
            return ['', npop_parts[0]]

def convert_survey_type(survey):
    ## Not sure what "HLV" and "A" mean. 
    if survey=='LV':
        return 'Likely Voters'
    if survey=='RV':
        return 'Registered Voters'
    return survey
        
if __name__ == '__main__':
    for this_file_key in files_2008:
        print "Processing file %s" % this_file_key
        file_location = files_2008[this_file_key]
        reader = csv.DictReader(open(file_location, 'Urb'))
    
        outfile = open("data/cleaned_2008_" + this_file_key + ".csv", 'w')
        outfile.write(",".join(field_names) +"\n")
        dw = csv.DictWriter(outfile, fieldnames=field_names, restval='', extrasaction='ignore')
    
    
        for row in reader:
        
            # fix 
        
            raw_date = row['DATES']
            raw_npop = row['N/POP']
            sample_size = None
            survey_type = None
            #print row
            if raw_date:            
                [start_date, end_date] = parse_poll_dates(raw_date)

                midpoint = get_midpoint(start_date, end_date)
                if midpoint:
                    days_before_caucus_td = caucus_date_2008 - midpoint
                    days_before_caucus = days_before_caucus_td.days
                else:
                    days_before_caucus = -1
            
                # add data to row so it gets written
                row['days_to_caucus'] = days_before_caucus            
                row['Start Date'] = start_date
                row['End Date'] = end_date
        
            if raw_npop:
                [sample_size, survey_type] = parse_npop(raw_npop)
                print "Got sample size=%s, survey_type = %s" % (sample_size, survey_type)
                row['Number of Observations'] = sample_size
                row['Population'] = convert_survey_type(survey_type)
        
            dw.writerow(row)
        
        outfile.close()
        