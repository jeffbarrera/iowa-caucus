import csv

from rpy2 import robjects
from rpy2.robjects.packages import importr

# for lpepa:
importr('lpridge')

candidates = {
    2008:['GIULIANI','HUCKABEE','MCCAIN','PAUL','ROMNEY','THOMPSON'],
    2012:['Romney','Gingrich','Santorum','Paul','Huntsman','Perry'],
    2016:['Trump','Cruz','Rubio','Carson','Bush','Christie','Paul','Kasich','Huckabee']
}

# note capitalization. Sigh. 
results = {
            2008: 
            {
                'HUCKABEE':34.4,
                'ROMNEY':25.2,
                'THOMPSON':13.4,
                'MCCAIN':13,
                'PAUL':9.9,
                'GIULIANI':3.4
            },
            2012:
            {
                'Santorum':24.6,
                'Romney':24.5,
                'Paul':21.4,
                'Gingrich':13.3,
                'Perry':10.3,
                'Bachmann':5.0,
                'Huntsman':0.6,
                'Cain':0.0
            }
        }
        
def get_file_as_named_rows(filename):
    reader = csv.reader(open(filename, 'r'))
    results = {}
    header = reader.__next__()
    for colname in header:
        results[colname] = []
    
    for row in reader:
        for i, value in enumerate(row):
            results[header[i]].append(value)
    return results

def convert_nonints_to_na(array):
    count=0
    for i, val in enumerate(array):
        try:
            int(val)
        except ValueError:
            array[i]='NA'
            count+=1
            
    return [array, count]
    
    
# estimators -- these just take the final value of the estimate. 
# should be rewritten to cut off a few days before the election and interpolate the rest. 

def estimate_epa(bandwidth):
    
    robjects.r("cand_epan <- lpepa(df$dtc, df$huck, bandw=%s, n.out=100)" % bandwidth)
    est_value = robjects.r('''cand_epan$est[100]''')[0]
    
    return est_value

def estimate_ksmooth(bandwidth):

    robjects.r("gaussian_smooth <- ksmooth(df$dtc, df$huck, \"normal\", bandwidth=%s, n.points=100)" % bandwidth)
    est_value = robjects.r('''gaussian_smooth$y[100]''')[0]

    return est_value

def estimate_lowess(f):

    robjects.r("cand_lowess <- lowess(df$dtc, df$huck, f=%s)" % f)
    est_value = robjects.r('''cand_lowess$y[length(cand_lowess$y)]''')[0]

    return est_value
