import csv

from jf_utils import get_file_as_named_rows, candidates, results, convert_nonints_to_na  

# requires rpy2 ; install w/ 'pip install rpy2'
from rpy2 import robjects
from rpy2.robjects.packages import importr


# for lpepa:
importr('lpridge')

if __name__ == "__main__":
    
    
    field_names = ['window_type', 'bw_value', 'year', 'scope', 'candidate', 'result', 'estimate', 'squared_error']
    outfilename  = "eplog.csv"
    outfile = open(outfilename, 'w')
    outfile.write(",".join(field_names) +"\n")
    dw = csv.DictWriter(outfile, fieldnames=field_names, restval='', extrasaction='ignore')
    
    mse_field_names = ['window_type', 'bw_value', 'scope', 'mse', 'se']
    mse_outfilename  = "eplog_mse.csv"
    mse_outfile = open(mse_outfilename, 'w')
    mse_outfile.write(",".join(mse_field_names) +"\n")
    mse_dw = csv.DictWriter(mse_outfile, fieldnames=mse_field_names, restval='', extrasaction='ignore')
    
    # super inefficent to have this in outer loop, but conceptually easier to get cyclewise-error
    for bandwidth in range(1,20):
        print ">>> BANDWIDTH: %s" % bandwidth
        
        for scope in ['iowa']:
            error_array = []
            
            for year in [2008, 2012]:

                print "Handling %s %s" % (year, scope)
                result = get_file_as_named_rows('pollster/data/cleaned_' + str(year) + '_' + scope + '.csv')
                days_to_caucus = result['days_to_caucus']
                dtc_count = len(days_to_caucus)
            
                # r object for dtc
                dtc_vec = robjects.IntVector(days_to_caucus)
                
            
                #print "dtc:\n%s" % days_to_caucus
                for candidate in candidates[year]:
                
                    candidate_result = results[year][candidate]
                
                    candidate_data = result[candidate]
                    cd_count = len(candidate_data)
                    # we got problems if these rows are different length
                    assert dtc_count==cd_count
                    [candidate_data, count] = convert_nonints_to_na(candidate_data)
                    if len(candidate_data)==count:
                        # no regression to run if it's all NAs
                        continue
                
                    candidate_vector_raw = "c(" + ",".join(candidate_data) + ")"
                    #print candidate_vector_raw
                    robjects.r("this_cand<-%s" % candidate_vector_raw)
                    robjects.r("df = data.frame(%s, this_cand)" % (dtc_vec.r_repr()))
                    robjects.r('''
                        colnames(df) <- c('dtc', 'huck')
                        df<-na.omit(df)
                        df$dtc = -df$dtc
                        df <- subset(df, dtc<1)
                    ''')

                    robjects.r("cand_epan <- lpepa(df$dtc, df$huck, bandw=%s, n.out=100)" % bandwidth)
                    est_value = robjects.r('''cand_epan$est[100]''')[0]
                
                    # field_names = ['window_type', 'bw_value', 'year', 'scope', 'candidate', 'result', 'estimate']
                    squared_error = (candidate_result - est_value)*(candidate_result - est_value)
                    row_result = {'window_type':'epanechnikov', 'bw_value':bandwidth, 'year':year, 'scope':scope, 'candidate':candidate, 'result':candidate_result, 'estimate':est_value, 'squared_error':squared_error}
                    print row_result
                    
                    error_array.append(squared_error)
                    
                    dw.writerow(row_result)
                    
                
            se = sum(error_array)
            count = len(error_array)
            mse = se/count
            mse_result = {'window_type':'epanechnikov', 'bw_value':bandwidth, 'scope':scope, 'mse':mse, 'se':se}
            mse_dw.writerow(mse_result)
        
        
                    