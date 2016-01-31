import csv

from jf_utils import get_file_as_named_rows, candidates, results, convert_nonints_to_na  

# requires rpy2 ; install w/ 'pip install rpy2'
from rpy2 import robjects
from rpy2.robjects.packages import importr


# for lpepa:
importr('lpridge')

if __name__ == "__main__":
    
    POLLING_CUTOFF = -2
    
    field_names = ['window_type', 'bw_value', 'year', 'scope', 'trendline_width', 'candidate', 'result', 'estimate', 'squared_error']
    outfilename  = "ep_interp_log.csv"
    outfile = open(outfilename, 'w')
    outfile.write(",".join(field_names) +"\n")
    dw = csv.DictWriter(outfile, fieldnames=field_names, restval='', extrasaction='ignore')
    
    mse_field_names = ['window_type', 'bw_value', 'trendline_width', 'scope', 'mse', 'se']
    mse_outfilename  = "ep_interp_mse.csv"
    mse_outfile = open(mse_outfilename, 'w')
    mse_outfile.write(",".join(mse_field_names) +"\n")
    mse_dw = csv.DictWriter(mse_outfile, fieldnames=mse_field_names, restval='', extrasaction='ignore')
    
    # hardcode the bandwidth to what we think is optimal
    
    for trendline_width in [1,2,3,4,5,6,7,8,9,10,11,12,13]:
        bandwidth = 13
        print ">>> BANDWIDTH: %s" % bandwidth
        
        for scope in ['iowa']:
            error_array = []
            
            # How many days ahead of the caucus does our polling end? 
             
            
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
                    # manufacture the string representation of a vector in r from the data
                    candidate_vector_raw = "c(" + ",".join(candidate_data) + ")"
                    # input the vector from our homemade string
                    robjects.r("this_cand<-%s" % candidate_vector_raw)
                    # get a dataframe with just two elements, the polling and the days to caucus
                    robjects.r("df = data.frame(%s, this_cand)" % (dtc_vec.r_repr()))
                    robjects.r('''
                        colnames(df) <- c('dtc', 'poll')
                        df<-na.omit(df)
                        df$dtc = -df$dtc''')
                        
                    robjects.r("df <- subset(df, dtc < %s)" % (POLLING_CUTOFF+1))
                    
                    robjects.r('''
                        df <- subset(df, dtc > -31)
                        min_t <- min(df$dtc)
                        max_t <- max(df$dtc)
                        num_steps <- max_t - min_t + 1
                        cand_epan <- lpepa(df$dtc, df$poll, bandw=13, n.out=num_steps)
                    ''')
                                        
                    est_value = robjects.r('''cand_epan$est[num_steps]''')[0]
                    
                    # The estimated value is the polling average at POLLING_CUTOFF days before an election
                    # Now test possible trend lines by drawing a line from polls at POLLING_CUTOFF to
                    # POLLING_CUTOFF-x where x is between 1 and 13. Record the squared error and write 
                    # it to file as well so we can see which is working best. 
                    
                    robjects.r("trendline_width <- %s" % (trendline_width))
                    robjects.r('''
                        poll_trend <- structure(list(  x=c(cand_epan$x.out[num_steps-trendline_width], cand_epan$x.out[num_steps]), y=c(cand_epan$est[num_steps-trendline_width], cand_epan$est[num_steps])), .Names = c("x", "y"))
                    ''')
                    interpolated_est = robjects.r("unname(lm(y~x,data=poll_trend)$coefficients[1])")[0]
                    print "Esimated value at %s days is %s ; interpolated est is %s" % (POLLING_CUTOFF, est_value, interpolated_est)
                
                    # field_names = ['window_type', 'bw_value', 'year', 'scope', 'candidate', 'result', 'estimate']
                    squared_error = (candidate_result - interpolated_est)*(candidate_result - interpolated_est)
                    row_result = {'window_type':'epanechnikov', 'trendline_width':trendline_width, 'bw_value':bandwidth, 'year':year, 'scope':scope, 'candidate':candidate, 'result':candidate_result, 'estimate':interpolated_est, 'squared_error':squared_error}
                    print row_result
                    
                    error_array.append(squared_error)
                    
                    dw.writerow(row_result)
                    
                
            se = sum(error_array)
            count = len(error_array)
            mse = se/count
            mse_result = {'window_type':'epanechnikov', 'bw_value':bandwidth, 'trendline_width': trendline_width, 'scope':scope, 'mse':mse, 'se':se}
            mse_dw.writerow(mse_result)
        
        
                    