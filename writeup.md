# Predictions

<b>We predict XXX to be the winner of the 2016 Iowa Republican caucus.</b>

Our prediction for the 9 candidates' votes are shares are below.

1) <b>Donald Trump</b>:
<br>2) <b>Ted Cruz</b>:
<br>3) <b>Marco Rubio</b>:
<br>4) <b>Jeb Bush</b>:
<br>5) <b>Ben Carson</b>:
<br>6) <b>Chris Christie</b>:
<br>7) <b>Rand Paul</b>:
<br>8) <b>Mike Huckabee</b>:
<br>9) <b>John Kasich</b>:

# Our Model

## Key Features: In-state polling

The best indication of a candidates' standing in Iowa is in-state polling conducted close to the caucus. Coming up with a weighted poll average is not trivial, and we considered several approaches: linear regression, lowess regression and non-parametric regression with gaussian and epanechnekov kernels. Just eyeballing the graphs of unweighted linear regression (taking into account a variety of time scales) shows it to be less useful than the other methods. 

As training data we used polls aggregated for Iowa by pollster.com (which was later acqured by The Huffington Post) for [2008](http://www.pollster.com/polls/ia/08-ia-rep-pres-primary.html) and [2012](http://elections.huffingtonpost.com/pollster/2012-iowa-gop-primary.csv). (All of the code used in this project is available at: [https://github.com/jeffbarrera/iowa-caucus/](https://github.com/jeffbarrera/iowa-caucus/) ). We wrote python scripts ([2008](https://github.com/jeffbarrera/iowa-caucus/blob/master/pollster/clean_2008_data.py) ; [2012/16](https://github.com/jeffbarrera/iowa-caucus/blob/master/pollster/clean_2012_2016_data.py) ) to standardize the data across years and add one key variable: the number of days before the Iowa Caucus is held.  We also wrote scripts to test several techniques under a variety of circumstances: [lowess](https://github.com/jeffbarrera/iowa-caucus/blob/master/test_lowess.py) ([results](https://github.com/jeffbarrera/iowa-caucus/blob/master/lowesslog_mse.csv)), and first-order non-parametric regression with [gaussian](https://github.com/jeffbarrera/iowa-caucus/blob/master/test_ksmooth_bandwidths.py) ([results](https://github.com/jeffbarrera/iowa-caucus/blob/master/kplog_mse.csv)) and [epanechnikov](https://github.com/jeffbarrera/iowa-caucus/blob/master/test_epanechnikov_bandwidth.py) ([results](https://github.com/jeffbarrera/iowa-caucus/blob/master/eplog_mse.csv)) kernels. We tested each regression with a variety of bandwidths (or f values in the case of lowess) and compared the final estimated point with the actual polling result. 
The most accurate estimation result in terms of overall mse for 2008 and 2012 was obtained using an epanechnikov kernel with a bandwidth of 13 days, which gave us a mean squared error of [7.29](https://github.com/jeffbarrera/iowa-caucus/blob/master/eplog_mse.csv#L14). 
One additional complication is that prior years' polling included results the day of the caucus; the most recent poll results we have access to are several days ahead of the caucus. Thus we need to interpolate from a point several days ahead of the caucus to a final result; we tested several approaches and found the best to be TK TK. 

## National Polling
TKTK

## Regressing in-state, national polling
TKTK




## Features not included

#### Campaign finance
Reports filed with the Federal Elections Commission give some insight into a candidates' fundraising and spending, but we chose to disregard these as not predictive of vote share for several reasons:

- This year is different! One candidate (guess who) has been the beneficiary of millions in 'earned media'--coverage that's not paid for. Because he's been so effective in winning earned media, he hasn't sought contributions in the same manner as other candidates. Thus many of the usual governing assumptions (probably) don't hold.

- Candidates' reports are filed at a significant lag. Quarterly reports covering the fourth quarter of 2015 are due Jan. 31, but do not reflect any spending or fundraising that took place in 2016. Polling data is generally much more current than that.
 
- The 2012 and 2008 Iowa caucuses were held Jan. 2 (two days after the end of a filing period) whereas the 2016 caucuses are held Feb. 1 (a month after the end of the most recently available candidate spending data). Thus a relationship between financial figures for 2008 and 2012 wouldn't necessarily hold true for 2016. 

-  The way that campaigns spend money is in flux and increasingly money spent is excluded from public accounting. Increasingly spending from a candidates' principal campaign committee is overshadowed by money spent by independent expenditure only committees (aka super PACs). In 2012, super PACs primarly spent money on media buys, but increasingly these "outside" groups are taking on tasks previously handled by candidate committees, including last-minute voter targetting and mobilization. Other money spent by non-profit groups is not publically reported at all, and anecdotal reports suggest this type of spending is rising. 

### Endorsements

Fivethirtyeight uses a weighted endorsements system to help predict primary results. What their data show[footnote], however, is that there are far fewer endorsements this year than in previous cycles. 



### Crosstabs available in polls

Most reputable polls provide results cross-tabulated by various demographic groups. We were unable to find any easily available aggregation of poll crosstabs (and the inconsistent approach pollsters take would make this a considerable challenge). Nonetheless, we believe this might be a useful indicator. Were this data available in bulk we might be able to make different assumptions about the electorate. Data suggests many potential voters who say they plan to participate in caucuses do not actually do so; we believe voter subgroups' lie to pollsters at a differential rate, introducting a meaningful bias into polls. 