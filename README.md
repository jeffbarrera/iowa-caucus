# Predicting the GOP Iowa Caucuses

A statistical model to predict vote shares for Republican candidates in the 2016 Iowa Caucus, written in a mix of R and Python. Created by Jeff Barrera and Jacob Fenton as a project for _PoliSci 355B: Machine Learning for Social Scientists_, taught by Justin Grimmer at Stanford University.

An explanation of our model is available at https://github.com/jeffbarrera/iowa-caucus/blob/master/writeup.pdf.


# Data Sources

## Endorsements

Raw data, to be converted into 538's endorsement points and imported into R:
NVRMIND, no strong signal
- 2008: http://www.gwu.edu/%7Eaction/2008/cands08/endorse08el.html
- 2012: http://www.p2012.org/candidates/natendorseprecaucus.html
- 2016: http://projects.fivethirtyeight.com/2016-endorsement-primary/

## Prediction Markets

Surprisingly hard to find consistent data, the best I've got is Betfair for 2012 and 2016:

- Betfair 2016: https://www.betfairpredicts.com
- Betfair 2012 (data in the article): http://cosmiclog.nbcnews.com/_news/2012/01/03/9923436-political-markets-get-first-2012-test
- 2016: http://predictwise.com/politics/2016-president-primaries
- 2012 IEM: http://tippie.uiowa.edu/iem/markets/republicannomination.html
- 2012 Intrade: https://webcache.googleusercontent.com/search?q=cache:Xjm9JnxmvjoJ:https://wenowproject.wordpress.com/2012/01/02/betting-on-iowa-2/+&cd=1&hl=en&ct=clnk&gl=us&client=safari 
