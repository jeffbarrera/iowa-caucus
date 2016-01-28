# iowa-caucus
GOP Iowa Caucus prediction challenge for Poli Sci 355B.

# Progress Updates
Figured this could be a helpful way for me to keep track of things

### 1/27/16
Jeff
- Built initial framework to extrapolate election-day vote shares from polling predictions
- Need to add evaluation suite to more precisely test model accuracy
- Need to weight polls by 538's rankings
- IDEA: might be worth averaging the result from this model (which is supposed to show trends) with the average vote share in the final few days (which theoretically should be more accurate). Or maybe try throwing both of these as predictors into a final model, along with the state/natl diff and the prediction markets?



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



------------------------

- build model that extrapolates polling averages per candidate over time
