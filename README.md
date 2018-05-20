# WaterfallCharts
Create waterfall charts using R &amp; ggplot2.

## What this is ?
[Waterfall charts][1] show the incremental impact of event or activities. They are commonly used in consulting and business presentations.

For example, the following chart show the cash balance at the start of a period and all the inflows and outflows of cash during the period due to each factor (sales, refunds, payouts, court losses, court wins, contracts) and finally the ending cash position.

![Screenshot](waterfall.png)

This code is based on the [ggplot2: Waterfall Charts][2] blogpost from May 2010 and updated to:

- run with R 3.5.0 and ggplot2 2.2.1
- run as a function
- parameterise the y scale format and colours of bars by type (inflow, outflow, net balance


[1]: https://en.wikipedia.org/wiki/Waterfall_chart "Wikipedia: Waterfall Chart"
[2]: https://learnr.wordpress.com/2010/05/10/ggplot2-waterfall-charts/ "ggplot2: Waterfall Charts"
