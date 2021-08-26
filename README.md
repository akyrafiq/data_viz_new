# Data Visualisation Mini-Blog 2021
As a challenge for 2021, I am going to try and do some data vis work everyday. I shall keep of it all in this repo and eventually branch it out into separate repos. I will have  brief summary of what I have done here and update it time to time.

- [Visualisation 1: New Coronavirus Variant](#visualisation-1)
- [Visualisation 2: Football League Tables](#visualisation-2)
- [Visualisation 3: Cricket](#visualisation-3)
- [Visualisation 4: Portuguese Wildfires](#visualisation-4)
- [Visualisation 5: Capitol Hill Riots Tweets](#visualisation-5)
- [Visualisation 6: India Farmers protests Media Astroturfing](#visualisation-6)
- [Break and update](#break-and-update)
- [Visualisation 7: Cricket DRS system](#visualisation-7)
- [Visualisation 8: New York Electric Car Rebates](#visualisation-8)
- [Visualisation 9: Random Macroeconomics](#visualisation-9)
- [Visualisation 10: Pakistani Cricket](#visualisation-10)
- [Visualisation 11: UK Election Results](#visualisation-11)
- [Visualisation 12: Dogecoin](#visualisation-12)

## Visualisation 1
### New Coronavirus Variant

At the end of 2020, a new variant of the coronavirus was found in the UK. Initial findings found it to be more contagious than the original variant but less deadly. Whilst this might make it seem it cancels out, due to how exponential growth works, the new variant can prove to be even more deadlier. I tried to demonstrate this with a few plots. I have assumed the current R_0 of the virus is 1.1 and it has a fatality rate of 0.1%. Then with the new variant, I assume it has an R_0 70% greater than the original strain but is only five times less deadly.

![The rate of infections compared between the two variants.](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%201/infections.png)
![The rate of infections compared between the two variants but without log axes to demonstrate how big  the divergence between the two trends are.](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%201/infections_nonlog.png)
![The rate of death compared between the two variants.](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%201/deaths.png)

The first two plots show how the rate of infection varies between the two variants. The first uses a log axis so that they are comparable but I also show it without the log axes to demonstrate how big the divergence between the two trends are. The third plot shows an estimate of roughly how many people are expected to die from the amount infected each day. Realistically there should be a lag added in to show when these dates actually occur, but again this was done purely for demonstration purposes.
(Updates 27/01: In a UK government press briefing, it was announced that the new variant in fact had an even higher fatality rate than the original virus... so the situation is even worse than first imagined)

## Visualisation 2
### Football League Tables

I have been gathering data of league tables across several leagues for the past few seasons. I currently trying to estimate the distributions of various metrics and observe the dynamics which take place within these leagues. I have data for the Bundesliga, Ligue 1, Premier League, MLS, Serie A and La Liga. So far, I have the following kinds of plots:

#### Histograms
Visualising how various statistics have varied across seasons.

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%202/hist/laliga/point_hist.png)

#### Statistical Distributions

I have used the Weibull distribution (A fat tailed distribution accounting for rare events) and the plots above to estimate the statistical distribution of things like goals scored and points attained across the leagues

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%202/dist/epl/points_dist.png####)

#### Density Plots
Here I have compared how things in the league vary by league position

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%202/plot/bund/XvP.png)

Will require a much more in depth write-up and trimming down of results (I currently have over 100 graphs and need to trim down which ones are of actual interest....).


## Visualisation 3:
### Cricket

I am trying to work out the efficiency of the IPL Auction system. It's a question I have thought about for a while, as with sports player markets in general (What determines a sport's players value? What kind of players are prone to overvaluation/undervaluation?'). At the moment I only have ball by ball data. Just conducting some exploratory analysis on to find the stand out performers each season and then combine this with auction data. Ideally want to get it done before the meg auction later this year.

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%203/ipl/plots/boundaries.png)

I also got interested in the career of Kusal Mendis- who started off his career so well but has failed to maintain consistency- so I want to see what happened and compare with other promising cricketers.

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%203/cricinfo/kusal2.png)

I then found another package, cricketR, which is especially built for cricket analysis, which is capable of what I was doing before with prebuilt functions. I will use this at a later date looking more into certain players.

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%203/cricinfo/tendulkar_avg2.png)

## Visualisation 4
### Portuguese Wildfires

Me having an attempt of replicating some coursework I did at university with a totally different dataset. The aim is to find a 'rare event distribution' for wildfires in a region. Here I have chosen the Weibull distribution again, but I’ll compare to a method using MLE at some point.

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%204/dist.png)

## Visualisation 5
### Capitol Hill Riots Tweets

I am analysing a dataset of tweets about the Capitol Hill riots that unfolded on January 6th. Firstly, I have made a map visualising where were these tweets sent:
![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%205/heatmap.png)
I plan to do some NLP work on this building on a framework I learnt two years ago.

## Visualisation 6
### India Farmers protests Media Astroturfing

Over the past few months, there have been widespread protests in India in regards to new farming laws being introduced in India. This has become a very polarising issue with India and the discourse has spread beyond. Public figures like Greta Thunberg and Rihanna tweeted out about the issue, which brought about a lot of online backlash. One bizarre trend started at the beginning of February, where many high profile Indian celebrities all began tweeting the hashtag #IndiaAgainstPropaganda. It all seemed very scripted as most messages read the same and released all within the same day. This hashtag was amplified across Twitter by thousands of accounts. Another trend was #GretaThunbergExposed, with various people trying to say Greta Thunberg was part of a large conspiracy against the Indian Government. I had a quick look into a few trends to see how this is being carried out by bot activity, often a tell tale sign being the number of digits in the username. For people who are unaware, the easiest to use bot generators on Twitter output a username with an 8 digit string at the end. A people have caught on, people who create these automated /sock puppet accounts have become smarter with their naming conventions but it is still a common technique used. 

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%206/bots1.png)
![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%206/bots2.png)

I decided to try and visualise. So I made wordclouds centred around the two main hashtags trending at the time, #IndiaAgainstPropaganda and #GretaThunbergExposed.

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%206/IAP_wordcloud.png)

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%206/GTE_wordcloud.png)

A lot of it is the work of the notorious BJP IT Cells. I unfortunately have been threatened/harassed by them in the past. I want to do a more thorough look into their activity but given restrictions of the Twitter API, I can only mine data in small chunks.

## Break and update

So it has been a couple of months since my last update to this post. I have just been very busy with a few projects I have been working on and have also been in the process of moving jobs. I had an issue with this repository where I had accidentally uploaded some personal information. After trying and filing to remove it from my Github history, I just ended up deleting the whole repo and just reuploading it (Hence why this is now called Data Viz New). However, even though I have been off, there have been a few things I have worked upon that I can share here and will tidy up at a later date.

## Visualisation 7
### Cricket DRS system

The DRS system has been around in cricket for just over a decade now but it hasn't stopped many fans of the game bemoaning ertain umpires as being terrible. Thus, I did a short exploratory analysis on which umpires are making the biggest errors that need overturning. I also investigated which batsmen review the most and when exactly are reviews taken over the course of an innings.

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%207/umpires.png)
![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%207/bat_for.png)
![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%207/overs.png)

## Visualisation 8
### New York Electric Car Rebates

I took part in a fun little event called DataSci Live. It is experimenting whether we can make Data Science an 'eSport'. I was given a challenge to complete within 90 minutes. The question was regarding the Electric Car Rebate Scheme in New York. We had to find out who exactly was gaining rebates (What % were white, earning over $100k, male). These are the visualisations I came up within the time but would be worth a revisit. I will post a link to the recording once it is up on YouTube.

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%208/Rebates%20by%20County.png)

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%208/Wealthiest%20counties.png)

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%208/Wealthiest%20counties%20by%20race.png)

## Visualisation 9
### Random Macroeconomics

This was a short tutorial I had looking at some macroeconomic data in R

## Visualisation 12
### Dogecoin

Nothing fancy here, just showing how Dogecoin has skyrocketed this year.

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%2012/doge.png)

A few months later, I decided to check back in, and whilst the peak wasn't sustained, Dogecoin is still considerably more valuable compared to the beginning of the year.

![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%2012/doge_aug.png)

It might be easier to see the gap with a log scale on the y axis


![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%2012/doge_aug_log.png)

## Visualisation 14
### Going Beyond Wolverhampton

I recently began hosting a local radio show in my hometown of Wolverhampton and I made a quick script to keep track of how many people are tuning in.


![](https://github.com/akyrafiq/data_viz_new/blob/main/Visualisation%2014/plots/2021-05-17_views.png)
