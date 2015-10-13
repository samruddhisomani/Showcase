Install the following libraries.

    library (ggplot2)
    library(cowplot)
    library(gridExtra)

Read in Georgia2000 data with the first row as the variables names.

    ga2000 <- read.csv('https://raw.githubusercontent.com/jgscott/STA380/master/data/georgia2000.csv', header=TRUE)

Calculate additional variables to facilitate analysis of vote
undercount.

*Diff = Ballots - Votes* This is the vote undercount--the number of
ballots that were not counted.

*Pct = Diff/Ballots* This the the undercount scaled by the number of
ballots in each county.

Change categorical variables `poor`, `urban`, and `atlanta` from type
*int* to type *factor* so that these variables are interpreted as
discrete categorical variables rather than continuous variables. Thus,
we can color code our graphics.

    ga2000$diff<-ga2000$ballots-ga2000$votes
    ga2000$poor<-as.factor(ga2000$poor)
    ga2000$pct<-(ga2000$diff)/(ga2000$ballots)
    ga2000$poor<-factor(ga2000$poor)
    ga2000$urban<-factor(ga2000$urban)
    ga2000$atlanta<-factor(ga2000$atlanta)

Use pairs to create a set of scatterplots relating the correlations of
all the variables with each other.

    pairs(ga2000)

![](../Images/pairs.png?raw=TRUE)

We see that `diff` is strongly correlated with `votes` and `ballots`.
Thus, further analysis should focus on `pct`, the percent difference
between `ballots` and `votes`. This will ensure that larger counties
with more voters do not overshadow smaller counties with fewer voters in
our analysis.

Upon further examination, we see that `pct` appears to be correlated
with `atlanta`, `urban`, and `poor`. Different equipment types (`equip`)
do not appear to have much of an effect on `pct`. However, we will
examine this relationship more closely in a bivariate plot.

We create bivariate plots to better visualize particular aspects of the
data. The titles below represent the key takeaways from each plot.

    g1<-ggplot (aes(x=equip, y=pct, fill=equip), data=ga2000)+geom_boxplot(colour='black')+
    theme_minimal() +
    xlab("Equipment")+
    ylab("Percent Undercount")+
    ggtitle ('Undercounting is consistent across equipment.') +
    guides (fill=FALSE)

    g2<-ggplot (aes(x=poor, fill=equip), data=ga2000)+geom_bar(position="fill", aes(colour="black")) +
    theme_minimal() +
    ggtitle ('Poor counties use more levers.\nLess poor counties use more optical machines.') +
    scale_fill_discrete ("Equipment") +
    scale_x_discrete(labels=c('No', 'Yes'))+
    xlab("Poor")+
    ylab("Fraction of Counties per Category") +
    scale_color_identity() +
    theme(legend.key = element_rect(colour = "black", size = 1))

    g3<-ggplot (aes(x=equip, y=pct, col=poor), data=ga2000)+
    geom_point(size=4, position = position_jitter(width=.10), alpha=.5)+theme_minimal()+
    xlab("Equipment")+
    ylab("Percent Undercount")+
    ggtitle ('Poor counties have more undercounting regardless of equipment') +
    scale_color_discrete("Poor", labels=c('No', 'Yes'))

    g4<-ggplot (aes(x=urban, fill=equip), data=ga2000)+
    geom_bar(position="fill", aes(color='black')) +
    theme_minimal() +
    ggtitle ('More urban counties use optical and punch systems.\nMore rural counties use more lever systems') +
    scale_fill_discrete ("Equipment") +
    scale_x_discrete(labels=c('No', 'Yes')) +
    xlab("Predominantly Urban")+
    ylab("Fraction of Counties per Category") +
    scale_color_identity() +
    theme(legend.key = element_rect(colour = "black", size = 1))

    g5<-ggplot (aes(x=equip, y=pct, col=urban), data=ga2000)+
    geom_point(size=4, position = position_jitter(width=.10), alpha=.5)+
    theme_minimal()+
    xlab("Equipment")+
    ylab("Percent Undercount")+
    ggtitle ('Rural counties show more undercounting regardless of equipment') +
    scale_color_discrete("Urban", labels=c('No', 'Yes'))

    g6<-ggplot (aes(x=atlanta), data=ga2000)+
    geom_bar(size=4)+
    theme_minimal()+
    xlab("Equipment")+
    ylab("Number of Counties")+
    ggtitle ('There are too few Atlanta counties to make meaningful comparisons.\n However, Atlanta counties should behave similarly to other urban counties.')
    + scale_x_discrete("Atlanta", labels=c('No', 'Yes'))

    g7<-ggplot (aes(x=equip, y=perAA,fill=equip),data=ga2000)+
    geom_boxplot()+
    theme_minimal()+
    xlab("Equipment")+
    ylab("Percent African American")+
    ggtitle ('Counties that use optical tend to have a lower\npercentages of African Americans.') +
    scale_fill_discrete(guide=FALSE)

    g8<-ggplot (aes(x=perAA, y=pct),data=ga2000)+
    geom_point(aes(color=poor),size=4, alpha=.5)+
    theme_minimal()+xlab("Percent African American")+
    ylab("Percent Undercount")+
    ggtitle ('Percent undercount goes up slightly as percent African American increases, but there\nappears to be a strong division between poor counties and less poor counties') +
    geom_smooth(se=FALSE, method='lm', colour='black', size=1.1)+
    scale_colour_discrete ("Poor",labels=c('<25%','>25%'))

    grid.arrange(g1,g6,g2,g3,g4,g5,g7,g8,ncol=2)

![](../Images/Vote.png?raw=TRUE)

### Conclusion

We see that poor and rural counties are more likely to use different
kinds of voting equipment than rich counties. However, certain kinds of
voting equipment are not associated with higher undercount percentages.
Moreover, poor and rural areas appear to suffer more undercounting
*regardless* of equipment. Percentage of African Americans does not
explain anything after accounting for poverty.
