## Introduction

Narrative control is a central tool of geopolitics. This project applies
Strategic Narrative Theory, which explains how states construct and
project coherent narratives to legitimize actions, shape public
perception, and influence both domestic and international audiences
(Miskimmon et al., 2013). The specific research question is: How do
Russian and U.S. officials frame the conflicts in Chechnya (1999–2002)
and Ukraine (2022–2025), and how do these narratives evolve over time?
By comparing Chechnya and Ukraine I aim to explore the continuity and
evolution in Russia’s strategic narrative and rhetorical techniques. The
US response serves as a useful counterpoint not only contrasting with
Russia’s, but also different between the two conflicts. While some may
question whether Chechnya is a domestic or international conflict and
whether it is appropriate to compare it with Ukraine, my focus was
solely on how state discourse is constructed and deployed. In fact, I
believe that comparing these two cases may reveal deeper insights,
especially how one conflict may have led to the other, and what this
suggests about international responses today. For this analysis, I
compiled a dataset of official speeches and public statements by Russian
and U.S. leaders, including presidents, foreign ministers, and
secretaries of state. These texts were collected from government
archives, scraped from official websites, and translated where
necessary, forming the empirical basis for a comparative discourse
analysis across time, actors, and conflicts.

## Data and Methods

### Corpus Overview

This study draws on 357 official speeches, statements, and interviews
from U.S. and Russian government actors related to the conflicts in
Chechnya (1999–2002) and Ukraine (2022–2025).

<table>
<thead>
<tr>
<th>Conflict</th>
<th>Country</th>
<th>Documents</th>
</tr>
</thead>
<tbody>
<tr>
<td>Chechnya</td>
<td>U.S.</td>
<td>111</td>
</tr>
<tr>
<td>Chechnya</td>
<td>Russia</td>
<td>45</td>
</tr>
<tr>
<td>Ukraine</td>
<td>U.S.</td>
<td>183</td>
</tr>
<tr>
<td>Ukraine</td>
<td>Russia</td>
<td>18</td>
</tr>
<tr>
<td><strong>Total</strong></td>
<td></td>
<td><strong>357</strong></td>
</tr>
</tbody>
</table>

To address imbalance in corpus size, all word-based analyses (e.g.,
TF-IDF, sentiment) were normalized per 1,000 words per speech.

### Speakers and Sources

**U.S. Narrative**  
- *Chechnya (1999–2002)*: Presidents Clinton and Bush; Secretaries
Albright and Powell  
- *Ukraine (2022–2025)*: Presidents Biden and Trump; Secretary Blinken
and Senator Rubio  
- **Sources**: WhiteHouse.gov, State.gov, via Wayback Machine

**Russian Narrative**  
- *Chechnya*: President Putin; Foreign Minister Igor Ivanov  
- *Ukraine*: President Putin; Foreign Minister Sergey Lavrov  
- **Sources**: Kremlin.ru, Mid.ru, via Wayback Machine

> **Note**: Boris Yeltsin is excluded due to lack of archived data
> before December 31, 1999.

### Challenges and Limitations

**Manual Collection**: Some data was scraped; other parts (e.g., Ivanov)
were compiled manually due to scraping restrictions.

**Translation**: Russian-language texts were manually translated by the
author to preserve framing and tone. This avoids distortion from
automated tools, but may introduce subjectivity.

**Selection Bias**: Particularly for older materials (Chechnya-era),
archival gaps may limit completeness.

**Time Constraints**: The dataset, while diverse, is not exhaustive.

## Analysis and Results

    ## # A tibble: 4 × 3
    ##   country conflict     n
    ##   <chr>   <chr>    <int>
    ## 1 Russia  Chechnya    45
    ## 2 Russia  Ukraine     18
    ## 3 US      Chechnya   111
    ## 4 US      Ukraine    183
    ## # A tibble: 37,374 × 2
    ##    word              n
    ##    <chr>         <int>
    ##  1 security       1877
    ##  2 support        1603
    ##  3 international  1507
    ##  4 global         1091
    ##  5 war             997
    ##  6 cooperation     978
    ##  7 economic        942
    ##  8 peace           919
    ##  9 energy          849
    ## 10 efforts         829
    ## # ℹ 37,364 more rows

![](README_files/figure-markdown_strict/sentiment%20analysis%20and%20visualization-1.png)

*In reference to Chechnya, the US discourse demonstrates significantly
higher usage of positive sentiment and trust words. Russian discourse
also includes a high number of trust words, but also shows negative
sentiment. Negative emotions like anger, fear, and sadness are
relatively balanced between the two countries, but slightly higher in
Russian rhetoric. In reference to Ukraine, the US again demonstrates a
high number of positive sentiment and trust words. Russian discourse
also contains a high number of trust words, but again with negative
sentiment. Other sentiments that dominate in both countries are anger,
anticipation and fear. Russia shows anger, disgust, negative sentiment,
while also showing more positive sentiment and sadness on Chechnya than
on Ukraine. The US demonstrates more positive sentiment, joy and
surprise regarding Chechnya than Ukraine, and slightly more fear and
anger in relation to Ukraine than Chechnya. Overall, the analysis
demonstrates that Russia uses far less emotive language in the official
rhetoric than the US does.*

    ## # A tibble: 4 × 3
    ##   country conflict     n
    ##   <chr>   <chr>    <int>
    ## 1 Russia  Chechnya    45
    ## 2 Russia  Ukraine     18
    ## 3 US      Chechnya   111
    ## 4 US      Ukraine    183
    ## # A tibble: 37,374 × 2
    ##    word              n
    ##    <chr>         <int>
    ##  1 security       1877
    ##  2 support        1603
    ##  3 international  1507
    ##  4 global         1091
    ##  5 war             997
    ##  6 cooperation     978
    ##  7 economic        942
    ##  8 peace           919
    ##  9 energy          849
    ## 10 efforts         829
    ## # ℹ 37,364 more rows

![](README_files/figure-markdown_strict/additional%20cleaning%20and%20TF-IDF%20by%20Country%20+%20Conflict-1.png)

*TF-IDF analysis shows keywords revealing the Russian and US discourse
on Chechnya and Ukraine. Russian use of terms like “terror acts”,
“extremist”, “conquerors”, “heroism” and “defended” shows a portrayal of
Chechnya as a defence against terrorism. The narrative tone is heroic.
US discourse using terms like “”hizballah”, “qaeda”, “islamic” shows
linking the conflict to the broader war on terror. The narrative tone is
terrorism-linked. In relation to Ukraine, Russia uses terms like
“security”, “threat”, “terrorist”, framing Ukraine as a national
security issue. The narrative tone is strategic and defensive. The US
uses terms like “aggression”, “sovereignty”, “democracy”, portraying
Ukraine as a victim of aggression. The narrative tone is democratic and
ideological.*

*Interestingly, the US discourse on Chechnya contained the keyword
“freedom”, but as the context extraction revealed it is not about
advocating for Chechen self-determination or independence, but focusing
on individual rights within Russia, such as press freedom and treatment
of prisoners. For instance, Bill Clinton’s statement on November 12,
1999 speaks to progress in: “press freedom, democracy, minority rights,
and the treatment of prisoners…” This reveals that “freedom” refers to
liberal democratic norms inside Russia, not to political freedom or
autonomy for Chechnya itself. Clinton’s more direct and powerful remark
on November 18, 1999, reveales the limits of US critique: “If they had
put you in jail instead of electing you President, I would hope that
every leader…would have stood up for you and for freedom in Russia and
not said, ‘Well, that is an internal Russian affair…’” drawing a line
between internal affairs and international concern: the US will not
interfere with Russia’s territorial claims, but will speak out when
state power becomes abusive, especially if it violates civil or
democratic rights. A key transformation appears in the post-9/11
discourse. In a White House press briefing on September 26, 2001,
reporters challenge the sudden shift in the US labeling of Chechen
fighters as “terrorists.” The dialogue includes: “All of a sudden,
you’re calling them terrorists? … Is this what Putin has asked for, in
exchange for his help?” A clear pivot to counterterrorism rhetoric that
reflects the US’s new interest in maintaining good relationships with
Russia. The US framing of Ukraine suggests that Ukraine’s defense is not
just national, it’s existential for the liberal international order.
Blinken states: “Everything that we and our allies and partners do… has
the same purpose: to help Ukraine defend its sovereignty… and to stand
up for the international rules and principles…” This casts Russia’s
invasion as a threat not just to Ukraine, but to global order. In stark
contrast, Russia’s framing centering on its “security” and perceived
threats. In Vladimir Putin’s speech on February 21, 2022, he warns:
“Если Россия столкнётся с такой угрозой, как принятие Украины в НАТО…
угрозы для нашей страны многократно возрастут.” (If Russia faces such a
threat as Ukraine joining NATO, the threats to our country will increase
many times over.) Here, Putin flips the narrative: Ukraine’s sovereignty
is seen as an existential threat to Russian national security. He
further grounds this in international language: “…принцип равной и
неделимой безопасности… не укреплять свою безопасность за счёт
безопасности других государств.” (The principle of equal and indivisible
security… not strengthening one’s security at the expense of others.)
Here, Putin uses multilateral language to argue that Western support for
Ukraine violates Russia’s right to security, thus reframing the
aggressor as the aggrieved. The US response offers a perspective that is
different from Russia’s, but also not the same between the two cases
themselves, shifting on sovereignty, religious framing, and geopolitical
motivations between the two conflicts.*

## Conclusion

## Reflection

This was by far the most challenging assignment I’ve done, and I’m not
sure everything went right.

## AI/Resources statement

While doing this assignment I consulted the links provided on the CFSS
website, the lecture slides available on Canvas, and the relevant
sections of the [R for Data
Science](https://r4ds.hadley.nz/data-visualize.html#sec-ggplot2-calls).
Additionally, I referred many times to ChatGPT for clarification and
debugging, especially while scraping.
