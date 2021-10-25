







# Data





The National Survey of Tax and Benefit (hereafter, NaSTab) is
an annual financial panel survey
implemented by The Korea Institute of Taxation and Finance
to study the tax burden of households and the benefits
that households receive from the government.
The subjects of this survey are general households and
household members living in 15 cities and provinces nationwide.
This survey is based on a face-to-face interview. [^interview]
The NaSTaB data is constructed
as the subjects represent the population of Korean society.
This enables us to derive giving price elasticity of population
without re-weighting samples, which is used in the extant research.
Moreover, note that subjects are not limited to the taxpayer or
income earner reflecting the population.

In the analysis,
we use data from 2013 to 2017 since we focus on the 2014 tax reform.
This is because, as Table \@ref(tab:tabTaxRate) shows,
the giving price before 2014 was changed frequently
and incorporating the data before 2012
captures the effects of another tax reform than the reform in 2014.
Note that, since tax credit was introduced after 2014 and
the credit rate was unchanged since 2014,
the giving price does not depend on the income tax rate after 2014.
In addition, we exclude the subject of the sample, whose age is under 23,
since they are not likely to have income or assets.

[^interview]: If it is difficult for investigators to meet subjects, another family member answers on behalf of him.


<table class="table table" style="width: auto !important; margin-left: auto; margin-right: auto; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Std.Dev. </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="2"><td colspan="7" style="border-bottom: 1px solid;"><strong>Charitable Donations</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Annual chariatable giving (unit: 10,000KRW) </td>
   <td style="text-align:right;"> 67848 </td>
   <td style="text-align:right;"> 29.52 </td>
   <td style="text-align:right;"> 132.91 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10000.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Dummary of donation &gt; 0 </td>
   <td style="text-align:right;"> 67848 </td>
   <td style="text-align:right;"> 0.20 </td>
   <td style="text-align:right;"> 0.40 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr grouplength="3"><td colspan="7" style="border-bottom: 1px solid;"><strong>Income, giving price, and tax report</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Annual taxable labor income (unit: 10,000KRW) </td>
   <td style="text-align:right;"> 53269 </td>
   <td style="text-align:right;"> 1876.12 </td>
   <td style="text-align:right;"> 2700.97 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 900.00 </td>
   <td style="text-align:right;"> 91772.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> First giving relative price </td>
   <td style="text-align:right;"> 62877 </td>
   <td style="text-align:right;"> 0.86 </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 0.85 </td>
   <td style="text-align:right;"> 0.94 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Dummy of declaration of a tax relief </td>
   <td style="text-align:right;"> 12172 </td>
   <td style="text-align:right;"> 0.48 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr grouplength="6"><td colspan="7" style="border-bottom: 1px solid;"><strong>Individual Characteristics</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Age </td>
   <td style="text-align:right;"> 67848 </td>
   <td style="text-align:right;"> 51.35 </td>
   <td style="text-align:right;"> 15.81 </td>
   <td style="text-align:right;"> 24.00 </td>
   <td style="text-align:right;"> 50.00 </td>
   <td style="text-align:right;"> 104.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Female dummy </td>
   <td style="text-align:right;"> 67848 </td>
   <td style="text-align:right;"> 0.53 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Employee dummy </td>
   <td style="text-align:right;"> 42362 </td>
   <td style="text-align:right;"> 0.53 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> University graduate </td>
   <td style="text-align:right;"> 67842 </td>
   <td style="text-align:right;"> 0.41 </td>
   <td style="text-align:right;"> 0.49 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> High school graduate dummy </td>
   <td style="text-align:right;"> 67842 </td>
   <td style="text-align:right;"> 0.35 </td>
   <td style="text-align:right;"> 0.48 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Junior high school graduate dummy </td>
   <td style="text-align:right;"> 67842 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.43 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
</tbody>
</table>



![plot of chunk SummaryOutcome](figure/SummaryOutcome-1.png)


Table \@ref(tab:SummaryCovariate)
shows summary statistics of our data.[^Question]
The first panel of this table shows variables about charitable giving.
The NaSTaB asks respondents to answer the amount of donation last year.
This is the first outcome variables.
Using this, we make a dummy taking 1 if respondent donated last year.
This is the second outcome variables to
estimate the price effect on the decision of donations.
This table shows that
the average amount of donation is almost 300,000 KRW (300 USD),
and the proportion of donors is roughly 20\%.
Figure \@ref(fig:SummaryOutcome) shows the time-series of two variables.
The blue line shows the average amount of donation among donors.
In each year, its value is nearly 1.5 million KRW (1,500 USD),
which is 7\% of average annual taxable income.
The gray bar shows the proportion of donors.
After the tax reform, the proportion of donors decreases
by 2 percentage points.
After that, the proportion of donors is greter than 20\%.

[^Question]: Respondents answer the amount of donation for seven specific purposes last year. Seven specific purposes are policitical parties, educational organizations, social welfare organizations, organizations for culutre and art, religious groups, charity activies organaized by religious group, other purposes. We sum up the amount of donations, and consider it as the annual charitable giving.


![plot of chunk SummaryPrice](figure/SummaryPrice-1.png)


The second panel of Table \@ref(tab:SummaryCovariate)
shows variables about income, tax report, and the giving price.
NaSTaB asks respondents to answer the annual labor income last year.
In our sample,
the average annual taxable income is 18.76 million KRW (18,760 USD).
According to the National Tax Statistical Yearbook
published by Korean National Tax Service,
the average annual taxable income is
32.77 million (32,770 USD) from 2012 to 2018
for employees who submitted the tax return.
Since our sample includes subjects with no labor income, such as housewives,
our sample mean of income is lower than
the average income calculated by the public organizations.
In Figure \@ref(fig:SummaryPrice),
the gray bars show the distribution of annual taxable income in 2013.
The income distribution is right-skewed.

Using this variable,
we construct the giving price
under the tax deduction system (2012 and 2013).[^fprice]
After the tax reform (after 2014),
the giving price is 0.85 regardless of labor income,
as we explained in the section \@ref(institutional-background).
In Figure \@ref(fig:SummaryPrice),
the blue line shows the giving price in 2012 and 2013,
while the red dashed line shows the giving price after 2014.
From this figure, 
those whose annual income is less than 120,000,000 KRW
(120,000 USD) in 2013 could receive benefit from the 2014 tax reform
because the tax reform decreases the giving price.
On the other hand,
those whose annual income is greater than
460,000,000 KRW (460,000 USD) in 2013 had a loss by the 2014 tax reform
since the tax reform increases the giving price.

[^fprice]: The giving price shown in Table \@ref(tab:SummaryCovariate) is the *first* giving price. The giving price can be manipulated by an amount of donation. To avoid this endogeneity, we use the giving price where the amount of donation is zero. We will discuss this issue in the next section.

The NaSTaB also asks respondents
to answer whether they declared a tax relief of giving.
Although this variable is unique,
the sample size is relatively small due to unanswering.
This survey investigates separately for the case of *total* income
(for example, business income, dividend income, rental income)
and the case of *labor* income.
We make a dummy taking one
if respondents applied for a total income deduction of giving
or a labor income deduction of giving.
Table \@ref(tab:SummaryCovariate) shows the proportion of declaration is about 48\%.

