






# Issue96に関するレポート

[tracking issue here](https://github.com/KatoPachi/NASTAB/issues/96)

## 異時点間の代替性の検証

- 問題の所在：2014年の寄付税制のアナウンスが2013年にあったので、これによる異時点間の代替性が生じているかもしれない
  - Anticipation effect
  - 所得ラグの推定は所得税の累進性に対応したものなので、この効果に対する明確な対処となっていない
- 対応１：所得階層別の寄付額の記述統計の確認する
  - アナウンスによる異時点間の代替性が視覚的に生じているかどうか
  - 平行トレンドが満たされていれば、anticipation effectは特に問題なし
- 以下の図は所得階層を3区分にして、各所得階層の各年の寄付額の対数値の平均をプロット
  - income < 1200：2014年改革で寄付の相対価格が下落するグループ
  - income b/w 1200 and 4600：2014年改革で寄付の相対価格が変化しないグループ
  - income > 4600：2014年改革で寄付の相対価格が増加するグループ
  - **視覚的に、異時点間の代替性が生じているとは思えないし、平行トレンドも満たされているようにも見える**


![plot of chunk SummaryOutcomebyIncome1](figure/SummaryOutcomebyIncome1-1.png)


- 2013年の寄付対数値の平均が1となるようにしても、平行トレンドが満たされていて、異時点間の代替性が生じていると思われる


![plot of chunk SummaryOutcomebyIncome2](figure/SummaryOutcomebyIncome2-1.png)


- 対策２：異時点間の代替性によって影響をうける制度改革の直前と直後のデータを落として、推定する
  - 以下の表は2013年と2014年のデータを落として、First priceの弾力性を推定した結果
  - 比較のために、フルサンプルの結果も併せて示す
  - データを落としても、結果に大きな変化はない
  - **異時点間の代替性があったとしても、結果は大きく変化しない**

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<caption>First price elasticity: Full sample</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Overall </th>
   <th style="text-align:center;"> Intensive </th>
   <th style="text-align:center;"> Extensive </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> log(first giving price) </td>
   <td style="text-align:center;"> -1.241*** </td>
   <td style="text-align:center;"> -0.904*** </td>
   <td style="text-align:center;"> -0.267*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.227) </td>
   <td style="text-align:center;"> (0.249) </td>
   <td style="text-align:center;"> (0.051) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> log(annual taxable income) </td>
   <td style="text-align:center;"> 4.946*** </td>
   <td style="text-align:center;"> 1.571** </td>
   <td style="text-align:center;"> 1.102*** </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.949) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.653) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.220) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 53267 </td>
   <td style="text-align:center;"> 11637 </td>
   <td style="text-align:center;"> 53267 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Adj. </td>
   <td style="text-align:center;"> 0.530 </td>
   <td style="text-align:center;"> 0.678 </td>
   <td style="text-align:center;"> 0.462 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Within </td>
   <td style="text-align:center;"> 0.015 </td>
   <td style="text-align:center;"> 0.030 </td>
   <td style="text-align:center;"> 0.014 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FE: pid </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FE: year </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Age (squared age) </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year x Education </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year x Gender </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year x Resident Area </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Note: * p &lt; 0.1, ** p &lt; 0.05, *** p &lt; 0.01. Starand errors are culustered at individual level.</td></tr></tfoot>
</table>



<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<caption>First price elasticity: Exclude data with year 2013 and 2014</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Overall </th>
   <th style="text-align:center;"> Intensive </th>
   <th style="text-align:center;"> Extensive </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> log(first giving price) </td>
   <td style="text-align:center;"> -1.318*** </td>
   <td style="text-align:center;"> -0.917** </td>
   <td style="text-align:center;"> -0.298*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.324) </td>
   <td style="text-align:center;"> (0.359) </td>
   <td style="text-align:center;"> (0.073) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> log(annual taxable income) </td>
   <td style="text-align:center;"> 4.237*** </td>
   <td style="text-align:center;"> 1.430* </td>
   <td style="text-align:center;"> 0.985*** </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (1.125) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.758) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.260) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 38374 </td>
   <td style="text-align:center;"> 8715 </td>
   <td style="text-align:center;"> 38374 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Adj. </td>
   <td style="text-align:center;"> 0.535 </td>
   <td style="text-align:center;"> 0.675 </td>
   <td style="text-align:center;"> 0.467 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Within </td>
   <td style="text-align:center;"> 0.013 </td>
   <td style="text-align:center;"> 0.032 </td>
   <td style="text-align:center;"> 0.012 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FE: pid </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FE: year </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Age (squared age) </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year x Education </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year x Gender </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year x Resident Area </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Note: * p &lt; 0.1, ** p &lt; 0.05, *** p &lt; 0.01. Starand errors are culustered at individual level.</td></tr></tfoot>
</table>


## 自営業者と給与所得者の異質性

- IIPFの宮崎先生のSuggestion
- Reduced formで推定してpreliminalyな結果として見せて、tax reliefに起因するものと論じることは可能かも（加藤の意見）

以下に給与所得者（Wage earners）と自営業者（Self-employed）でサブサンプルに分けて推定した結果を示す。
ただし、やってみたはいいが、解釈が難しい・・・・。


<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>First price elasticity: Wage earners</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Overall </th>
   <th style="text-align:center;"> Intensive </th>
   <th style="text-align:center;"> Extensive </th>
   <th style="text-align:center;"> Overall  </th>
   <th style="text-align:center;"> Intensive  </th>
   <th style="text-align:center;"> Extensive  </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> log(first giving price) </td>
   <td style="text-align:center;"> -1.802*** </td>
   <td style="text-align:center;"> -1.170*** </td>
   <td style="text-align:center;"> -0.353*** </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.364) </td>
   <td style="text-align:center;"> (0.354) </td>
   <td style="text-align:center;"> (0.085) </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> log(first giving price) X 1 = apply tax relief </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> -1.912*** </td>
   <td style="text-align:center;"> -1.362*** </td>
   <td style="text-align:center;"> -0.259 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> (0.738) </td>
   <td style="text-align:center;"> (0.457) </td>
   <td style="text-align:center;"> (0.177) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> log(annual taxable income) </td>
   <td style="text-align:center;"> 4.489*** </td>
   <td style="text-align:center;"> 0.956 </td>
   <td style="text-align:center;"> 1.037*** </td>
   <td style="text-align:center;"> -0.870 </td>
   <td style="text-align:center;"> 0.713 </td>
   <td style="text-align:center;"> -0.539 </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (1.492) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.807) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.356) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (2.216) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.891) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.506) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 22235 </td>
   <td style="text-align:center;"> 6538 </td>
   <td style="text-align:center;"> 22235 </td>
   <td style="text-align:center;"> 5619 </td>
   <td style="text-align:center;"> 3983 </td>
   <td style="text-align:center;"> 5619 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Adj. </td>
   <td style="text-align:center;"> 0.521 </td>
   <td style="text-align:center;"> 0.682 </td>
   <td style="text-align:center;"> 0.446 </td>
   <td style="text-align:center;"> 0.423 </td>
   <td style="text-align:center;"> 0.713 </td>
   <td style="text-align:center;"> 0.250 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Within </td>
   <td style="text-align:center;"> 0.021 </td>
   <td style="text-align:center;"> 0.043 </td>
   <td style="text-align:center;"> 0.020 </td>
   <td style="text-align:center;"> 0.064 </td>
   <td style="text-align:center;"> 0.062 </td>
   <td style="text-align:center;"> 0.066 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FE: pid </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FE: year </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Age (squared age) </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year x Education </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year x Gender </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year x Resident Area </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
</tbody>
</table>



<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>First price elasticity: Self-employed</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Overall </th>
   <th style="text-align:center;"> Intensive </th>
   <th style="text-align:center;"> Extensive </th>
   <th style="text-align:center;"> Overall  </th>
   <th style="text-align:center;"> Intensive  </th>
   <th style="text-align:center;"> Extensive  </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> log(first giving price) </td>
   <td style="text-align:center;"> 0.785 </td>
   <td style="text-align:center;"> -2.164* </td>
   <td style="text-align:center;"> 0.321*** </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.491) </td>
   <td style="text-align:center;"> (1.188) </td>
   <td style="text-align:center;"> (0.119) </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> log(first giving price) X 1 = apply tax relief </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> -11.160*** </td>
   <td style="text-align:center;"> -2.959** </td>
   <td style="text-align:center;"> -1.982*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> (1.896) </td>
   <td style="text-align:center;"> (1.237) </td>
   <td style="text-align:center;"> (0.391) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> log(annual taxable income) </td>
   <td style="text-align:center;"> 1.375 </td>
   <td style="text-align:center;"> -4.080 </td>
   <td style="text-align:center;"> 0.270 </td>
   <td style="text-align:center;"> 0.268 </td>
   <td style="text-align:center;"> -12.149** </td>
   <td style="text-align:center;"> 1.180 </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (1.474) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (3.381) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.352) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (8.086) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (5.106) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (1.873) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 20076 </td>
   <td style="text-align:center;"> 3269 </td>
   <td style="text-align:center;"> 20076 </td>
   <td style="text-align:center;"> 6330 </td>
   <td style="text-align:center;"> 1229 </td>
   <td style="text-align:center;"> 6330 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Adj. </td>
   <td style="text-align:center;"> 0.532 </td>
   <td style="text-align:center;"> 0.713 </td>
   <td style="text-align:center;"> 0.458 </td>
   <td style="text-align:center;"> 0.553 </td>
   <td style="text-align:center;"> 0.770 </td>
   <td style="text-align:center;"> 0.477 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Within </td>
   <td style="text-align:center;"> 0.017 </td>
   <td style="text-align:center;"> 0.087 </td>
   <td style="text-align:center;"> 0.018 </td>
   <td style="text-align:center;"> 0.072 </td>
   <td style="text-align:center;"> 0.244 </td>
   <td style="text-align:center;"> 0.057 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FE: pid </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FE: year </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Age (squared age) </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year x Education </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year x Gender </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year x Resident Area </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
</tbody>
</table>

