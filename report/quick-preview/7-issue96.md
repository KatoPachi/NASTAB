






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

