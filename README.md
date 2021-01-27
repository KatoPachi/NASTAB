# Project NaSTaB

Researchers
- Hiroki Kato (Osaka University): Data Analyst and Project manager
- Tsuyoshi Goto (Chiba University): Literature Reviewer and Advisor of data analysis 
- Yong-Rok Kim (Kobe University): Institution Reviewer

## Our Research Project

多くの国は金銭的寄付のインセンティブとして、tax benefitを用いた寄付価格の減少を用いている。
しかしながら、tax benefitの利用は政府の収入を減らすことにつながるので、政府の公共財供給を減らしかねない。
したがって、tax benefitの利用は金銭的寄付を通じた公共財供給の増加による便益と政府の公共財供給の減少によるコストで決まる。
本プロジェクトは韓国の2014年の租税改正を利用して価格弾力性を推定し、「税金が最上の寄付」と考えている人がこの価格に対してどのような反応をするかを検証することを目的とする。

# About This Repo

このリポジトリはデータ分析用に使用します。
ただ、データ分析以外の用件もここで一括管理できるようにしています。

## About Branch (update: 2020/8/23)
- "preprocess"ブランチ：@daraterkim さん専用。データの成形などの前処理のためのブランチ。データ処理が終わったら、empiricsブランチに反映させる。
- "emprics"ブランチ（旧"Develop"ブランチ）：実証分析のコードを更新します
- "Report"ブランチ：Developブランチで分析が終了したら、このブランチに変更を反映する。そのあと、ミーティングなどで使用する資料スライドや文書ファイルを作成する
- "master"ブランチ：ミーティングなどの区切りがあれば、reportブランチの変更をこのブランチに反映し、適宜タグを作成します

## About Issues
データ分析やその他用件をここで発行します。
- **使用方法**："New Issue"を押して、用件をまとめてください。右側にあるAssigneesで「誰に対する用件」かを選択してください。Labelsは適当なものを選択してください。Projects（後述）はRepositoryタブからNaSTaBを選択してください
- **発行した用件が終了したら、一番下にあるClosedを押してください（戻すことも可能です。ClosedされているIssueを選択してReopenを押してください。）**

## About Projects
これは発行されているIssueを一覧することができます（Todoリスト）
- ToDoカラム：発行されたOpen issueがここに溜まっていきます
- In Progressカラム：ToDoにあるissueについて取り組み始めたら、自分でそのissueをこのカラムに移してください
- Doneカラム：issueをclosedしたものがここに溜まります
- Otherカラム：とくに決まりはありません。何かTodoがあったら適当に作ってください。
