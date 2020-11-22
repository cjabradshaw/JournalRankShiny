# JournalRankShiny

This R Shiny app provides a <em>κ</em>-resampled composite journal rank incorporating six user-supplied citation indices

There are many methods available to assess the relative citation performance of peer-reviewed journals. Regardless of their individual faults and advantages, citation-based metrics are used by researchers to maximise the citation potential of their articles, and by employers to rank academic track records. The absolute value of any particular index is arguably meaningless unless compared to other journals, and different metrics result in divergent rankings. To provide a simple yet more objective way to rank journals within and among disciplines, this app provides a <em>κ</em>-resampled composite journal rank incorporating six user-supplied citation indices: <a href="http://help.incites.clarivate.com/incitesLiveJCR/glossaryAZgroup/g8/4346-TRS.html">Impact Factor</a> (IF), <a href="http://help.incites.clarivate.com/incitesLiveJCR/glossaryAZgroup/g7/7751-TRS.html">Immediacy Index</a> (IM), <a href="https://scholar.google.com/intl/en/scholar/metrics.html#metrics">Google 5-year h-index</a> (h5), <a href="https://service.elsevier.com/app/answers/detail/a_id/30562/supporthub/scopus/">CiteScore</a> (CS), <a href="https://blog.scopus.com/posts/journal-metrics-in-scopus-source-normalized-impact-per-paper-snip">Source-Normalized Impact Per Paper</a> (SNIP), and <a href="https://www.scopusjournals.com/2019/02/scimago-journal-rank.html">SCImago Journal Rank</a> (SJR).

The output gives an index of relative rank uncertainty for all sample journals provided by the user. This Github <a href = "https://github.com/cjabradshaw/JournalRankShiny">repository</a> provides all the 'under-the-bonnet' code for the app. Read the related <a href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0149852">paper</a> and/or <a href="https://conservationbytes.com/2016/02/18/how-to-rank-journals/">blog post</a>.
