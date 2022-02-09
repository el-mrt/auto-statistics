app_descriptions <- list(
  "plot_mis_per_col" = "This plot shows the missing data in each feature. Note: x_label linebreakes decides when to split the labels. Flipping the coordinates can be helpful if long names are created.",
  "plot_mis_na_comb" = "This plot shows which features share missing data in each row. Note: x_label linebreakes decides when to split the labels. use names is often impractical with larger numbers of variables",
  "plot_mis_hist" = "This plot shows the distribution of missing data of a specified feature with respect to the target feature. Futher it is possible to extend this to a scatterplot and select a second feature.",
  "landing_descr" = "<p>This Shiny app was created as part of the Application of Data Analytics course and is designed to provide insight into the descriptive properties of a dataset and subsequently test various machine learning algorithms and strategies.</p>
    <p><strong>Overview: </strong></p>
    <p><span style='text-decoration: underline;'>Upload Data:</span></p>
    <p>Upload your data to the app.  You can also adjust parameters here to ensure that the data is read correctly and select the TargetFeature of the dataset. In addition, factor features can also be specified here.</p>
    <p><span style='text-decoration: underline;'>Deskriptive:</span></p>
    <p>Descriptive properties of a feature to be selected can be displayed here. These include a histogram of the variable, a scatterplot, including the Target feature, and a brief statistical summary.</p>
    <p><span style='text-decoration: underline;'>Data Insights:</span></p>
    <p>Here it is possible to determine the correlation between the individual features, although currently only numerical features are supported. Furthermore, a first determination of the feature importance can be performed.</p>
    <p><span style='text-decoration: underline;'>Missing Data:</span></p>
    <p>Here an overview of the missing data within the dataset is shown. On the one hand it can be seen how many entries per feature are missing and on the other hand combinations of features can be displayed which have missing data in the same observations. Furthermore, it is possible to look at the distribution of missing data within a feature in relation to the target feature. This graph can be extended with a second feature.</p>
    <p><span style='text-decoration: underline;'>Edit Data:</span><br />This tab provides the ability to make data adjustments. This includes removing entire columns and removing missing data.</p>
    <p><span style='text-decoration: underline;'>Report:</span></p>
    <p>It is possible to choose between three different reports. The 'Custom Report' contains all previously selected graphs. The descriptive report contains all characteristic values of the selected features and the ML report describes the result of the machine learning. The 'Top5' in the descriptive report refers to the fifth most important features. Currently the reports are output in HTML.</p>
    <p>&nbsp;</p>
    <p><em>A detailed manual can be found on the Github page.</em></p>
  ",
  "landing_descr_logo" = "
  <center>
  <a href='https://tu-dresden.de/bu/verkehr/ivw/bda?set_language=en'><img src='logo_big_data.png' alt='TUD Big Data' width='230' height='65'></a>
  <a href='https://github.com/el-mrt/auto-statistics'><img src='logo_github.png' alt='Github' width='65' height='65'></a>
  </center>
  "
  )
