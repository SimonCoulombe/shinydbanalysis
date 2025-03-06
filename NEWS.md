# shinydbanalysis 1.0.2  
-stop infinite loop in select table
-wider select table / sidebarsfor long table names 
-demo with module
-cleaner module input/output names  
- data_fetcher doesnt ingest full modules outputs from table_picker, filter_builder, summary_builder to be a bit cleaner.


# shinydbanalysis 1.0.1

* table picker create table reference using metadata instead of making a call to colnames()
info$metadata$column_name
* added restricted_columns    
* actually play nice with ADLS



# shinydbanalysis 1.0   
* initial release  
