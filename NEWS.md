# shindbanalysis 1.0.6
we can now group categorical variables on the fly.

# shindbanalysis 1.0.5
We can now group on numeric variables by creating "bandings on the fly"


# shindbanalysis 1.0.4   
in filter_builder_server, the observeEvent doesnt clear the dropdown when changing the selected table name anymore.  the reason:
sometimes the observe() that maintains the filter (based on selected_table_name, but also the created filters) would populate the list first, then the observeEvent would clear the tables.


# shindbanalysis 1.0.3   

- split data_fetcher into query_builder and data_fetcher
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
