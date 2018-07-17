
require(data.table)
require(stingi)
require(gsubfn)
require(magrittr)

# df data.table
# fn regexp matching column names
# ff format function
set_format<-function(
  df,
  fn="*", # all fields
  ff=list(comma=.%>%comma(digits=0)) # add commas to large numbers
){
  table_fields<-names(df)
  matched_table_fields<-table_fields[grepl(fn,table_fields)]
  if(length(matched_table_fields)<1)return(df)
  for(i in matched_table_fields){
    current_format<-attributes(df[[i]])$format
    new_format<-c(current_format,ff)
    attributes(df[[i]])$format<-new_format
  }
  if(!"nntable" %in% class(df)){
    class(df)<-c("nntable",class(df))
  }
  df
}









