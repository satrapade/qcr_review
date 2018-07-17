
temp_file_list<-list.files(
  path=c(".","graphics","test","utility"),
  pattern="(*\\.tex$)|(*\\.toc$)|(*\\.log$)|(*\\.pdf)"
)

file.remove(temp_file_list)


