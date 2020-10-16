while inotifywait -e close_write sum.ml; do dune build sum.exe; done
