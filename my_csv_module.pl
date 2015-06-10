:- module(my_csv_module,[csv_read_file_row_list/3]).

csv_read_file_row_list(File, List, Functor):-
	csv_read_file_row(File,Row,[functor(Functor)]),Row=..List.

