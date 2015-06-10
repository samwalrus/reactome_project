:-set_prolog_stack(global, limit(8*10**9)).




:-use_module('utility_module.pl').
:-use_module('reactome_utility_module.pl').
:-use_module('preprocessed_module.pl').
:-use_module('my_csv_module.pl').
:-use_module('lung_cancer.pl').
:-use_module('graphs_module.pl').
:-use_module('interface.pl').




:-load_rdf('raw_data/Homo sapiens.owl',List_of_RDF_statements),checklist(assert,List_of_RDF_statements).




:-unload_file(debug).
:-unload_file(load).


