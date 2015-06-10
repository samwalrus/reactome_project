%File Loader
:-working_directory(_X,'/home/sam/Documents/Version3').
:-set_prolog_stack(global, limit(8*10**9)). %Stack needs to be big.

csv_read_file_row_list(File, List, Functor):-
	csv_read_file_row(File,Row,[functor(Functor)]),Row=..List.

:-csv_read_file('probe_uniprot.csv', Rows, [functor(probe_uniProt)]), maplist(assert, Rows).
:-csv_read_file('probe_go.csv', Rows, [functor(probe_go)]), maplist(assert, Rows).



%read a barcode file
:-[load_geo2]. %for samples/4
:-[load_geo].  %for samples/3
:-['all_samples_all_reactions'].
:-['all_pathway_diffs_v2.pl'].
:-['whole_network_link.pl'].

:-prepare_db2('GSE2109_BarCode_no_quotes.csv'). %samples/4
:-prepare_db('GSE2109_BarCode_no_quotes.csv'). %samples/3

%read metadata
:-csv_read_file_row('SBVIMPROVER LC Metadata Training.csv',Row,[functor(collumns)]),Row=..List,assert(collumns(List)),!.

%assert
:- foreach(csv_read_file_row_list('SBVIMPROVER LC Metadata Training.csv', List,'sample_class'), assert(sample_class(List))).

:-load_rdf('Homo sapiens.owl',List_of_RDF_statements),checklist(assert,List_of_RDF_statements).

:-working_directory(_X,'/home/sam/Documents/Version3/files to assert').


:-consult(complexes_corrected).
:-consult(protein_sets_corrected).
:-consult(reaction_inputs_corrected).
:-consult(reaction_outputs_corrected).
:-consult(types_corrected).

:-working_directory(_X,'/home/sam/Documents/Version3').


