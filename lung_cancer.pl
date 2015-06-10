
:-module(lung_cancer,[
	     sample_diagnosis/3,
	     sample/1,
	     tuple_ac/3,
	     tuple_scc/3,
	     samples/1
	 ]).
:-use_module('utility_module.pl').



:-csv_read_file_row('raw_data/SBVIMPROVER LC Metadata Training.csv',Row,[functor(collumns)]),Row=..List,assert(collumns(List)),!.

:- foreach(csv_read_file_row_list('raw_data/SBVIMPROVER LC Metadata Training.csv', List,'sample_class'), assert(sample_class(List))).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%Class Predicates%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%This commented to surpess annoying warnings for the time being.

index_of_col(Col,Index):-
	collumns(List),
	List=[collumns|CoLList],
	index_of(CoLList,Col,Index).



sample_diagnosis(Sample,Diagnosis,Tumor):-
	index_of_col('Diagnosis',Diagnosis_Index),
	index_of_col('Sample ID',Sample_Id_Index),
	index_of_col('Tumor stage',Tumor_stage_Index),
	sample_class(Sample_List),
	Sample_List=[sample_class|Sample_Tuple],
	Sample_Tuple=[H|_T],H\='Dataset ID',
	nth0(Diagnosis_Index,Sample_Tuple,Diagnosis),
	nth0(Sample_Id_Index,Sample_Tuple,Sample),
	nth0(Tumor_stage_Index,Sample_Tuple,Tumor).

sample(Sample):-
	samples(Samples),
	member(Sample,Samples).

samples(Samples):-
	setof(Sample, tuple(Sample,_Y,_Z),Samples),!.

tuple_ac(Sample,Reaction,State):-
	sample_diagnosis(Sample,'AC',_),
	tuple(Sample,Reaction,State).

tuple_scc(Sample,Reaction,State):-
	sample_diagnosis(Sample,'SCC',_),
	tuple(Sample,Reaction,State).

