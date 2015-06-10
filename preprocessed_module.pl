:-module(preprocessed,
	[
			 complex/2,
			 protein_set/2,
			 inputs_to_reaction/2,
			 outputs_to_reaction/2,
			 type/2,
	                 tuple/3,
	                 link/3,
	                 link_type/3
		     ]).

/** <module> Preprocessed_version of predicates
* This module has some useful predicates for interacting with the
* owl:rdf ontology file for Reactome
* @author Sam Neaves
*/



%%	type(?Reactome_Id:reactome_id, ?Type: reactome_id) is det
%
%	Finds the type of a reactome component for protein_sets,
%	proteins, and complexes
%
%	@see reactome_utility:type_c/2 for calculated version
%

:-consult('Preprocessed_files/types.pl').


%%	complex(?Complex_Id:complex_id, ?List_of_components:list) is det
%
%	The preprocessed version of reactome_utility:complex_c/2
%
:-consult('Preprocessed_files/complexes.pl').


%%	protein_set(?Protein_Set_Id: protein_id, ?List_Of_Members:list)is det
%
%	@see reactome_utility:protein_set_c/2 for the calculated version
%

:-consult('Preprocessed_files/protein_sets.pl').

%%	outputs_to_reaction(?Reaction_id: reaction_id,?List_Of_Members:list)is det
%
%	@see reactome_utility:outputs_to_reaction_c/2 for the calculated
%	version
%

:-consult('Preprocessed_files/reaction_inputs.pl').

%%	inputs_to_reaction(?Reaction_id: reaction_id,?List_Of_Members:list)is det
%
%	@see reactome_utility:inputs_to_reaction_c/2 for the calculated
%	version
%

:-consult('Preprocessed_files/reaction_outputs.pl').

%%	outputs_to_reaction(?Reaction_id:
%	reaction_id,?List_Of_Members:list)is det
%
%	@see reactome_utility:outputs_to_reaction_c/2 for the
%	calculated version
%


:-consult('Preprocessed_files/linking_entities_whole_network').


%%	link(?Reaction_id: reaction_id,?Reaction_id2: reaction_id,?Entity_id: reactome_id)is det
%
%	@see
%	version
%

:-consult('Preprocessed_files/whole_network_link').

%%	link_type(?Reaction_id:reaction_id,?Reaction_id2:reaction_id,?Link_type:link_type)is det
%
%	@see
%	version
%



:-consult('Preprocessed_files/all_samples_all_reactions').


%%	tuple(?Reaction_id: reaction_id,?List_Of_Members:list)is det
%
%	@see reactome_utility:inputs_to_reaction_c/2 for the calculated
%	version
%

