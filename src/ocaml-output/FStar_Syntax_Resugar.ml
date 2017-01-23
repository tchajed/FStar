
open Prims

let core_term_to_surface_term = (fun t -> (failwith "boo"))


let core_qualifier_to_surface_qualifier = (fun q -> (failwith "ah"))


let sigelt_to_decl : FStar_Syntax_Syntax.sigelt  ->  FStar_Parser_AST.decl = (fun s -> (match (s) with
| FStar_Syntax_Syntax.Sig_inductive_typ (lid, unames, bs, t, mdts, dcs, qs, r) -> begin
(failwith "no")
end
| FStar_Syntax_Syntax.Sig_bundle (_74_15) -> begin
(failwith "no")
end
| FStar_Syntax_Syntax.Sig_datacon (_74_18) -> begin
(failwith "no")
end
| FStar_Syntax_Syntax.Sig_declare_typ (lid, unames, t, qs, r) -> begin
(

let d = (FStar_List.filter_map core_qualifier_to_surface_qualifier qs)
in (let _174_7 = (let _174_6 = (let _174_5 = (core_term_to_surface_term t)
in ((lid.FStar_Ident.ident), (_174_5)))
in FStar_Parser_AST.Val (_174_6))
in (FStar_Parser_AST.mk_decl _174_7 r d)))
end
| FStar_Syntax_Syntax.Sig_let (_74_29) -> begin
(failwith "no")
end
| FStar_Syntax_Syntax.Sig_main (_74_32) -> begin
(failwith "no")
end
| FStar_Syntax_Syntax.Sig_assume (_74_35) -> begin
(failwith "no")
end
| FStar_Syntax_Syntax.Sig_new_effect (_74_38) -> begin
(failwith "no")
end
| FStar_Syntax_Syntax.Sig_new_effect_for_free (_74_41) -> begin
(failwith "no")
end
| FStar_Syntax_Syntax.Sig_sub_effect (_74_44) -> begin
(failwith "no")
end
| FStar_Syntax_Syntax.Sig_effect_abbrev (_74_47) -> begin
(failwith "no")
end
| FStar_Syntax_Syntax.Sig_pragma (_74_50) -> begin
(failwith "no")
end))




