#light "off"


module FStar.Syntax.Resugar

open FStar
open FStar.Ident
open FStar.Parser.AST
open FStar.Syntax.Syntax

module S = FStar.Syntax.Syntax
module AST = FStar.Parser.AST
let core_term_to_surface_term t = failwith "boo"
let core_qualifier_to_surface_qualifier q = match q with
    | S.Private -> AST.Private
    | _ -> failwith "no" 
let sigelt_to_decl s = match s with
    | Sig_inductive_typ (lid, unames, bs, t, mdts, dcs, qs, r) ->
        failwith "no"
    | Sig_bundle _ ->
        failwith "no"
    | Sig_datacon _ ->
        failwith "no"
    | Sig_declare_typ (lid, unames, t, qs, r) ->
        // check qualifiers against options
        let d = List.filter_map core_qualifier_to_surface_qualifier qs in
        mk_decl (Val (lid.ident, core_term_to_surface_term t)) r d
    | Sig_let _ -> 
        failwith "no"
    | Sig_main _ ->
        failwith "no"
    | Sig_assume _ ->
        failwith "no"
    | Sig_new_effect _ ->
        failwith "no"
    | Sig_new_effect_for_free _ ->
        failwith "no"
    | Sig_sub_effect _ ->
        failwith "no"
    | Sig_effect_abbrev _ ->
        failwith "no"
    | Sig_pragma _ ->
        failwith "no"