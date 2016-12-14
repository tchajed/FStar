
open Prims

type local_binding =
(FStar_Ident.ident * FStar_Syntax_Syntax.bv * Prims.bool)


type rec_binding =
(FStar_Ident.ident * FStar_Ident.lid * FStar_Syntax_Syntax.delta_depth)


type module_abbrev =
(FStar_Ident.ident * FStar_Ident.lident)


type open_kind =
| Open_module
| Open_namespace


let is_Open_module = (fun _discr_ -> (match (_discr_) with
| Open_module (_) -> begin
true
end
| _ -> begin
false
end))


let is_Open_namespace = (fun _discr_ -> (match (_discr_) with
| Open_namespace (_) -> begin
true
end
| _ -> begin
false
end))


type open_module_or_namespace =
(FStar_Ident.lident * open_kind)


type record_or_dc =
{typename : FStar_Ident.lident; constrname : FStar_Ident.ident; parms : FStar_Syntax_Syntax.binders; fields : (FStar_Ident.ident * FStar_Syntax_Syntax.typ) Prims.list; is_private_or_abstract : Prims.bool; is_record : Prims.bool}


let is_Mkrecord_or_dc : record_or_dc  ->  Prims.bool = (Obj.magic ((fun _ -> (FStar_All.failwith "Not yet implemented:is_Mkrecord_or_dc"))))


type scope_mod =
| Local_binding of local_binding
| Rec_binding of rec_binding
| Module_abbrev of module_abbrev
| Open_module_or_namespace of open_module_or_namespace
| Top_level_def of FStar_Ident.ident
| Record_or_dc of record_or_dc


let is_Local_binding = (fun _discr_ -> (match (_discr_) with
| Local_binding (_) -> begin
true
end
| _ -> begin
false
end))


let is_Rec_binding = (fun _discr_ -> (match (_discr_) with
| Rec_binding (_) -> begin
true
end
| _ -> begin
false
end))


let is_Module_abbrev = (fun _discr_ -> (match (_discr_) with
| Module_abbrev (_) -> begin
true
end
| _ -> begin
false
end))


let is_Open_module_or_namespace = (fun _discr_ -> (match (_discr_) with
| Open_module_or_namespace (_) -> begin
true
end
| _ -> begin
false
end))


let is_Top_level_def = (fun _discr_ -> (match (_discr_) with
| Top_level_def (_) -> begin
true
end
| _ -> begin
false
end))


let is_Record_or_dc = (fun _discr_ -> (match (_discr_) with
| Record_or_dc (_) -> begin
true
end
| _ -> begin
false
end))


let ___Local_binding____0 = (fun projectee -> (match (projectee) with
| Local_binding (_64_40) -> begin
_64_40
end))


let ___Rec_binding____0 = (fun projectee -> (match (projectee) with
| Rec_binding (_64_43) -> begin
_64_43
end))


let ___Module_abbrev____0 = (fun projectee -> (match (projectee) with
| Module_abbrev (_64_46) -> begin
_64_46
end))


let ___Open_module_or_namespace____0 = (fun projectee -> (match (projectee) with
| Open_module_or_namespace (_64_49) -> begin
_64_49
end))


let ___Top_level_def____0 = (fun projectee -> (match (projectee) with
| Top_level_def (_64_52) -> begin
_64_52
end))


let ___Record_or_dc____0 = (fun projectee -> (match (projectee) with
| Record_or_dc (_64_55) -> begin
_64_55
end))


type string_set =
Prims.string FStar_Util.set


type exported_id_kind =
| Exported_id_term_type
| Exported_id_field


let is_Exported_id_term_type = (fun _discr_ -> (match (_discr_) with
| Exported_id_term_type (_) -> begin
true
end
| _ -> begin
false
end))


let is_Exported_id_field = (fun _discr_ -> (match (_discr_) with
| Exported_id_field (_) -> begin
true
end
| _ -> begin
false
end))


type exported_id_set =
exported_id_kind  ->  string_set FStar_ST.ref


type env =
{curmodule : FStar_Ident.lident Prims.option; curmonad : FStar_Ident.ident Prims.option; modules : (FStar_Ident.lident * FStar_Syntax_Syntax.modul) Prims.list; scope_mods : scope_mod Prims.list; exported_ids : exported_id_set FStar_Util.smap; trans_exported_ids : exported_id_set FStar_Util.smap; includes : FStar_Ident.lident Prims.list FStar_ST.ref FStar_Util.smap; sigaccum : FStar_Syntax_Syntax.sigelts; sigmap : (FStar_Syntax_Syntax.sigelt * Prims.bool) FStar_Util.smap; default_result_effect : FStar_Ident.lident; iface : Prims.bool; admitted_iface : Prims.bool; expect_typ : Prims.bool}


let is_Mkenv : env  ->  Prims.bool = (Obj.magic ((fun _ -> (FStar_All.failwith "Not yet implemented:is_Mkenv"))))


type foundname =
| Term_name of (FStar_Syntax_Syntax.typ * Prims.bool)
| Eff_name of (FStar_Syntax_Syntax.sigelt * FStar_Ident.lident)


let is_Term_name = (fun _discr_ -> (match (_discr_) with
| Term_name (_) -> begin
true
end
| _ -> begin
false
end))


let is_Eff_name = (fun _discr_ -> (match (_discr_) with
| Eff_name (_) -> begin
true
end
| _ -> begin
false
end))


let ___Term_name____0 = (fun projectee -> (match (projectee) with
| Term_name (_64_72) -> begin
_64_72
end))


let ___Eff_name____0 = (fun projectee -> (match (projectee) with
| Eff_name (_64_75) -> begin
_64_75
end))


let all_exported_id_kinds : exported_id_kind Prims.list = (Exported_id_field)::(Exported_id_term_type)::[]


let open_modules : env  ->  (FStar_Ident.lident * FStar_Syntax_Syntax.modul) Prims.list = (fun e -> e.modules)


let current_module : env  ->  FStar_Ident.lident = (fun env -> (match (env.curmodule) with
| None -> begin
(FStar_All.failwith "Unset current module")
end
| Some (m) -> begin
m
end))


let qual : FStar_Ident.lident  ->  FStar_Ident.ident  ->  FStar_Ident.lident = FStar_Syntax_Util.qual_id


let qualify : env  ->  FStar_Ident.ident  ->  FStar_Ident.lident = (fun env id -> (match (env.curmonad) with
| None -> begin
(let _161_188 = (current_module env)
in (qual _161_188 id))
end
| Some (monad) -> begin
(let _161_190 = (let _161_189 = (current_module env)
in (qual _161_189 monad))
in (FStar_Syntax_Util.mk_field_projector_name_from_ident _161_190 id))
end))


let new_sigmap = (fun _64_88 -> (match (()) with
| () -> begin
(FStar_Util.smap_create (Prims.parse_int "100"))
end))


let empty_env : Prims.unit  ->  env = (fun _64_89 -> (match (()) with
| () -> begin
(let _161_197 = (new_sigmap ())
in (let _161_196 = (new_sigmap ())
in (let _161_195 = (new_sigmap ())
in (let _161_194 = (new_sigmap ())
in {curmodule = None; curmonad = None; modules = []; scope_mods = []; exported_ids = _161_197; trans_exported_ids = _161_196; includes = _161_195; sigaccum = []; sigmap = _161_194; default_result_effect = FStar_Syntax_Const.effect_Tot_lid; iface = false; admitted_iface = false; expect_typ = false}))))
end))


let sigmap : env  ->  (FStar_Syntax_Syntax.sigelt * Prims.bool) FStar_Util.smap = (fun env -> env.sigmap)


let has_all_in_scope : env  ->  Prims.bool = (fun env -> (FStar_List.existsb (fun _64_95 -> (match (_64_95) with
| (m, _64_94) -> begin
(FStar_Ident.lid_equals m FStar_Syntax_Const.all_lid)
end)) env.modules))


let default_total : env  ->  env = (fun env -> (

let _64_97 = env
in {curmodule = _64_97.curmodule; curmonad = _64_97.curmonad; modules = _64_97.modules; scope_mods = _64_97.scope_mods; exported_ids = _64_97.exported_ids; trans_exported_ids = _64_97.trans_exported_ids; includes = _64_97.includes; sigaccum = _64_97.sigaccum; sigmap = _64_97.sigmap; default_result_effect = FStar_Syntax_Const.effect_Tot_lid; iface = _64_97.iface; admitted_iface = _64_97.admitted_iface; expect_typ = _64_97.expect_typ}))


let default_ml : env  ->  env = (fun env -> if (has_all_in_scope env) then begin
(

let _64_100 = env
in {curmodule = _64_100.curmodule; curmonad = _64_100.curmonad; modules = _64_100.modules; scope_mods = _64_100.scope_mods; exported_ids = _64_100.exported_ids; trans_exported_ids = _64_100.trans_exported_ids; includes = _64_100.includes; sigaccum = _64_100.sigaccum; sigmap = _64_100.sigmap; default_result_effect = FStar_Syntax_Const.effect_ML_lid; iface = _64_100.iface; admitted_iface = _64_100.admitted_iface; expect_typ = _64_100.expect_typ})
end else begin
env
end)


let set_bv_range : FStar_Syntax_Syntax.bv  ->  FStar_Range.range  ->  FStar_Syntax_Syntax.bv = (fun bv r -> (

let id = (

let _64_104 = bv.FStar_Syntax_Syntax.ppname
in {FStar_Ident.idText = _64_104.FStar_Ident.idText; FStar_Ident.idRange = r})
in (

let _64_107 = bv
in {FStar_Syntax_Syntax.ppname = id; FStar_Syntax_Syntax.index = _64_107.FStar_Syntax_Syntax.index; FStar_Syntax_Syntax.sort = _64_107.FStar_Syntax_Syntax.sort})))


let bv_to_name : FStar_Syntax_Syntax.bv  ->  FStar_Range.range  ->  FStar_Syntax_Syntax.term = (fun bv r -> (FStar_Syntax_Syntax.bv_to_name (set_bv_range bv r)))


let unmangleMap : (Prims.string * Prims.string * FStar_Syntax_Syntax.delta_depth * FStar_Syntax_Syntax.fv_qual Prims.option) Prims.list = ((("op_ColonColon"), ("Cons"), (FStar_Syntax_Syntax.Delta_constant), (Some (FStar_Syntax_Syntax.Data_ctor))))::((("not"), ("op_Negation"), (FStar_Syntax_Syntax.Delta_equational), (None)))::[]


let unmangleOpName : FStar_Ident.ident  ->  (FStar_Syntax_Syntax.term * Prims.bool) Prims.option = (fun id -> (

let t = (FStar_Util.find_map unmangleMap (fun _64_116 -> (match (_64_116) with
| (x, y, dd, dq) -> begin
if (id.FStar_Ident.idText = x) then begin
(let _161_219 = (let _161_218 = (FStar_Ident.lid_of_path (("Prims")::(y)::[]) id.FStar_Ident.idRange)
in (FStar_Syntax_Syntax.fvar _161_218 dd dq))
in Some (_161_219))
end else begin
None
end
end)))
in (match (t) with
| Some (v) -> begin
Some (((v), (false)))
end
| None -> begin
None
end)))


type 'a cont_t =
| Cont_ok of 'a
| Cont_fail
| Cont_ignore


let is_Cont_ok = (fun _ _discr_ -> (match (_discr_) with
| Cont_ok (_) -> begin
true
end
| _ -> begin
false
end))


let is_Cont_fail = (fun _ _discr_ -> (match (_discr_) with
| Cont_fail (_) -> begin
true
end
| _ -> begin
false
end))


let is_Cont_ignore = (fun _ _discr_ -> (match (_discr_) with
| Cont_ignore (_) -> begin
true
end
| _ -> begin
false
end))


let ___Cont_ok____0 = (fun projectee -> (match (projectee) with
| Cont_ok (_64_124) -> begin
_64_124
end))


let option_of_cont = (fun k_ignore _64_1 -> (match (_64_1) with
| Cont_ok (a) -> begin
Some (a)
end
| Cont_fail -> begin
None
end
| Cont_ignore -> begin
(k_ignore ())
end))


let find_in_record = (fun ns id record cont -> (

let typename' = (FStar_Ident.lid_of_ids (FStar_List.append ns ((record.typename.FStar_Ident.ident)::[])))
in if (FStar_Ident.lid_equals typename' record.typename) then begin
(

let fname = (FStar_Ident.lid_of_ids (FStar_List.append record.typename.FStar_Ident.ns ((id)::[])))
in (

let find = (FStar_Util.find_map record.fields (fun _64_141 -> (match (_64_141) with
| (f, _64_140) -> begin
if (id.FStar_Ident.idText = f.FStar_Ident.idText) then begin
Some (record)
end else begin
None
end
end)))
in (match (find) with
| Some (r) -> begin
(cont r)
end
| None -> begin
Cont_ignore
end)))
end else begin
Cont_ignore
end))


let get_exported_id_set : env  ->  Prims.string  ->  (exported_id_kind  ->  string_set FStar_ST.ref) Prims.option = (fun e mname -> (FStar_Util.smap_try_find e.exported_ids mname))


let get_trans_exported_id_set : env  ->  Prims.string  ->  (exported_id_kind  ->  string_set FStar_ST.ref) Prims.option = (fun e mname -> (FStar_Util.smap_try_find e.trans_exported_ids mname))


let string_of_exported_id_kind : exported_id_kind  ->  Prims.string = (fun _64_2 -> (match (_64_2) with
| Exported_id_field -> begin
"field"
end
| Exported_id_term_type -> begin
"term/type"
end))


let find_in_module_with_includes = (fun eikind find_in_module find_in_module_default env ns id -> (

let idstr = id.FStar_Ident.idText
in (

let rec aux = (fun _64_3 -> (match (_64_3) with
| [] -> begin
find_in_module_default
end
| (modul)::q -> begin
(

let mname = modul.FStar_Ident.str
in (

let not_shadowed = (match ((get_exported_id_set env mname)) with
| None -> begin
true
end
| Some (mex) -> begin
(

let mexports = (let _161_289 = (mex eikind)
in (FStar_ST.read _161_289))
in (FStar_Util.set_mem idstr mexports))
end)
in (

let mincludes = (match ((FStar_Util.smap_try_find env.includes mname)) with
| None -> begin
[]
end
| Some (minc) -> begin
(FStar_ST.read minc)
end)
in (

let look_into = if not_shadowed then begin
(let _161_290 = (qual modul id)
in (find_in_module _161_290))
end else begin
Cont_ignore
end
in (match (look_into) with
| Cont_ignore -> begin
(aux (FStar_List.append mincludes q))
end
| _64_180 -> begin
look_into
end)))))
end))
in (aux ((ns)::[])))))


let is_exported_id_field : exported_id_kind  ->  Prims.bool = (fun _64_4 -> (match (_64_4) with
| Exported_id_field -> begin
true
end
| _64_184 -> begin
false
end))


let try_lookup_id'' = (fun env id eikind k_local_binding k_rec_binding k_record find_in_module lookup_default_id -> (

let check_local_binding_id = (fun _64_5 -> (match (_64_5) with
| (id', _64_197, _64_199) -> begin
(id'.FStar_Ident.idText = id.FStar_Ident.idText)
end))
in (

let check_rec_binding_id = (fun _64_6 -> (match (_64_6) with
| (id', _64_205, _64_207) -> begin
(id'.FStar_Ident.idText = id.FStar_Ident.idText)
end))
in (

let curmod_ns = (let _161_323 = (current_module env)
in (FStar_Ident.ids_of_lid _161_323))
in (

let proc = (fun _64_7 -> (match (_64_7) with
| Local_binding (l) when (check_local_binding_id l) -> begin
(k_local_binding l)
end
| Rec_binding (r) when (check_rec_binding_id r) -> begin
(k_rec_binding r)
end
| Open_module_or_namespace (ns, _64_218) -> begin
(find_in_module_with_includes eikind find_in_module Cont_ignore env ns id)
end
| Top_level_def (id') when (id'.FStar_Ident.idText = id.FStar_Ident.idText) -> begin
(lookup_default_id Cont_ignore id)
end
| Record_or_dc (r) when (is_exported_id_field eikind) -> begin
(let _161_327 = (FStar_Ident.lid_of_ids curmod_ns)
in (find_in_module_with_includes Exported_id_field (fun lid -> (

let id = lid.FStar_Ident.ident
in (find_in_record lid.FStar_Ident.ns id r k_record))) Cont_ignore env _161_327 id))
end
| _64_228 -> begin
Cont_ignore
end))
in (

let rec aux = (fun _64_8 -> (match (_64_8) with
| (a)::q -> begin
(let _161_331 = (proc a)
in (option_of_cont (fun _64_235 -> (aux q)) _161_331))
end
| [] -> begin
(let _161_333 = (lookup_default_id Cont_fail id)
in (option_of_cont (fun _64_238 -> None) _161_333))
end))
in (aux env.scope_mods)))))))


let found_local_binding = (fun r _64_244 -> (match (_64_244) with
| (id', x, mut) -> begin
(let _161_336 = (bv_to_name x r)
in ((_161_336), (mut)))
end))


let find_in_module = (fun env lid k_global_def k_not_found -> (match ((FStar_Util.smap_try_find (sigmap env) lid.FStar_Ident.str)) with
| Some (sb) -> begin
(k_global_def lid sb)
end
| None -> begin
k_not_found
end))


let try_lookup_id : env  ->  FStar_Ident.ident  ->  (FStar_Syntax_Syntax.term * Prims.bool) Prims.option = (fun env id -> (match ((unmangleOpName id)) with
| Some (f) -> begin
Some (f)
end
| _64_257 -> begin
(try_lookup_id'' env id Exported_id_term_type (fun r -> (let _161_352 = (found_local_binding id.FStar_Ident.idRange r)
in Cont_ok (_161_352))) (fun _64_269 -> Cont_fail) (fun _64_267 -> Cont_ignore) (fun i -> (find_in_module env i (fun _64_263 _64_265 -> Cont_fail) Cont_ignore)) (fun _64_258 _64_260 -> Cont_fail))
end))


let lookup_default_id = (fun env id k_global_def k_not_found -> (

let find_in_monad = (match (env.curmonad) with
| Some (_64_278) -> begin
(

let lid = (qualify env id)
in (match ((FStar_Util.smap_try_find (sigmap env) lid.FStar_Ident.str)) with
| Some (r) -> begin
(let _161_370 = (k_global_def lid r)
in Some (_161_370))
end
| None -> begin
None
end))
end
| None -> begin
None
end)
in (match (find_in_monad) with
| Some (v) -> begin
v
end
| None -> begin
(

let lid = (let _161_371 = (current_module env)
in (qual _161_371 id))
in (find_in_module env lid k_global_def k_not_found))
end)))


let module_is_defined : env  ->  FStar_Ident.lident  ->  Prims.bool = (fun env lid -> ((let _161_376 = (current_module env)
in (FStar_Ident.lid_equals lid _161_376)) || (FStar_List.existsb (fun x -> (FStar_Ident.lid_equals lid (Prims.fst x))) env.modules)))


let resolve_module_name : env  ->  FStar_Ident.lident  ->  Prims.bool  ->  FStar_Ident.lident Prims.option = (fun env lid honor_ns -> (

let nslen = (FStar_List.length lid.FStar_Ident.ns)
in (

let rec aux = (fun _64_9 -> (match (_64_9) with
| [] -> begin
if (module_is_defined env lid) then begin
Some (lid)
end else begin
None
end
end
| (Open_module_or_namespace (ns, Open_namespace))::q when honor_ns -> begin
(

let new_lid = (let _161_388 = (let _161_387 = (FStar_Ident.path_of_lid ns)
in (let _161_386 = (FStar_Ident.path_of_lid lid)
in (FStar_List.append _161_387 _161_386)))
in (FStar_Ident.lid_of_path _161_388 (FStar_Ident.range_of_lid lid)))
in if (module_is_defined env new_lid) then begin
Some (new_lid)
end else begin
(aux q)
end)
end
| (Module_abbrev (name, modul))::_64_308 when ((nslen = (Prims.parse_int "0")) && (name.FStar_Ident.idText = lid.FStar_Ident.ident.FStar_Ident.idText)) -> begin
Some (modul)
end
| (_64_316)::q -> begin
(aux q)
end))
in (aux env.scope_mods))))


let resolve_in_open_namespaces'' = (fun env lid eikind k_local_binding k_rec_binding k_record f_module l_default -> (match (lid.FStar_Ident.ns) with
| (_64_330)::_64_328 -> begin
(match ((let _161_416 = (let _161_415 = (FStar_Ident.lid_of_ids lid.FStar_Ident.ns)
in (FStar_Ident.set_lid_range _161_415 (FStar_Ident.range_of_lid lid)))
in (resolve_module_name env _161_416 true))) with
| None -> begin
None
end
| Some (modul) -> begin
(let _161_418 = (find_in_module_with_includes eikind f_module Cont_fail env modul lid.FStar_Ident.ident)
in (option_of_cont (fun _64_335 -> None) _161_418))
end)
end
| [] -> begin
(try_lookup_id'' env lid.FStar_Ident.ident eikind k_local_binding k_rec_binding k_record f_module l_default)
end))


let cont_of_option = (fun k_none _64_10 -> (match (_64_10) with
| Some (v) -> begin
Cont_ok (v)
end
| None -> begin
k_none
end))


let resolve_in_open_namespaces' = (fun env lid k_local_binding k_rec_binding k_global_def -> (

let k_global_def' = (fun k lid def -> (let _161_444 = (k_global_def lid def)
in (cont_of_option k _161_444)))
in (

let f_module = (fun lid' -> (

let k = Cont_ignore
in (find_in_module env lid' (k_global_def' k) k)))
in (

let l_default = (fun k i -> (lookup_default_id env i (k_global_def' k) k))
in (resolve_in_open_namespaces'' env lid Exported_id_term_type (fun l -> (let _161_452 = (k_local_binding l)
in (cont_of_option Cont_fail _161_452))) (fun r -> (let _161_454 = (k_rec_binding r)
in (cont_of_option Cont_fail _161_454))) (fun _64_360 -> Cont_ignore) f_module l_default)))))


let fv_qual_of_se : FStar_Syntax_Syntax.sigelt  ->  FStar_Syntax_Syntax.fv_qual Prims.option = (fun _64_12 -> (match (_64_12) with
| FStar_Syntax_Syntax.Sig_datacon (_64_366, _64_368, _64_370, l, _64_373, quals, _64_376, _64_378) -> begin
(

let qopt = (FStar_Util.find_map quals (fun _64_11 -> (match (_64_11) with
| FStar_Syntax_Syntax.RecordConstructor (_64_383, fs) -> begin
Some (FStar_Syntax_Syntax.Record_ctor (((l), (fs))))
end
| _64_388 -> begin
None
end)))
in (match (qopt) with
| None -> begin
Some (FStar_Syntax_Syntax.Data_ctor)
end
| x -> begin
x
end))
end
| FStar_Syntax_Syntax.Sig_declare_typ (_64_393, _64_395, _64_397, quals, _64_400) -> begin
None
end
| _64_404 -> begin
None
end))


let lb_fv : FStar_Syntax_Syntax.letbinding Prims.list  ->  FStar_Ident.lident  ->  FStar_Syntax_Syntax.fv = (fun lbs lid -> (let _161_464 = (FStar_Util.find_map lbs (fun lb -> (

let fv = (FStar_Util.right lb.FStar_Syntax_Syntax.lbname)
in if (FStar_Syntax_Syntax.fv_eq_lid fv lid) then begin
Some (fv)
end else begin
None
end)))
in (FStar_All.pipe_right _161_464 FStar_Util.must)))


let ns_of_lid_equals : FStar_Ident.lident  ->  FStar_Ident.lident  ->  Prims.bool = (fun lid ns -> (((FStar_List.length lid.FStar_Ident.ns) = (FStar_List.length (FStar_Ident.ids_of_lid ns))) && (let _161_469 = (FStar_Ident.lid_of_ids lid.FStar_Ident.ns)
in (FStar_Ident.lid_equals _161_469 ns))))


let try_lookup_name : Prims.bool  ->  Prims.bool  ->  env  ->  FStar_Ident.lident  ->  foundname Prims.option = (fun any_val exclude_interf env lid -> (

let occurrence_range = (FStar_Ident.range_of_lid lid)
in (

let k_global_def = (fun source_lid _64_16 -> (match (_64_16) with
| (_64_420, true) when exclude_interf -> begin
None
end
| (se, _64_425) -> begin
(match (se) with
| FStar_Syntax_Syntax.Sig_inductive_typ (_64_428) -> begin
(let _161_484 = (let _161_483 = (let _161_482 = (FStar_Syntax_Syntax.fvar source_lid FStar_Syntax_Syntax.Delta_constant None)
in ((_161_482), (false)))
in Term_name (_161_483))
in Some (_161_484))
end
| FStar_Syntax_Syntax.Sig_datacon (_64_431) -> begin
(let _161_488 = (let _161_487 = (let _161_486 = (let _161_485 = (fv_qual_of_se se)
in (FStar_Syntax_Syntax.fvar source_lid FStar_Syntax_Syntax.Delta_constant _161_485))
in ((_161_486), (false)))
in Term_name (_161_487))
in Some (_161_488))
end
| FStar_Syntax_Syntax.Sig_let ((_64_434, lbs), _64_438, _64_440, _64_442) -> begin
(

let fv = (lb_fv lbs source_lid)
in (let _161_491 = (let _161_490 = (let _161_489 = (FStar_Syntax_Syntax.fvar source_lid fv.FStar_Syntax_Syntax.fv_delta fv.FStar_Syntax_Syntax.fv_qual)
in ((_161_489), (false)))
in Term_name (_161_490))
in Some (_161_491)))
end
| FStar_Syntax_Syntax.Sig_declare_typ (lid, _64_448, _64_450, quals, _64_453) -> begin
if (any_val || (FStar_All.pipe_right quals (FStar_Util.for_some (fun _64_13 -> (match (_64_13) with
| FStar_Syntax_Syntax.Assumption -> begin
true
end
| _64_459 -> begin
false
end))))) then begin
(

let lid = (FStar_Ident.set_lid_range lid (FStar_Ident.range_of_lid source_lid))
in (

let dd = if ((FStar_Syntax_Util.is_primop_lid lid) || ((ns_of_lid_equals lid FStar_Syntax_Const.prims_lid) && (FStar_All.pipe_right quals (FStar_Util.for_some (fun _64_14 -> (match (_64_14) with
| (FStar_Syntax_Syntax.Projector (_)) | (FStar_Syntax_Syntax.Discriminator (_)) -> begin
true
end
| _64_469 -> begin
false
end)))))) then begin
FStar_Syntax_Syntax.Delta_equational
end else begin
FStar_Syntax_Syntax.Delta_constant
end
in (match ((FStar_Util.find_map quals (fun _64_15 -> (match (_64_15) with
| FStar_Syntax_Syntax.Reflectable (refl_monad) -> begin
Some (refl_monad)
end
| _64_475 -> begin
None
end)))) with
| Some (refl_monad) -> begin
(

let refl_const = (FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_constant (FStar_Const.Const_reflect (refl_monad))) None occurrence_range)
in Some (Term_name (((refl_const), (false)))))
end
| _64_480 -> begin
(let _161_498 = (let _161_497 = (let _161_496 = (let _161_495 = (fv_qual_of_se se)
in (FStar_Syntax_Syntax.fvar lid dd _161_495))
in ((_161_496), (false)))
in Term_name (_161_497))
in Some (_161_498))
end)))
end else begin
None
end
end
| (FStar_Syntax_Syntax.Sig_new_effect_for_free (ne, _)) | (FStar_Syntax_Syntax.Sig_new_effect (ne, _)) -> begin
Some (Eff_name (((se), ((FStar_Ident.set_lid_range ne.FStar_Syntax_Syntax.mname (FStar_Ident.range_of_lid source_lid))))))
end
| FStar_Syntax_Syntax.Sig_effect_abbrev (_64_491) -> begin
Some (Eff_name (((se), (source_lid))))
end
| _64_494 -> begin
None
end)
end))
in (

let k_local_binding = (fun r -> (let _161_502 = (let _161_501 = (found_local_binding (FStar_Ident.range_of_lid lid) r)
in Term_name (_161_501))
in Some (_161_502)))
in (

let k_rec_binding = (fun _64_501 -> (match (_64_501) with
| (id, l, dd) -> begin
(let _161_507 = (let _161_506 = (let _161_505 = (FStar_Syntax_Syntax.fvar (FStar_Ident.set_lid_range l (FStar_Ident.range_of_lid lid)) dd None)
in ((_161_505), (false)))
in Term_name (_161_506))
in Some (_161_507))
end))
in (

let found_unmangled = (match (lid.FStar_Ident.ns) with
| [] -> begin
(match ((unmangleOpName lid.FStar_Ident.ident)) with
| Some (f) -> begin
Some (Term_name (f))
end
| _64_506 -> begin
None
end)
end
| _64_508 -> begin
None
end)
in (match (found_unmangled) with
| None -> begin
(resolve_in_open_namespaces' env lid k_local_binding k_rec_binding k_global_def)
end
| x -> begin
x
end)))))))


let try_lookup_effect_name' : Prims.bool  ->  env  ->  FStar_Ident.lident  ->  (FStar_Syntax_Syntax.sigelt * FStar_Ident.lident) Prims.option = (fun exclude_interf env lid -> (match ((try_lookup_name true exclude_interf env lid)) with
| Some (Eff_name (o, l)) -> begin
Some (((o), (l)))
end
| _64_521 -> begin
None
end))


let try_lookup_effect_name : env  ->  FStar_Ident.lident  ->  FStar_Ident.lident Prims.option = (fun env l -> (match ((try_lookup_effect_name' (not (env.iface)) env l)) with
| Some (o, l) -> begin
Some (l)
end
| _64_529 -> begin
None
end))


let try_lookup_effect_name_and_attributes : env  ->  FStar_Ident.lident  ->  (FStar_Ident.lident * FStar_Syntax_Syntax.cflags Prims.list) Prims.option = (fun env l -> (match ((try_lookup_effect_name' (not (env.iface)) env l)) with
| Some (FStar_Syntax_Syntax.Sig_new_effect (ne, _64_534), l) -> begin
Some (((l), (ne.FStar_Syntax_Syntax.cattributes)))
end
| Some (FStar_Syntax_Syntax.Sig_new_effect_for_free (ne, _64_542), l) -> begin
Some (((l), (ne.FStar_Syntax_Syntax.cattributes)))
end
| Some (FStar_Syntax_Syntax.Sig_effect_abbrev (_64_549, _64_551, _64_553, _64_555, _64_557, cattributes, _64_560), l) -> begin
Some (((l), (cattributes)))
end
| _64_567 -> begin
None
end))


let try_lookup_effect_defn : env  ->  FStar_Ident.lident  ->  FStar_Syntax_Syntax.eff_decl Prims.option = (fun env l -> (match ((try_lookup_effect_name' (not (env.iface)) env l)) with
| Some (FStar_Syntax_Syntax.Sig_new_effect (ne, _64_572), _64_576) -> begin
Some (ne)
end
| Some (FStar_Syntax_Syntax.Sig_new_effect_for_free (ne, _64_581), _64_585) -> begin
Some (ne)
end
| _64_589 -> begin
None
end))


let is_effect_name : env  ->  FStar_Ident.lident  ->  Prims.bool = (fun env lid -> (match ((try_lookup_effect_name env lid)) with
| None -> begin
false
end
| Some (_64_594) -> begin
true
end))


let lookup_letbinding_quals : env  ->  FStar_Ident.lident  ->  FStar_Syntax_Syntax.qualifier Prims.list = (fun env lid -> (

let k_global_def = (fun lid _64_17 -> (match (_64_17) with
| (FStar_Syntax_Syntax.Sig_declare_typ (lid, _64_603, _64_605, quals, _64_608), _64_612) -> begin
Some (quals)
end
| _64_615 -> begin
None
end))
in (match ((resolve_in_open_namespaces' env lid (fun _64_618 -> None) (fun _64_616 -> None) k_global_def)) with
| Some (quals) -> begin
quals
end
| _64_623 -> begin
[]
end)))


let try_lookup_module : env  ->  Prims.string Prims.list  ->  FStar_Syntax_Syntax.modul Prims.option = (fun env path -> (match ((FStar_List.tryFind (fun _64_628 -> (match (_64_628) with
| (mlid, modul) -> begin
((FStar_Ident.path_of_lid mlid) = path)
end)) env.modules)) with
| Some (_64_630, modul) -> begin
Some (modul)
end
| None -> begin
None
end))


let try_lookup_let : env  ->  FStar_Ident.lident  ->  FStar_Syntax_Syntax.term Prims.option = (fun env lid -> (

let k_global_def = (fun lid _64_18 -> (match (_64_18) with
| (FStar_Syntax_Syntax.Sig_let ((_64_641, lbs), _64_645, _64_647, _64_649), _64_653) -> begin
(

let fv = (lb_fv lbs lid)
in (let _161_553 = (FStar_Syntax_Syntax.fvar lid fv.FStar_Syntax_Syntax.fv_delta fv.FStar_Syntax_Syntax.fv_qual)
in Some (_161_553)))
end
| _64_657 -> begin
None
end))
in (resolve_in_open_namespaces' env lid (fun _64_660 -> None) (fun _64_658 -> None) k_global_def)))


let try_lookup_definition : env  ->  FStar_Ident.lident  ->  FStar_Syntax_Syntax.term Prims.option = (fun env lid -> (

let k_global_def = (fun lid _64_19 -> (match (_64_19) with
| (FStar_Syntax_Syntax.Sig_let (lbs, _64_669, _64_671, _64_673), _64_677) -> begin
(FStar_Util.find_map (Prims.snd lbs) (fun lb -> (match (lb.FStar_Syntax_Syntax.lbname) with
| FStar_Util.Inr (fv) when (FStar_Syntax_Syntax.fv_eq_lid fv lid) -> begin
Some (lb.FStar_Syntax_Syntax.lbdef)
end
| _64_683 -> begin
None
end)))
end
| _64_685 -> begin
None
end))
in (resolve_in_open_namespaces' env lid (fun _64_688 -> None) (fun _64_686 -> None) k_global_def)))


let try_lookup_lid' : Prims.bool  ->  Prims.bool  ->  env  ->  FStar_Ident.lident  ->  (FStar_Syntax_Syntax.term * Prims.bool) Prims.option = (fun any_val exclude_interf env lid -> (match ((try_lookup_name any_val exclude_interf env lid)) with
| Some (Term_name (e, mut)) -> begin
Some (((e), (mut)))
end
| _64_700 -> begin
None
end))


let try_lookup_lid : env  ->  FStar_Ident.lident  ->  (FStar_Syntax_Syntax.term * Prims.bool) Prims.option = (fun env l -> (try_lookup_lid' env.iface false env l))


let try_lookup_datacon : env  ->  FStar_Ident.lident  ->  FStar_Syntax_Syntax.fv Prims.option = (fun env lid -> (

let k_global_def = (fun lid _64_21 -> (match (_64_21) with
| (FStar_Syntax_Syntax.Sig_declare_typ (_64_709, _64_711, _64_713, quals, _64_716), _64_720) -> begin
if (FStar_All.pipe_right quals (FStar_Util.for_some (fun _64_20 -> (match (_64_20) with
| FStar_Syntax_Syntax.Assumption -> begin
true
end
| _64_725 -> begin
false
end)))) then begin
(let _161_588 = (FStar_Syntax_Syntax.lid_as_fv lid FStar_Syntax_Syntax.Delta_constant None)
in Some (_161_588))
end else begin
None
end
end
| (FStar_Syntax_Syntax.Sig_datacon (_64_727), _64_730) -> begin
(let _161_589 = (FStar_Syntax_Syntax.lid_as_fv lid FStar_Syntax_Syntax.Delta_constant (Some (FStar_Syntax_Syntax.Data_ctor)))
in Some (_161_589))
end
| _64_733 -> begin
None
end))
in (resolve_in_open_namespaces' env lid (fun _64_736 -> None) (fun _64_734 -> None) k_global_def)))


let find_all_datacons : env  ->  FStar_Ident.lident  ->  FStar_Ident.lident Prims.list Prims.option = (fun env lid -> (

let k_global_def = (fun lid _64_22 -> (match (_64_22) with
| (FStar_Syntax_Syntax.Sig_inductive_typ (_64_744, _64_746, _64_748, _64_750, _64_752, datas, _64_755, _64_757), _64_761) -> begin
Some (datas)
end
| _64_764 -> begin
None
end))
in (resolve_in_open_namespaces' env lid (fun _64_767 -> None) (fun _64_765 -> None) k_global_def)))


let record_cache_aux_with_filter : (((Prims.unit  ->  Prims.unit) * (Prims.unit  ->  Prims.unit) * (Prims.unit  ->  record_or_dc Prims.list) * (record_or_dc  ->  Prims.unit) * (Prims.unit  ->  Prims.unit)) * (Prims.unit  ->  Prims.unit)) = (

let record_cache = (FStar_Util.mk_ref (([])::[]))
in (

let push = (fun _64_771 -> (match (()) with
| () -> begin
(let _161_624 = (let _161_623 = (let _161_621 = (FStar_ST.read record_cache)
in (FStar_List.hd _161_621))
in (let _161_622 = (FStar_ST.read record_cache)
in (_161_623)::_161_622))
in (FStar_ST.op_Colon_Equals record_cache _161_624))
end))
in (

let pop = (fun _64_773 -> (match (()) with
| () -> begin
(let _161_628 = (let _161_627 = (FStar_ST.read record_cache)
in (FStar_List.tl _161_627))
in (FStar_ST.op_Colon_Equals record_cache _161_628))
end))
in (

let peek = (fun _64_775 -> (match (()) with
| () -> begin
(let _161_631 = (FStar_ST.read record_cache)
in (FStar_List.hd _161_631))
end))
in (

let insert = (fun r -> (let _161_638 = (let _161_637 = (let _161_634 = (peek ())
in (r)::_161_634)
in (let _161_636 = (let _161_635 = (FStar_ST.read record_cache)
in (FStar_List.tl _161_635))
in (_161_637)::_161_636))
in (FStar_ST.op_Colon_Equals record_cache _161_638)))
in (

let commit = (fun _64_779 -> (match (()) with
| () -> begin
(match ((FStar_ST.read record_cache)) with
| (hd)::(_64_782)::tl -> begin
(FStar_ST.op_Colon_Equals record_cache ((hd)::tl))
end
| _64_787 -> begin
(FStar_All.failwith "Impossible")
end)
end))
in (

let filter = (fun _64_789 -> (match (()) with
| () -> begin
(

let rc = (peek ())
in (

let _64_791 = (pop ())
in (match (()) with
| () -> begin
(

let filtered = (FStar_List.filter (fun r -> (not (r.is_private_or_abstract))) rc)
in (let _161_645 = (let _161_644 = (FStar_ST.read record_cache)
in (filtered)::_161_644)
in (FStar_ST.op_Colon_Equals record_cache _161_645)))
end)))
end))
in (

let aux = ((push), (pop), (peek), (insert), (commit))
in ((aux), (filter))))))))))


let record_cache_aux : ((Prims.unit  ->  Prims.unit) * (Prims.unit  ->  Prims.unit) * (Prims.unit  ->  record_or_dc Prims.list) * (record_or_dc  ->  Prims.unit) * (Prims.unit  ->  Prims.unit)) = (

let _64_798 = record_cache_aux_with_filter
in (match (_64_798) with
| (aux, _64_797) -> begin
aux
end))


let filter_record_cache : Prims.unit  ->  Prims.unit = (

let _64_802 = record_cache_aux_with_filter
in (match (_64_802) with
| (_64_800, filter) -> begin
filter
end))


let push_record_cache : Prims.unit  ->  Prims.unit = (

let _64_812 = record_cache_aux
in (match (_64_812) with
| (push, _64_805, _64_807, _64_809, _64_811) -> begin
push
end))


let pop_record_cache : Prims.unit  ->  Prims.unit = (

let _64_822 = record_cache_aux
in (match (_64_822) with
| (_64_814, pop, _64_817, _64_819, _64_821) -> begin
pop
end))


let peek_record_cache : Prims.unit  ->  record_or_dc Prims.list = (

let _64_832 = record_cache_aux
in (match (_64_832) with
| (_64_824, _64_826, peek, _64_829, _64_831) -> begin
peek
end))


let insert_record_cache : record_or_dc  ->  Prims.unit = (

let _64_842 = record_cache_aux
in (match (_64_842) with
| (_64_834, _64_836, _64_838, insert, _64_841) -> begin
insert
end))


let commit_record_cache : Prims.unit  ->  Prims.unit = (

let _64_852 = record_cache_aux
in (match (_64_852) with
| (_64_844, _64_846, _64_848, _64_850, commit) -> begin
commit
end))


let extract_record : env  ->  scope_mod Prims.list FStar_ST.ref  ->  FStar_Syntax_Syntax.sigelt  ->  Prims.unit = (fun e new_globs _64_26 -> (match (_64_26) with
| FStar_Syntax_Syntax.Sig_bundle (sigs, _64_858, _64_860, _64_862) -> begin
(

let is_rec = (FStar_Util.for_some (fun _64_23 -> (match (_64_23) with
| (FStar_Syntax_Syntax.RecordType (_)) | (FStar_Syntax_Syntax.RecordConstructor (_)) -> begin
true
end
| _64_873 -> begin
false
end)))
in (

let find_dc = (fun dc -> (FStar_All.pipe_right sigs (FStar_Util.find_opt (fun _64_24 -> (match (_64_24) with
| FStar_Syntax_Syntax.Sig_datacon (lid, _64_880, _64_882, _64_884, _64_886, _64_888, _64_890, _64_892) -> begin
(FStar_Ident.lid_equals dc lid)
end
| _64_896 -> begin
false
end)))))
in (FStar_All.pipe_right sigs (FStar_List.iter (fun _64_25 -> (match (_64_25) with
| FStar_Syntax_Syntax.Sig_inductive_typ (typename, univs, parms, _64_902, _64_904, (dc)::[], tags, _64_909) -> begin
(match ((let _161_847 = (find_dc dc)
in (FStar_All.pipe_left FStar_Util.must _161_847))) with
| FStar_Syntax_Syntax.Sig_datacon (constrname, _64_914, t, _64_917, _64_919, _64_921, _64_923, _64_925) -> begin
(

let _64_931 = (FStar_Syntax_Util.arrow_formals t)
in (match (_64_931) with
| (formals, _64_930) -> begin
(

let is_rec = (is_rec tags)
in (

let formals' = (FStar_All.pipe_right formals (FStar_List.collect (fun _64_935 -> (match (_64_935) with
| (x, q) -> begin
if ((FStar_Syntax_Syntax.is_null_bv x) || (is_rec && (FStar_Syntax_Syntax.is_implicit q))) then begin
[]
end else begin
(((x), (q)))::[]
end
end))))
in (

let fields' = (FStar_All.pipe_right formals' (FStar_List.map (fun _64_939 -> (match (_64_939) with
| (x, q) -> begin
(let _161_850 = if is_rec then begin
(FStar_Syntax_Util.unmangle_field_name x.FStar_Syntax_Syntax.ppname)
end else begin
x.FStar_Syntax_Syntax.ppname
end
in ((_161_850), (x.FStar_Syntax_Syntax.sort)))
end))))
in (

let fields = fields'
in (

let record = {typename = typename; constrname = constrname.FStar_Ident.ident; parms = parms; fields = fields; is_private_or_abstract = ((FStar_List.contains FStar_Syntax_Syntax.Private tags) || (FStar_List.contains FStar_Syntax_Syntax.Abstract tags)); is_record = is_rec}
in (

let _64_943 = (let _161_852 = (let _161_851 = (FStar_ST.read new_globs)
in (Record_or_dc (record))::_161_851)
in (FStar_ST.op_Colon_Equals new_globs _161_852))
in (match (()) with
| () -> begin
(

let _64_957 = (

let add_field = (fun _64_948 -> (match (_64_948) with
| (id, _64_947) -> begin
(

let modul = (let _161_855 = (FStar_Ident.lid_of_ids constrname.FStar_Ident.ns)
in _161_855.FStar_Ident.str)
in (match ((get_exported_id_set e modul)) with
| Some (my_ex) -> begin
(

let my_exported_ids = (my_ex Exported_id_field)
in (

let _64_953 = (let _161_858 = (let _161_857 = (FStar_ST.read my_exported_ids)
in (FStar_Util.set_add id.FStar_Ident.idText _161_857))
in (FStar_ST.op_Colon_Equals my_exported_ids _161_858))
in (match (()) with
| () -> begin
(

let projname = (let _161_860 = (let _161_859 = (FStar_Syntax_Util.mk_field_projector_name_from_ident constrname id)
in _161_859.FStar_Ident.ident)
in _161_860.FStar_Ident.idText)
in (

let _64_955 = (let _161_862 = (let _161_861 = (FStar_ST.read my_exported_ids)
in (FStar_Util.set_add projname _161_861))
in (FStar_ST.op_Colon_Equals my_exported_ids _161_862))
in ()))
end)))
end
| None -> begin
()
end))
end))
in (FStar_List.iter add_field fields'))
in (match (()) with
| () -> begin
(insert_record_cache record)
end))
end)))))))
end))
end
| _64_959 -> begin
()
end)
end
| _64_961 -> begin
()
end))))))
end
| _64_963 -> begin
()
end))


let try_lookup_record_or_dc_by_field_name : env  ->  FStar_Ident.lident  ->  record_or_dc Prims.option = (fun env fieldname -> (

let find_in_cache = (fun fieldname -> (

let _64_970 = ((fieldname.FStar_Ident.ns), (fieldname.FStar_Ident.ident))
in (match (_64_970) with
| (ns, id) -> begin
(let _161_873 = (peek_record_cache ())
in (FStar_Util.find_map _161_873 (fun record -> (let _161_872 = (find_in_record ns id record (fun r -> Cont_ok (r)))
in (option_of_cont (fun _64_973 -> None) _161_872)))))
end)))
in (resolve_in_open_namespaces'' env fieldname Exported_id_field (fun _64_982 -> Cont_ignore) (fun _64_980 -> Cont_ignore) (fun r -> Cont_ok (r)) (fun fn -> (let _161_878 = (find_in_cache fn)
in (cont_of_option Cont_ignore _161_878))) (fun k _64_976 -> k))))


let try_lookup_record_by_field_name : env  ->  FStar_Ident.lident  ->  record_or_dc Prims.option = (fun env fieldname -> (match ((try_lookup_record_or_dc_by_field_name env fieldname)) with
| Some (r) when r.is_record -> begin
Some (r)
end
| _64_989 -> begin
None
end))


let belongs_to_record : env  ->  FStar_Ident.lident  ->  record_or_dc  ->  Prims.bool = (fun env lid record -> (match ((try_lookup_record_by_field_name env lid)) with
| Some (record') when ((let _161_891 = (FStar_Ident.path_of_ns record.typename.FStar_Ident.ns)
in (FStar_Ident.text_of_path _161_891)) = (let _161_892 = (FStar_Ident.path_of_ns record'.typename.FStar_Ident.ns)
in (FStar_Ident.text_of_path _161_892))) -> begin
(match ((find_in_record record.typename.FStar_Ident.ns lid.FStar_Ident.ident record (fun _64_995 -> Cont_ok (())))) with
| Cont_ok (_64_998) -> begin
true
end
| _64_1001 -> begin
false
end)
end
| _64_1003 -> begin
false
end))


let try_lookup_dc_by_field_name : env  ->  FStar_Ident.lident  ->  (FStar_Ident.lident * Prims.bool) Prims.option = (fun env fieldname -> (match ((try_lookup_record_or_dc_by_field_name env fieldname)) with
| Some (r) -> begin
(let _161_900 = (let _161_899 = (let _161_898 = (FStar_Ident.lid_of_ids (FStar_List.append r.typename.FStar_Ident.ns ((r.constrname)::[])))
in (FStar_Ident.set_lid_range _161_898 (FStar_Ident.range_of_lid fieldname)))
in ((_161_899), (r.is_record)))
in Some (_161_900))
end
| _64_1009 -> begin
None
end))


let string_set_ref_new : Prims.unit  ->  Prims.string FStar_Util.set FStar_ST.ref = (fun _64_1010 -> (match (()) with
| () -> begin
(let _161_903 = (FStar_Util.new_set FStar_Util.compare FStar_Util.hashcode)
in (FStar_Util.mk_ref _161_903))
end))


let exported_id_set_new : Prims.unit  ->  exported_id_kind  ->  Prims.string FStar_Util.set FStar_ST.ref = (fun _64_1011 -> (match (()) with
| () -> begin
(

let term_type_set = (string_set_ref_new ())
in (

let field_set = (string_set_ref_new ())
in (fun _64_27 -> (match (_64_27) with
| Exported_id_term_type -> begin
term_type_set
end
| Exported_id_field -> begin
field_set
end))))
end))


let empty_include_smap : FStar_Ident.lident Prims.list FStar_ST.ref FStar_Util.smap = (new_sigmap ())


let empty_exported_id_smap : exported_id_set FStar_Util.smap = (new_sigmap ())


let unique : Prims.bool  ->  Prims.bool  ->  env  ->  FStar_Ident.lident  ->  Prims.bool = (fun any_val exclude_if env lid -> (

let filter_scope_mods = (fun _64_28 -> (match (_64_28) with
| Rec_binding (_64_1023) -> begin
true
end
| _64_1026 -> begin
false
end))
in (

let this_env = (

let _64_1028 = env
in (let _161_923 = (FStar_List.filter filter_scope_mods env.scope_mods)
in {curmodule = _64_1028.curmodule; curmonad = _64_1028.curmonad; modules = _64_1028.modules; scope_mods = _161_923; exported_ids = empty_exported_id_smap; trans_exported_ids = _64_1028.trans_exported_ids; includes = empty_include_smap; sigaccum = _64_1028.sigaccum; sigmap = _64_1028.sigmap; default_result_effect = _64_1028.default_result_effect; iface = _64_1028.iface; admitted_iface = _64_1028.admitted_iface; expect_typ = _64_1028.expect_typ}))
in (match ((try_lookup_lid' any_val exclude_if this_env lid)) with
| None -> begin
true
end
| Some (_64_1033) -> begin
false
end))))


let push_scope_mod : env  ->  scope_mod  ->  env = (fun env scope_mod -> (

let _64_1037 = env
in {curmodule = _64_1037.curmodule; curmonad = _64_1037.curmonad; modules = _64_1037.modules; scope_mods = (scope_mod)::env.scope_mods; exported_ids = _64_1037.exported_ids; trans_exported_ids = _64_1037.trans_exported_ids; includes = _64_1037.includes; sigaccum = _64_1037.sigaccum; sigmap = _64_1037.sigmap; default_result_effect = _64_1037.default_result_effect; iface = _64_1037.iface; admitted_iface = _64_1037.admitted_iface; expect_typ = _64_1037.expect_typ}))


let push_bv' : env  ->  FStar_Ident.ident  ->  Prims.bool  ->  (env * FStar_Syntax_Syntax.bv) = (fun env x is_mutable -> (

let bv = (FStar_Syntax_Syntax.gen_bv x.FStar_Ident.idText (Some (x.FStar_Ident.idRange)) FStar_Syntax_Syntax.tun)
in (((push_scope_mod env (Local_binding (((x), (bv), (is_mutable)))))), (bv))))


let push_bv_mutable : env  ->  FStar_Ident.ident  ->  (env * FStar_Syntax_Syntax.bv) = (fun env x -> (push_bv' env x true))


let push_bv : env  ->  FStar_Ident.ident  ->  (env * FStar_Syntax_Syntax.bv) = (fun env x -> (push_bv' env x false))


let push_top_level_rec_binding : env  ->  FStar_Ident.ident  ->  FStar_Syntax_Syntax.delta_depth  ->  env = (fun env x dd -> (

let l = (qualify env x)
in if (unique false true env l) then begin
(push_scope_mod env (Rec_binding (((x), (l), (dd)))))
end else begin
(Prims.raise (FStar_Syntax_Syntax.Error ((((Prims.strcat "Duplicate top-level names " l.FStar_Ident.str)), ((FStar_Ident.range_of_lid l))))))
end))


let push_sigelt : env  ->  FStar_Syntax_Syntax.sigelt  ->  env = (fun env s -> (

let err = (fun l -> (

let sopt = (FStar_Util.smap_try_find (sigmap env) l.FStar_Ident.str)
in (

let r = (match (sopt) with
| Some (se, _64_1058) -> begin
(match ((let _161_954 = (FStar_Syntax_Util.lids_of_sigelt se)
in (FStar_Util.find_opt (FStar_Ident.lid_equals l) _161_954))) with
| Some (l) -> begin
(FStar_All.pipe_left FStar_Range.string_of_range (FStar_Ident.range_of_lid l))
end
| None -> begin
"<unknown>"
end)
end
| None -> begin
"<unknown>"
end)
in (let _161_957 = (let _161_956 = (let _161_955 = (FStar_Util.format2 "Duplicate top-level names [%s]; previously declared at %s" (FStar_Ident.text_of_lid l) r)
in ((_161_955), ((FStar_Ident.range_of_lid l))))
in FStar_Syntax_Syntax.Error (_161_956))
in (Prims.raise _161_957)))))
in (

let globals = (FStar_ST.alloc env.scope_mods)
in (

let env = (

let _64_1077 = (match (s) with
| FStar_Syntax_Syntax.Sig_let (_64_1068) -> begin
((false), (true))
end
| FStar_Syntax_Syntax.Sig_bundle (_64_1071) -> begin
((true), (true))
end
| _64_1074 -> begin
((false), (false))
end)
in (match (_64_1077) with
| (any_val, exclude_if) -> begin
(

let lids = (FStar_Syntax_Util.lids_of_sigelt s)
in (match ((FStar_Util.find_map lids (fun l -> if (not ((unique any_val exclude_if env l))) then begin
Some (l)
end else begin
None
end))) with
| None -> begin
(

let _64_1081 = (extract_record env globals s)
in (

let _64_1083 = env
in {curmodule = _64_1083.curmodule; curmonad = _64_1083.curmonad; modules = _64_1083.modules; scope_mods = _64_1083.scope_mods; exported_ids = _64_1083.exported_ids; trans_exported_ids = _64_1083.trans_exported_ids; includes = _64_1083.includes; sigaccum = (s)::env.sigaccum; sigmap = _64_1083.sigmap; default_result_effect = _64_1083.default_result_effect; iface = _64_1083.iface; admitted_iface = _64_1083.admitted_iface; expect_typ = _64_1083.expect_typ}))
end
| Some (l) -> begin
(err l)
end))
end))
in (

let env = (

let _64_1088 = env
in (let _161_959 = (FStar_ST.read globals)
in {curmodule = _64_1088.curmodule; curmonad = _64_1088.curmonad; modules = _64_1088.modules; scope_mods = _161_959; exported_ids = _64_1088.exported_ids; trans_exported_ids = _64_1088.trans_exported_ids; includes = _64_1088.includes; sigaccum = _64_1088.sigaccum; sigmap = _64_1088.sigmap; default_result_effect = _64_1088.default_result_effect; iface = _64_1088.iface; admitted_iface = _64_1088.admitted_iface; expect_typ = _64_1088.expect_typ}))
in (

let _64_1105 = (match (s) with
| FStar_Syntax_Syntax.Sig_bundle (ses, _64_1093, _64_1095, _64_1097) -> begin
(let _161_962 = (FStar_List.map (fun se -> (let _161_961 = (FStar_Syntax_Util.lids_of_sigelt se)
in ((_161_961), (se)))) ses)
in ((env), (_161_962)))
end
| _64_1102 -> begin
(let _161_965 = (let _161_964 = (let _161_963 = (FStar_Syntax_Util.lids_of_sigelt s)
in ((_161_963), (s)))
in (_161_964)::[])
in ((env), (_161_965)))
end)
in (match (_64_1105) with
| (env, lss) -> begin
(

let _64_1117 = (FStar_All.pipe_right lss (FStar_List.iter (fun _64_1108 -> (match (_64_1108) with
| (lids, se) -> begin
(FStar_All.pipe_right lids (FStar_List.iter (fun lid -> (

let _64_1110 = (let _161_969 = (let _161_968 = (FStar_ST.read globals)
in (Top_level_def (lid.FStar_Ident.ident))::_161_968)
in (FStar_ST.op_Colon_Equals globals _161_969))
in (match (()) with
| () -> begin
(

let modul = (let _161_970 = (FStar_Ident.lid_of_ids lid.FStar_Ident.ns)
in _161_970.FStar_Ident.str)
in (

let _64_1116 = (match ((get_exported_id_set env modul)) with
| Some (f) -> begin
(

let my_exported_ids = (f Exported_id_term_type)
in (let _161_973 = (let _161_972 = (FStar_ST.read my_exported_ids)
in (FStar_Util.set_add lid.FStar_Ident.ident.FStar_Ident.idText _161_972))
in (FStar_ST.op_Colon_Equals my_exported_ids _161_973)))
end
| None -> begin
()
end)
in (match (()) with
| () -> begin
(FStar_Util.smap_add (sigmap env) lid.FStar_Ident.str ((se), ((env.iface && (not (env.admitted_iface))))))
end)))
end)))))
end))))
in (

let env = (

let _64_1119 = env
in (let _161_974 = (FStar_ST.read globals)
in {curmodule = _64_1119.curmodule; curmonad = _64_1119.curmonad; modules = _64_1119.modules; scope_mods = _161_974; exported_ids = _64_1119.exported_ids; trans_exported_ids = _64_1119.trans_exported_ids; includes = _64_1119.includes; sigaccum = _64_1119.sigaccum; sigmap = _64_1119.sigmap; default_result_effect = _64_1119.default_result_effect; iface = _64_1119.iface; admitted_iface = _64_1119.admitted_iface; expect_typ = _64_1119.expect_typ}))
in env))
end)))))))


let push_namespace : env  ->  FStar_Ident.lident  ->  env = (fun env ns -> (

let _64_1134 = (match ((resolve_module_name env ns false)) with
| None -> begin
(

let modules = env.modules
in if (FStar_All.pipe_right modules (FStar_Util.for_some (fun _64_1129 -> (match (_64_1129) with
| (m, _64_1128) -> begin
(FStar_Util.starts_with (Prims.strcat (FStar_Ident.text_of_lid m) ".") (Prims.strcat (FStar_Ident.text_of_lid ns) "."))
end)))) then begin
((ns), (Open_namespace))
end else begin
(let _161_982 = (let _161_981 = (let _161_980 = (FStar_Util.format1 "Namespace %s cannot be found" (FStar_Ident.text_of_lid ns))
in ((_161_980), ((FStar_Ident.range_of_lid ns))))
in FStar_Syntax_Syntax.Error (_161_981))
in (Prims.raise _161_982))
end)
end
| Some (ns') -> begin
((ns'), (Open_module))
end)
in (match (_64_1134) with
| (ns', kd) -> begin
(push_scope_mod env (Open_module_or_namespace (((ns'), (kd)))))
end)))


let push_include : env  ->  FStar_Ident.lident  ->  env = (fun env ns -> (match ((resolve_module_name env ns false)) with
| Some (ns) -> begin
(

let env = (push_scope_mod env (Open_module_or_namespace (((ns), (Open_module)))))
in (

let curmod = (let _161_987 = (current_module env)
in _161_987.FStar_Ident.str)
in (

let _64_1144 = (match ((FStar_Util.smap_try_find env.includes curmod)) with
| None -> begin
()
end
| Some (incl) -> begin
(let _161_989 = (let _161_988 = (FStar_ST.read incl)
in (ns)::_161_988)
in (FStar_ST.op_Colon_Equals incl _161_989))
end)
in (match (()) with
| () -> begin
(match ((get_trans_exported_id_set env ns.FStar_Ident.str)) with
| Some (ns_trans_exports) -> begin
(

let _64_1161 = (match ((let _161_996 = (get_exported_id_set env curmod)
in (let _161_995 = (get_trans_exported_id_set env curmod)
in ((_161_996), (_161_995))))) with
| (Some (cur_exports), Some (cur_trans_exports)) -> begin
(

let update_exports = (fun k -> (

let ns_ex = (let _161_1001 = (ns_trans_exports k)
in (FStar_ST.read _161_1001))
in (

let ex = (cur_exports k)
in (

let _64_1156 = (let _161_1003 = (let _161_1002 = (FStar_ST.read ex)
in (FStar_Util.set_difference _161_1002 ns_ex))
in (FStar_ST.op_Colon_Equals ex _161_1003))
in (match (()) with
| () -> begin
(

let trans_ex = (cur_trans_exports k)
in (

let _64_1158 = (let _161_1005 = (let _161_1004 = (FStar_ST.read ex)
in (FStar_Util.set_union _161_1004 ns_ex))
in (FStar_ST.op_Colon_Equals trans_ex _161_1005))
in ()))
end)))))
in (FStar_List.iter update_exports all_exported_id_kinds))
end
| _64_1160 -> begin
()
end)
in (match (()) with
| () -> begin
env
end))
end
| None -> begin
(let _161_1014 = (let _161_1013 = (let _161_1012 = (FStar_Util.format1 "include: Module %s was not prepared" ns.FStar_Ident.str)
in ((_161_1012), ((FStar_Ident.range_of_lid ns))))
in FStar_Syntax_Syntax.Error (_161_1013))
in (Prims.raise _161_1014))
end)
end))))
end
| _64_1164 -> begin
(let _161_1017 = (let _161_1016 = (let _161_1015 = (FStar_Util.format1 "include: Module %s cannot be found" ns.FStar_Ident.str)
in ((_161_1015), ((FStar_Ident.range_of_lid ns))))
in FStar_Syntax_Syntax.Error (_161_1016))
in (Prims.raise _161_1017))
end))


let push_module_abbrev : env  ->  FStar_Ident.ident  ->  FStar_Ident.lident  ->  env = (fun env x l -> if (module_is_defined env l) then begin
(push_scope_mod env (Module_abbrev (((x), (l)))))
end else begin
(let _161_1026 = (let _161_1025 = (let _161_1024 = (FStar_Util.format1 "Module %s cannot be found" (FStar_Ident.text_of_lid l))
in ((_161_1024), ((FStar_Ident.range_of_lid l))))
in FStar_Syntax_Syntax.Error (_161_1025))
in (Prims.raise _161_1026))
end)


let check_admits : env  ->  Prims.unit = (fun env -> (FStar_All.pipe_right env.sigaccum (FStar_List.iter (fun se -> (match (se) with
| FStar_Syntax_Syntax.Sig_declare_typ (l, u, t, quals, r) -> begin
(match ((try_lookup_lid env l)) with
| None -> begin
(

let _64_1178 = (let _161_1032 = (let _161_1031 = (FStar_Range.string_of_range (FStar_Ident.range_of_lid l))
in (let _161_1030 = (FStar_Syntax_Print.lid_to_string l)
in (FStar_Util.format2 "%s: Warning: Admitting %s without a definition\n" _161_1031 _161_1030)))
in (FStar_Util.print_string _161_1032))
in (FStar_Util.smap_add (sigmap env) l.FStar_Ident.str ((FStar_Syntax_Syntax.Sig_declare_typ (((l), (u), (t), ((FStar_Syntax_Syntax.Assumption)::quals), (r)))), (false))))
end
| Some (_64_1181) -> begin
()
end)
end
| _64_1184 -> begin
()
end)))))


let finish : env  ->  FStar_Syntax_Syntax.modul  ->  env = (fun env modul -> (

let _64_1244 = (FStar_All.pipe_right modul.FStar_Syntax_Syntax.declarations (FStar_List.iter (fun _64_30 -> (match (_64_30) with
| FStar_Syntax_Syntax.Sig_bundle (ses, quals, _64_1191, _64_1193) -> begin
if ((FStar_List.contains FStar_Syntax_Syntax.Private quals) || (FStar_List.contains FStar_Syntax_Syntax.Abstract quals)) then begin
(FStar_All.pipe_right ses (FStar_List.iter (fun _64_29 -> (match (_64_29) with
| FStar_Syntax_Syntax.Sig_datacon (lid, _64_1199, _64_1201, _64_1203, _64_1205, _64_1207, _64_1209, _64_1211) -> begin
(FStar_Util.smap_remove (sigmap env) lid.FStar_Ident.str)
end
| _64_1215 -> begin
()
end))))
end else begin
()
end
end
| FStar_Syntax_Syntax.Sig_declare_typ (lid, _64_1218, _64_1220, quals, _64_1223) -> begin
if (FStar_List.contains FStar_Syntax_Syntax.Private quals) then begin
(FStar_Util.smap_remove (sigmap env) lid.FStar_Ident.str)
end else begin
()
end
end
| FStar_Syntax_Syntax.Sig_let ((_64_1227, lbs), r, _64_1232, quals) -> begin
(

let _64_1237 = if ((FStar_List.contains FStar_Syntax_Syntax.Private quals) || (FStar_List.contains FStar_Syntax_Syntax.Abstract quals)) then begin
(FStar_All.pipe_right lbs (FStar_List.iter (fun lb -> (let _161_1043 = (let _161_1042 = (let _161_1041 = (let _161_1040 = (FStar_Util.right lb.FStar_Syntax_Syntax.lbname)
in _161_1040.FStar_Syntax_Syntax.fv_name)
in _161_1041.FStar_Syntax_Syntax.v)
in _161_1042.FStar_Ident.str)
in (FStar_Util.smap_remove (sigmap env) _161_1043)))))
end else begin
()
end
in if ((FStar_List.contains FStar_Syntax_Syntax.Abstract quals) && (not ((FStar_List.contains FStar_Syntax_Syntax.Private quals)))) then begin
(FStar_All.pipe_right lbs (FStar_List.iter (fun lb -> (

let lid = (let _161_1046 = (let _161_1045 = (FStar_Util.right lb.FStar_Syntax_Syntax.lbname)
in _161_1045.FStar_Syntax_Syntax.fv_name)
in _161_1046.FStar_Syntax_Syntax.v)
in (

let decl = FStar_Syntax_Syntax.Sig_declare_typ (((lid), (lb.FStar_Syntax_Syntax.lbunivs), (lb.FStar_Syntax_Syntax.lbtyp), ((FStar_Syntax_Syntax.Assumption)::quals), (r)))
in (FStar_Util.smap_add (sigmap env) lid.FStar_Ident.str ((decl), (false))))))))
end else begin
()
end)
end
| _64_1243 -> begin
()
end))))
in (

let curmod = (let _161_1047 = (current_module env)
in _161_1047.FStar_Ident.str)
in (

let _64_1258 = (match ((let _161_1053 = (get_exported_id_set env curmod)
in (let _161_1052 = (get_trans_exported_id_set env curmod)
in ((_161_1053), (_161_1052))))) with
| (Some (cur_ex), Some (cur_trans_ex)) -> begin
(

let update_exports = (fun eikind -> (

let cur_ex_set = (let _161_1058 = (cur_ex eikind)
in (FStar_ST.read _161_1058))
in (

let cur_trans_ex_set_ref = (cur_trans_ex eikind)
in (let _161_1060 = (let _161_1059 = (FStar_ST.read cur_trans_ex_set_ref)
in (FStar_Util.set_union cur_ex_set _161_1059))
in (FStar_ST.op_Colon_Equals cur_trans_ex_set_ref _161_1060)))))
in (FStar_List.iter update_exports all_exported_id_kinds))
end
| _64_1257 -> begin
()
end)
in (match (()) with
| () -> begin
(

let _64_1259 = (filter_record_cache ())
in (match (()) with
| () -> begin
(

let _64_1260 = env
in {curmodule = None; curmonad = _64_1260.curmonad; modules = (((modul.FStar_Syntax_Syntax.name), (modul)))::env.modules; scope_mods = []; exported_ids = _64_1260.exported_ids; trans_exported_ids = _64_1260.trans_exported_ids; includes = _64_1260.includes; sigaccum = []; sigmap = _64_1260.sigmap; default_result_effect = _64_1260.default_result_effect; iface = _64_1260.iface; admitted_iface = _64_1260.admitted_iface; expect_typ = _64_1260.expect_typ})
end))
end)))))


type env_stack_ops =
{push : env  ->  env; mark : env  ->  env; reset_mark : env  ->  env; commit_mark : env  ->  env; pop : env  ->  env}


let is_Mkenv_stack_ops : env_stack_ops  ->  Prims.bool = (Obj.magic ((fun _ -> (FStar_All.failwith "Not yet implemented:is_Mkenv_stack_ops"))))


let stack_ops : env_stack_ops = (

let stack = (FStar_Util.mk_ref [])
in (

let push = (fun env -> (

let _64_1271 = (push_record_cache ())
in (

let _64_1273 = (let _161_1116 = (let _161_1115 = (FStar_ST.read stack)
in (env)::_161_1115)
in (FStar_ST.op_Colon_Equals stack _161_1116))
in (

let _64_1275 = env
in (let _161_1117 = (FStar_Util.smap_copy (sigmap env))
in {curmodule = _64_1275.curmodule; curmonad = _64_1275.curmonad; modules = _64_1275.modules; scope_mods = _64_1275.scope_mods; exported_ids = _64_1275.exported_ids; trans_exported_ids = _64_1275.trans_exported_ids; includes = _64_1275.includes; sigaccum = _64_1275.sigaccum; sigmap = _161_1117; default_result_effect = _64_1275.default_result_effect; iface = _64_1275.iface; admitted_iface = _64_1275.admitted_iface; expect_typ = _64_1275.expect_typ})))))
in (

let pop = (fun env -> (match ((FStar_ST.read stack)) with
| (env)::tl -> begin
(

let _64_1282 = (pop_record_cache ())
in (

let _64_1284 = (FStar_ST.op_Colon_Equals stack tl)
in env))
end
| _64_1287 -> begin
(FStar_All.failwith "Impossible: Too many pops")
end))
in (

let commit_mark = (fun env -> (

let _64_1290 = (commit_record_cache ())
in (match ((FStar_ST.read stack)) with
| (_64_1294)::tl -> begin
(

let _64_1296 = (FStar_ST.op_Colon_Equals stack tl)
in env)
end
| _64_1299 -> begin
(FStar_All.failwith "Impossible: Too many pops")
end)))
in {push = push; mark = push; reset_mark = pop; commit_mark = commit_mark; pop = pop}))))


let push : env  ->  env = (fun env -> (stack_ops.push env))


let mark : env  ->  env = (fun env -> (stack_ops.mark env))


let reset_mark : env  ->  env = (fun env -> (stack_ops.reset_mark env))


let commit_mark : env  ->  env = (fun env -> (stack_ops.commit_mark env))


let pop : env  ->  env = (fun env -> (stack_ops.pop env))


let export_interface : FStar_Ident.lident  ->  env  ->  env = (fun m env -> (

let sigelt_in_m = (fun se -> (match ((FStar_Syntax_Util.lids_of_sigelt se)) with
| (l)::_64_1310 -> begin
(l.FStar_Ident.nsstr = m.FStar_Ident.str)
end
| _64_1314 -> begin
false
end))
in (

let sm = (sigmap env)
in (

let env = (pop env)
in (

let keys = (FStar_Util.smap_keys sm)
in (

let sm' = (sigmap env)
in (

let _64_1338 = (FStar_All.pipe_right keys (FStar_List.iter (fun k -> (match ((FStar_Util.smap_try_find sm' k)) with
| Some (se, true) when (sigelt_in_m se) -> begin
(

let _64_1324 = (FStar_Util.smap_remove sm' k)
in (

let se = (match (se) with
| FStar_Syntax_Syntax.Sig_declare_typ (l, u, t, q, r) -> begin
FStar_Syntax_Syntax.Sig_declare_typ (((l), (u), (t), ((FStar_Syntax_Syntax.Assumption)::q), (r)))
end
| _64_1334 -> begin
se
end)
in (FStar_Util.smap_add sm' k ((se), (false)))))
end
| _64_1337 -> begin
()
end))))
in env)))))))


let finish_module_or_interface : env  ->  FStar_Syntax_Syntax.modul  ->  env = (fun env modul -> (

let _64_1342 = if (not (modul.FStar_Syntax_Syntax.is_interface)) then begin
(check_admits env)
end else begin
()
end
in (finish env modul)))


let prepare_module_or_interface : Prims.bool  ->  Prims.bool  ->  env  ->  FStar_Ident.lident  ->  (env * Prims.bool) = (fun intf admitted env mname -> (

let prep = (fun env -> (

let open_ns = if (FStar_Ident.lid_equals mname FStar_Syntax_Const.prims_lid) then begin
[]
end else begin
if (FStar_Util.starts_with "FStar." (FStar_Ident.text_of_lid mname)) then begin
(FStar_Syntax_Const.prims_lid)::(FStar_Syntax_Const.fstar_ns_lid)::[]
end else begin
(FStar_Syntax_Const.prims_lid)::(FStar_Syntax_Const.st_lid)::(FStar_Syntax_Const.all_lid)::(FStar_Syntax_Const.fstar_ns_lid)::[]
end
end
in (

let open_ns = if ((FStar_List.length mname.FStar_Ident.ns) <> (Prims.parse_int "0")) then begin
(

let ns = (FStar_Ident.lid_of_ids mname.FStar_Ident.ns)
in (ns)::open_ns)
end else begin
open_ns
end
in (

let _64_1353 = (let _161_1153 = (exported_id_set_new ())
in (FStar_Util.smap_add env.exported_ids mname.FStar_Ident.str _161_1153))
in (match (()) with
| () -> begin
(

let _64_1354 = (let _161_1154 = (exported_id_set_new ())
in (FStar_Util.smap_add env.trans_exported_ids mname.FStar_Ident.str _161_1154))
in (match (()) with
| () -> begin
(

let _64_1355 = (let _161_1155 = (FStar_Util.mk_ref [])
in (FStar_Util.smap_add env.includes mname.FStar_Ident.str _161_1155))
in (match (()) with
| () -> begin
(

let _64_1356 = env
in (let _161_1157 = (FStar_List.map (fun lid -> Open_module_or_namespace (((lid), (Open_namespace)))) open_ns)
in {curmodule = Some (mname); curmonad = _64_1356.curmonad; modules = _64_1356.modules; scope_mods = _161_1157; exported_ids = _64_1356.exported_ids; trans_exported_ids = _64_1356.trans_exported_ids; includes = _64_1356.includes; sigaccum = _64_1356.sigaccum; sigmap = env.sigmap; default_result_effect = if ((FStar_Ident.lid_equals mname FStar_Syntax_Const.all_lid) || (has_all_in_scope env)) then begin
FStar_Syntax_Const.effect_ML_lid
end else begin
FStar_Syntax_Const.effect_Tot_lid
end; iface = intf; admitted_iface = admitted; expect_typ = _64_1356.expect_typ}))
end))
end))
end)))))
in (match ((FStar_All.pipe_right env.modules (FStar_Util.find_opt (fun _64_1362 -> (match (_64_1362) with
| (l, _64_1361) -> begin
(FStar_Ident.lid_equals l mname)
end))))) with
| None -> begin
(let _161_1159 = (prep env)
in ((_161_1159), (false)))
end
| Some (_64_1365, m) -> begin
(

let _64_1369 = if ((not (m.FStar_Syntax_Syntax.is_interface)) || intf) then begin
(let _161_1162 = (let _161_1161 = (let _161_1160 = (FStar_Util.format1 "Duplicate module or interface name: %s" mname.FStar_Ident.str)
in ((_161_1160), ((FStar_Ident.range_of_lid mname))))
in FStar_Syntax_Syntax.Error (_161_1161))
in (Prims.raise _161_1162))
end else begin
()
end
in (let _161_1164 = (let _161_1163 = (push env)
in (prep _161_1163))
in ((_161_1164), (true))))
end)))


let enter_monad_scope : env  ->  FStar_Ident.ident  ->  env = (fun env mname -> (match (env.curmonad) with
| Some (mname') -> begin
(Prims.raise (FStar_Syntax_Syntax.Error ((((Prims.strcat "Trying to define monad " (Prims.strcat mname.FStar_Ident.idText (Prims.strcat ", but already in monad scope " mname'.FStar_Ident.idText)))), (mname.FStar_Ident.idRange)))))
end
| None -> begin
(

let _64_1376 = env
in {curmodule = _64_1376.curmodule; curmonad = Some (mname); modules = _64_1376.modules; scope_mods = _64_1376.scope_mods; exported_ids = _64_1376.exported_ids; trans_exported_ids = _64_1376.trans_exported_ids; includes = _64_1376.includes; sigaccum = _64_1376.sigaccum; sigmap = _64_1376.sigmap; default_result_effect = _64_1376.default_result_effect; iface = _64_1376.iface; admitted_iface = _64_1376.admitted_iface; expect_typ = _64_1376.expect_typ})
end))


let fail_or = (fun env lookup lid -> (match ((lookup lid)) with
| None -> begin
(

let opened_modules = (FStar_List.map (fun _64_1385 -> (match (_64_1385) with
| (lid, _64_1384) -> begin
(FStar_Ident.text_of_lid lid)
end)) env.modules)
in (

let msg = (FStar_Util.format1 "Identifier not found: [%s]" (FStar_Ident.text_of_lid lid))
in (

let msg = if ((FStar_List.length lid.FStar_Ident.ns) = (Prims.parse_int "0")) then begin
msg
end else begin
(

let modul = (let _161_1176 = (FStar_Ident.lid_of_ids lid.FStar_Ident.ns)
in (FStar_Ident.set_lid_range _161_1176 (FStar_Ident.range_of_lid lid)))
in (match ((resolve_module_name env modul true)) with
| None -> begin
(

let opened_modules = (FStar_String.concat ", " opened_modules)
in (FStar_Util.format3 "%s\nModule %s does not belong to the list of modules in scope, namely %s" msg modul.FStar_Ident.str opened_modules))
end
| Some (modul') when (not ((FStar_List.existsb (fun m -> (m = modul'.FStar_Ident.str)) opened_modules))) -> begin
(

let opened_modules = (FStar_String.concat ", " opened_modules)
in (FStar_Util.format4 "%s\nModule %s resolved into %s, which does not belong to the list of modules in scope, namely %s" msg modul.FStar_Ident.str modul'.FStar_Ident.str opened_modules))
end
| Some (modul') -> begin
(FStar_Util.format4 "%s\nModule %s resolved into %s, definition %s not found" msg modul.FStar_Ident.str modul'.FStar_Ident.str lid.FStar_Ident.ident.FStar_Ident.idText)
end))
end
in (Prims.raise (FStar_Syntax_Syntax.Error (((msg), ((FStar_Ident.range_of_lid lid)))))))))
end
| Some (r) -> begin
r
end))


let fail_or2 = (fun lookup id -> (match ((lookup id)) with
| None -> begin
(Prims.raise (FStar_Syntax_Syntax.Error ((((Prims.strcat "Identifier not found [" (Prims.strcat id.FStar_Ident.idText "]"))), (id.FStar_Ident.idRange)))))
end
| Some (r) -> begin
r
end))




