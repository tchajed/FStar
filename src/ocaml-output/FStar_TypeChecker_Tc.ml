
open Prims

let set_hint_correlator : FStar_TypeChecker_Env.env  ->  FStar_Syntax_Syntax.sigelt  ->  FStar_TypeChecker_Env.env = (fun env se -> (

let uu____7 = (FStar_Options.reuse_hint_for ())
in (match (uu____7) with
| Some (l) -> begin
(

let lid = (let _0_237 = (FStar_TypeChecker_Env.current_module env)
in (FStar_Ident.lid_add_suffix _0_237 l))
in (

let uu___81_11 = env
in {FStar_TypeChecker_Env.solver = uu___81_11.FStar_TypeChecker_Env.solver; FStar_TypeChecker_Env.range = uu___81_11.FStar_TypeChecker_Env.range; FStar_TypeChecker_Env.curmodule = uu___81_11.FStar_TypeChecker_Env.curmodule; FStar_TypeChecker_Env.gamma = uu___81_11.FStar_TypeChecker_Env.gamma; FStar_TypeChecker_Env.gamma_cache = uu___81_11.FStar_TypeChecker_Env.gamma_cache; FStar_TypeChecker_Env.modules = uu___81_11.FStar_TypeChecker_Env.modules; FStar_TypeChecker_Env.expected_typ = uu___81_11.FStar_TypeChecker_Env.expected_typ; FStar_TypeChecker_Env.sigtab = uu___81_11.FStar_TypeChecker_Env.sigtab; FStar_TypeChecker_Env.is_pattern = uu___81_11.FStar_TypeChecker_Env.is_pattern; FStar_TypeChecker_Env.instantiate_imp = uu___81_11.FStar_TypeChecker_Env.instantiate_imp; FStar_TypeChecker_Env.effects = uu___81_11.FStar_TypeChecker_Env.effects; FStar_TypeChecker_Env.generalize = uu___81_11.FStar_TypeChecker_Env.generalize; FStar_TypeChecker_Env.letrecs = uu___81_11.FStar_TypeChecker_Env.letrecs; FStar_TypeChecker_Env.top_level = uu___81_11.FStar_TypeChecker_Env.top_level; FStar_TypeChecker_Env.check_uvars = uu___81_11.FStar_TypeChecker_Env.check_uvars; FStar_TypeChecker_Env.use_eq = uu___81_11.FStar_TypeChecker_Env.use_eq; FStar_TypeChecker_Env.is_iface = uu___81_11.FStar_TypeChecker_Env.is_iface; FStar_TypeChecker_Env.admit = uu___81_11.FStar_TypeChecker_Env.admit; FStar_TypeChecker_Env.lax = uu___81_11.FStar_TypeChecker_Env.lax; FStar_TypeChecker_Env.lax_universes = uu___81_11.FStar_TypeChecker_Env.lax_universes; FStar_TypeChecker_Env.type_of = uu___81_11.FStar_TypeChecker_Env.type_of; FStar_TypeChecker_Env.universe_of = uu___81_11.FStar_TypeChecker_Env.universe_of; FStar_TypeChecker_Env.use_bv_sorts = uu___81_11.FStar_TypeChecker_Env.use_bv_sorts; FStar_TypeChecker_Env.qname_and_index = Some (((lid), ((Prims.parse_int "0"))))}))
end
| None -> begin
(

let lids = (FStar_Syntax_Util.lids_of_sigelt se)
in (

let lid = (match (lids) with
| [] -> begin
(let _0_240 = (FStar_TypeChecker_Env.current_module env)
in (let _0_239 = (let _0_238 = (FStar_Syntax_Syntax.next_id ())
in (FStar_All.pipe_right _0_238 FStar_Util.string_of_int))
in (FStar_Ident.lid_add_suffix _0_240 _0_239)))
end
| (l)::uu____18 -> begin
l
end)
in (

let uu___82_20 = env
in {FStar_TypeChecker_Env.solver = uu___82_20.FStar_TypeChecker_Env.solver; FStar_TypeChecker_Env.range = uu___82_20.FStar_TypeChecker_Env.range; FStar_TypeChecker_Env.curmodule = uu___82_20.FStar_TypeChecker_Env.curmodule; FStar_TypeChecker_Env.gamma = uu___82_20.FStar_TypeChecker_Env.gamma; FStar_TypeChecker_Env.gamma_cache = uu___82_20.FStar_TypeChecker_Env.gamma_cache; FStar_TypeChecker_Env.modules = uu___82_20.FStar_TypeChecker_Env.modules; FStar_TypeChecker_Env.expected_typ = uu___82_20.FStar_TypeChecker_Env.expected_typ; FStar_TypeChecker_Env.sigtab = uu___82_20.FStar_TypeChecker_Env.sigtab; FStar_TypeChecker_Env.is_pattern = uu___82_20.FStar_TypeChecker_Env.is_pattern; FStar_TypeChecker_Env.instantiate_imp = uu___82_20.FStar_TypeChecker_Env.instantiate_imp; FStar_TypeChecker_Env.effects = uu___82_20.FStar_TypeChecker_Env.effects; FStar_TypeChecker_Env.generalize = uu___82_20.FStar_TypeChecker_Env.generalize; FStar_TypeChecker_Env.letrecs = uu___82_20.FStar_TypeChecker_Env.letrecs; FStar_TypeChecker_Env.top_level = uu___82_20.FStar_TypeChecker_Env.top_level; FStar_TypeChecker_Env.check_uvars = uu___82_20.FStar_TypeChecker_Env.check_uvars; FStar_TypeChecker_Env.use_eq = uu___82_20.FStar_TypeChecker_Env.use_eq; FStar_TypeChecker_Env.is_iface = uu___82_20.FStar_TypeChecker_Env.is_iface; FStar_TypeChecker_Env.admit = uu___82_20.FStar_TypeChecker_Env.admit; FStar_TypeChecker_Env.lax = uu___82_20.FStar_TypeChecker_Env.lax; FStar_TypeChecker_Env.lax_universes = uu___82_20.FStar_TypeChecker_Env.lax_universes; FStar_TypeChecker_Env.type_of = uu___82_20.FStar_TypeChecker_Env.type_of; FStar_TypeChecker_Env.universe_of = uu___82_20.FStar_TypeChecker_Env.universe_of; FStar_TypeChecker_Env.use_bv_sorts = uu___82_20.FStar_TypeChecker_Env.use_bv_sorts; FStar_TypeChecker_Env.qname_and_index = Some (((lid), ((Prims.parse_int "0"))))})))
end)))


let log : FStar_TypeChecker_Env.env  ->  Prims.bool = (fun env -> ((FStar_Options.log_types ()) && (not ((let _0_241 = (FStar_TypeChecker_Env.current_module env)
in (FStar_Ident.lid_equals FStar_Syntax_Const.prims_lid _0_241))))))


let tc_check_trivial_guard : FStar_TypeChecker_Env.env  ->  FStar_Syntax_Syntax.term  ->  FStar_Syntax_Syntax.typ  ->  FStar_Syntax_Syntax.term = (fun env t k -> (

let uu____35 = (FStar_TypeChecker_TcTerm.tc_check_tot_or_gtot_term env t k)
in (match (uu____35) with
| (t, c, g) -> begin
((FStar_ST.write t.FStar_Syntax_Syntax.tk (Some (c.FStar_Syntax_Syntax.res_typ.FStar_Syntax_Syntax.n)));
(FStar_TypeChecker_Rel.force_trivial_guard env g);
t;
)
end)))


let recheck_debug : Prims.string  ->  FStar_TypeChecker_Env.env  ->  FStar_Syntax_Syntax.term  ->  FStar_Syntax_Syntax.term = (fun s env t -> ((

let uu____57 = (FStar_TypeChecker_Env.debug env (FStar_Options.Other ("ED")))
in (match (uu____57) with
| true -> begin
(let _0_242 = (FStar_Syntax_Print.term_to_string t)
in (FStar_Util.print2 "Term has been %s-transformed to:\n%s\n----------\n" s _0_242))
end
| uu____58 -> begin
()
end));
(

let uu____59 = (FStar_TypeChecker_TcTerm.tc_term env t)
in (match (uu____59) with
| (t', uu____64, uu____65) -> begin
((

let uu____67 = (FStar_TypeChecker_Env.debug env (FStar_Options.Other ("ED")))
in (match (uu____67) with
| true -> begin
(let _0_243 = (FStar_Syntax_Print.term_to_string t')
in (FStar_Util.print1 "Re-checked; got:\n%s\n----------\n" _0_243))
end
| uu____68 -> begin
()
end));
t;
)
end));
))


let check_and_gen : FStar_TypeChecker_Env.env  ->  FStar_Syntax_Syntax.term  ->  FStar_Syntax_Syntax.typ  ->  FStar_Syntax_Syntax.tscheme = (fun env t k -> (let _0_244 = (tc_check_trivial_guard env t k)
in (FStar_TypeChecker_Util.generalize_universes env _0_244)))


let check_nogen = (fun env t k -> (

let t = (tc_check_trivial_guard env t k)
in (let _0_245 = (FStar_TypeChecker_Normalize.normalize ((FStar_TypeChecker_Normalize.Beta)::[]) env t)
in (([]), (_0_245)))))


let monad_signature : FStar_TypeChecker_Env.env  ->  FStar_Ident.lident  ->  FStar_Syntax_Syntax.term  ->  (FStar_Syntax_Syntax.bv * (FStar_Syntax_Syntax.term', FStar_Syntax_Syntax.term') FStar_Syntax_Syntax.syntax) = (fun env m s -> (

let fail = (fun uu____120 -> (Prims.raise (FStar_Errors.Error ((let _0_246 = (FStar_TypeChecker_Err.unexpected_signature_for_monad env m s)
in ((_0_246), ((FStar_Ident.range_of_lid m))))))))
in (

let s = (FStar_Syntax_Subst.compress s)
in (match (s.FStar_Syntax_Syntax.n) with
| FStar_Syntax_Syntax.Tm_arrow (bs, c) -> begin
(

let bs = (FStar_Syntax_Subst.open_binders bs)
in (match (bs) with
| ((a, uu____148))::((wp, uu____150))::[] -> begin
((a), (wp.FStar_Syntax_Syntax.sort))
end
| uu____159 -> begin
(fail ())
end))
end
| uu____160 -> begin
(fail ())
end))))


let open_univ_vars : FStar_Syntax_Syntax.univ_names  ->  (FStar_Syntax_Syntax.bv * FStar_Syntax_Syntax.aqual) Prims.list  ->  FStar_Syntax_Syntax.comp  ->  (FStar_Syntax_Syntax.univ_names * (FStar_Syntax_Syntax.bv * FStar_Syntax_Syntax.aqual) Prims.list * FStar_Syntax_Syntax.comp) = (fun uvs binders c -> (match (binders) with
| [] -> begin
(

let uu____190 = (FStar_Syntax_Subst.open_univ_vars_comp uvs c)
in (match (uu____190) with
| (uvs, c) -> begin
((uvs), ([]), (c))
end))
end
| uu____206 -> begin
(

let t' = (FStar_Syntax_Util.arrow binders c)
in (

let uu____213 = (FStar_Syntax_Subst.open_univ_vars uvs t')
in (match (uu____213) with
| (uvs, t') -> begin
(

let uu____224 = (FStar_Syntax_Subst.compress t').FStar_Syntax_Syntax.n
in (match (uu____224) with
| FStar_Syntax_Syntax.Tm_arrow (binders, c) -> begin
((uvs), (binders), (c))
end
| uu____248 -> begin
(failwith "Impossible")
end))
end)))
end))


let rec tc_eff_decl : FStar_TypeChecker_Env.env_t  ->  FStar_Syntax_Syntax.eff_decl  ->  FStar_Syntax_Syntax.eff_decl = (fun env0 ed -> (

let uu____317 = (FStar_Syntax_Subst.open_term' ed.FStar_Syntax_Syntax.binders ed.FStar_Syntax_Syntax.signature)
in (match (uu____317) with
| (effect_params_un, signature_un, opening) -> begin
(

let uu____324 = (FStar_TypeChecker_TcTerm.tc_tparams env0 effect_params_un)
in (match (uu____324) with
| (effect_params, env, uu____330) -> begin
(

let uu____331 = (FStar_TypeChecker_TcTerm.tc_trivial_guard env signature_un)
in (match (uu____331) with
| (signature, uu____335) -> begin
(

let ed = (

let uu___83_337 = ed
in {FStar_Syntax_Syntax.qualifiers = uu___83_337.FStar_Syntax_Syntax.qualifiers; FStar_Syntax_Syntax.cattributes = uu___83_337.FStar_Syntax_Syntax.cattributes; FStar_Syntax_Syntax.mname = uu___83_337.FStar_Syntax_Syntax.mname; FStar_Syntax_Syntax.univs = uu___83_337.FStar_Syntax_Syntax.univs; FStar_Syntax_Syntax.binders = effect_params; FStar_Syntax_Syntax.signature = signature; FStar_Syntax_Syntax.ret_wp = uu___83_337.FStar_Syntax_Syntax.ret_wp; FStar_Syntax_Syntax.bind_wp = uu___83_337.FStar_Syntax_Syntax.bind_wp; FStar_Syntax_Syntax.if_then_else = uu___83_337.FStar_Syntax_Syntax.if_then_else; FStar_Syntax_Syntax.ite_wp = uu___83_337.FStar_Syntax_Syntax.ite_wp; FStar_Syntax_Syntax.stronger = uu___83_337.FStar_Syntax_Syntax.stronger; FStar_Syntax_Syntax.close_wp = uu___83_337.FStar_Syntax_Syntax.close_wp; FStar_Syntax_Syntax.assert_p = uu___83_337.FStar_Syntax_Syntax.assert_p; FStar_Syntax_Syntax.assume_p = uu___83_337.FStar_Syntax_Syntax.assume_p; FStar_Syntax_Syntax.null_wp = uu___83_337.FStar_Syntax_Syntax.null_wp; FStar_Syntax_Syntax.trivial = uu___83_337.FStar_Syntax_Syntax.trivial; FStar_Syntax_Syntax.repr = uu___83_337.FStar_Syntax_Syntax.repr; FStar_Syntax_Syntax.return_repr = uu___83_337.FStar_Syntax_Syntax.return_repr; FStar_Syntax_Syntax.bind_repr = uu___83_337.FStar_Syntax_Syntax.bind_repr; FStar_Syntax_Syntax.actions = uu___83_337.FStar_Syntax_Syntax.actions})
in (

let ed = (match (effect_params) with
| [] -> begin
ed
end
| uu____341 -> begin
(

let op = (fun ts -> (

let t1 = (FStar_Syntax_Subst.subst opening (Prims.snd ts))
in (([]), (t1))))
in (

let uu___84_359 = ed
in (let _0_262 = (op ed.FStar_Syntax_Syntax.ret_wp)
in (let _0_261 = (op ed.FStar_Syntax_Syntax.bind_wp)
in (let _0_260 = (op ed.FStar_Syntax_Syntax.if_then_else)
in (let _0_259 = (op ed.FStar_Syntax_Syntax.ite_wp)
in (let _0_258 = (op ed.FStar_Syntax_Syntax.stronger)
in (let _0_257 = (op ed.FStar_Syntax_Syntax.close_wp)
in (let _0_256 = (op ed.FStar_Syntax_Syntax.assert_p)
in (let _0_255 = (op ed.FStar_Syntax_Syntax.assume_p)
in (let _0_254 = (op ed.FStar_Syntax_Syntax.null_wp)
in (let _0_253 = (op ed.FStar_Syntax_Syntax.trivial)
in (let _0_252 = (Prims.snd (op (([]), (ed.FStar_Syntax_Syntax.repr))))
in (let _0_251 = (op ed.FStar_Syntax_Syntax.return_repr)
in (let _0_250 = (op ed.FStar_Syntax_Syntax.bind_repr)
in (let _0_249 = (FStar_List.map (fun a -> (

let uu___85_363 = a
in (let _0_248 = (Prims.snd (op (([]), (a.FStar_Syntax_Syntax.action_defn))))
in (let _0_247 = (Prims.snd (op (([]), (a.FStar_Syntax_Syntax.action_typ))))
in {FStar_Syntax_Syntax.action_name = uu___85_363.FStar_Syntax_Syntax.action_name; FStar_Syntax_Syntax.action_unqualified_name = uu___85_363.FStar_Syntax_Syntax.action_unqualified_name; FStar_Syntax_Syntax.action_univs = uu___85_363.FStar_Syntax_Syntax.action_univs; FStar_Syntax_Syntax.action_defn = _0_248; FStar_Syntax_Syntax.action_typ = _0_247})))) ed.FStar_Syntax_Syntax.actions)
in {FStar_Syntax_Syntax.qualifiers = uu___84_359.FStar_Syntax_Syntax.qualifiers; FStar_Syntax_Syntax.cattributes = uu___84_359.FStar_Syntax_Syntax.cattributes; FStar_Syntax_Syntax.mname = uu___84_359.FStar_Syntax_Syntax.mname; FStar_Syntax_Syntax.univs = uu___84_359.FStar_Syntax_Syntax.univs; FStar_Syntax_Syntax.binders = uu___84_359.FStar_Syntax_Syntax.binders; FStar_Syntax_Syntax.signature = uu___84_359.FStar_Syntax_Syntax.signature; FStar_Syntax_Syntax.ret_wp = _0_262; FStar_Syntax_Syntax.bind_wp = _0_261; FStar_Syntax_Syntax.if_then_else = _0_260; FStar_Syntax_Syntax.ite_wp = _0_259; FStar_Syntax_Syntax.stronger = _0_258; FStar_Syntax_Syntax.close_wp = _0_257; FStar_Syntax_Syntax.assert_p = _0_256; FStar_Syntax_Syntax.assume_p = _0_255; FStar_Syntax_Syntax.null_wp = _0_254; FStar_Syntax_Syntax.trivial = _0_253; FStar_Syntax_Syntax.repr = _0_252; FStar_Syntax_Syntax.return_repr = _0_251; FStar_Syntax_Syntax.bind_repr = _0_250; FStar_Syntax_Syntax.actions = _0_249}))))))))))))))))
end)
in (

let wp_with_fresh_result_type = (fun env mname signature -> (

let fail = (fun t -> (Prims.raise (FStar_Errors.Error ((let _0_263 = (FStar_TypeChecker_Err.unexpected_signature_for_monad env mname t)
in ((_0_263), ((FStar_Ident.range_of_lid mname))))))))
in (

let uu____394 = (FStar_Syntax_Subst.compress signature).FStar_Syntax_Syntax.n
in (match (uu____394) with
| FStar_Syntax_Syntax.Tm_arrow (bs, c) -> begin
(

let bs = (FStar_Syntax_Subst.open_binders bs)
in (match (bs) with
| ((a, uu____417))::((wp, uu____419))::[] -> begin
((a), (wp.FStar_Syntax_Syntax.sort))
end
| uu____428 -> begin
(fail signature)
end))
end
| uu____429 -> begin
(fail signature)
end))))
in (

let uu____430 = (wp_with_fresh_result_type env ed.FStar_Syntax_Syntax.mname ed.FStar_Syntax_Syntax.signature)
in (match (uu____430) with
| (a, wp_a) -> begin
(

let fresh_effect_signature = (fun uu____448 -> (

let uu____449 = (FStar_TypeChecker_TcTerm.tc_trivial_guard env signature_un)
in (match (uu____449) with
| (signature, uu____457) -> begin
(wp_with_fresh_result_type env ed.FStar_Syntax_Syntax.mname signature)
end)))
in (

let env = (let _0_264 = (FStar_Syntax_Syntax.new_bv None ed.FStar_Syntax_Syntax.signature)
in (FStar_TypeChecker_Env.push_bv env _0_264))
in ((

let uu____460 = (FStar_All.pipe_left (FStar_TypeChecker_Env.debug env0) (FStar_Options.Other ("ED")))
in (match (uu____460) with
| true -> begin
(let _0_269 = (FStar_Syntax_Print.lid_to_string ed.FStar_Syntax_Syntax.mname)
in (let _0_268 = (FStar_Syntax_Print.binders_to_string " " ed.FStar_Syntax_Syntax.binders)
in (let _0_267 = (FStar_Syntax_Print.term_to_string ed.FStar_Syntax_Syntax.signature)
in (let _0_266 = (FStar_Syntax_Print.term_to_string (FStar_Syntax_Syntax.bv_to_name a))
in (let _0_265 = (FStar_Syntax_Print.term_to_string a.FStar_Syntax_Syntax.sort)
in (FStar_Util.print5 "Checking effect signature: %s %s : %s\n(a is %s:%s)\n" _0_269 _0_268 _0_267 _0_266 _0_265))))))
end
| uu____461 -> begin
()
end));
(

let check_and_gen' = (fun env uu____473 k -> (match (uu____473) with
| (uu____478, t) -> begin
(check_and_gen env t k)
end))
in (

let return_wp = (

let expected_k = (let _0_274 = (let _0_272 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_271 = (let _0_270 = (FStar_Syntax_Syntax.null_binder (FStar_Syntax_Syntax.bv_to_name a))
in (_0_270)::[])
in (_0_272)::_0_271))
in (let _0_273 = (FStar_Syntax_Syntax.mk_GTotal wp_a)
in (FStar_Syntax_Util.arrow _0_274 _0_273)))
in (check_and_gen' env ed.FStar_Syntax_Syntax.ret_wp expected_k))
in (

let bind_wp = (

let uu____487 = (fresh_effect_signature ())
in (match (uu____487) with
| (b, wp_b) -> begin
(

let a_wp_b = (let _0_277 = (let _0_275 = (FStar_Syntax_Syntax.null_binder (FStar_Syntax_Syntax.bv_to_name a))
in (_0_275)::[])
in (let _0_276 = (FStar_Syntax_Syntax.mk_Total wp_b)
in (FStar_Syntax_Util.arrow _0_277 _0_276)))
in (

let expected_k = (let _0_288 = (let _0_286 = (FStar_Syntax_Syntax.null_binder FStar_TypeChecker_Common.t_range)
in (let _0_285 = (let _0_284 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_283 = (let _0_282 = (FStar_Syntax_Syntax.mk_binder b)
in (let _0_281 = (let _0_280 = (FStar_Syntax_Syntax.null_binder wp_a)
in (let _0_279 = (let _0_278 = (FStar_Syntax_Syntax.null_binder a_wp_b)
in (_0_278)::[])
in (_0_280)::_0_279))
in (_0_282)::_0_281))
in (_0_284)::_0_283))
in (_0_286)::_0_285))
in (let _0_287 = (FStar_Syntax_Syntax.mk_Total wp_b)
in (FStar_Syntax_Util.arrow _0_288 _0_287)))
in (check_and_gen' env ed.FStar_Syntax_Syntax.bind_wp expected_k)))
end))
in (

let if_then_else = (

let p = (let _0_290 = (let _0_289 = (FStar_Syntax_Util.type_u ())
in (FStar_All.pipe_right _0_289 Prims.fst))
in (FStar_Syntax_Syntax.new_bv (Some ((FStar_Ident.range_of_lid ed.FStar_Syntax_Syntax.mname))) _0_290))
in (

let expected_k = (let _0_299 = (let _0_297 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_296 = (let _0_295 = (FStar_Syntax_Syntax.mk_binder p)
in (let _0_294 = (let _0_293 = (FStar_Syntax_Syntax.null_binder wp_a)
in (let _0_292 = (let _0_291 = (FStar_Syntax_Syntax.null_binder wp_a)
in (_0_291)::[])
in (_0_293)::_0_292))
in (_0_295)::_0_294))
in (_0_297)::_0_296))
in (let _0_298 = (FStar_Syntax_Syntax.mk_Total wp_a)
in (FStar_Syntax_Util.arrow _0_299 _0_298)))
in (check_and_gen' env ed.FStar_Syntax_Syntax.if_then_else expected_k)))
in (

let ite_wp = (

let expected_k = (let _0_304 = (let _0_302 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_301 = (let _0_300 = (FStar_Syntax_Syntax.null_binder wp_a)
in (_0_300)::[])
in (_0_302)::_0_301))
in (let _0_303 = (FStar_Syntax_Syntax.mk_Total wp_a)
in (FStar_Syntax_Util.arrow _0_304 _0_303)))
in (check_and_gen' env ed.FStar_Syntax_Syntax.ite_wp expected_k))
in (

let stronger = (

let uu____516 = (FStar_Syntax_Util.type_u ())
in (match (uu____516) with
| (t, uu____520) -> begin
(

let expected_k = (let _0_311 = (let _0_309 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_308 = (let _0_307 = (FStar_Syntax_Syntax.null_binder wp_a)
in (let _0_306 = (let _0_305 = (FStar_Syntax_Syntax.null_binder wp_a)
in (_0_305)::[])
in (_0_307)::_0_306))
in (_0_309)::_0_308))
in (let _0_310 = (FStar_Syntax_Syntax.mk_Total t)
in (FStar_Syntax_Util.arrow _0_311 _0_310)))
in (check_and_gen' env ed.FStar_Syntax_Syntax.stronger expected_k))
end))
in (

let close_wp = (

let b = (let _0_313 = (let _0_312 = (FStar_Syntax_Util.type_u ())
in (FStar_All.pipe_right _0_312 Prims.fst))
in (FStar_Syntax_Syntax.new_bv (Some ((FStar_Ident.range_of_lid ed.FStar_Syntax_Syntax.mname))) _0_313))
in (

let b_wp_a = (let _0_316 = (let _0_314 = (FStar_Syntax_Syntax.null_binder (FStar_Syntax_Syntax.bv_to_name b))
in (_0_314)::[])
in (let _0_315 = (FStar_Syntax_Syntax.mk_Total wp_a)
in (FStar_Syntax_Util.arrow _0_316 _0_315)))
in (

let expected_k = (let _0_323 = (let _0_321 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_320 = (let _0_319 = (FStar_Syntax_Syntax.mk_binder b)
in (let _0_318 = (let _0_317 = (FStar_Syntax_Syntax.null_binder b_wp_a)
in (_0_317)::[])
in (_0_319)::_0_318))
in (_0_321)::_0_320))
in (let _0_322 = (FStar_Syntax_Syntax.mk_Total wp_a)
in (FStar_Syntax_Util.arrow _0_323 _0_322)))
in (check_and_gen' env ed.FStar_Syntax_Syntax.close_wp expected_k))))
in (

let assert_p = (

let expected_k = (let _0_331 = (let _0_329 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_328 = (let _0_327 = (FStar_Syntax_Syntax.null_binder (let _0_324 = (FStar_Syntax_Util.type_u ())
in (FStar_All.pipe_right _0_324 Prims.fst)))
in (let _0_326 = (let _0_325 = (FStar_Syntax_Syntax.null_binder wp_a)
in (_0_325)::[])
in (_0_327)::_0_326))
in (_0_329)::_0_328))
in (let _0_330 = (FStar_Syntax_Syntax.mk_Total wp_a)
in (FStar_Syntax_Util.arrow _0_331 _0_330)))
in (check_and_gen' env ed.FStar_Syntax_Syntax.assert_p expected_k))
in (

let assume_p = (

let expected_k = (let _0_339 = (let _0_337 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_336 = (let _0_335 = (FStar_Syntax_Syntax.null_binder (let _0_332 = (FStar_Syntax_Util.type_u ())
in (FStar_All.pipe_right _0_332 Prims.fst)))
in (let _0_334 = (let _0_333 = (FStar_Syntax_Syntax.null_binder wp_a)
in (_0_333)::[])
in (_0_335)::_0_334))
in (_0_337)::_0_336))
in (let _0_338 = (FStar_Syntax_Syntax.mk_Total wp_a)
in (FStar_Syntax_Util.arrow _0_339 _0_338)))
in (check_and_gen' env ed.FStar_Syntax_Syntax.assume_p expected_k))
in (

let null_wp = (

let expected_k = (let _0_342 = (let _0_340 = (FStar_Syntax_Syntax.mk_binder a)
in (_0_340)::[])
in (let _0_341 = (FStar_Syntax_Syntax.mk_Total wp_a)
in (FStar_Syntax_Util.arrow _0_342 _0_341)))
in (check_and_gen' env ed.FStar_Syntax_Syntax.null_wp expected_k))
in (

let trivial_wp = (

let uu____551 = (FStar_Syntax_Util.type_u ())
in (match (uu____551) with
| (t, uu____555) -> begin
(

let expected_k = (let _0_347 = (let _0_345 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_344 = (let _0_343 = (FStar_Syntax_Syntax.null_binder wp_a)
in (_0_343)::[])
in (_0_345)::_0_344))
in (let _0_346 = (FStar_Syntax_Syntax.mk_GTotal t)
in (FStar_Syntax_Util.arrow _0_347 _0_346)))
in (check_and_gen' env ed.FStar_Syntax_Syntax.trivial expected_k))
end))
in (

let uu____559 = (

let uu____565 = (FStar_Syntax_Subst.compress ed.FStar_Syntax_Syntax.repr).FStar_Syntax_Syntax.n
in (match (uu____565) with
| FStar_Syntax_Syntax.Tm_unknown -> begin
((ed.FStar_Syntax_Syntax.repr), (ed.FStar_Syntax_Syntax.bind_repr), (ed.FStar_Syntax_Syntax.return_repr), (ed.FStar_Syntax_Syntax.actions))
end
| uu____572 -> begin
(

let repr = (

let uu____574 = (FStar_Syntax_Util.type_u ())
in (match (uu____574) with
| (t, uu____578) -> begin
(

let expected_k = (let _0_352 = (let _0_350 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_349 = (let _0_348 = (FStar_Syntax_Syntax.null_binder wp_a)
in (_0_348)::[])
in (_0_350)::_0_349))
in (let _0_351 = (FStar_Syntax_Syntax.mk_GTotal t)
in (FStar_Syntax_Util.arrow _0_352 _0_351)))
in (tc_check_trivial_guard env ed.FStar_Syntax_Syntax.repr expected_k))
end))
in (

let mk_repr' = (fun t wp -> (

let repr = (FStar_TypeChecker_Normalize.normalize ((FStar_TypeChecker_Normalize.EraseUniverses)::(FStar_TypeChecker_Normalize.AllowUnboundUniverses)::[]) env repr)
in ((FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_app ((let _0_356 = (let _0_355 = (FStar_Syntax_Syntax.as_arg t)
in (let _0_354 = (let _0_353 = (FStar_Syntax_Syntax.as_arg wp)
in (_0_353)::[])
in (_0_355)::_0_354))
in ((repr), (_0_356)))))) None FStar_Range.dummyRange)))
in (

let mk_repr = (fun a wp -> (let _0_357 = (FStar_Syntax_Syntax.bv_to_name a)
in (mk_repr' _0_357 wp)))
in (

let destruct_repr = (fun t -> (

let uu____620 = (FStar_Syntax_Subst.compress t).FStar_Syntax_Syntax.n
in (match (uu____620) with
| FStar_Syntax_Syntax.Tm_app (uu____627, ((t, uu____629))::((wp, uu____631))::[]) -> begin
((t), (wp))
end
| uu____665 -> begin
(failwith "Unexpected repr type")
end)))
in (

let bind_repr = (

let r = (let _0_358 = (FStar_Syntax_Syntax.lid_as_fv FStar_Syntax_Const.range_0 FStar_Syntax_Syntax.Delta_constant None)
in (FStar_All.pipe_right _0_358 FStar_Syntax_Syntax.fv_to_tm))
in (

let uu____674 = (fresh_effect_signature ())
in (match (uu____674) with
| (b, wp_b) -> begin
(

let a_wp_b = (let _0_361 = (let _0_359 = (FStar_Syntax_Syntax.null_binder (FStar_Syntax_Syntax.bv_to_name a))
in (_0_359)::[])
in (let _0_360 = (FStar_Syntax_Syntax.mk_Total wp_b)
in (FStar_Syntax_Util.arrow _0_361 _0_360)))
in (

let wp_f = (FStar_Syntax_Syntax.gen_bv "wp_f" None wp_a)
in (

let wp_g = (FStar_Syntax_Syntax.gen_bv "wp_g" None a_wp_b)
in (

let x_a = (let _0_362 = (FStar_Syntax_Syntax.bv_to_name a)
in (FStar_Syntax_Syntax.gen_bv "x_a" None _0_362))
in (

let wp_g_x = ((let _0_366 = (FStar_Syntax_Syntax.bv_to_name wp_g)
in (let _0_365 = (let _0_364 = (let _0_363 = (FStar_Syntax_Syntax.bv_to_name x_a)
in (FStar_All.pipe_left FStar_Syntax_Syntax.as_arg _0_363))
in (_0_364)::[])
in (FStar_Syntax_Syntax.mk_Tm_app _0_366 _0_365))) None FStar_Range.dummyRange)
in (

let res = (

let wp = ((let _0_378 = (let _0_367 = (FStar_TypeChecker_Env.inst_tscheme bind_wp)
in (FStar_All.pipe_right _0_367 Prims.snd))
in (let _0_377 = (let _0_376 = (let _0_375 = (let _0_374 = (FStar_Syntax_Syntax.bv_to_name a)
in (let _0_373 = (let _0_372 = (FStar_Syntax_Syntax.bv_to_name b)
in (let _0_371 = (let _0_370 = (FStar_Syntax_Syntax.bv_to_name wp_f)
in (let _0_369 = (let _0_368 = (FStar_Syntax_Syntax.bv_to_name wp_g)
in (_0_368)::[])
in (_0_370)::_0_369))
in (_0_372)::_0_371))
in (_0_374)::_0_373))
in (r)::_0_375)
in (FStar_List.map FStar_Syntax_Syntax.as_arg _0_376))
in (FStar_Syntax_Syntax.mk_Tm_app _0_378 _0_377))) None FStar_Range.dummyRange)
in (mk_repr b wp))
in (

let expected_k = (let _0_396 = (let _0_394 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_393 = (let _0_392 = (FStar_Syntax_Syntax.mk_binder b)
in (let _0_391 = (let _0_390 = (FStar_Syntax_Syntax.mk_binder wp_f)
in (let _0_389 = (let _0_388 = (FStar_Syntax_Syntax.null_binder (let _0_379 = (FStar_Syntax_Syntax.bv_to_name wp_f)
in (mk_repr a _0_379)))
in (let _0_387 = (let _0_386 = (FStar_Syntax_Syntax.mk_binder wp_g)
in (let _0_385 = (let _0_384 = (FStar_Syntax_Syntax.null_binder (let _0_383 = (let _0_380 = (FStar_Syntax_Syntax.mk_binder x_a)
in (_0_380)::[])
in (let _0_382 = (let _0_381 = (mk_repr b wp_g_x)
in (FStar_All.pipe_left FStar_Syntax_Syntax.mk_Total _0_381))
in (FStar_Syntax_Util.arrow _0_383 _0_382))))
in (_0_384)::[])
in (_0_386)::_0_385))
in (_0_388)::_0_387))
in (_0_390)::_0_389))
in (_0_392)::_0_391))
in (_0_394)::_0_393))
in (let _0_395 = (FStar_Syntax_Syntax.mk_Total res)
in (FStar_Syntax_Util.arrow _0_396 _0_395)))
in (

let uu____713 = (FStar_TypeChecker_TcTerm.tc_tot_or_gtot_term env expected_k)
in (match (uu____713) with
| (expected_k, uu____718, uu____719) -> begin
(

let env = (FStar_TypeChecker_Env.set_range env (Prims.snd ed.FStar_Syntax_Syntax.bind_repr).FStar_Syntax_Syntax.pos)
in (

let env = (

let uu___86_723 = env
in {FStar_TypeChecker_Env.solver = uu___86_723.FStar_TypeChecker_Env.solver; FStar_TypeChecker_Env.range = uu___86_723.FStar_TypeChecker_Env.range; FStar_TypeChecker_Env.curmodule = uu___86_723.FStar_TypeChecker_Env.curmodule; FStar_TypeChecker_Env.gamma = uu___86_723.FStar_TypeChecker_Env.gamma; FStar_TypeChecker_Env.gamma_cache = uu___86_723.FStar_TypeChecker_Env.gamma_cache; FStar_TypeChecker_Env.modules = uu___86_723.FStar_TypeChecker_Env.modules; FStar_TypeChecker_Env.expected_typ = uu___86_723.FStar_TypeChecker_Env.expected_typ; FStar_TypeChecker_Env.sigtab = uu___86_723.FStar_TypeChecker_Env.sigtab; FStar_TypeChecker_Env.is_pattern = uu___86_723.FStar_TypeChecker_Env.is_pattern; FStar_TypeChecker_Env.instantiate_imp = uu___86_723.FStar_TypeChecker_Env.instantiate_imp; FStar_TypeChecker_Env.effects = uu___86_723.FStar_TypeChecker_Env.effects; FStar_TypeChecker_Env.generalize = uu___86_723.FStar_TypeChecker_Env.generalize; FStar_TypeChecker_Env.letrecs = uu___86_723.FStar_TypeChecker_Env.letrecs; FStar_TypeChecker_Env.top_level = uu___86_723.FStar_TypeChecker_Env.top_level; FStar_TypeChecker_Env.check_uvars = uu___86_723.FStar_TypeChecker_Env.check_uvars; FStar_TypeChecker_Env.use_eq = uu___86_723.FStar_TypeChecker_Env.use_eq; FStar_TypeChecker_Env.is_iface = uu___86_723.FStar_TypeChecker_Env.is_iface; FStar_TypeChecker_Env.admit = uu___86_723.FStar_TypeChecker_Env.admit; FStar_TypeChecker_Env.lax = true; FStar_TypeChecker_Env.lax_universes = uu___86_723.FStar_TypeChecker_Env.lax_universes; FStar_TypeChecker_Env.type_of = uu___86_723.FStar_TypeChecker_Env.type_of; FStar_TypeChecker_Env.universe_of = uu___86_723.FStar_TypeChecker_Env.universe_of; FStar_TypeChecker_Env.use_bv_sorts = uu___86_723.FStar_TypeChecker_Env.use_bv_sorts; FStar_TypeChecker_Env.qname_and_index = uu___86_723.FStar_TypeChecker_Env.qname_and_index})
in (

let br = (check_and_gen' env ed.FStar_Syntax_Syntax.bind_repr expected_k)
in br)))
end)))))))))
end)))
in (

let return_repr = (

let x_a = (let _0_397 = (FStar_Syntax_Syntax.bv_to_name a)
in (FStar_Syntax_Syntax.gen_bv "x_a" None _0_397))
in (

let res = (

let wp = ((let _0_404 = (let _0_398 = (FStar_TypeChecker_Env.inst_tscheme return_wp)
in (FStar_All.pipe_right _0_398 Prims.snd))
in (let _0_403 = (let _0_402 = (let _0_401 = (FStar_Syntax_Syntax.bv_to_name a)
in (let _0_400 = (let _0_399 = (FStar_Syntax_Syntax.bv_to_name x_a)
in (_0_399)::[])
in (_0_401)::_0_400))
in (FStar_List.map FStar_Syntax_Syntax.as_arg _0_402))
in (FStar_Syntax_Syntax.mk_Tm_app _0_404 _0_403))) None FStar_Range.dummyRange)
in (mk_repr a wp))
in (

let expected_k = (let _0_409 = (let _0_407 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_406 = (let _0_405 = (FStar_Syntax_Syntax.mk_binder x_a)
in (_0_405)::[])
in (_0_407)::_0_406))
in (let _0_408 = (FStar_Syntax_Syntax.mk_Total res)
in (FStar_Syntax_Util.arrow _0_409 _0_408)))
in (

let uu____745 = (FStar_TypeChecker_TcTerm.tc_tot_or_gtot_term env expected_k)
in (match (uu____745) with
| (expected_k, uu____753, uu____754) -> begin
(

let env = (FStar_TypeChecker_Env.set_range env (Prims.snd ed.FStar_Syntax_Syntax.return_repr).FStar_Syntax_Syntax.pos)
in (

let uu____757 = (check_and_gen' env ed.FStar_Syntax_Syntax.return_repr expected_k)
in (match (uu____757) with
| (univs, repr) -> begin
(match (univs) with
| [] -> begin
(([]), (repr))
end
| uu____769 -> begin
(Prims.raise (FStar_Errors.Error ((("Unexpected universe-polymorphic return for effect"), (repr.FStar_Syntax_Syntax.pos)))))
end)
end)))
end)))))
in (

let actions = (

let check_action = (fun act -> (

let uu____780 = (FStar_TypeChecker_TcTerm.tc_tot_or_gtot_term env act.FStar_Syntax_Syntax.action_typ)
in (match (uu____780) with
| (act_typ, uu____785, g_t) -> begin
(

let env' = (FStar_TypeChecker_Env.set_expected_typ env act_typ)
in ((

let uu____789 = (FStar_TypeChecker_Env.debug env (FStar_Options.Other ("ED")))
in (match (uu____789) with
| true -> begin
(let _0_411 = (FStar_Syntax_Print.term_to_string act.FStar_Syntax_Syntax.action_defn)
in (let _0_410 = (FStar_Syntax_Print.term_to_string act_typ)
in (FStar_Util.print3 "Checking action %s:\n[definition]: %s\n[cps\'d type]: %s\n" (FStar_Ident.text_of_lid act.FStar_Syntax_Syntax.action_name) _0_411 _0_410)))
end
| uu____790 -> begin
()
end));
(

let uu____791 = (FStar_TypeChecker_TcTerm.tc_tot_or_gtot_term env' act.FStar_Syntax_Syntax.action_defn)
in (match (uu____791) with
| (act_defn, uu____796, g_a) -> begin
(

let act_defn = (FStar_TypeChecker_Normalize.normalize ((FStar_TypeChecker_Normalize.UnfoldUntil (FStar_Syntax_Syntax.Delta_constant))::[]) env act_defn)
in (

let act_typ = (FStar_TypeChecker_Normalize.normalize ((FStar_TypeChecker_Normalize.UnfoldUntil (FStar_Syntax_Syntax.Delta_constant))::(FStar_TypeChecker_Normalize.Eager_unfolding)::(FStar_TypeChecker_Normalize.Beta)::[]) env act_typ)
in (

let uu____800 = (

let act_typ = (FStar_Syntax_Subst.compress act_typ)
in (match (act_typ.FStar_Syntax_Syntax.n) with
| FStar_Syntax_Syntax.Tm_arrow (bs, c) -> begin
(

let uu____818 = (FStar_Syntax_Subst.open_comp bs c)
in (match (uu____818) with
| (bs, uu____824) -> begin
(

let res = (mk_repr' FStar_Syntax_Syntax.tun FStar_Syntax_Syntax.tun)
in (

let k = (let _0_412 = (FStar_Syntax_Syntax.mk_Total res)
in (FStar_Syntax_Util.arrow bs _0_412))
in (

let uu____831 = (FStar_TypeChecker_TcTerm.tc_tot_or_gtot_term env k)
in (match (uu____831) with
| (k, uu____838, g) -> begin
((k), (g))
end))))
end))
end
| uu____840 -> begin
(Prims.raise (FStar_Errors.Error ((let _0_415 = (let _0_414 = (FStar_Syntax_Print.term_to_string act_typ)
in (let _0_413 = (FStar_Syntax_Print.tag_of_term act_typ)
in (FStar_Util.format2 "Actions must have function types (not: %s, a.k.a. %s)" _0_414 _0_413)))
in ((_0_415), (act_defn.FStar_Syntax_Syntax.pos))))))
end))
in (match (uu____800) with
| (expected_k, g_k) -> begin
(

let g = (FStar_TypeChecker_Rel.teq env act_typ expected_k)
in ((let _0_418 = (let _0_417 = (let _0_416 = (FStar_TypeChecker_Rel.conj_guard g_t g)
in (FStar_TypeChecker_Rel.conj_guard g_k _0_416))
in (FStar_TypeChecker_Rel.conj_guard g_a _0_417))
in (FStar_TypeChecker_Rel.force_trivial_guard env _0_418));
(

let act_typ = (

let uu____850 = (FStar_Syntax_Subst.compress expected_k).FStar_Syntax_Syntax.n
in (match (uu____850) with
| FStar_Syntax_Syntax.Tm_arrow (bs, c) -> begin
(

let uu____865 = (FStar_Syntax_Subst.open_comp bs c)
in (match (uu____865) with
| (bs, c) -> begin
(

let uu____872 = (destruct_repr (FStar_Syntax_Util.comp_result c))
in (match (uu____872) with
| (a, wp) -> begin
(

let c = (let _0_422 = (let _0_419 = (env.FStar_TypeChecker_Env.universe_of env a)
in (_0_419)::[])
in (let _0_421 = (let _0_420 = (FStar_Syntax_Syntax.as_arg wp)
in (_0_420)::[])
in {FStar_Syntax_Syntax.comp_univs = _0_422; FStar_Syntax_Syntax.effect_name = ed.FStar_Syntax_Syntax.mname; FStar_Syntax_Syntax.result_typ = a; FStar_Syntax_Syntax.effect_args = _0_421; FStar_Syntax_Syntax.flags = []}))
in (let _0_423 = (FStar_Syntax_Syntax.mk_Comp c)
in (FStar_Syntax_Util.arrow bs _0_423)))
end))
end))
end
| uu____892 -> begin
(failwith "")
end))
in (

let uu____895 = (FStar_TypeChecker_Util.generalize_universes env act_defn)
in (match (uu____895) with
| (univs, act_defn) -> begin
(

let act_typ = (FStar_TypeChecker_Normalize.normalize ((FStar_TypeChecker_Normalize.Beta)::[]) env act_typ)
in (

let uu___87_901 = act
in {FStar_Syntax_Syntax.action_name = uu___87_901.FStar_Syntax_Syntax.action_name; FStar_Syntax_Syntax.action_unqualified_name = uu___87_901.FStar_Syntax_Syntax.action_unqualified_name; FStar_Syntax_Syntax.action_univs = univs; FStar_Syntax_Syntax.action_defn = act_defn; FStar_Syntax_Syntax.action_typ = act_typ}))
end)));
))
end))))
end));
))
end)))
in (FStar_All.pipe_right ed.FStar_Syntax_Syntax.actions (FStar_List.map check_action)))
in ((repr), (bind_repr), (return_repr), (actions)))))))))
end))
in (match (uu____559) with
| (repr, bind_repr, return_repr, actions) -> begin
(

let t = (let _0_424 = (FStar_Syntax_Syntax.mk_Total ed.FStar_Syntax_Syntax.signature)
in (FStar_Syntax_Util.arrow ed.FStar_Syntax_Syntax.binders _0_424))
in (

let uu____917 = (FStar_TypeChecker_Util.generalize_universes env0 t)
in (match (uu____917) with
| (univs, t) -> begin
(

let signature = (

let uu____923 = (let _0_425 = (FStar_Syntax_Subst.compress t).FStar_Syntax_Syntax.n
in ((effect_params), (_0_425)))
in (match (uu____923) with
| ([], uu____926) -> begin
t
end
| (uu____932, FStar_Syntax_Syntax.Tm_arrow (uu____933, c)) -> begin
(FStar_Syntax_Util.comp_result c)
end
| uu____945 -> begin
(failwith "Impossible")
end))
in (

let close = (fun n ts -> (

let ts = (let _0_426 = (FStar_Syntax_Subst.close_tscheme effect_params ts)
in (FStar_Syntax_Subst.close_univ_vars_tscheme univs _0_426))
in (

let m = (FStar_List.length (Prims.fst ts))
in ((

let uu____960 = (((n >= (Prims.parse_int "0")) && (not ((FStar_Syntax_Util.is_unknown (Prims.snd ts))))) && (m <> n))
in (match (uu____960) with
| true -> begin
(

let error = (match ((m < n)) with
| true -> begin
"not universe-polymorphic enough"
end
| uu____968 -> begin
"too universe-polymorphic"
end)
in (failwith (let _0_428 = (FStar_Util.string_of_int n)
in (let _0_427 = (FStar_Syntax_Print.tscheme_to_string ts)
in (FStar_Util.format3 "The effect combinator is %s (n=%s) (%s)" error _0_428 _0_427)))))
end
| uu____969 -> begin
()
end));
ts;
))))
in (

let close_action = (fun act -> (

let uu____974 = (close (~- ((Prims.parse_int "1"))) ((act.FStar_Syntax_Syntax.action_univs), (act.FStar_Syntax_Syntax.action_defn)))
in (match (uu____974) with
| (univs, defn) -> begin
(

let uu____979 = (close (~- ((Prims.parse_int "1"))) ((act.FStar_Syntax_Syntax.action_univs), (act.FStar_Syntax_Syntax.action_typ)))
in (match (uu____979) with
| (univs', typ) -> begin
(

let uu___88_985 = act
in {FStar_Syntax_Syntax.action_name = uu___88_985.FStar_Syntax_Syntax.action_name; FStar_Syntax_Syntax.action_unqualified_name = uu___88_985.FStar_Syntax_Syntax.action_unqualified_name; FStar_Syntax_Syntax.action_univs = univs; FStar_Syntax_Syntax.action_defn = defn; FStar_Syntax_Syntax.action_typ = typ})
end))
end)))
in (

let nunivs = (FStar_List.length univs)
in (

let ed = (

let uu___89_990 = ed
in (let _0_442 = (close (Prims.parse_int "0") return_wp)
in (let _0_441 = (close (Prims.parse_int "1") bind_wp)
in (let _0_440 = (close (Prims.parse_int "0") if_then_else)
in (let _0_439 = (close (Prims.parse_int "0") ite_wp)
in (let _0_438 = (close (Prims.parse_int "0") stronger)
in (let _0_437 = (close (Prims.parse_int "1") close_wp)
in (let _0_436 = (close (Prims.parse_int "0") assert_p)
in (let _0_435 = (close (Prims.parse_int "0") assume_p)
in (let _0_434 = (close (Prims.parse_int "0") null_wp)
in (let _0_433 = (close (Prims.parse_int "0") trivial_wp)
in (let _0_432 = (Prims.snd (close (Prims.parse_int "0") (([]), (repr))))
in (let _0_431 = (close (Prims.parse_int "0") return_repr)
in (let _0_430 = (close (Prims.parse_int "1") bind_repr)
in (let _0_429 = (FStar_List.map close_action actions)
in {FStar_Syntax_Syntax.qualifiers = uu___89_990.FStar_Syntax_Syntax.qualifiers; FStar_Syntax_Syntax.cattributes = uu___89_990.FStar_Syntax_Syntax.cattributes; FStar_Syntax_Syntax.mname = uu___89_990.FStar_Syntax_Syntax.mname; FStar_Syntax_Syntax.univs = univs; FStar_Syntax_Syntax.binders = effect_params; FStar_Syntax_Syntax.signature = signature; FStar_Syntax_Syntax.ret_wp = _0_442; FStar_Syntax_Syntax.bind_wp = _0_441; FStar_Syntax_Syntax.if_then_else = _0_440; FStar_Syntax_Syntax.ite_wp = _0_439; FStar_Syntax_Syntax.stronger = _0_438; FStar_Syntax_Syntax.close_wp = _0_437; FStar_Syntax_Syntax.assert_p = _0_436; FStar_Syntax_Syntax.assume_p = _0_435; FStar_Syntax_Syntax.null_wp = _0_434; FStar_Syntax_Syntax.trivial = _0_433; FStar_Syntax_Syntax.repr = _0_432; FStar_Syntax_Syntax.return_repr = _0_431; FStar_Syntax_Syntax.bind_repr = _0_430; FStar_Syntax_Syntax.actions = _0_429})))))))))))))))
in ((

let uu____994 = ((FStar_TypeChecker_Env.debug env FStar_Options.Low) || (FStar_All.pipe_left (FStar_TypeChecker_Env.debug env) (FStar_Options.Other ("ED"))))
in (match (uu____994) with
| true -> begin
(FStar_Util.print_string (FStar_Syntax_Print.eff_decl_to_string false ed))
end
| uu____995 -> begin
()
end));
ed;
))))))
end)))
end)))))))))))));
)))
end)))))
end))
end))
end)))
and cps_and_elaborate : FStar_TypeChecker_Env.env_t  ->  FStar_Syntax_Syntax.eff_decl  ->  (FStar_Syntax_Syntax.sigelt Prims.list * FStar_Syntax_Syntax.eff_decl * FStar_Syntax_Syntax.sigelt Prims.option) = (fun env ed -> (

let uu____998 = (FStar_Syntax_Subst.open_term ed.FStar_Syntax_Syntax.binders ed.FStar_Syntax_Syntax.signature)
in (match (uu____998) with
| (effect_binders_un, signature_un) -> begin
(

let uu____1008 = (FStar_TypeChecker_TcTerm.tc_tparams env effect_binders_un)
in (match (uu____1008) with
| (effect_binders, env, uu____1019) -> begin
(

let uu____1020 = (FStar_TypeChecker_TcTerm.tc_trivial_guard env signature_un)
in (match (uu____1020) with
| (signature, uu____1029) -> begin
(

let effect_binders = (FStar_List.map (fun uu____1038 -> (match (uu____1038) with
| (bv, qual) -> begin
(let _0_444 = (

let uu___90_1045 = bv
in (let _0_443 = (FStar_TypeChecker_Normalize.normalize ((FStar_TypeChecker_Normalize.EraseUniverses)::[]) env bv.FStar_Syntax_Syntax.sort)
in {FStar_Syntax_Syntax.ppname = uu___90_1045.FStar_Syntax_Syntax.ppname; FStar_Syntax_Syntax.index = uu___90_1045.FStar_Syntax_Syntax.index; FStar_Syntax_Syntax.sort = _0_443}))
in ((_0_444), (qual)))
end)) effect_binders)
in (

let uu____1046 = (

let uu____1051 = (FStar_Syntax_Subst.compress signature_un).FStar_Syntax_Syntax.n
in (match (uu____1051) with
| FStar_Syntax_Syntax.Tm_arrow (((a, uu____1057))::[], effect_marker) -> begin
((a), (effect_marker))
end
| uu____1072 -> begin
(failwith "bad shape for effect-for-free signature")
end))
in (match (uu____1046) with
| (a, effect_marker) -> begin
(

let a = (

let uu____1089 = (FStar_Syntax_Syntax.is_null_bv a)
in (match (uu____1089) with
| true -> begin
(let _0_445 = Some ((FStar_Syntax_Syntax.range_of_bv a))
in (FStar_Syntax_Syntax.gen_bv "a" _0_445 a.FStar_Syntax_Syntax.sort))
end
| uu____1090 -> begin
a
end))
in (

let open_and_check = (fun t -> (

let subst = (FStar_Syntax_Subst.opening_of_binders effect_binders)
in (

let t = (FStar_Syntax_Subst.subst subst t)
in (

let uu____1099 = (FStar_TypeChecker_TcTerm.tc_term env t)
in (match (uu____1099) with
| (t, comp, uu____1107) -> begin
((t), (comp))
end)))))
in (

let mk = (fun x -> (FStar_Syntax_Syntax.mk x None signature.FStar_Syntax_Syntax.pos))
in (

let uu____1118 = (open_and_check ed.FStar_Syntax_Syntax.repr)
in (match (uu____1118) with
| (repr, _comp) -> begin
((

let uu____1129 = (FStar_TypeChecker_Env.debug env (FStar_Options.Other ("ED")))
in (match (uu____1129) with
| true -> begin
(let _0_446 = (FStar_Syntax_Print.term_to_string repr)
in (FStar_Util.print1 "Representation is: %s\n" _0_446))
end
| uu____1130 -> begin
()
end));
(

let dmff_env = (FStar_TypeChecker_DMFF.empty env (FStar_TypeChecker_TcTerm.tc_constant FStar_Range.dummyRange))
in (

let wp_type = (FStar_TypeChecker_DMFF.star_type dmff_env repr)
in (

let wp_type = (recheck_debug "*" env wp_type)
in (

let wp_a = (let _0_451 = (mk (FStar_Syntax_Syntax.Tm_app ((let _0_450 = (let _0_449 = (let _0_448 = (FStar_Syntax_Syntax.bv_to_name a)
in (let _0_447 = (FStar_Syntax_Syntax.as_implicit false)
in ((_0_448), (_0_447))))
in (_0_449)::[])
in ((wp_type), (_0_450))))))
in (FStar_TypeChecker_Normalize.normalize ((FStar_TypeChecker_Normalize.Beta)::[]) env _0_451))
in (

let effect_signature = (

let binders = (let _0_456 = (let _0_452 = (FStar_Syntax_Syntax.as_implicit false)
in ((a), (_0_452)))
in (let _0_455 = (let _0_454 = (let _0_453 = (FStar_Syntax_Syntax.gen_bv "dijkstra_wp" None wp_a)
in (FStar_All.pipe_right _0_453 FStar_Syntax_Syntax.mk_binder))
in (_0_454)::[])
in (_0_456)::_0_455))
in (

let binders = (FStar_Syntax_Subst.close_binders binders)
in (mk (FStar_Syntax_Syntax.Tm_arrow (((binders), (effect_marker)))))))
in (

let effect_signature = (recheck_debug "turned into the effect signature" env effect_signature)
in (

let sigelts = (FStar_Util.mk_ref [])
in (

let mk_lid = (fun name -> (FStar_Ident.lid_of_path (FStar_Ident.path_of_text (Prims.strcat (FStar_Ident.text_of_lid ed.FStar_Syntax_Syntax.mname) (Prims.strcat "_" name))) FStar_Range.dummyRange))
in (

let elaborate_and_star = (fun dmff_env item -> (

let uu____1181 = item
in (match (uu____1181) with
| (u_item, item) -> begin
(

let uu____1193 = (open_and_check item)
in (match (uu____1193) with
| (item, item_comp) -> begin
((

let uu____1203 = (not ((FStar_Syntax_Util.is_total_lcomp item_comp)))
in (match (uu____1203) with
| true -> begin
(Prims.raise (FStar_Errors.Err ((let _0_458 = (FStar_Syntax_Print.term_to_string item)
in (let _0_457 = (FStar_Syntax_Print.lcomp_to_string item_comp)
in (FStar_Util.format2 "Computation for [%s] is not total : %s !" _0_458 _0_457))))))
end
| uu____1204 -> begin
()
end));
(

let uu____1205 = (FStar_TypeChecker_DMFF.star_expr dmff_env item)
in (match (uu____1205) with
| (item_t, item_wp, item_elab) -> begin
(

let item_wp = (recheck_debug "*" env item_wp)
in (

let item_elab = (recheck_debug "_" env item_elab)
in ((dmff_env), (item_t), (item_wp), (item_elab))))
end));
)
end))
end)))
in (

let uu____1218 = (elaborate_and_star dmff_env ed.FStar_Syntax_Syntax.bind_repr)
in (match (uu____1218) with
| (dmff_env, uu____1229, bind_wp, bind_elab) -> begin
(

let uu____1232 = (elaborate_and_star dmff_env ed.FStar_Syntax_Syntax.return_repr)
in (match (uu____1232) with
| (dmff_env, uu____1243, return_wp, return_elab) -> begin
(

let lift_from_pure_wp = (

let uu____1247 = (FStar_Syntax_Subst.compress return_wp).FStar_Syntax_Syntax.n
in (match (uu____1247) with
| FStar_Syntax_Syntax.Tm_abs ((b1)::(b2)::bs, body, what) -> begin
(

let uu____1283 = (

let uu____1291 = (let _0_459 = (FStar_Syntax_Util.abs bs body None)
in (FStar_Syntax_Subst.open_term ((b1)::(b2)::[]) _0_459))
in (match (uu____1291) with
| ((b1)::(b2)::[], body) -> begin
((b1), (b2), (body))
end
| uu____1332 -> begin
(failwith "Impossible : open_term not preserving binders arity")
end))
in (match (uu____1283) with
| (b1, b2, body) -> begin
(

let env0 = (let _0_460 = (FStar_TypeChecker_DMFF.get_env dmff_env)
in (FStar_TypeChecker_Env.push_binders _0_460 ((b1)::(b2)::[])))
in (

let wp_b1 = (let _0_465 = (mk (FStar_Syntax_Syntax.Tm_app ((let _0_464 = (let _0_463 = (let _0_462 = (FStar_Syntax_Syntax.bv_to_name (Prims.fst b1))
in (let _0_461 = (FStar_Syntax_Syntax.as_implicit false)
in ((_0_462), (_0_461))))
in (_0_463)::[])
in ((wp_type), (_0_464))))))
in (FStar_TypeChecker_Normalize.normalize ((FStar_TypeChecker_Normalize.Beta)::[]) env0 _0_465))
in (

let uu____1368 = (let _0_467 = (let _0_466 = (FStar_Syntax_Util.unascribe wp_b1)
in (FStar_TypeChecker_Normalize.eta_expand_with_type body _0_466))
in (FStar_All.pipe_left FStar_Syntax_Util.abs_formals _0_467))
in (match (uu____1368) with
| (bs, body, what') -> begin
(

let t2 = (Prims.fst b2).FStar_Syntax_Syntax.sort
in (

let pure_wp_type = (FStar_TypeChecker_DMFF.double_star t2)
in (

let wp = (FStar_Syntax_Syntax.gen_bv "wp" None pure_wp_type)
in (

let body = ((let _0_471 = (FStar_Syntax_Syntax.bv_to_name wp)
in (let _0_470 = (let _0_469 = (let _0_468 = (FStar_Syntax_Util.abs ((b2)::[]) body what')
in ((_0_468), (None)))
in (_0_469)::[])
in (FStar_Syntax_Syntax.mk_Tm_app _0_471 _0_470))) None FStar_Range.dummyRange)
in (let _0_475 = (let _0_473 = (let _0_472 = (FStar_Syntax_Syntax.mk_binder wp)
in (_0_472)::[])
in (b1)::_0_473)
in (let _0_474 = (FStar_Syntax_Util.abs bs body what)
in (FStar_Syntax_Util.abs _0_475 _0_474 (Some (FStar_Util.Inr (((FStar_Syntax_Const.effect_GTot_lid), ([]))))))))))))
end))))
end))
end
| uu____1436 -> begin
(failwith "unexpected shape for return")
end))
in (

let return_wp = (

let uu____1438 = (FStar_Syntax_Subst.compress return_wp).FStar_Syntax_Syntax.n
in (match (uu____1438) with
| FStar_Syntax_Syntax.Tm_abs ((b1)::(b2)::bs, body, what) -> begin
(let _0_476 = (FStar_Syntax_Util.abs bs body what)
in (FStar_Syntax_Util.abs ((b1)::(b2)::[]) _0_476 (Some (FStar_Util.Inr (((FStar_Syntax_Const.effect_GTot_lid), ([])))))))
end
| uu____1489 -> begin
(failwith "unexpected shape for return")
end))
in (

let bind_wp = (

let uu____1491 = (FStar_Syntax_Subst.compress bind_wp).FStar_Syntax_Syntax.n
in (match (uu____1491) with
| FStar_Syntax_Syntax.Tm_abs (binders, body, what) -> begin
(

let r = (FStar_Syntax_Syntax.lid_as_fv FStar_Syntax_Const.range_lid (FStar_Syntax_Syntax.Delta_defined_at_level ((Prims.parse_int "1"))) None)
in (let _0_479 = (let _0_478 = (let _0_477 = (FStar_Syntax_Syntax.null_binder (mk (FStar_Syntax_Syntax.Tm_fvar (r))))
in (_0_477)::[])
in (FStar_List.append _0_478 binders))
in (FStar_Syntax_Util.abs _0_479 body what)))
end
| uu____1518 -> begin
(failwith "unexpected shape for bind")
end))
in (

let apply_close = (fun t -> (match (((FStar_List.length effect_binders) = (Prims.parse_int "0"))) with
| true -> begin
t
end
| uu____1535 -> begin
(let _0_481 = (mk (FStar_Syntax_Syntax.Tm_app ((let _0_480 = (Prims.snd (FStar_Syntax_Util.args_of_binders effect_binders))
in ((t), (_0_480))))))
in (FStar_Syntax_Subst.close effect_binders _0_481))
end))
in (

let register = (fun name item -> (

let uu____1545 = (let _0_483 = (mk_lid name)
in (let _0_482 = (FStar_Syntax_Util.abs effect_binders item None)
in (FStar_TypeChecker_Util.mk_toplevel_definition env _0_483 _0_482)))
in (match (uu____1545) with
| (sigelt, fv) -> begin
((let _0_485 = (let _0_484 = (FStar_ST.read sigelts)
in (sigelt)::_0_484)
in (FStar_ST.write sigelts _0_485));
fv;
)
end)))
in (

let lift_from_pure_wp = (register "lift_from_pure" lift_from_pure_wp)
in (

let return_wp = (register "return_wp" return_wp)
in ((let _0_487 = (let _0_486 = (FStar_ST.read sigelts)
in (FStar_Syntax_Syntax.Sig_pragma (((FStar_Syntax_Syntax.SetOptions ("--admit_smt_queries true")), (FStar_Range.dummyRange))))::_0_486)
in (FStar_ST.write sigelts _0_487));
(

let return_elab = (register "return_elab" return_elab)
in ((let _0_489 = (let _0_488 = (FStar_ST.read sigelts)
in (FStar_Syntax_Syntax.Sig_pragma (((FStar_Syntax_Syntax.SetOptions ("--admit_smt_queries false")), (FStar_Range.dummyRange))))::_0_488)
in (FStar_ST.write sigelts _0_489));
(

let bind_wp = (register "bind_wp" bind_wp)
in ((let _0_491 = (let _0_490 = (FStar_ST.read sigelts)
in (FStar_Syntax_Syntax.Sig_pragma (((FStar_Syntax_Syntax.SetOptions ("--admit_smt_queries true")), (FStar_Range.dummyRange))))::_0_490)
in (FStar_ST.write sigelts _0_491));
(

let bind_elab = (register "bind_elab" bind_elab)
in ((let _0_493 = (let _0_492 = (FStar_ST.read sigelts)
in (FStar_Syntax_Syntax.Sig_pragma (((FStar_Syntax_Syntax.SetOptions ("--admit_smt_queries false")), (FStar_Range.dummyRange))))::_0_492)
in (FStar_ST.write sigelts _0_493));
(

let uu____1595 = (FStar_List.fold_left (fun uu____1602 action -> (match (uu____1602) with
| (dmff_env, actions) -> begin
(

let uu____1614 = (elaborate_and_star dmff_env ((action.FStar_Syntax_Syntax.action_univs), (action.FStar_Syntax_Syntax.action_defn)))
in (match (uu____1614) with
| (dmff_env, action_t, action_wp, action_elab) -> begin
(

let name = action.FStar_Syntax_Syntax.action_name.FStar_Ident.ident.FStar_Ident.idText
in (

let action_typ_with_wp = (FStar_TypeChecker_DMFF.trans_F dmff_env action_t action_wp)
in (

let action_elab = (register (Prims.strcat name "_elab") action_elab)
in (

let action_typ_with_wp = (register (Prims.strcat name "_complete_type") action_typ_with_wp)
in (let _0_497 = (let _0_496 = (

let uu___91_1631 = action
in (let _0_495 = (apply_close action_elab)
in (let _0_494 = (apply_close action_typ_with_wp)
in {FStar_Syntax_Syntax.action_name = uu___91_1631.FStar_Syntax_Syntax.action_name; FStar_Syntax_Syntax.action_unqualified_name = uu___91_1631.FStar_Syntax_Syntax.action_unqualified_name; FStar_Syntax_Syntax.action_univs = uu___91_1631.FStar_Syntax_Syntax.action_univs; FStar_Syntax_Syntax.action_defn = _0_495; FStar_Syntax_Syntax.action_typ = _0_494})))
in (_0_496)::actions)
in ((dmff_env), (_0_497)))))))
end))
end)) ((dmff_env), ([])) ed.FStar_Syntax_Syntax.actions)
in (match (uu____1595) with
| (dmff_env, actions) -> begin
(

let actions = (FStar_List.rev actions)
in (

let repr = (

let wp = (FStar_Syntax_Syntax.gen_bv "wp_a" None wp_a)
in (

let binders = (let _0_500 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_499 = (let _0_498 = (FStar_Syntax_Syntax.mk_binder wp)
in (_0_498)::[])
in (_0_500)::_0_499))
in (let _0_507 = (let _0_506 = (mk (FStar_Syntax_Syntax.Tm_app ((let _0_504 = (let _0_503 = (let _0_502 = (FStar_Syntax_Syntax.bv_to_name a)
in (let _0_501 = (FStar_Syntax_Syntax.as_implicit false)
in ((_0_502), (_0_501))))
in (_0_503)::[])
in ((repr), (_0_504))))))
in (let _0_505 = (FStar_Syntax_Syntax.bv_to_name wp)
in (FStar_TypeChecker_DMFF.trans_F dmff_env _0_506 _0_505)))
in (FStar_Syntax_Util.abs binders _0_507 None))))
in (

let repr = (recheck_debug "FC" env repr)
in (

let repr = (register "repr" repr)
in (

let uu____1662 = (

let uu____1667 = (let _0_508 = (FStar_Syntax_Subst.compress wp_type)
in (FStar_All.pipe_left FStar_Syntax_Util.unascribe _0_508)).FStar_Syntax_Syntax.n
in (match (uu____1667) with
| FStar_Syntax_Syntax.Tm_abs ((type_param)::effect_param, arrow, uu____1675) -> begin
(

let uu____1702 = (

let uu____1711 = (FStar_Syntax_Subst.open_term ((type_param)::effect_param) arrow)
in (match (uu____1711) with
| ((b)::bs, body) -> begin
((b), (bs), (body))
end
| uu____1742 -> begin
(failwith "Impossible : open_term nt preserving binders arity")
end))
in (match (uu____1702) with
| (type_param, effect_param, arrow) -> begin
(

let uu____1770 = (let _0_509 = (FStar_Syntax_Subst.compress arrow)
in (FStar_All.pipe_left FStar_Syntax_Util.unascribe _0_509)).FStar_Syntax_Syntax.n
in (match (uu____1770) with
| FStar_Syntax_Syntax.Tm_arrow (wp_binders, c) -> begin
(

let uu____1787 = (FStar_Syntax_Subst.open_comp wp_binders c)
in (match (uu____1787) with
| (wp_binders, c) -> begin
(

let uu____1796 = (FStar_List.partition (fun uu____1807 -> (match (uu____1807) with
| (bv, uu____1811) -> begin
(let _0_511 = (let _0_510 = (FStar_Syntax_Free.names bv.FStar_Syntax_Syntax.sort)
in (FStar_All.pipe_right _0_510 (FStar_Util.set_mem (Prims.fst type_param))))
in (FStar_All.pipe_right _0_511 Prims.op_Negation))
end)) wp_binders)
in (match (uu____1796) with
| (pre_args, post_args) -> begin
(

let post = (match (post_args) with
| (post)::[] -> begin
post
end
| uu____1843 -> begin
(failwith (let _0_512 = (FStar_Syntax_Print.term_to_string arrow)
in (FStar_Util.format1 "Impossible: multiple post candidates %s" _0_512)))
end)
in (let _0_514 = (FStar_Syntax_Util.arrow pre_args c)
in (let _0_513 = (FStar_Syntax_Util.abs ((type_param)::effect_param) (Prims.fst post).FStar_Syntax_Syntax.sort None)
in ((_0_514), (_0_513)))))
end))
end))
end
| uu____1858 -> begin
(failwith (let _0_515 = (FStar_Syntax_Print.term_to_string arrow)
in (FStar_Util.format1 "Impossible: pre/post arrow %s" _0_515)))
end))
end))
end
| uu____1863 -> begin
(failwith (let _0_516 = (FStar_Syntax_Print.term_to_string wp_type)
in (FStar_Util.format1 "Impossible: pre/post abs %s" _0_516)))
end))
in (match (uu____1662) with
| (pre, post) -> begin
((Prims.ignore (register "pre" pre));
(Prims.ignore (register "post" post));
(Prims.ignore (register "wp" wp_type));
(

let ed = (

let uu___92_1883 = ed
in (let _0_527 = (FStar_Syntax_Subst.close_binders effect_binders)
in (let _0_526 = (FStar_Syntax_Subst.close effect_binders effect_signature)
in (let _0_525 = (let _0_517 = (apply_close return_wp)
in (([]), (_0_517)))
in (let _0_524 = (let _0_518 = (apply_close bind_wp)
in (([]), (_0_518)))
in (let _0_523 = (apply_close repr)
in (let _0_522 = (let _0_519 = (apply_close return_elab)
in (([]), (_0_519)))
in (let _0_521 = (let _0_520 = (apply_close bind_elab)
in (([]), (_0_520)))
in {FStar_Syntax_Syntax.qualifiers = uu___92_1883.FStar_Syntax_Syntax.qualifiers; FStar_Syntax_Syntax.cattributes = uu___92_1883.FStar_Syntax_Syntax.cattributes; FStar_Syntax_Syntax.mname = uu___92_1883.FStar_Syntax_Syntax.mname; FStar_Syntax_Syntax.univs = uu___92_1883.FStar_Syntax_Syntax.univs; FStar_Syntax_Syntax.binders = _0_527; FStar_Syntax_Syntax.signature = _0_526; FStar_Syntax_Syntax.ret_wp = _0_525; FStar_Syntax_Syntax.bind_wp = _0_524; FStar_Syntax_Syntax.if_then_else = uu___92_1883.FStar_Syntax_Syntax.if_then_else; FStar_Syntax_Syntax.ite_wp = uu___92_1883.FStar_Syntax_Syntax.ite_wp; FStar_Syntax_Syntax.stronger = uu___92_1883.FStar_Syntax_Syntax.stronger; FStar_Syntax_Syntax.close_wp = uu___92_1883.FStar_Syntax_Syntax.close_wp; FStar_Syntax_Syntax.assert_p = uu___92_1883.FStar_Syntax_Syntax.assert_p; FStar_Syntax_Syntax.assume_p = uu___92_1883.FStar_Syntax_Syntax.assume_p; FStar_Syntax_Syntax.null_wp = uu___92_1883.FStar_Syntax_Syntax.null_wp; FStar_Syntax_Syntax.trivial = uu___92_1883.FStar_Syntax_Syntax.trivial; FStar_Syntax_Syntax.repr = _0_523; FStar_Syntax_Syntax.return_repr = _0_522; FStar_Syntax_Syntax.bind_repr = _0_521; FStar_Syntax_Syntax.actions = actions}))))))))
in (

let uu____1896 = (FStar_TypeChecker_DMFF.gen_wps_for_free env effect_binders a wp_a ed)
in (match (uu____1896) with
| (sigelts', ed) -> begin
((

let uu____1907 = (FStar_TypeChecker_Env.debug env (FStar_Options.Other ("ED")))
in (match (uu____1907) with
| true -> begin
(FStar_Util.print_string (FStar_Syntax_Print.eff_decl_to_string true ed))
end
| uu____1908 -> begin
()
end));
(

let lift_from_pure_opt = (match (((FStar_List.length effect_binders) = (Prims.parse_int "0"))) with
| true -> begin
(

let lift_from_pure = (let _0_529 = Some ((let _0_528 = (apply_close lift_from_pure_wp)
in (([]), (_0_528))))
in {FStar_Syntax_Syntax.source = FStar_Syntax_Const.effect_PURE_lid; FStar_Syntax_Syntax.target = ed.FStar_Syntax_Syntax.mname; FStar_Syntax_Syntax.lift_wp = _0_529; FStar_Syntax_Syntax.lift = None})
in Some (FStar_Syntax_Syntax.Sig_sub_effect (((lift_from_pure), (FStar_Range.dummyRange)))))
end
| uu____1925 -> begin
None
end)
in (let _0_531 = (let _0_530 = (FStar_List.rev (FStar_ST.read sigelts))
in (FStar_List.append _0_530 sigelts'))
in ((_0_531), (ed), (lift_from_pure_opt))));
)
end)));
)
end))))))
end));
));
));
));
))))))))
end))
end)))))))))));
)
end)))))
end)))
end))
end))
end)))
and tc_lex_t : FStar_TypeChecker_Env.env  ->  FStar_Syntax_Syntax.sigelt Prims.list  ->  FStar_Syntax_Syntax.qualifier Prims.list  ->  FStar_Ident.lident Prims.list  ->  FStar_Syntax_Syntax.sigelt = (fun env ses quals lids -> (match (ses) with
| (FStar_Syntax_Syntax.Sig_inductive_typ (lex_t, [], [], t, uu____1947, uu____1948, [], r))::(FStar_Syntax_Syntax.Sig_datacon (lex_top, [], _t_top, _lex_t_top, _0_532, [], uu____1953, r1))::(FStar_Syntax_Syntax.Sig_datacon (lex_cons, [], _t_cons, _lex_t_cons, _0_533, [], uu____1958, r2))::[] when (((_0_532 = (Prims.parse_int "0")) && (_0_533 = (Prims.parse_int "0"))) && (((FStar_Ident.lid_equals lex_t FStar_Syntax_Const.lex_t_lid) && (FStar_Ident.lid_equals lex_top FStar_Syntax_Const.lextop_lid)) && (FStar_Ident.lid_equals lex_cons FStar_Syntax_Const.lexcons_lid))) -> begin
(

let u = (FStar_Syntax_Syntax.new_univ_name (Some (r)))
in (

let t = (FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_type (FStar_Syntax_Syntax.U_name (u))) None r)
in (

let t = (FStar_Syntax_Subst.close_univ_vars ((u)::[]) t)
in (

let tc = FStar_Syntax_Syntax.Sig_inductive_typ (((lex_t), ((u)::[]), ([]), (t), ([]), ((FStar_Syntax_Const.lextop_lid)::(FStar_Syntax_Const.lexcons_lid)::[]), ([]), (r)))
in (

let utop = (FStar_Syntax_Syntax.new_univ_name (Some (r1)))
in (

let lex_top_t = ((FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_uinst ((let _0_534 = (FStar_Syntax_Syntax.fvar (FStar_Ident.set_lid_range FStar_Syntax_Const.lex_t_lid r1) FStar_Syntax_Syntax.Delta_constant None)
in ((_0_534), ((FStar_Syntax_Syntax.U_name (utop))::[])))))) None r1)
in (

let lex_top_t = (FStar_Syntax_Subst.close_univ_vars ((utop)::[]) lex_top_t)
in (

let dc_lextop = FStar_Syntax_Syntax.Sig_datacon (((lex_top), ((utop)::[]), (lex_top_t), (FStar_Syntax_Const.lex_t_lid), ((Prims.parse_int "0")), ([]), ([]), (r1)))
in (

let ucons1 = (FStar_Syntax_Syntax.new_univ_name (Some (r2)))
in (

let ucons2 = (FStar_Syntax_Syntax.new_univ_name (Some (r2)))
in (

let lex_cons_t = (

let a = (let _0_535 = (FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_type (FStar_Syntax_Syntax.U_name (ucons1))) None r2)
in (FStar_Syntax_Syntax.new_bv (Some (r2)) _0_535))
in (

let hd = (let _0_536 = (FStar_Syntax_Syntax.bv_to_name a)
in (FStar_Syntax_Syntax.new_bv (Some (r2)) _0_536))
in (

let tl = (let _0_538 = ((FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_uinst ((let _0_537 = (FStar_Syntax_Syntax.fvar (FStar_Ident.set_lid_range FStar_Syntax_Const.lex_t_lid r2) FStar_Syntax_Syntax.Delta_constant None)
in ((_0_537), ((FStar_Syntax_Syntax.U_name (ucons2))::[])))))) None r2)
in (FStar_Syntax_Syntax.new_bv (Some (r2)) _0_538))
in (

let res = ((FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_uinst ((let _0_539 = (FStar_Syntax_Syntax.fvar (FStar_Ident.set_lid_range FStar_Syntax_Const.lex_t_lid r2) FStar_Syntax_Syntax.Delta_constant None)
in ((_0_539), ((FStar_Syntax_Syntax.U_max ((FStar_Syntax_Syntax.U_name (ucons1))::(FStar_Syntax_Syntax.U_name (ucons2))::[]))::[])))))) None r2)
in (let _0_540 = (FStar_Syntax_Syntax.mk_Total res)
in (FStar_Syntax_Util.arrow ((((a), (Some (FStar_Syntax_Syntax.imp_tag))))::(((hd), (None)))::(((tl), (None)))::[]) _0_540))))))
in (

let lex_cons_t = (FStar_Syntax_Subst.close_univ_vars ((ucons1)::(ucons2)::[]) lex_cons_t)
in (

let dc_lexcons = FStar_Syntax_Syntax.Sig_datacon (((lex_cons), ((ucons1)::(ucons2)::[]), (lex_cons_t), (FStar_Syntax_Const.lex_t_lid), ((Prims.parse_int "0")), ([]), ([]), (r2)))
in FStar_Syntax_Syntax.Sig_bundle ((let _0_541 = (FStar_TypeChecker_Env.get_range env)
in (((tc)::(dc_lextop)::(dc_lexcons)::[]), ([]), (lids), (_0_541)))))))))))))))))
end
| uu____2080 -> begin
(failwith (let _0_542 = (FStar_Syntax_Print.sigelt_to_string (FStar_Syntax_Syntax.Sig_bundle (((ses), ([]), (lids), (FStar_Range.dummyRange)))))
in (FStar_Util.format1 "Unexpected lex_t: %s\n" _0_542)))
end))
and tc_assume : FStar_TypeChecker_Env.env  ->  FStar_Ident.lident  ->  FStar_Syntax_Syntax.formula  ->  FStar_Syntax_Syntax.qualifier Prims.list  ->  FStar_Range.range  ->  FStar_Syntax_Syntax.sigelt = (fun env lid phi quals r -> (

let env = (FStar_TypeChecker_Env.set_range env r)
in (

let uu____2092 = (FStar_Syntax_Util.type_u ())
in (match (uu____2092) with
| (k, uu____2096) -> begin
(

let phi = (let _0_543 = (tc_check_trivial_guard env phi k)
in (FStar_All.pipe_right _0_543 (FStar_TypeChecker_Normalize.normalize ((FStar_TypeChecker_Normalize.Beta)::(FStar_TypeChecker_Normalize.Eager_unfolding)::[]) env)))
in ((FStar_TypeChecker_Util.check_uvars r phi);
FStar_Syntax_Syntax.Sig_assume (((lid), (phi), (quals), (r)));
))
end))))
and tc_inductive : FStar_TypeChecker_Env.env_t  ->  FStar_Syntax_Syntax.sigelt Prims.list  ->  FStar_Syntax_Syntax.qualifier Prims.list  ->  FStar_Ident.lident Prims.list  ->  (FStar_Syntax_Syntax.sigelt Prims.list * FStar_Syntax_Syntax.sigelt Prims.list) = (fun env ses quals lids -> (

let env0 = env
in (

let uu____2108 = (FStar_TypeChecker_TcInductive.check_inductive_well_typedness env ses quals lids)
in (match (uu____2108) with
| (sig_bndle, tcs, datas) -> begin
(

let data_ops_ses = (let _0_544 = (FStar_List.map (FStar_TypeChecker_Util.mk_data_operations quals env tcs) datas)
in (FStar_All.pipe_right _0_544 FStar_List.flatten))
in ((

let uu____2132 = ((FStar_Options.no_positivity ()) || (FStar_Options.lax ()))
in (match (uu____2132) with
| true -> begin
()
end
| uu____2133 -> begin
(

let env = (FStar_TypeChecker_Env.push_sigelt env0 sig_bndle)
in (FStar_List.iter (fun ty -> (

let b = (FStar_TypeChecker_TcInductive.check_positivity ty env)
in (match ((not (b))) with
| true -> begin
(

let uu____2138 = (match (ty) with
| FStar_Syntax_Syntax.Sig_inductive_typ (lid, uu____2144, uu____2145, uu____2146, uu____2147, uu____2148, uu____2149, r) -> begin
((lid), (r))
end
| uu____2157 -> begin
(failwith "Impossible!")
end)
in (match (uu____2138) with
| (lid, r) -> begin
(FStar_Errors.report r (Prims.strcat "Inductive type " (Prims.strcat lid.FStar_Ident.str " does not satisfy the positivity condition")))
end))
end
| uu____2162 -> begin
()
end))) tcs))
end));
(

let skip_prims_type = (fun uu____2166 -> (

let lid = (

let ty = (FStar_List.hd tcs)
in (match (ty) with
| FStar_Syntax_Syntax.Sig_inductive_typ (lid, uu____2170, uu____2171, uu____2172, uu____2173, uu____2174, uu____2175, uu____2176) -> begin
lid
end
| uu____2183 -> begin
(failwith "Impossible")
end))
in (

let types_to_skip = ("c_False")::("c_True")::("equals")::("h_equals")::("c_and")::("c_or")::[]
in (FStar_List.existsb (fun s -> (s = lid.FStar_Ident.ident.FStar_Ident.idText)) types_to_skip))))
in (

let is_noeq = (FStar_List.existsb (fun q -> (q = FStar_Syntax_Syntax.Noeq)) quals)
in (

let uu____2189 = ((((FStar_List.length tcs) = (Prims.parse_int "0")) || ((FStar_Ident.lid_equals env.FStar_TypeChecker_Env.curmodule FStar_Syntax_Const.prims_lid) && (skip_prims_type ()))) || is_noeq)
in (match (uu____2189) with
| true -> begin
(((sig_bndle)::[]), (data_ops_ses))
end
| uu____2198 -> begin
(

let is_unopteq = (FStar_List.existsb (fun q -> (q = FStar_Syntax_Syntax.Unopteq)) quals)
in (

let ses = (match (is_unopteq) with
| true -> begin
(FStar_TypeChecker_TcInductive.unoptimized_haseq_scheme sig_bndle tcs datas env0 tc_assume)
end
| uu____2204 -> begin
(FStar_TypeChecker_TcInductive.optimized_haseq_scheme sig_bndle tcs datas env0 tc_assume)
end)
in (let _0_547 = (let _0_546 = FStar_Syntax_Syntax.Sig_bundle ((let _0_545 = (FStar_TypeChecker_Env.get_range env0)
in (((FStar_List.append tcs datas)), (quals), (lids), (_0_545))))
in (_0_546)::ses)
in ((_0_547), (data_ops_ses)))))
end))));
))
end))))
and tc_decl : FStar_TypeChecker_Env.env  ->  FStar_Syntax_Syntax.sigelt  ->  (FStar_Syntax_Syntax.sigelt Prims.list * FStar_TypeChecker_Env.env * FStar_Syntax_Syntax.sigelt Prims.list) = (fun env se -> (

let env = (set_hint_correlator env se)
in ((FStar_TypeChecker_Util.check_sigelt_quals env se);
(match (se) with
| (FStar_Syntax_Syntax.Sig_inductive_typ (_)) | (FStar_Syntax_Syntax.Sig_datacon (_)) -> begin
(failwith "Impossible bare data-constructor")
end
| FStar_Syntax_Syntax.Sig_bundle (ses, quals, lids, r) when (FStar_All.pipe_right lids (FStar_Util.for_some (FStar_Ident.lid_equals FStar_Syntax_Const.lex_t_lid))) -> begin
(

let env = (FStar_TypeChecker_Env.set_range env r)
in (

let se = (tc_lex_t env ses quals lids)
in (let _0_548 = (FStar_TypeChecker_Env.push_sigelt env se)
in (((se)::[]), (_0_548), ([])))))
end
| FStar_Syntax_Syntax.Sig_bundle (ses, quals, lids, r) -> begin
(

let env = (FStar_TypeChecker_Env.set_range env r)
in (

let uu____2257 = (tc_inductive env ses quals lids)
in (match (uu____2257) with
| (ses, projectors_ses) -> begin
(

let env = (FStar_List.fold_left (fun env' se -> (FStar_TypeChecker_Env.push_sigelt env' se)) env ses)
in ((ses), (env), (projectors_ses)))
end)))
end
| FStar_Syntax_Syntax.Sig_pragma (p, r) -> begin
(

let set_options = (fun t s -> (

let uu____2287 = (FStar_Options.set_options t s)
in (match (uu____2287) with
| FStar_Getopt.Success -> begin
()
end
| FStar_Getopt.Help -> begin
(Prims.raise (FStar_Errors.Error ((("Failed to process pragma: use \'fstar --help\' to see which options are available"), (r)))))
end
| FStar_Getopt.Error (s) -> begin
(Prims.raise (FStar_Errors.Error ((((Prims.strcat "Failed to process pragma: " s)), (r)))))
end)))
in (match (p) with
| FStar_Syntax_Syntax.LightOff -> begin
((match ((p = FStar_Syntax_Syntax.LightOff)) with
| true -> begin
(FStar_Options.set_ml_ish ())
end
| uu____2295 -> begin
()
end);
(((se)::[]), (env), ([]));
)
end
| FStar_Syntax_Syntax.SetOptions (o) -> begin
((set_options FStar_Options.Set o);
(((se)::[]), (env), ([]));
)
end
| FStar_Syntax_Syntax.ResetOptions (sopt) -> begin
((let _0_549 = (FStar_Options.restore_cmd_line_options false)
in (FStar_All.pipe_right _0_549 Prims.ignore));
(match (sopt) with
| None -> begin
()
end
| Some (s) -> begin
(set_options FStar_Options.Reset s)
end);
(env.FStar_TypeChecker_Env.solver.FStar_TypeChecker_Env.refresh ());
(((se)::[]), (env), ([]));
)
end))
end
| FStar_Syntax_Syntax.Sig_new_effect_for_free (uu____2310) -> begin
(failwith "impossible")
end
| FStar_Syntax_Syntax.Sig_new_effect (ne, r) -> begin
(

let ne = (tc_eff_decl env ne)
in (

let se = FStar_Syntax_Syntax.Sig_new_effect (((ne), (r)))
in (

let env = (FStar_TypeChecker_Env.push_sigelt env se)
in (

let uu____2323 = (FStar_All.pipe_right ne.FStar_Syntax_Syntax.actions (FStar_List.fold_left (fun uu____2334 a -> (match (uu____2334) with
| (env, ses) -> begin
(

let se_let = (FStar_Syntax_Util.action_as_lb ne.FStar_Syntax_Syntax.mname a)
in (let _0_550 = (FStar_TypeChecker_Env.push_sigelt env se_let)
in ((_0_550), ((se_let)::ses))))
end)) ((env), ((se)::[]))))
in (match (uu____2323) with
| (env, ses) -> begin
(((se)::[]), (env), ([]))
end)))))
end
| FStar_Syntax_Syntax.Sig_sub_effect (sub, r) -> begin
(

let ed_src = (FStar_TypeChecker_Env.get_effect_decl env sub.FStar_Syntax_Syntax.source)
in (

let ed_tgt = (FStar_TypeChecker_Env.get_effect_decl env sub.FStar_Syntax_Syntax.target)
in (

let uu____2364 = (let _0_551 = (FStar_TypeChecker_Env.lookup_effect_lid env sub.FStar_Syntax_Syntax.source)
in (monad_signature env sub.FStar_Syntax_Syntax.source _0_551))
in (match (uu____2364) with
| (a, wp_a_src) -> begin
(

let uu____2380 = (let _0_552 = (FStar_TypeChecker_Env.lookup_effect_lid env sub.FStar_Syntax_Syntax.target)
in (monad_signature env sub.FStar_Syntax_Syntax.target _0_552))
in (match (uu____2380) with
| (b, wp_b_tgt) -> begin
(

let wp_a_tgt = (let _0_555 = (let _0_554 = FStar_Syntax_Syntax.NT ((let _0_553 = (FStar_Syntax_Syntax.bv_to_name a)
in ((b), (_0_553))))
in (_0_554)::[])
in (FStar_Syntax_Subst.subst _0_555 wp_b_tgt))
in (

let expected_k = (let _0_560 = (let _0_558 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_557 = (let _0_556 = (FStar_Syntax_Syntax.null_binder wp_a_src)
in (_0_556)::[])
in (_0_558)::_0_557))
in (let _0_559 = (FStar_Syntax_Syntax.mk_Total wp_a_tgt)
in (FStar_Syntax_Util.arrow _0_560 _0_559)))
in (

let repr_type = (fun eff_name a wp -> (

let no_reify = (fun l -> (Prims.raise (FStar_Errors.Error ((let _0_562 = (FStar_Util.format1 "Effect %s cannot be reified" l.FStar_Ident.str)
in (let _0_561 = (FStar_TypeChecker_Env.get_range env)
in ((_0_562), (_0_561))))))))
in (

let uu____2420 = (FStar_TypeChecker_Env.effect_decl_opt env eff_name)
in (match (uu____2420) with
| None -> begin
(no_reify eff_name)
end
| Some (ed) -> begin
(

let repr = (FStar_TypeChecker_Env.inst_effect_fun_with ((FStar_Syntax_Syntax.U_unknown)::[]) env ed (([]), (ed.FStar_Syntax_Syntax.repr)))
in (

let uu____2427 = (not ((FStar_All.pipe_right ed.FStar_Syntax_Syntax.qualifiers (FStar_List.contains FStar_Syntax_Syntax.Reifiable))))
in (match (uu____2427) with
| true -> begin
(no_reify eff_name)
end
| uu____2431 -> begin
(let _0_567 = (FStar_TypeChecker_Env.get_range env)
in ((FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_app ((let _0_566 = (let _0_565 = (FStar_Syntax_Syntax.as_arg a)
in (let _0_564 = (let _0_563 = (FStar_Syntax_Syntax.as_arg wp)
in (_0_563)::[])
in (_0_565)::_0_564))
in ((repr), (_0_566)))))) None _0_567))
end)))
end))))
in (

let uu____2441 = (match (((sub.FStar_Syntax_Syntax.lift), (sub.FStar_Syntax_Syntax.lift_wp))) with
| (None, None) -> begin
(failwith "Impossible")
end
| (lift, Some (uu____2456, lift_wp)) -> begin
(let _0_568 = (check_and_gen env lift_wp expected_k)
in ((lift), (_0_568)))
end
| (Some (what, lift), None) -> begin
(

let dmff_env = (FStar_TypeChecker_DMFF.empty env (FStar_TypeChecker_TcTerm.tc_constant FStar_Range.dummyRange))
in (

let uu____2483 = (FStar_TypeChecker_DMFF.star_expr dmff_env lift)
in (match (uu____2483) with
| (uu____2490, lift_wp, lift_elab) -> begin
(

let uu____2493 = (recheck_debug "lift-wp" env lift_wp)
in (

let uu____2494 = (recheck_debug "lift-elab" env lift_elab)
in ((Some ((([]), (lift_elab)))), ((([]), (lift_wp))))))
end)))
end)
in (match (uu____2441) with
| (lift, lift_wp) -> begin
(

let lax = env.FStar_TypeChecker_Env.lax
in (

let env = (

let uu___93_2518 = env
in {FStar_TypeChecker_Env.solver = uu___93_2518.FStar_TypeChecker_Env.solver; FStar_TypeChecker_Env.range = uu___93_2518.FStar_TypeChecker_Env.range; FStar_TypeChecker_Env.curmodule = uu___93_2518.FStar_TypeChecker_Env.curmodule; FStar_TypeChecker_Env.gamma = uu___93_2518.FStar_TypeChecker_Env.gamma; FStar_TypeChecker_Env.gamma_cache = uu___93_2518.FStar_TypeChecker_Env.gamma_cache; FStar_TypeChecker_Env.modules = uu___93_2518.FStar_TypeChecker_Env.modules; FStar_TypeChecker_Env.expected_typ = uu___93_2518.FStar_TypeChecker_Env.expected_typ; FStar_TypeChecker_Env.sigtab = uu___93_2518.FStar_TypeChecker_Env.sigtab; FStar_TypeChecker_Env.is_pattern = uu___93_2518.FStar_TypeChecker_Env.is_pattern; FStar_TypeChecker_Env.instantiate_imp = uu___93_2518.FStar_TypeChecker_Env.instantiate_imp; FStar_TypeChecker_Env.effects = uu___93_2518.FStar_TypeChecker_Env.effects; FStar_TypeChecker_Env.generalize = uu___93_2518.FStar_TypeChecker_Env.generalize; FStar_TypeChecker_Env.letrecs = uu___93_2518.FStar_TypeChecker_Env.letrecs; FStar_TypeChecker_Env.top_level = uu___93_2518.FStar_TypeChecker_Env.top_level; FStar_TypeChecker_Env.check_uvars = uu___93_2518.FStar_TypeChecker_Env.check_uvars; FStar_TypeChecker_Env.use_eq = uu___93_2518.FStar_TypeChecker_Env.use_eq; FStar_TypeChecker_Env.is_iface = uu___93_2518.FStar_TypeChecker_Env.is_iface; FStar_TypeChecker_Env.admit = uu___93_2518.FStar_TypeChecker_Env.admit; FStar_TypeChecker_Env.lax = true; FStar_TypeChecker_Env.lax_universes = uu___93_2518.FStar_TypeChecker_Env.lax_universes; FStar_TypeChecker_Env.type_of = uu___93_2518.FStar_TypeChecker_Env.type_of; FStar_TypeChecker_Env.universe_of = uu___93_2518.FStar_TypeChecker_Env.universe_of; FStar_TypeChecker_Env.use_bv_sorts = uu___93_2518.FStar_TypeChecker_Env.use_bv_sorts; FStar_TypeChecker_Env.qname_and_index = uu___93_2518.FStar_TypeChecker_Env.qname_and_index})
in (

let lift = (match (lift) with
| None -> begin
None
end
| Some (uu____2522, lift) -> begin
(

let uu____2529 = (let _0_569 = (FStar_TypeChecker_Env.lookup_effect_lid env sub.FStar_Syntax_Syntax.source)
in (monad_signature env sub.FStar_Syntax_Syntax.source _0_569))
in (match (uu____2529) with
| (a, wp_a_src) -> begin
(

let wp_a = (FStar_Syntax_Syntax.new_bv None wp_a_src)
in (

let a_typ = (FStar_Syntax_Syntax.bv_to_name a)
in (

let wp_a_typ = (FStar_Syntax_Syntax.bv_to_name wp_a)
in (

let repr_f = (repr_type sub.FStar_Syntax_Syntax.source a_typ wp_a_typ)
in (

let repr_result = (

let lift_wp = (FStar_TypeChecker_Normalize.normalize ((FStar_TypeChecker_Normalize.EraseUniverses)::(FStar_TypeChecker_Normalize.AllowUnboundUniverses)::[]) env (Prims.snd lift_wp))
in (

let lift_wp_a = (let _0_574 = (FStar_TypeChecker_Env.get_range env)
in ((FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_app ((let _0_573 = (let _0_572 = (FStar_Syntax_Syntax.as_arg a_typ)
in (let _0_571 = (let _0_570 = (FStar_Syntax_Syntax.as_arg wp_a_typ)
in (_0_570)::[])
in (_0_572)::_0_571))
in ((lift_wp), (_0_573)))))) None _0_574))
in (repr_type sub.FStar_Syntax_Syntax.target a_typ lift_wp_a)))
in (

let expected_k = (let _0_581 = (let _0_579 = (FStar_Syntax_Syntax.mk_binder a)
in (let _0_578 = (let _0_577 = (FStar_Syntax_Syntax.mk_binder wp_a)
in (let _0_576 = (let _0_575 = (FStar_Syntax_Syntax.null_binder repr_f)
in (_0_575)::[])
in (_0_577)::_0_576))
in (_0_579)::_0_578))
in (let _0_580 = (FStar_Syntax_Syntax.mk_Total repr_result)
in (FStar_Syntax_Util.arrow _0_581 _0_580)))
in (

let uu____2567 = (FStar_TypeChecker_TcTerm.tc_tot_or_gtot_term env expected_k)
in (match (uu____2567) with
| (expected_k, uu____2573, uu____2574) -> begin
(

let lift = (check_and_gen env lift expected_k)
in Some (lift))
end))))))))
end))
end)
in (

let env = (

let uu___94_2577 = env
in {FStar_TypeChecker_Env.solver = uu___94_2577.FStar_TypeChecker_Env.solver; FStar_TypeChecker_Env.range = uu___94_2577.FStar_TypeChecker_Env.range; FStar_TypeChecker_Env.curmodule = uu___94_2577.FStar_TypeChecker_Env.curmodule; FStar_TypeChecker_Env.gamma = uu___94_2577.FStar_TypeChecker_Env.gamma; FStar_TypeChecker_Env.gamma_cache = uu___94_2577.FStar_TypeChecker_Env.gamma_cache; FStar_TypeChecker_Env.modules = uu___94_2577.FStar_TypeChecker_Env.modules; FStar_TypeChecker_Env.expected_typ = uu___94_2577.FStar_TypeChecker_Env.expected_typ; FStar_TypeChecker_Env.sigtab = uu___94_2577.FStar_TypeChecker_Env.sigtab; FStar_TypeChecker_Env.is_pattern = uu___94_2577.FStar_TypeChecker_Env.is_pattern; FStar_TypeChecker_Env.instantiate_imp = uu___94_2577.FStar_TypeChecker_Env.instantiate_imp; FStar_TypeChecker_Env.effects = uu___94_2577.FStar_TypeChecker_Env.effects; FStar_TypeChecker_Env.generalize = uu___94_2577.FStar_TypeChecker_Env.generalize; FStar_TypeChecker_Env.letrecs = uu___94_2577.FStar_TypeChecker_Env.letrecs; FStar_TypeChecker_Env.top_level = uu___94_2577.FStar_TypeChecker_Env.top_level; FStar_TypeChecker_Env.check_uvars = uu___94_2577.FStar_TypeChecker_Env.check_uvars; FStar_TypeChecker_Env.use_eq = uu___94_2577.FStar_TypeChecker_Env.use_eq; FStar_TypeChecker_Env.is_iface = uu___94_2577.FStar_TypeChecker_Env.is_iface; FStar_TypeChecker_Env.admit = uu___94_2577.FStar_TypeChecker_Env.admit; FStar_TypeChecker_Env.lax = lax; FStar_TypeChecker_Env.lax_universes = uu___94_2577.FStar_TypeChecker_Env.lax_universes; FStar_TypeChecker_Env.type_of = uu___94_2577.FStar_TypeChecker_Env.type_of; FStar_TypeChecker_Env.universe_of = uu___94_2577.FStar_TypeChecker_Env.universe_of; FStar_TypeChecker_Env.use_bv_sorts = uu___94_2577.FStar_TypeChecker_Env.use_bv_sorts; FStar_TypeChecker_Env.qname_and_index = uu___94_2577.FStar_TypeChecker_Env.qname_and_index})
in (

let sub = (

let uu___95_2579 = sub
in {FStar_Syntax_Syntax.source = uu___95_2579.FStar_Syntax_Syntax.source; FStar_Syntax_Syntax.target = uu___95_2579.FStar_Syntax_Syntax.target; FStar_Syntax_Syntax.lift_wp = Some (lift_wp); FStar_Syntax_Syntax.lift = lift})
in (

let se = FStar_Syntax_Syntax.Sig_sub_effect (((sub), (r)))
in (

let env = (FStar_TypeChecker_Env.push_sigelt env se)
in (((se)::[]), (env), ([])))))))))
end)))))
end))
end))))
end
| FStar_Syntax_Syntax.Sig_effect_abbrev (lid, uvs, tps, c, tags, flags, r) -> begin
(

let env0 = env
in (

let env = (FStar_TypeChecker_Env.set_range env r)
in (

let uu____2598 = (FStar_Syntax_Subst.open_comp tps c)
in (match (uu____2598) with
| (tps, c) -> begin
(

let uu____2608 = (FStar_TypeChecker_TcTerm.tc_tparams env tps)
in (match (uu____2608) with
| (tps, env, us) -> begin
(

let uu____2620 = (FStar_TypeChecker_TcTerm.tc_comp env c)
in (match (uu____2620) with
| (c, u, g) -> begin
((FStar_TypeChecker_Rel.force_trivial_guard env g);
(

let tps = (FStar_Syntax_Subst.close_binders tps)
in (

let c = (FStar_Syntax_Subst.close_comp tps c)
in (

let uu____2635 = (let _0_582 = (FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_arrow (((tps), (c)))) None r)
in (FStar_TypeChecker_Util.generalize_universes env0 _0_582))
in (match (uu____2635) with
| (uvs, t) -> begin
(

let uu____2653 = (

let uu____2661 = (let _0_583 = (FStar_Syntax_Subst.compress t).FStar_Syntax_Syntax.n
in ((tps), (_0_583)))
in (match (uu____2661) with
| ([], FStar_Syntax_Syntax.Tm_arrow (uu____2671, c)) -> begin
(([]), (c))
end
| (uu____2695, FStar_Syntax_Syntax.Tm_arrow (tps, c)) -> begin
((tps), (c))
end
| uu____2713 -> begin
(failwith "Impossible")
end))
in (match (uu____2653) with
| (tps, c) -> begin
((match ((((FStar_List.length uvs) <> (Prims.parse_int "1")) && (not ((FStar_Ident.lid_equals lid FStar_Syntax_Const.effect_Lemma_lid))))) with
| true -> begin
(

let uu____2743 = (FStar_Syntax_Subst.open_univ_vars uvs t)
in (match (uu____2743) with
| (uu____2746, t) -> begin
(Prims.raise (FStar_Errors.Error ((let _0_587 = (let _0_586 = (FStar_Syntax_Print.lid_to_string lid)
in (let _0_585 = (FStar_All.pipe_right (FStar_List.length uvs) FStar_Util.string_of_int)
in (let _0_584 = (FStar_Syntax_Print.term_to_string t)
in (FStar_Util.format3 "Effect abbreviations must be polymorphic in exactly 1 universe; %s has %s universes (%s)" _0_586 _0_585 _0_584))))
in ((_0_587), (r))))))
end))
end
| uu____2750 -> begin
()
end);
(

let se = FStar_Syntax_Syntax.Sig_effect_abbrev (((lid), (uvs), (tps), (c), (tags), (flags), (r)))
in (

let env = (FStar_TypeChecker_Env.push_sigelt env0 se)
in (((se)::[]), (env), ([]))));
)
end))
end))));
)
end))
end))
end))))
end
| (FStar_Syntax_Syntax.Sig_declare_typ (_, _, _, quals, _)) | (FStar_Syntax_Syntax.Sig_let (_, _, _, quals, _)) when (FStar_All.pipe_right quals (FStar_Util.for_some (fun uu___76_2779 -> (match (uu___76_2779) with
| FStar_Syntax_Syntax.OnlyName -> begin
true
end
| uu____2780 -> begin
false
end)))) -> begin
(([]), (env), ([]))
end
| FStar_Syntax_Syntax.Sig_declare_typ (lid, uvs, t, quals, r) -> begin
(

let env = (FStar_TypeChecker_Env.set_range env r)
in (

let uu____2791 = (match ((uvs = [])) with
| true -> begin
(let _0_588 = (Prims.fst (FStar_Syntax_Util.type_u ()))
in (check_and_gen env t _0_588))
end
| uu____2792 -> begin
(

let uu____2793 = (FStar_Syntax_Subst.open_univ_vars uvs t)
in (match (uu____2793) with
| (uvs, t) -> begin
(let _0_591 = (let _0_590 = (let _0_589 = (Prims.fst (FStar_Syntax_Util.type_u ()))
in (tc_check_trivial_guard env t _0_589))
in (FStar_All.pipe_left (FStar_Syntax_Subst.close_univ_vars uvs) _0_590))
in ((uvs), (_0_591)))
end))
end)
in (match (uu____2791) with
| (uvs, t) -> begin
(

let se = FStar_Syntax_Syntax.Sig_declare_typ (((lid), (uvs), (t), (quals), (r)))
in (

let env = (FStar_TypeChecker_Env.push_sigelt env se)
in (((se)::[]), (env), ([]))))
end)))
end
| FStar_Syntax_Syntax.Sig_assume (lid, phi, quals, r) -> begin
(

let se = (tc_assume env lid phi quals r)
in (

let env = (FStar_TypeChecker_Env.push_sigelt env se)
in (((se)::[]), (env), ([]))))
end
| FStar_Syntax_Syntax.Sig_main (e, r) -> begin
(

let env = (FStar_TypeChecker_Env.set_range env r)
in (

let env = (FStar_TypeChecker_Env.set_expected_typ env FStar_TypeChecker_Common.t_unit)
in (

let uu____2827 = (FStar_TypeChecker_TcTerm.tc_term env e)
in (match (uu____2827) with
| (e, c, g1) -> begin
(

let uu____2839 = (let _0_594 = Some ((FStar_Syntax_Util.ml_comp FStar_TypeChecker_Common.t_unit r))
in (let _0_593 = (let _0_592 = (c.FStar_Syntax_Syntax.comp ())
in ((e), (_0_592)))
in (FStar_TypeChecker_TcTerm.check_expected_effect env _0_594 _0_593)))
in (match (uu____2839) with
| (e, uu____2851, g) -> begin
((let _0_595 = (FStar_TypeChecker_Rel.conj_guard g1 g)
in (FStar_TypeChecker_Rel.force_trivial_guard env _0_595));
(

let se = FStar_Syntax_Syntax.Sig_main (((e), (r)))
in (

let env = (FStar_TypeChecker_Env.push_sigelt env se)
in (((se)::[]), (env), ([]))));
)
end))
end))))
end
| FStar_Syntax_Syntax.Sig_let (lbs, r, lids, quals, attrs) -> begin
(

let env = (FStar_TypeChecker_Env.set_range env r)
in (

let check_quals_eq = (fun l qopt q -> (match (qopt) with
| None -> begin
Some (q)
end
| Some (q') -> begin
(

let uu____2895 = (((FStar_List.length q) = (FStar_List.length q')) && (FStar_List.forall2 FStar_Syntax_Util.qualifier_equal q q'))
in (match (uu____2895) with
| true -> begin
Some (q)
end
| uu____2903 -> begin
(Prims.raise (FStar_Errors.Error ((let _0_599 = (let _0_598 = (FStar_Syntax_Print.lid_to_string l)
in (let _0_597 = (FStar_Syntax_Print.quals_to_string q)
in (let _0_596 = (FStar_Syntax_Print.quals_to_string q')
in (FStar_Util.format3 "Inconsistent qualifier annotations on %s; Expected {%s}, got {%s}" _0_598 _0_597 _0_596))))
in ((_0_599), (r))))))
end))
end))
in (

let uu____2906 = (FStar_All.pipe_right (Prims.snd lbs) (FStar_List.fold_left (fun uu____2927 lb -> (match (uu____2927) with
| (gen, lbs, quals_opt) -> begin
(

let lbname = (FStar_Util.right lb.FStar_Syntax_Syntax.lbname)
in (

let uu____2951 = (

let uu____2957 = (FStar_TypeChecker_Env.try_lookup_val_decl env lbname.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v)
in (match (uu____2957) with
| None -> begin
(match ((lb.FStar_Syntax_Syntax.lbunivs <> [])) with
| true -> begin
((false), (lb), (quals_opt))
end
| uu____2982 -> begin
((gen), (lb), (quals_opt))
end)
end
| Some ((uvs, tval), quals) -> begin
(

let quals_opt = (check_quals_eq lbname.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v quals_opt quals)
in ((match (lb.FStar_Syntax_Syntax.lbtyp.FStar_Syntax_Syntax.n) with
| FStar_Syntax_Syntax.Tm_unknown -> begin
()
end
| uu____3009 -> begin
(FStar_Errors.warn r "Annotation from val declaration overrides inline type annotation")
end);
(match (((lb.FStar_Syntax_Syntax.lbunivs <> []) && ((FStar_List.length lb.FStar_Syntax_Syntax.lbunivs) <> (FStar_List.length uvs)))) with
| true -> begin
(Prims.raise (FStar_Errors.Error ((("Inline universes are incoherent with annotation from val declaration"), (r)))))
end
| uu____3016 -> begin
()
end);
(let _0_600 = (FStar_Syntax_Syntax.mk_lb ((FStar_Util.Inr (lbname)), (uvs), (FStar_Syntax_Const.effect_ALL_lid), (tval), (lb.FStar_Syntax_Syntax.lbdef)))
in ((false), (_0_600), (quals_opt)));
))
end))
in (match (uu____2951) with
| (gen, lb, quals_opt) -> begin
((gen), ((lb)::lbs), (quals_opt))
end)))
end)) ((true), ([]), ((match ((quals = [])) with
| true -> begin
None
end
| uu____3047 -> begin
Some (quals)
end)))))
in (match (uu____2906) with
| (should_generalize, lbs', quals_opt) -> begin
(

let quals = (match (quals_opt) with
| None -> begin
(FStar_Syntax_Syntax.Visible_default)::[]
end
| Some (q) -> begin
(

let uu____3070 = (FStar_All.pipe_right q (FStar_Util.for_some (fun uu___77_3072 -> (match (uu___77_3072) with
| (FStar_Syntax_Syntax.Irreducible) | (FStar_Syntax_Syntax.Visible_default) | (FStar_Syntax_Syntax.Unfold_for_unification_and_vcgen) -> begin
true
end
| uu____3073 -> begin
false
end))))
in (match (uu____3070) with
| true -> begin
q
end
| uu____3075 -> begin
(FStar_Syntax_Syntax.Visible_default)::q
end))
end)
in (

let lbs' = (FStar_List.rev lbs')
in (

let e = ((FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_let ((let _0_601 = (FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_constant (FStar_Const.Const_unit)) None r)
in (((((Prims.fst lbs)), (lbs'))), (_0_601)))))) None r)
in (

let uu____3104 = (

let uu____3110 = (FStar_TypeChecker_TcTerm.tc_maybe_toplevel_term (

let uu___96_3114 = env
in {FStar_TypeChecker_Env.solver = uu___96_3114.FStar_TypeChecker_Env.solver; FStar_TypeChecker_Env.range = uu___96_3114.FStar_TypeChecker_Env.range; FStar_TypeChecker_Env.curmodule = uu___96_3114.FStar_TypeChecker_Env.curmodule; FStar_TypeChecker_Env.gamma = uu___96_3114.FStar_TypeChecker_Env.gamma; FStar_TypeChecker_Env.gamma_cache = uu___96_3114.FStar_TypeChecker_Env.gamma_cache; FStar_TypeChecker_Env.modules = uu___96_3114.FStar_TypeChecker_Env.modules; FStar_TypeChecker_Env.expected_typ = uu___96_3114.FStar_TypeChecker_Env.expected_typ; FStar_TypeChecker_Env.sigtab = uu___96_3114.FStar_TypeChecker_Env.sigtab; FStar_TypeChecker_Env.is_pattern = uu___96_3114.FStar_TypeChecker_Env.is_pattern; FStar_TypeChecker_Env.instantiate_imp = uu___96_3114.FStar_TypeChecker_Env.instantiate_imp; FStar_TypeChecker_Env.effects = uu___96_3114.FStar_TypeChecker_Env.effects; FStar_TypeChecker_Env.generalize = should_generalize; FStar_TypeChecker_Env.letrecs = uu___96_3114.FStar_TypeChecker_Env.letrecs; FStar_TypeChecker_Env.top_level = true; FStar_TypeChecker_Env.check_uvars = uu___96_3114.FStar_TypeChecker_Env.check_uvars; FStar_TypeChecker_Env.use_eq = uu___96_3114.FStar_TypeChecker_Env.use_eq; FStar_TypeChecker_Env.is_iface = uu___96_3114.FStar_TypeChecker_Env.is_iface; FStar_TypeChecker_Env.admit = uu___96_3114.FStar_TypeChecker_Env.admit; FStar_TypeChecker_Env.lax = uu___96_3114.FStar_TypeChecker_Env.lax; FStar_TypeChecker_Env.lax_universes = uu___96_3114.FStar_TypeChecker_Env.lax_universes; FStar_TypeChecker_Env.type_of = uu___96_3114.FStar_TypeChecker_Env.type_of; FStar_TypeChecker_Env.universe_of = uu___96_3114.FStar_TypeChecker_Env.universe_of; FStar_TypeChecker_Env.use_bv_sorts = uu___96_3114.FStar_TypeChecker_Env.use_bv_sorts; FStar_TypeChecker_Env.qname_and_index = uu___96_3114.FStar_TypeChecker_Env.qname_and_index}) e)
in (match (uu____3110) with
| ({FStar_Syntax_Syntax.n = FStar_Syntax_Syntax.Tm_let (lbs, e); FStar_Syntax_Syntax.tk = uu____3122; FStar_Syntax_Syntax.pos = uu____3123; FStar_Syntax_Syntax.vars = uu____3124}, uu____3125, g) when (FStar_TypeChecker_Rel.is_trivial g) -> begin
(

let quals = (match (e.FStar_Syntax_Syntax.n) with
| FStar_Syntax_Syntax.Tm_meta (uu____3144, FStar_Syntax_Syntax.Meta_desugared (FStar_Syntax_Syntax.Masked_effect)) -> begin
(FStar_Syntax_Syntax.HasMaskedEffect)::quals
end
| uu____3149 -> begin
quals
end)
in ((FStar_Syntax_Syntax.Sig_let (((lbs), (r), (lids), (quals), (attrs)))), (lbs)))
end
| uu____3159 -> begin
(failwith "impossible")
end))
in (match (uu____3104) with
| (se, lbs) -> begin
((

let uu____3182 = (log env)
in (match (uu____3182) with
| true -> begin
(let _0_606 = (let _0_605 = (FStar_All.pipe_right (Prims.snd lbs) (FStar_List.map (fun lb -> (

let should_log = (

let uu____3189 = (let _0_602 = (FStar_Util.right lb.FStar_Syntax_Syntax.lbname).FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v
in (FStar_TypeChecker_Env.try_lookup_val_decl env _0_602))
in (match (uu____3189) with
| None -> begin
true
end
| uu____3201 -> begin
false
end))
in (match (should_log) with
| true -> begin
(let _0_604 = (FStar_Syntax_Print.lbname_to_string lb.FStar_Syntax_Syntax.lbname)
in (let _0_603 = (FStar_Syntax_Print.term_to_string lb.FStar_Syntax_Syntax.lbtyp)
in (FStar_Util.format2 "let %s : %s" _0_604 _0_603)))
end
| uu____3206 -> begin
""
end)))))
in (FStar_All.pipe_right _0_605 (FStar_String.concat "\n")))
in (FStar_Util.print1 "%s\n" _0_606))
end
| uu____3207 -> begin
()
end));
(

let env = (FStar_TypeChecker_Env.push_sigelt env se)
in (((se)::[]), (env), ([])));
)
end)))))
end))))
end);
)))


let for_export : FStar_Ident.lident Prims.list  ->  FStar_Syntax_Syntax.sigelt  ->  (FStar_Syntax_Syntax.sigelt Prims.list * FStar_Ident.lident Prims.list) = (fun hidden se -> (

let is_abstract = (fun quals -> (FStar_All.pipe_right quals (FStar_Util.for_some (fun uu___78_3234 -> (match (uu___78_3234) with
| FStar_Syntax_Syntax.Abstract -> begin
true
end
| uu____3235 -> begin
false
end)))))
in (

let is_hidden_proj_or_disc = (fun q -> (match (q) with
| (FStar_Syntax_Syntax.Projector (l, _)) | (FStar_Syntax_Syntax.Discriminator (l)) -> begin
(FStar_All.pipe_right hidden (FStar_Util.for_some (FStar_Ident.lid_equals l)))
end
| uu____3243 -> begin
false
end))
in (match (se) with
| FStar_Syntax_Syntax.Sig_pragma (uu____3248) -> begin
(([]), (hidden))
end
| (FStar_Syntax_Syntax.Sig_inductive_typ (_)) | (FStar_Syntax_Syntax.Sig_datacon (_)) -> begin
(failwith "Impossible")
end
| FStar_Syntax_Syntax.Sig_bundle (ses, quals, uu____3261, r) -> begin
(

let uu____3269 = (is_abstract quals)
in (match (uu____3269) with
| true -> begin
(FStar_List.fold_right (fun se uu____3279 -> (match (uu____3279) with
| (out, hidden) -> begin
(match (se) with
| FStar_Syntax_Syntax.Sig_inductive_typ (l, us, bs, t, uu____3302, uu____3303, quals, r) -> begin
(

let dec = FStar_Syntax_Syntax.Sig_declare_typ ((let _0_608 = (let _0_607 = (FStar_Syntax_Syntax.mk_Total t)
in (FStar_Syntax_Util.arrow bs _0_607))
in ((l), (us), (_0_608), ((FStar_Syntax_Syntax.Assumption)::(FStar_Syntax_Syntax.New)::quals), (r))))
in (((dec)::out), (hidden)))
end
| FStar_Syntax_Syntax.Sig_datacon (l, us, t, uu____3321, uu____3322, uu____3323, uu____3324, r) -> begin
(

let dec = FStar_Syntax_Syntax.Sig_declare_typ (((l), (us), (t), ((FStar_Syntax_Syntax.Assumption)::[]), (r)))
in (((dec)::out), ((l)::hidden)))
end
| uu____3334 -> begin
((out), (hidden))
end)
end)) ses (([]), (hidden)))
end
| uu____3339 -> begin
(((se)::[]), (hidden))
end))
end
| FStar_Syntax_Syntax.Sig_assume (uu____3342, uu____3343, quals, uu____3345) -> begin
(

let uu____3348 = (is_abstract quals)
in (match (uu____3348) with
| true -> begin
(([]), (hidden))
end
| uu____3355 -> begin
(((se)::[]), (hidden))
end))
end
| FStar_Syntax_Syntax.Sig_declare_typ (l, us, t, quals, r) -> begin
(

let uu____3365 = (FStar_All.pipe_right quals (FStar_Util.for_some is_hidden_proj_or_disc))
in (match (uu____3365) with
| true -> begin
(((FStar_Syntax_Syntax.Sig_declare_typ (((l), (us), (t), ((FStar_Syntax_Syntax.Assumption)::[]), (r))))::[]), ((l)::hidden))
end
| uu____3374 -> begin
(

let uu____3375 = (FStar_All.pipe_right quals (FStar_Util.for_some (fun uu___79_3377 -> (match (uu___79_3377) with
| (FStar_Syntax_Syntax.Assumption) | (FStar_Syntax_Syntax.Projector (_)) | (FStar_Syntax_Syntax.Discriminator (_)) -> begin
true
end
| uu____3380 -> begin
false
end))))
in (match (uu____3375) with
| true -> begin
(((se)::[]), (hidden))
end
| uu____3387 -> begin
(([]), (hidden))
end))
end))
end
| FStar_Syntax_Syntax.Sig_main (uu____3390) -> begin
(([]), (hidden))
end
| (FStar_Syntax_Syntax.Sig_new_effect (_)) | (FStar_Syntax_Syntax.Sig_new_effect_for_free (_)) | (FStar_Syntax_Syntax.Sig_sub_effect (_)) | (FStar_Syntax_Syntax.Sig_effect_abbrev (_)) -> begin
(((se)::[]), (hidden))
end
| FStar_Syntax_Syntax.Sig_let ((false, (lb)::[]), uu____3402, uu____3403, quals, uu____3405) when (FStar_All.pipe_right quals (FStar_Util.for_some is_hidden_proj_or_disc)) -> begin
(

let fv = (FStar_Util.right lb.FStar_Syntax_Syntax.lbname)
in (

let lid = fv.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v
in (

let uu____3423 = (FStar_All.pipe_right hidden (FStar_Util.for_some (FStar_Syntax_Syntax.fv_eq_lid fv)))
in (match (uu____3423) with
| true -> begin
(([]), (hidden))
end
| uu____3431 -> begin
(

let dec = FStar_Syntax_Syntax.Sig_declare_typ (((fv.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v), (lb.FStar_Syntax_Syntax.lbunivs), (lb.FStar_Syntax_Syntax.lbtyp), ((FStar_Syntax_Syntax.Assumption)::[]), ((FStar_Ident.range_of_lid lid))))
in (((dec)::[]), ((lid)::hidden)))
end))))
end
| FStar_Syntax_Syntax.Sig_let (lbs, r, l, quals, uu____3447) -> begin
(

let uu____3454 = (is_abstract quals)
in (match (uu____3454) with
| true -> begin
(let _0_610 = (FStar_All.pipe_right (Prims.snd lbs) (FStar_List.map (fun lb -> FStar_Syntax_Syntax.Sig_declare_typ ((let _0_609 = (FStar_Util.right lb.FStar_Syntax_Syntax.lbname).FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v
in ((_0_609), (lb.FStar_Syntax_Syntax.lbunivs), (lb.FStar_Syntax_Syntax.lbtyp), ((FStar_Syntax_Syntax.Assumption)::quals), (r)))))))
in ((_0_610), (hidden)))
end
| uu____3473 -> begin
(((se)::[]), (hidden))
end))
end))))


let tc_decls : FStar_TypeChecker_Env.env_t  ->  FStar_Syntax_Syntax.sigelt Prims.list  ->  (FStar_Syntax_Syntax.sigelt Prims.list * FStar_Syntax_Syntax.sigelt Prims.list * FStar_TypeChecker_Env.env_t) = (fun env ses -> (

let rec process_one_decl = (fun uu____3510 se -> (match (uu____3510) with
| (ses, exports, env, hidden) -> begin
((

let uu____3541 = (FStar_TypeChecker_Env.debug env FStar_Options.Low)
in (match (uu____3541) with
| true -> begin
(let _0_611 = (FStar_Syntax_Print.sigelt_to_string se)
in (FStar_Util.print1 ">>>>>>>>>>>>>>Checking top-level decl %s\n" _0_611))
end
| uu____3542 -> begin
()
end));
(

let uu____3543 = (tc_decl env se)
in (match (uu____3543) with
| (ses', env, ses_elaborated) -> begin
((

let uu____3565 = ((FStar_Options.log_types ()) || (FStar_All.pipe_left (FStar_TypeChecker_Env.debug env) (FStar_Options.Other ("LogTypes"))))
in (match (uu____3565) with
| true -> begin
(let _0_614 = (FStar_List.fold_left (fun s se -> (let _0_613 = (let _0_612 = (FStar_Syntax_Print.sigelt_to_string se)
in (Prims.strcat _0_612 "\n"))
in (Prims.strcat s _0_613))) "" ses')
in (FStar_Util.print1 "Checked: %s\n" _0_614))
end
| uu____3568 -> begin
()
end));
(FStar_List.iter (fun se -> (env.FStar_TypeChecker_Env.solver.FStar_TypeChecker_Env.encode_sig env se)) ses');
(

let uu____3571 = (FStar_List.fold_left (fun uu____3580 se -> (match (uu____3580) with
| (le, lh) -> begin
(

let tup = (for_export hidden se)
in (((FStar_List.rev_append (Prims.fst tup) le)), ((FStar_List.rev_append (Prims.snd tup) lh))))
end)) (([]), ([])) ses')
in (match (uu____3571) with
| (exported, hidden) -> begin
(FStar_List.fold_left process_one_decl (((FStar_List.rev_append ses' ses)), (((FStar_List.rev_append exported []))::exports), (env), (hidden)) ses_elaborated)
end));
)
end));
)
end))
in (

let uu____3636 = (FStar_List.fold_left (fun acc se -> (match (se) with
| FStar_Syntax_Syntax.Sig_new_effect_for_free (ne, r) -> begin
(

let uu____3673 = acc
in (match (uu____3673) with
| (uu____3690, uu____3691, env, uu____3693) -> begin
(

let uu____3702 = (cps_and_elaborate env ne)
in (match (uu____3702) with
| (ses, ne, lift_from_pure_opt) -> begin
(

let ses = (match (lift_from_pure_opt) with
| Some (lift) -> begin
(FStar_List.append ses ((FStar_Syntax_Syntax.Sig_new_effect (((ne), (r))))::(lift)::[]))
end
| None -> begin
(FStar_List.append ses ((FStar_Syntax_Syntax.Sig_new_effect (((ne), (r))))::[]))
end)
in (FStar_List.fold_left process_one_decl acc ses))
end))
end))
end
| uu____3735 -> begin
(process_one_decl acc se)
end)) (([]), ([]), (env), ([])) ses)
in (match (uu____3636) with
| (ses, exports, env, uu____3749) -> begin
(let _0_615 = (FStar_All.pipe_right (FStar_List.rev_append exports []) FStar_List.flatten)
in (((FStar_List.rev_append ses [])), (_0_615), (env)))
end))))


let tc_partial_modul : FStar_TypeChecker_Env.env  ->  FStar_Syntax_Syntax.modul  ->  (FStar_Syntax_Syntax.modul * FStar_Syntax_Syntax.sigelt Prims.list * FStar_TypeChecker_Env.env) = (fun env modul -> (

let verify = (FStar_Options.should_verify modul.FStar_Syntax_Syntax.name.FStar_Ident.str)
in (

let action = (match (verify) with
| true -> begin
"Verifying"
end
| uu____3777 -> begin
"Lax-checking"
end)
in (

let label = (match (modul.FStar_Syntax_Syntax.is_interface) with
| true -> begin
"interface"
end
| uu____3779 -> begin
"implementation"
end)
in ((

let uu____3781 = (FStar_Options.debug_any ())
in (match (uu____3781) with
| true -> begin
(FStar_Util.print3 "%s %s of %s\n" action label modul.FStar_Syntax_Syntax.name.FStar_Ident.str)
end
| uu____3782 -> begin
()
end));
(

let name = (FStar_Util.format2 "%s %s" (match (modul.FStar_Syntax_Syntax.is_interface) with
| true -> begin
"interface"
end
| uu____3784 -> begin
"module"
end) modul.FStar_Syntax_Syntax.name.FStar_Ident.str)
in (

let msg = (Prims.strcat "Internals for " name)
in (

let env = (

let uu___97_3787 = env
in {FStar_TypeChecker_Env.solver = uu___97_3787.FStar_TypeChecker_Env.solver; FStar_TypeChecker_Env.range = uu___97_3787.FStar_TypeChecker_Env.range; FStar_TypeChecker_Env.curmodule = uu___97_3787.FStar_TypeChecker_Env.curmodule; FStar_TypeChecker_Env.gamma = uu___97_3787.FStar_TypeChecker_Env.gamma; FStar_TypeChecker_Env.gamma_cache = uu___97_3787.FStar_TypeChecker_Env.gamma_cache; FStar_TypeChecker_Env.modules = uu___97_3787.FStar_TypeChecker_Env.modules; FStar_TypeChecker_Env.expected_typ = uu___97_3787.FStar_TypeChecker_Env.expected_typ; FStar_TypeChecker_Env.sigtab = uu___97_3787.FStar_TypeChecker_Env.sigtab; FStar_TypeChecker_Env.is_pattern = uu___97_3787.FStar_TypeChecker_Env.is_pattern; FStar_TypeChecker_Env.instantiate_imp = uu___97_3787.FStar_TypeChecker_Env.instantiate_imp; FStar_TypeChecker_Env.effects = uu___97_3787.FStar_TypeChecker_Env.effects; FStar_TypeChecker_Env.generalize = uu___97_3787.FStar_TypeChecker_Env.generalize; FStar_TypeChecker_Env.letrecs = uu___97_3787.FStar_TypeChecker_Env.letrecs; FStar_TypeChecker_Env.top_level = uu___97_3787.FStar_TypeChecker_Env.top_level; FStar_TypeChecker_Env.check_uvars = uu___97_3787.FStar_TypeChecker_Env.check_uvars; FStar_TypeChecker_Env.use_eq = uu___97_3787.FStar_TypeChecker_Env.use_eq; FStar_TypeChecker_Env.is_iface = modul.FStar_Syntax_Syntax.is_interface; FStar_TypeChecker_Env.admit = (not (verify)); FStar_TypeChecker_Env.lax = uu___97_3787.FStar_TypeChecker_Env.lax; FStar_TypeChecker_Env.lax_universes = uu___97_3787.FStar_TypeChecker_Env.lax_universes; FStar_TypeChecker_Env.type_of = uu___97_3787.FStar_TypeChecker_Env.type_of; FStar_TypeChecker_Env.universe_of = uu___97_3787.FStar_TypeChecker_Env.universe_of; FStar_TypeChecker_Env.use_bv_sorts = uu___97_3787.FStar_TypeChecker_Env.use_bv_sorts; FStar_TypeChecker_Env.qname_and_index = uu___97_3787.FStar_TypeChecker_Env.qname_and_index})
in ((env.FStar_TypeChecker_Env.solver.FStar_TypeChecker_Env.push msg);
(

let env = (FStar_TypeChecker_Env.set_current_module env modul.FStar_Syntax_Syntax.name)
in (

let uu____3790 = (tc_decls env modul.FStar_Syntax_Syntax.declarations)
in (match (uu____3790) with
| (ses, exports, env) -> begin
(((

let uu___98_3808 = modul
in {FStar_Syntax_Syntax.name = uu___98_3808.FStar_Syntax_Syntax.name; FStar_Syntax_Syntax.declarations = ses; FStar_Syntax_Syntax.exports = uu___98_3808.FStar_Syntax_Syntax.exports; FStar_Syntax_Syntax.is_interface = uu___98_3808.FStar_Syntax_Syntax.is_interface})), (exports), (env))
end)));
))));
)))))


let tc_more_partial_modul : FStar_TypeChecker_Env.env  ->  FStar_Syntax_Syntax.modul  ->  FStar_Syntax_Syntax.sigelt Prims.list  ->  (FStar_Syntax_Syntax.modul * FStar_Syntax_Syntax.sigelt Prims.list * FStar_TypeChecker_Env.env) = (fun env modul decls -> (

let uu____3824 = (tc_decls env decls)
in (match (uu____3824) with
| (ses, exports, env) -> begin
(

let modul = (

let uu___99_3842 = modul
in {FStar_Syntax_Syntax.name = uu___99_3842.FStar_Syntax_Syntax.name; FStar_Syntax_Syntax.declarations = (FStar_List.append modul.FStar_Syntax_Syntax.declarations ses); FStar_Syntax_Syntax.exports = uu___99_3842.FStar_Syntax_Syntax.exports; FStar_Syntax_Syntax.is_interface = uu___99_3842.FStar_Syntax_Syntax.is_interface})
in ((modul), (exports), (env)))
end)))


let check_exports : FStar_TypeChecker_Env.env  ->  FStar_Syntax_Syntax.modul  ->  FStar_Syntax_Syntax.sigelt Prims.list  ->  Prims.unit = (fun env modul exports -> (

let env = (

let uu___100_3856 = env
in {FStar_TypeChecker_Env.solver = uu___100_3856.FStar_TypeChecker_Env.solver; FStar_TypeChecker_Env.range = uu___100_3856.FStar_TypeChecker_Env.range; FStar_TypeChecker_Env.curmodule = uu___100_3856.FStar_TypeChecker_Env.curmodule; FStar_TypeChecker_Env.gamma = uu___100_3856.FStar_TypeChecker_Env.gamma; FStar_TypeChecker_Env.gamma_cache = uu___100_3856.FStar_TypeChecker_Env.gamma_cache; FStar_TypeChecker_Env.modules = uu___100_3856.FStar_TypeChecker_Env.modules; FStar_TypeChecker_Env.expected_typ = uu___100_3856.FStar_TypeChecker_Env.expected_typ; FStar_TypeChecker_Env.sigtab = uu___100_3856.FStar_TypeChecker_Env.sigtab; FStar_TypeChecker_Env.is_pattern = uu___100_3856.FStar_TypeChecker_Env.is_pattern; FStar_TypeChecker_Env.instantiate_imp = uu___100_3856.FStar_TypeChecker_Env.instantiate_imp; FStar_TypeChecker_Env.effects = uu___100_3856.FStar_TypeChecker_Env.effects; FStar_TypeChecker_Env.generalize = uu___100_3856.FStar_TypeChecker_Env.generalize; FStar_TypeChecker_Env.letrecs = uu___100_3856.FStar_TypeChecker_Env.letrecs; FStar_TypeChecker_Env.top_level = true; FStar_TypeChecker_Env.check_uvars = uu___100_3856.FStar_TypeChecker_Env.check_uvars; FStar_TypeChecker_Env.use_eq = uu___100_3856.FStar_TypeChecker_Env.use_eq; FStar_TypeChecker_Env.is_iface = uu___100_3856.FStar_TypeChecker_Env.is_iface; FStar_TypeChecker_Env.admit = uu___100_3856.FStar_TypeChecker_Env.admit; FStar_TypeChecker_Env.lax = true; FStar_TypeChecker_Env.lax_universes = true; FStar_TypeChecker_Env.type_of = uu___100_3856.FStar_TypeChecker_Env.type_of; FStar_TypeChecker_Env.universe_of = uu___100_3856.FStar_TypeChecker_Env.universe_of; FStar_TypeChecker_Env.use_bv_sorts = uu___100_3856.FStar_TypeChecker_Env.use_bv_sorts; FStar_TypeChecker_Env.qname_and_index = uu___100_3856.FStar_TypeChecker_Env.qname_and_index})
in (

let check_term = (fun lid univs t -> (

let uu____3867 = (FStar_Syntax_Subst.open_univ_vars univs t)
in (match (uu____3867) with
| (univs, t) -> begin
((

let uu____3873 = (let _0_616 = (FStar_TypeChecker_Env.debug (FStar_TypeChecker_Env.set_current_module env modul.FStar_Syntax_Syntax.name))
in (FStar_All.pipe_left _0_616 (FStar_Options.Other ("Exports"))))
in (match (uu____3873) with
| true -> begin
(let _0_620 = (FStar_Syntax_Print.lid_to_string lid)
in (let _0_619 = (let _0_617 = (FStar_All.pipe_right univs (FStar_List.map (fun x -> (FStar_Syntax_Print.univ_to_string (FStar_Syntax_Syntax.U_name (x))))))
in (FStar_All.pipe_right _0_617 (FStar_String.concat ", ")))
in (let _0_618 = (FStar_Syntax_Print.term_to_string t)
in (FStar_Util.print3 "Checking for export %s <%s> : %s\n" _0_620 _0_619 _0_618))))
end
| uu____3877 -> begin
()
end));
(

let env = (FStar_TypeChecker_Env.push_univ_vars env univs)
in (let _0_621 = (FStar_TypeChecker_TcTerm.tc_trivial_guard env t)
in (FStar_All.pipe_right _0_621 Prims.ignore)));
)
end)))
in (

let check_term = (fun lid univs t -> ((FStar_Errors.message_prefix.FStar_Errors.set_prefix (let _0_623 = (FStar_Syntax_Print.lid_to_string modul.FStar_Syntax_Syntax.name)
in (let _0_622 = (FStar_Syntax_Print.lid_to_string lid)
in (FStar_Util.format2 "Interface of %s violates its abstraction (add a \'private\' qualifier to \'%s\'?)" _0_623 _0_622))));
(check_term lid univs t);
(FStar_Errors.message_prefix.FStar_Errors.clear_prefix ());
))
in (

let rec check_sigelt = (fun uu___80_3898 -> (match (uu___80_3898) with
| FStar_Syntax_Syntax.Sig_bundle (ses, quals, uu____3901, uu____3902) -> begin
(

let uu____3909 = (not ((FStar_All.pipe_right quals (FStar_List.contains FStar_Syntax_Syntax.Private))))
in (match (uu____3909) with
| true -> begin
(FStar_All.pipe_right ses (FStar_List.iter check_sigelt))
end
| uu____3912 -> begin
()
end))
end
| FStar_Syntax_Syntax.Sig_inductive_typ (l, univs, binders, typ, uu____3917, uu____3918, uu____3919, r) -> begin
(

let t = ((FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_arrow ((let _0_624 = (FStar_Syntax_Syntax.mk_Total typ)
in ((binders), (_0_624)))))) None r)
in (check_term l univs t))
end
| FStar_Syntax_Syntax.Sig_datacon (l, univs, t, uu____3941, uu____3942, uu____3943, uu____3944, uu____3945) -> begin
(check_term l univs t)
end
| FStar_Syntax_Syntax.Sig_declare_typ (l, univs, t, quals, uu____3954) -> begin
(

let uu____3957 = (not ((FStar_All.pipe_right quals (FStar_List.contains FStar_Syntax_Syntax.Private))))
in (match (uu____3957) with
| true -> begin
(check_term l univs t)
end
| uu____3959 -> begin
()
end))
end
| FStar_Syntax_Syntax.Sig_let ((uu____3960, lbs), uu____3962, uu____3963, quals, uu____3965) -> begin
(

let uu____3977 = (not ((FStar_All.pipe_right quals (FStar_List.contains FStar_Syntax_Syntax.Private))))
in (match (uu____3977) with
| true -> begin
(FStar_All.pipe_right lbs (FStar_List.iter (fun lb -> (

let fv = (FStar_Util.right lb.FStar_Syntax_Syntax.lbname)
in (check_term fv.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v lb.FStar_Syntax_Syntax.lbunivs lb.FStar_Syntax_Syntax.lbtyp)))))
end
| uu____3986 -> begin
()
end))
end
| FStar_Syntax_Syntax.Sig_effect_abbrev (l, univs, binders, comp, quals, flags, r) -> begin
(

let uu____3998 = (not ((FStar_All.pipe_right quals (FStar_List.contains FStar_Syntax_Syntax.Private))))
in (match (uu____3998) with
| true -> begin
(

let arrow = ((FStar_Syntax_Syntax.mk (FStar_Syntax_Syntax.Tm_arrow (((binders), (comp))))) None r)
in (check_term l univs arrow))
end
| uu____4011 -> begin
()
end))
end
| (FStar_Syntax_Syntax.Sig_main (_)) | (FStar_Syntax_Syntax.Sig_assume (_)) | (FStar_Syntax_Syntax.Sig_new_effect (_)) | (FStar_Syntax_Syntax.Sig_new_effect_for_free (_)) | (FStar_Syntax_Syntax.Sig_sub_effect (_)) | (FStar_Syntax_Syntax.Sig_pragma (_)) -> begin
()
end))
in (match ((FStar_Ident.lid_equals modul.FStar_Syntax_Syntax.name FStar_Syntax_Const.prims_lid)) with
| true -> begin
()
end
| uu____4018 -> begin
(FStar_List.iter check_sigelt exports)
end))))))


let finish_partial_modul : FStar_TypeChecker_Env.env  ->  FStar_Syntax_Syntax.modul  ->  FStar_Syntax_Syntax.sigelts  ->  Prims.bool  ->  (FStar_Syntax_Syntax.modul * FStar_TypeChecker_Env.env) = (fun env modul exports encode -> (

let modul = (

let uu___101_4034 = modul
in {FStar_Syntax_Syntax.name = uu___101_4034.FStar_Syntax_Syntax.name; FStar_Syntax_Syntax.declarations = uu___101_4034.FStar_Syntax_Syntax.declarations; FStar_Syntax_Syntax.exports = exports; FStar_Syntax_Syntax.is_interface = modul.FStar_Syntax_Syntax.is_interface})
in (

let env = (FStar_TypeChecker_Env.finish_module env modul)
in ((

let uu____4037 = (not ((FStar_Options.lax ())))
in (match (uu____4037) with
| true -> begin
(check_exports env modul exports)
end
| uu____4038 -> begin
()
end));
(env.FStar_TypeChecker_Env.solver.FStar_TypeChecker_Env.pop (Prims.strcat "Ending modul " modul.FStar_Syntax_Syntax.name.FStar_Ident.str));
(match (encode) with
| true -> begin
((env.FStar_TypeChecker_Env.solver.FStar_TypeChecker_Env.encode_modul env modul);
(env.FStar_TypeChecker_Env.solver.FStar_TypeChecker_Env.refresh ());
)
end
| uu____4042 -> begin
()
end);
(

let uu____4044 = (not ((FStar_Options.interactive ())))
in (match (uu____4044) with
| true -> begin
(let _0_625 = (FStar_Options.restore_cmd_line_options true)
in (FStar_All.pipe_right _0_625 Prims.ignore))
end
| uu____4045 -> begin
()
end));
((modul), (env));
))))


let tc_modul : FStar_TypeChecker_Env.env  ->  FStar_Syntax_Syntax.modul  ->  Prims.bool  ->  (FStar_Syntax_Syntax.modul * FStar_TypeChecker_Env.env) = (fun env modul encode -> (

let uu____4057 = (tc_partial_modul env modul)
in (match (uu____4057) with
| (modul, non_private_decls, env) -> begin
(finish_partial_modul env modul non_private_decls encode)
end)))


let check_module : FStar_TypeChecker_Env.env  ->  FStar_Syntax_Syntax.modul  ->  (FStar_Syntax_Syntax.modul * FStar_TypeChecker_Env.env) = (fun env m -> (

let env0 = env
in (

let env = (

let uu___102_4079 = env
in {FStar_TypeChecker_Env.solver = uu___102_4079.FStar_TypeChecker_Env.solver; FStar_TypeChecker_Env.range = uu___102_4079.FStar_TypeChecker_Env.range; FStar_TypeChecker_Env.curmodule = uu___102_4079.FStar_TypeChecker_Env.curmodule; FStar_TypeChecker_Env.gamma = uu___102_4079.FStar_TypeChecker_Env.gamma; FStar_TypeChecker_Env.gamma_cache = uu___102_4079.FStar_TypeChecker_Env.gamma_cache; FStar_TypeChecker_Env.modules = uu___102_4079.FStar_TypeChecker_Env.modules; FStar_TypeChecker_Env.expected_typ = uu___102_4079.FStar_TypeChecker_Env.expected_typ; FStar_TypeChecker_Env.sigtab = uu___102_4079.FStar_TypeChecker_Env.sigtab; FStar_TypeChecker_Env.is_pattern = uu___102_4079.FStar_TypeChecker_Env.is_pattern; FStar_TypeChecker_Env.instantiate_imp = uu___102_4079.FStar_TypeChecker_Env.instantiate_imp; FStar_TypeChecker_Env.effects = uu___102_4079.FStar_TypeChecker_Env.effects; FStar_TypeChecker_Env.generalize = uu___102_4079.FStar_TypeChecker_Env.generalize; FStar_TypeChecker_Env.letrecs = uu___102_4079.FStar_TypeChecker_Env.letrecs; FStar_TypeChecker_Env.top_level = uu___102_4079.FStar_TypeChecker_Env.top_level; FStar_TypeChecker_Env.check_uvars = uu___102_4079.FStar_TypeChecker_Env.check_uvars; FStar_TypeChecker_Env.use_eq = uu___102_4079.FStar_TypeChecker_Env.use_eq; FStar_TypeChecker_Env.is_iface = uu___102_4079.FStar_TypeChecker_Env.is_iface; FStar_TypeChecker_Env.admit = uu___102_4079.FStar_TypeChecker_Env.admit; FStar_TypeChecker_Env.lax = true; FStar_TypeChecker_Env.lax_universes = uu___102_4079.FStar_TypeChecker_Env.lax_universes; FStar_TypeChecker_Env.type_of = uu___102_4079.FStar_TypeChecker_Env.type_of; FStar_TypeChecker_Env.universe_of = uu___102_4079.FStar_TypeChecker_Env.universe_of; FStar_TypeChecker_Env.use_bv_sorts = uu___102_4079.FStar_TypeChecker_Env.use_bv_sorts; FStar_TypeChecker_Env.qname_and_index = uu___102_4079.FStar_TypeChecker_Env.qname_and_index})
in ((FStar_Options.set_option "lax" (FStar_Options.Bool (true)));
(

let let_quals = (FStar_List.fold_left (fun l d -> (match (d) with
| FStar_Syntax_Syntax.Sig_let (uu____4104, uu____4105, lids, quals, uu____4108) -> begin
(((lids), (quals)))::l
end
| uu____4121 -> begin
l
end)) [] m.FStar_Syntax_Syntax.declarations)
in ((

let uu____4127 = (FStar_Options.debug_any ())
in (match (uu____4127) with
| true -> begin
(let _0_626 = (FStar_Syntax_Print.lid_to_string m.FStar_Syntax_Syntax.name)
in (FStar_Util.print2 "Checking %s: %s\n" (match (m.FStar_Syntax_Syntax.is_interface) with
| true -> begin
"i\'face"
end
| uu____4128 -> begin
"module"
end) _0_626))
end
| uu____4129 -> begin
()
end));
(

let env = (

let uu___103_4131 = env
in (let _0_627 = (not ((FStar_Options.should_verify m.FStar_Syntax_Syntax.name.FStar_Ident.str)))
in {FStar_TypeChecker_Env.solver = uu___103_4131.FStar_TypeChecker_Env.solver; FStar_TypeChecker_Env.range = uu___103_4131.FStar_TypeChecker_Env.range; FStar_TypeChecker_Env.curmodule = uu___103_4131.FStar_TypeChecker_Env.curmodule; FStar_TypeChecker_Env.gamma = uu___103_4131.FStar_TypeChecker_Env.gamma; FStar_TypeChecker_Env.gamma_cache = uu___103_4131.FStar_TypeChecker_Env.gamma_cache; FStar_TypeChecker_Env.modules = uu___103_4131.FStar_TypeChecker_Env.modules; FStar_TypeChecker_Env.expected_typ = uu___103_4131.FStar_TypeChecker_Env.expected_typ; FStar_TypeChecker_Env.sigtab = uu___103_4131.FStar_TypeChecker_Env.sigtab; FStar_TypeChecker_Env.is_pattern = uu___103_4131.FStar_TypeChecker_Env.is_pattern; FStar_TypeChecker_Env.instantiate_imp = uu___103_4131.FStar_TypeChecker_Env.instantiate_imp; FStar_TypeChecker_Env.effects = uu___103_4131.FStar_TypeChecker_Env.effects; FStar_TypeChecker_Env.generalize = uu___103_4131.FStar_TypeChecker_Env.generalize; FStar_TypeChecker_Env.letrecs = uu___103_4131.FStar_TypeChecker_Env.letrecs; FStar_TypeChecker_Env.top_level = uu___103_4131.FStar_TypeChecker_Env.top_level; FStar_TypeChecker_Env.check_uvars = uu___103_4131.FStar_TypeChecker_Env.check_uvars; FStar_TypeChecker_Env.use_eq = uu___103_4131.FStar_TypeChecker_Env.use_eq; FStar_TypeChecker_Env.is_iface = uu___103_4131.FStar_TypeChecker_Env.is_iface; FStar_TypeChecker_Env.admit = uu___103_4131.FStar_TypeChecker_Env.admit; FStar_TypeChecker_Env.lax = _0_627; FStar_TypeChecker_Env.lax_universes = uu___103_4131.FStar_TypeChecker_Env.lax_universes; FStar_TypeChecker_Env.type_of = uu___103_4131.FStar_TypeChecker_Env.type_of; FStar_TypeChecker_Env.universe_of = uu___103_4131.FStar_TypeChecker_Env.universe_of; FStar_TypeChecker_Env.use_bv_sorts = uu___103_4131.FStar_TypeChecker_Env.use_bv_sorts; FStar_TypeChecker_Env.qname_and_index = uu___103_4131.FStar_TypeChecker_Env.qname_and_index}))
in (

let uu____4132 = (tc_modul env m (FStar_Ident.lid_equals m.FStar_Syntax_Syntax.name FStar_Syntax_Const.prims_lid))
in (match (uu____4132) with
| (m, env) -> begin
((

let uu____4140 = (FStar_Options.dump_module m.FStar_Syntax_Syntax.name.FStar_Ident.str)
in (match (uu____4140) with
| true -> begin
(let _0_628 = (FStar_Syntax_Print.modul_to_string m)
in (FStar_Util.print1 "%s\n" _0_628))
end
| uu____4141 -> begin
()
end));
(

let uu____4142 = (match ((FStar_Ident.lid_equals m.FStar_Syntax_Syntax.name FStar_Syntax_Const.prims_lid)) with
| true -> begin
((m), (env))
end
| uu____4147 -> begin
((env0.FStar_TypeChecker_Env.solver.FStar_TypeChecker_Env.refresh ());
(

let env0 = (

let uu___104_4150 = env0
in {FStar_TypeChecker_Env.solver = uu___104_4150.FStar_TypeChecker_Env.solver; FStar_TypeChecker_Env.range = uu___104_4150.FStar_TypeChecker_Env.range; FStar_TypeChecker_Env.curmodule = uu___104_4150.FStar_TypeChecker_Env.curmodule; FStar_TypeChecker_Env.gamma = uu___104_4150.FStar_TypeChecker_Env.gamma; FStar_TypeChecker_Env.gamma_cache = uu___104_4150.FStar_TypeChecker_Env.gamma_cache; FStar_TypeChecker_Env.modules = uu___104_4150.FStar_TypeChecker_Env.modules; FStar_TypeChecker_Env.expected_typ = uu___104_4150.FStar_TypeChecker_Env.expected_typ; FStar_TypeChecker_Env.sigtab = uu___104_4150.FStar_TypeChecker_Env.sigtab; FStar_TypeChecker_Env.is_pattern = uu___104_4150.FStar_TypeChecker_Env.is_pattern; FStar_TypeChecker_Env.instantiate_imp = uu___104_4150.FStar_TypeChecker_Env.instantiate_imp; FStar_TypeChecker_Env.effects = uu___104_4150.FStar_TypeChecker_Env.effects; FStar_TypeChecker_Env.generalize = uu___104_4150.FStar_TypeChecker_Env.generalize; FStar_TypeChecker_Env.letrecs = uu___104_4150.FStar_TypeChecker_Env.letrecs; FStar_TypeChecker_Env.top_level = uu___104_4150.FStar_TypeChecker_Env.top_level; FStar_TypeChecker_Env.check_uvars = uu___104_4150.FStar_TypeChecker_Env.check_uvars; FStar_TypeChecker_Env.use_eq = uu___104_4150.FStar_TypeChecker_Env.use_eq; FStar_TypeChecker_Env.is_iface = uu___104_4150.FStar_TypeChecker_Env.is_iface; FStar_TypeChecker_Env.admit = uu___104_4150.FStar_TypeChecker_Env.admit; FStar_TypeChecker_Env.lax = false; FStar_TypeChecker_Env.lax_universes = true; FStar_TypeChecker_Env.type_of = uu___104_4150.FStar_TypeChecker_Env.type_of; FStar_TypeChecker_Env.universe_of = uu___104_4150.FStar_TypeChecker_Env.universe_of; FStar_TypeChecker_Env.use_bv_sorts = uu___104_4150.FStar_TypeChecker_Env.use_bv_sorts; FStar_TypeChecker_Env.qname_and_index = uu___104_4150.FStar_TypeChecker_Env.qname_and_index})
in ((FStar_Options.set_option "lax" (FStar_Options.Bool (false)));
(

let decls = (FStar_List.map (fun d -> (match (d) with
| FStar_Syntax_Syntax.Sig_let (p1, p2, lids, uu____4158, p3) -> begin
(

let uu____4166 = (match ((FStar_List.find (fun uu____4179 -> (match (uu____4179) with
| (lids', uu____4185) -> begin
(lids' = lids)
end)) let_quals)) with
| Some (v) -> begin
v
end
| None -> begin
(failwith "Impossible!")
end)
in (match (uu____4166) with
| (uu____4208, quals) -> begin
FStar_Syntax_Syntax.Sig_let (((p1), (p2), (lids), (quals), (p3)))
end))
end
| uu____4217 -> begin
d
end)) m.FStar_Syntax_Syntax.declarations)
in (

let m = (

let uu___105_4219 = m
in {FStar_Syntax_Syntax.name = uu___105_4219.FStar_Syntax_Syntax.name; FStar_Syntax_Syntax.declarations = decls; FStar_Syntax_Syntax.exports = uu___105_4219.FStar_Syntax_Syntax.exports; FStar_Syntax_Syntax.is_interface = uu___105_4219.FStar_Syntax_Syntax.is_interface})
in (tc_modul env0 m true)));
));
)
end)
in (match (uu____4142) with
| (m, env) -> begin
((

let uu____4225 = (FStar_Options.dump_module m.FStar_Syntax_Syntax.name.FStar_Ident.str)
in (match (uu____4225) with
| true -> begin
(let _0_629 = (FStar_Syntax_Print.modul_to_string m)
in (FStar_Util.print1 "%s\n" _0_629))
end
| uu____4226 -> begin
()
end));
((m), (env));
)
end));
)
end)));
));
))))




