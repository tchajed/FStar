#light "off"
module FStar.Errors
open FStar.All
open FStar.Util
open FStar.Range

exception Err of string
exception Error of string * Range.range
exception Warning of string * Range.range

module BU = FStar.Util

type issue_level =
| ENotImplemented
| EInfo
| EWarning
| EError

type issue = {
    issue_message: string;
    issue_level: issue_level;
    issue_range: option<Range.range>
}

type error_handler = {
    eh_add_one: issue -> unit;
    eh_count_errors: unit -> int;
    eh_report: unit -> list<issue>;
    eh_clear: unit -> unit
}

let print_issue issue =
    let level_header =
        match issue.issue_level with
        | EInfo -> "(Info)"
        | EWarning -> "(Warning)"
        | EError -> "(Error)"
        | ENotImplemented -> "Feature not yet implemented:" in
    let range_str, see_also_str =
        match issue.issue_range with
        | None -> "", ""
        | Some r ->
          (BU.format1 "%s : " (Range.string_of_use_range r),
           (if r.use_range = r.def_range then ""
            else BU.format1 " (see also %s)" (Range.string_of_range r))) in
    BU.print_error (BU.format4 "%s : %s %s%s\n" range_str level_header issue.issue_message see_also_str)

let compare_issues i1 i2 =
    match i1.issue_range, i2.issue_range with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some r1, Some r2 -> Range.compare_use_range r1 r2

let default_handler =
    let errs : ref<list<issue>> = BU.mk_ref [] in
    let add_one (e: issue) =
        match e.issue_level with
        | EError -> errs := e :: !errs
        | _ -> print_issue e in
    let count_errors () =
        List.length !errs in
    let report () =
        let sorted = List.sortWith compare_issues !errs in
        List.iter print_issue sorted;
        sorted in
    let clear () =
        errs := [] in
    { eh_add_one = add_one;
      eh_count_errors = count_errors;
      eh_report = report;
      eh_clear = clear }

let current_handler =
    BU.mk_ref default_handler

let get_err_count () = (!current_handler).eh_count_errors ()

let set_handler handler =
    if get_err_count () = 0
    then current_handler := handler
    else failwith "Cannot replace dirty error handler"

let mk_issue level range msg =
    { issue_level = level; issue_range = range; issue_message = msg }

let add_one level r msg =
    atomically (fun () -> (!current_handler).eh_add_one (mk_issue level r msg))

let diag r msg = if Options.debug_any() then add_one EInfo (Some r) msg

let warn r msg = add_one EWarning (Some r) msg

let err r msg = add_one EError (Some r) msg

type error_message_prefix = {
    set_prefix: string -> unit;
    append_prefix: string -> string;
    clear_prefix: unit -> unit;
}

let message_prefix =
    let pfx = BU.mk_ref None in
    let set_prefix s = pfx := Some s in
    let clear_prefix () = pfx := None in
    let append_prefix s = match !pfx with
        | None -> s
        | Some p -> p ^ ": " ^ s in
    {set_prefix=set_prefix;
     clear_prefix=clear_prefix;
     append_prefix=append_prefix}

let add_errors errs =
    atomically (fun () -> List.iter (fun (msg, r) -> err r (message_prefix.append_prefix msg)) errs)

let report_all () =
    (!current_handler).eh_report ()

let clear () =
    (!current_handler).eh_clear ()

let err_exn = function
    | Error(msg, r) ->
        add_one EError (Some r) (message_prefix.append_prefix msg)
    | NYI msg ->
        add_one ENotImplemented None (message_prefix.append_prefix msg)
    | Err msg ->
        add_one EError None (message_prefix.append_prefix msg)
    | e -> raise e

let handleable = function
  | Error _
  | NYI _
  | Err _ -> true
  | _ -> false
