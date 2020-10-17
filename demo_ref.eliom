(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

(* Demo for Eliom references and Os_date *)

open%shared Eliom_content.Html.F

(* Service for this demo *)
let%server service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["demo-ref"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

(* Make service available on the client *)
let%client service = ~%service

(* Name for demo menu *)
let%shared name () = [%i18n S.demo_eliom_ref]

(* Class for the page containing this demo (for internal use) *)
let%shared page_class = "os-page-demo-ref"

(* An Eliom reference storing the last time the user visited the current
   page. It has scope Eliom_common.default_group_scope, which means that
   the value will be different for each user of the Web site, but the same
   for all the sessions of a same user.
   Ocsigen Start is creating a session group for each user.
*)
let%server last_visit =
  Eliom_reference.eref
    ~persistent:"demo_last_visit"
    ~scope:Eliom_common.default_group_scope
    None

(* Read & reset last_visit *)
let get_reset_last_visit () =
  let%lwt v  = Eliom_reference.get last_visit in
  let%lwt () = Eliom_reference.set last_visit (Some (Os_date.now ())) in
  Lwt.return v

(* Make get_reset_last_visit available to the client *)
let%client get_reset_last_visit =
  ~%(Eliom_client.server_function [%json : unit]
       (Os_session.connected_wrapper get_reset_last_visit))

(* Call get_reset_last_visit and produce pretty message *)
let%shared get_reset_last_visit_message () =
  let%lwt last_visit = get_reset_last_visit () in
  match last_visit with
  | None ->
    Lwt.return [%i18n demo_eliom_ref_first_visit]
  | Some last_visit ->
    Lwt.return
      ([%i18n demo_eliom_ref_last_visit]
       @ [ txt " "
         ; txt (Os_date.smart_time last_visit) ])

(* Generate page for this demo *)
let%shared page () =
  let%lwt last_visit_message = get_reset_last_visit_message () in
  Lwt.return [
    h1 [%i18n demo_eliom_ref]
  ; p [txt [%i18n S.demo_eliom_ref_1]]
  ; p [txt [%i18n S.demo_eliom_ref_2]]
  ; p last_visit_message
  ; p [txt [%i18n S.demo_eliom_ref_3]]
  ]
