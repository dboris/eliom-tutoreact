(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

(* Spinner demo *)

open%client Js_of_ocaml_lwt

(* Service for this demo *)
let%server service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["demo-spinner"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

(* Make service available on the client *)
let%client service = ~%service

(* Name for demo menu *)
let%shared name () = [%i18n S.demo_spinner]

(* Class for the page containing this demo (for internal use) *)
let%shared page_class = "os-page-demo-spinner"

(* Build the spinner *)
let%client make_spinner () =
  (* [Ot_spinner.with_spinner_no_lwt] accepts an Lwt thread "slowly"
     producing HTML content *)
  Ot_spinner.with_spinner_no_lwt
    (* sleep for 5 seconds to simulate a delay, then return content *)
    (let%lwt () = Lwt_js.sleep 5. in
     Lwt.return Eliom_content.Html.D.[
       txt [%i18n S.demo_spinner_content_ready]
     ; txt " "
     ; txt [%i18n S.demo_spinner_message_replace_spinner]
     ])

(* Page for this demo *)
let%shared page () =
  Lwt.return Eliom_content.Html.[
    F.h1 [%i18n demo_spinner]
  ; F.p [ F.txt [%i18n S.demo_spinner_description_ot] ]
  ; F.p [ F.txt [%i18n S.demo_spinner_description_1] ]
  ; F.p [ F.txt [%i18n S.demo_spinner_description_2] ]
  ; F.p [ F.txt [%i18n S.demo_spinner_description_3]]
  ; F.p [ F.txt [%i18n S.demo_spinner_generated_client_side]]
  ; C.node [%client (make_spinner () : [> `Div] Eliom_content.Html.elt) ]
  ]
