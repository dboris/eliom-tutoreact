(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

(* Ocsigen_i18n demo *)

open%shared Eliom_content.Html.F

(* Service for this demo *)
let%server service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["demo-i18n"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

(* Make service available on the client *)
let%client service = ~%service

(* Name for demo menu *)
let%shared name () = [%i18n S.internationalization ~capitalize:true]

(* Class for the page containing this demo (for internal use) *)
let%shared page_class = "os-page-demo-i18n"

(* Page for this demo *)
let%shared page () =
  (* Syntax [%i18n key] inserts the text corresponding to the key,
     in the language chosen by the user, as a list of HTML elements.
     Syntax [%i18n S.key] inserts the text as a string.
     It is possible to give parameters (here a boolean ~capitalize, or
     a piece of HTML text ~f1 or ~f2). Have a look at file
     assets/tutoreact_i18n.tsv
     to see how to write the corresponding translations.
  *)
  Lwt.return
    [ h1 [%i18n internationalization ~capitalize:true]
    ; p [%i18n internationalization_1]
    ; p [%i18n internationalization_2
          ~f1:[code [txt "assets/tutoreact_i18n.tsv"]]
          ~f2:[code [txt "tutoreact_i18n.eliom"]]
      ]
    ; p [txt [%i18n S.internationalization_3] ]
    ]