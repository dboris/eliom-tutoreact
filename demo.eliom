(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

[%%shared
  open Eliom_content.Html.D
]

(* drawer / demo welcome page ***********************************************)

let%shared handler myid_o () () =
  Tutoreact_container.page
    ~a:[ a_class ["os-page-demo"] ]
    myid_o
    [ h2 [%i18n general_principles]
    ; p [%i18n demo_intro_1]
    ; p [%i18n demo_intro_2]
    ; p [%i18n demo_widget_ot]
    ; p [%i18n demo_widget_see_drawer]
    ; p [%i18n demo_widget_feel_free]
    ; p [%i18n demo_intro_3]
    ]


let%shared () =
  let registerDemo (module D : Demo_tools.Page) =
    Tutoreact_base.App.register
      ~service:D.service
      (Tutoreact_page.Opt.connected_page @@ fun myid_o () () ->
        let%lwt p = D.page () in
        Tutoreact_container.page
          ~a:[a_class [D.page_class]]
          myid_o p)
  in
  List.iter registerDemo Demo_tools.demos;
  Tutoreact_base.App.register
    ~service:Tutoreact_services.demo_service
    (Tutoreact_page.Opt.connected_page handler)

(* [detail_page_handler] is not registered in [Demo_tools] because we
   - don't want to show detail pages in the menu. *)
let%shared () =
  let detail_page_handler =
    fun myid_o page () ->
      Tutoreact_container.page
        ~a:[a_class ["os-page-demo-transition"]]
        myid_o
        (Demo_pagetransition.make_detail_page page ()) in
  Tutoreact_base.App.register
    ~service:Demo_pagetransition.detail_page_service
    (Tutoreact_page.Opt.connected_page detail_page_handler)