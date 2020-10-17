(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

let%client add_email_notif () = ()
let%server add_email_notif () =
  if Eliom_reference.Volatile.get Os_user.user_already_exists
  then Os_msg.msg ~level:`Err ~onload:true [%i18n S.email_already_exists]

let%shared () =
  (* Registering services. Feel free to customize handlers. *)
  Eliom_registration.Action.register
    ~service:Os_services.set_personal_data_service
    Tutoreact_handlers.set_personal_data_handler;

  Eliom_registration.Redirection.register
    ~service:Os_services.set_password_service
    Tutoreact_handlers.set_password_handler;

  Eliom_registration.Action.register
    ~service:Os_services.forgot_password_service
    Tutoreact_handlers.forgot_password_handler;

  Eliom_registration.Action.register
    ~service:Os_services.preregister_service
    Tutoreact_handlers.preregister_handler;

  Eliom_registration.Action.register
    ~service:Os_services.sign_up_service
    Os_handlers.sign_up_handler;

  Eliom_registration.Action.register
    ~service:Os_services.connect_service
    Os_handlers.connect_handler;

  Eliom_registration.Unit.register
    ~service:Os_services.disconnect_service
    (Os_handlers.disconnect_handler ~main_page:true);

  Eliom_registration.Any.register
    ~service:Os_services.action_link_service
    (Os_session.Opt.connected_fun
       Tutoreact_handlers.action_link_handler);

  Eliom_registration.Action.register
    ~service:Os_services.add_email_service
    (fun () email -> let%lwt () = Os_handlers.add_email_handler () email in
      add_email_notif ();
      Lwt.return_unit
    );

  Eliom_registration.Action.register
    ~service:Os_services.update_language_service
    Tutoreact_handlers.update_language_handler;

  Tutoreact_base.App.register
    ~service:Os_services.main_service
    (Tutoreact_page.Opt.connected_page Tutoreact_handlers.main_service_handler);

  Tutoreact_base.App.register
    ~service:Tutoreact_services.about_service
    (Tutoreact_page.Opt.connected_page Tutoreact_handlers.about_handler);

  Tutoreact_base.App.register
    ~service:Tutoreact_services.settings_service
    (Tutoreact_page.Opt.connected_page Tutoreact_handlers.settings_handler)

let%server () =
  Eliom_registration.Ocaml.register
    ~service:Tutoreact_services.upload_user_avatar_service
    (Os_session.connected_fun Tutoreact_handlers.upload_user_avatar_handler)


(* Print more debugging information when <debugmode/> is in config file
   (DEBUG = yes in Makefile.options).
   Example of use:
   let section = Lwt_log.Section.make "Tutoreact:sectionname"
   ...
   Lwt_log.ign_info ~section "This is an information";
   (or ign_debug, ign_warning, ign_error etc.)
 *)
let%server _ =
  if Eliom_config.get_debugmode ()
  then begin
    ignore
      [%client (
        (* Eliom_config.debug_timings := true; *)
        (* Lwt_log_core.add_rule "eliom:client*" Lwt_log_js.Debug; *)
        (* Lwt_log_core.add_rule "os*" Lwt_log_js.Debug; *)
        Lwt_log_core.add_rule "Tutoreact*" Lwt_log_js.Debug
        (* Lwt_log_core.add_rule "*" Lwt_log_js.Debug *)
        : unit ) ];
    (* Lwt_log_core.add_rule "*" Lwt_log.Debug *)
    Lwt_log_core.add_rule "Tutoreact*" Lwt_log.Debug
  end