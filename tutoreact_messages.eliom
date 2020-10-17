[%%shared
open Eliom_lib
open Eliom_content.Html
open Eliom_content.Html.D
]

[%%server
module Db = struct

  let db = Ocsipersist.open_table "messages"

  let last_key =
    Eliom_reference.eref
      ~persistent:"index"
      ~scope:Eliom_common.global_scope (-1)

  let get_message id =
    let%lwt db = db in
    Ocsipersist.find db (string_of_int id)

  let get_messages () =
    let%lwt index = Eliom_reference.get last_key in
    let rec aux n l = if n > index then l else aux (n+1) (n::l) in
    Lwt.return (aux 0 [])

  let lock = Lwt_mutex.create ()

  let add_message v =
    let%lwt () = Lwt_mutex.lock lock in
    let%lwt index = Eliom_reference.get last_key in
    let index = index + 1 in
    let%lwt () = Eliom_reference.set last_key index in
    Lwt_mutex.unlock lock;
    let%lwt db = db in
    let%lwt () = Ocsipersist.add db (string_of_int index) v in
    Lwt.return index

end
]

let%server add_message_rpc =
  Eliom_client.server_function
    [%json: string]
    (Os_session.connected_rpc (fun userid value -> Db.add_message value))

(* Dummy *)
let%client display _ = Lwt.return [p [txt "Testing"]]

let%server display_messages () =
  let%lwt messages = Db.get_messages () in
  let%lwt l =
    Lwt_list.map_s
      (fun id ->
         let%lwt msg = Db.get_message id in
         Lwt.return (li [txt msg]))
      messages
  in
  Lwt.return (ul l)

let%server display userid_o =
  let%lwt messages = display_messages () in
  let l = match userid_o with
    | None ->
      []
    | _ ->
      let inp = Raw.input ~a:[a_input_type `Text] () in
      let _ = [%client
        (let open Js_of_ocaml_lwt.Lwt_js_events in
        let open Js_of_ocaml in
        let inp = To_dom.of_input ~%inp in
        async (fun () -> changes inp (fun _ _ ->
          let value = Js.to_string inp##.value in
          inp##.value := Js.string "";
          let%lwt _ = ~%add_message_rpc value in
          Lwt.return ()))
        : unit)
      ] in
      [inp]
  in
  Lwt.return (messages :: l)