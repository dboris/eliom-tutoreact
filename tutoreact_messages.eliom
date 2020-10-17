[%%shared
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

(* Dummy *)
let%client display _ = Lwt.return [p []]

let%server display _userid_o =
  let%lwt messages = Db.get_messages () in
  let%lwt l =
    Lwt_list.map_s
      (fun id ->
         let%lwt msg = Db.get_message id in
         Lwt.return (li [txt msg]))
      messages
  in
  Lwt.return [ul l]