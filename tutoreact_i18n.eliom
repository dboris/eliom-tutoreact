[%%shared type t = En|Fr]
[%%shared exception Unknown_language of string]
let%shared string_of_language = function 
| En -> "en"| Fr -> "fr"
let%shared language_of_string = function
| "en" -> En| "fr" -> Fr| s -> raise (Unknown_language s)
let%shared guess_language_of_string s = 
try language_of_string s 
with Unknown_language _ as e -> 
try language_of_string (String.sub s 0 (String.index s '-')) 
with Not_found -> 
raise e 
let%shared languages = [En;Fr]
let%shared default_language = En
let%server _language_ = Eliom_reference.Volatile.eref
~scope:Eliom_common.default_process_scope default_language
let%server get_language () = Eliom_reference.Volatile.get _language_
let%server set_language language = 
Eliom_reference.Volatile.set _language_ language

let%client _language_ = ref default_language
let%client get_language () = !_language_
let%client set_language language = _language_ := language

let%shared txt = Eliom_content.Html.F.txt
[%%shared
module Tr = struct
let welcome_text1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Welcome to Ocsigen Start. This is a template for applications based on Ocsigen (Eliom, Js_of_ocaml, etc.)."]
| Fr -> [txt "Bienvenue dans Ocsigen Start\194\160! Ceci est un template d'application \195\169crite avec Ocsigen (Eliom, Js_of_ocaml, etc.)."]
let welcome_text2 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Use it:"]
| Fr -> [txt "Utilisez-le\194\160:"]
let welcome_text3 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "As a basis for your own applications."]
| Fr -> [txt "Comme point de d\195\169part pour vos propres applications\194\160;"]
let welcome_text4 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "To learn the most important concepts of client-server programming with Ocsigen."]
| Fr -> [txt "Pour apprendre les principaux concepts de la programmation client-serveur avec Ocsigen."]
let welcome_text5 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This application contains:"]
| Fr -> [txt "Cette application contient\194\160:"]
let welcome_text6 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Features for user management (log-in form, user registration, activation links, password recovery, settings page, etc.)."]
| Fr -> [txt "Des fonctionnalit\195\169s de gestion des utilisateurs (connexion, cr\195\169ation d'utilisateur, liens d'activation, r\195\169cup\195\169ration de mot de passe, param\195\168tres de l'utilisateur,...)\194\160;"]
let welcome_text7 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "An extensive demo of the most important features you need to implement your own app. Read the source code to learn! And remove the demo part when you're ready to start with your own app."]
| Fr -> [txt "Une d\195\169mo des plus importantes fonctionnalit\195\169s dont vous avez besoin pour \195\169crire votre propre application. Lisez le code source pour apprendre\194\160! Ensuite enlevez la partie demo quand vous \195\170tes pr\195\170ts \195\160 commencer votre propre application\194\160;"]
let welcome_text8 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "A library with useful features (tips, notifications, etc.)."]
| Fr -> [txt "Une biblioth\195\168que avec de nombeuses fonctionnalit\195\169s utiles (tips, notifications, etc.)\194\160;"]
let welcome_text9 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "All the features you need to create a multilingual app."]
| Fr -> [txt "Tous les outils pour cr\195\169er une application multilingue\194\160;"]
let welcome_text10 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "A basic responsive CSS."]
| Fr -> [txt "Une feuille de style \"responsive\" basique."]
let welcome_text11 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This application is multi-platform: it can run as a client-server Web application (with server-side generated pages) and as a mobile app (with client-side generated pages) for Android, iOS or Windows. Have a look at the README file to learn how to generate the mobile apps, which you will be able to upload on Google Play or Apple App Store. "]
| Fr -> [txt "Cette application est multi-plateforme\194\160: elle peut tourner comme application Web client-serveur (avec des pages g\195\169n\195\169r\195\169es c\195\180t\195\169 serveur) ou bien comme application mobile pour iOS, Android ou Windows (avec des pages g\195\169n\195\169r\195\169es c\195\180t\195\169 client). Regardez le fichier README pour apprendre comment g\195\169n\195\169rer les applications mobiles que vous pourrez envoyer sur Google Play ou Apple App Store."]
let about_handler_template ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This template provides a skeleton for an Ocsigen application."]
| Fr -> [txt "Ce template fournit une base pour une application Ocsigen."]
let about_handler_license ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Feel free to modify the generated code and use it or redistribute it in any way you want."]
| Fr -> [txt "Vous \195\170tes libres de modifier le code g\195\169n\195\169r\195\169 et de l'utiliser ou le redistribuer comme vous le souhaitez."]
let footer_generated ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This application has been generated using the"]
| Fr -> [txt "Cette application a \195\169t\195\169 g\195\169n\195\169r\195\169e en utilisant le template d'"]
let footer_eliom_distillery ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "template for Eliom-distillery and uses the"]
| Fr -> [txt "avec Eliom-distillery et utilise les technologies"]
let footer_technology ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt " technology."]
| Fr -> [txt "."]
let home ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "H" else "h")];[txt "ome"]]
| Fr -> List.flatten [[txt (if capitalize then "H" else "h")];[txt "ome"]]
let about ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "A" else "a")];[txt "bout"]]
| Fr -> List.flatten [[txt (if capitalize then "\195\128" else "\195\160")];[txt " propos"]]
let demo ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Demo"]
| Fr -> [txt "D\195\169mo"]
let password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "P" else "p")];[txt "assword"]]
| Fr -> List.flatten [[txt (if capitalize then "M" else "m")];[txt "ot de passe"]]
let retype_password ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "retype your password"]
| Fr -> [txt "retapez votre mot de passe"]
let your_email ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "Y" else "y")];[txt "our email"]]
| Fr -> List.flatten [[txt (if capitalize then "V" else "v")];[txt "otre e-mail"]]
let your_password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "Y" else "y")];[txt "our password"]]
| Fr -> List.flatten [[txt (if capitalize then "V" else "v")];[txt "otre mot de passe"]]
let keep_logged_in ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "keep me logged in"]
| Fr -> [txt "rester connect\195\169"]
let sign_in ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "ign in"]]
| Fr -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "e connecter"]]
let forgot_your_password_q ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "F" else "f")];[txt "orgot your password?"]]
| Fr -> List.flatten [[txt (if capitalize then "M" else "m")];[txt "ot de passe oubli\195\169\194\160?"]]
let sign_up ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "ign up"]]
| Fr -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "'enregistrer"]]
let logout ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "L" else "l")];[txt "ogout"]]
| Fr -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "e d\195\169connecter"]]
let set_as_main_email ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "et as main email"]]
| Fr -> List.flatten [[txt (if capitalize then "D" else "d")];[txt "\195\169finir comme e-mail principal"]]
let validated ?(lang = get_language ()) () ?(capitalize=false) ?(f=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "V" else "v")];[txt "alidated"]]
| Fr -> List.flatten [[txt (if capitalize then "V" else "v")];[txt "alid\195\169"];[txt (if f then "e" else "")]]
let waiting_confirmation ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "W" else "w")];[txt "aiting for confirmation"]]
| Fr -> List.flatten [[txt (if capitalize then "E" else "e")];[txt "n attente de confirmation"]]
let main_email ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "M" else "m")];[txt "ain email"]]
| Fr -> List.flatten [[txt (if capitalize then "E" else "e")];[txt "-mail principal"]]
let change_password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "C" else "c")];[txt "hange your password:"]]
| Fr -> List.flatten [[txt (if capitalize then "C" else "c")];[txt "hanger votre mot de passe\194\160:"]]
let link_new_email ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Link a new email to your account:"]
| Fr -> [txt "Ajouter une adresse e-mail \195\160 votre compte\194\160:"]
let currently_registered_emails ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Currently registered emails:"]
| Fr -> [txt "E-mails actuellement enregistr\195\169s\194\160:"]
let settings ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "ettings"]]
| Fr -> List.flatten [[txt (if capitalize then "P" else "p")];[txt "aram\195\168tres"]]
let error ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "E" else "e")];[txt "rror"]]
| Fr -> List.flatten [[txt (if capitalize then "E" else "e")];[txt "rreur"]]
let example_tip ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This is an example of tip."]
| Fr -> [txt "Ceci est un exemple de tip."]
let look_module_tip ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Look at the code to see how it is defined."]
| Fr -> [txt "Regardez le code pour voir comment c'est d\195\169fini."]
let passwords_do_not_match ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Passwords do not match"]
| Fr -> [txt "Les mots de passe ne correspondent pas"]
let generate_action_link_key_subject_email ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "creation"]
| Fr -> [txt "cr\195\169ation"]
let sign_up_email_msg ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Welcome!\\r\\nTo confirm your email address, please click on this link:"]
| Fr -> [txt "Bienvenue\194\160!\\r\\nPour confirmer votre adresse e-mail, cliquer sur ce lien\194\160:"]
let email_already_exists ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Email already exists"]
| Fr -> [txt "Cet e-mail existe d\195\169j\195\160"]
let user_does_not_exist ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "user does not exist"]
| Fr -> [txt "Cet utilisateur n'existe pas"]
let account_not_activated ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Account not activated"]
| Fr -> [txt "Ce compte n'est pas activ\195\169"]
let wrong_password ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Wrong password"]
| Fr -> [txt "Mauvais mot de passe"]
let no_such_user ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "No such user"]
| Fr -> [txt "Cet utilisateur n'existe pas"]
let add_email_msg ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Welcome!\\r\\nTo confirm your email address, please click on this link:"]
| Fr -> [txt "Bienvenue\194\160!\\r\\nPour confirmer votre adresse e-mail, cliquez sur ce lien\194\160:"]
let invalid_action_key ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Invalid action key, please ask for a new one."]
| Fr -> [txt "Clef d'action invalide. Demandez en une nouvelle svp."]
let forgot_pwd_email_msg ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Hi,\\r\\nTo set a new password, please click on this link:"]
| Fr -> [txt "Bonjour,\\r\\nPour mettre \195\160 jour votre mot de passe, cliquez sur ce lien\194\160:"]
let must_be_connected_to_see_page ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "You must be connected to see this page."]
| Fr -> [txt "Vous devez \195\170tre connect\195\169 pour voir cette page."]
let email_address ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Email address"]
| Fr -> [txt "Adresse e-mail"]
let your_first_name ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Your first name"]
| Fr -> [txt "Votre pr\195\169nom"]
let your_last_name ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Your last name"]
| Fr -> [txt "Votre nom"]
let submit ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "ubmit"]]
| Fr -> List.flatten [[txt (if capitalize then "E" else "e")];[txt "nvoyer"]]
let see_help_again_from_beginning ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "See help again from beginning"]
| Fr -> [txt "Revoir l'aide depuis le d\195\169but"]
let personal_information_not_set ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Your personal information has not been set yet."]
| Fr -> [txt "Vous n'avez pas encore entr\195\169 vos donn\195\169es personnelles."]
let take_time_enter_name_password ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Please take time to enter your name and to set a password."]
| Fr -> [txt "Veuillez entrer votre nom et choisir un mot de passe svp."]
let wrong_data_fix ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Wrong data. Please fix."]
| Fr -> [txt "Donn\195\169es incorrectes. Veuillez corriger."]
let send ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "end"]]
| Fr -> List.flatten [[txt (if capitalize then "E" else "e")];[txt "nvoyer"]]
let recover_password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "R" else "r")];[txt "ecover password"]]
| Fr -> List.flatten [[txt (if capitalize then "R" else "r")];[txt "\195\169cup\195\169rer le mot de passe."]]
let welcome ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "W" else "w")];[txt "elcome!"]]
| Fr -> List.flatten [[txt (if capitalize then "B" else "b")];[txt "ienvenue\194\160!"]]
let log_in_to_see_page ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "L" else "l")];[txt "og in to see this page."]]
| Fr -> List.flatten [[txt (if capitalize then "C" else "c")];[txt "onnectez-vous pour voir cette page."]]
let you_click_on_date ?(lang = get_language ()) () ~d ~m ~y () =
match lang with
| En -> List.flatten [[txt "You clicked on "];y;[txt "/"];m;[txt "/"];d]
| Fr -> List.flatten [[txt "Vous avez cliqu\195\169 sur "];d;[txt "/"];m;[txt "/"];y]
let you_click_on_time ?(lang = get_language ()) () ~h ~m () =
match lang with
| En -> List.flatten [[txt "You clicked on "];h;[txt ":"];m]
| Fr -> List.flatten [[txt "Vous avez cliqu\195\169 sur "];h;[txt ":"];m]
let demo_cache ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Cache"]
| Fr -> [txt "Cache"]
let demo_cache_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Caching the data"]
| Fr -> [txt "Mise en cache des donn\195\169es"]
let demo_cache_2 ?(lang = get_language ()) () ~eliom_cscache ~os_user_proxy () =
match lang with
| En -> List.flatten [[txt "Module "];eliom_cscache;[txt " implements a cache of data that is designed for Eliom's client-server programming model. It permits saving a client-side copy of the data. Have a look at the module "];os_user_proxy;[txt " to see how it works (and use this module for getting information about Ocsigen Start's users)."]]
| Fr -> List.flatten [[txt "Le module "];eliom_cscache;[txt " impl\195\169mente un cache de donn\195\169es construit pour le mod\195\168le client-serveur d'Eliom. Il permet de sauvegarder une copie des donn\195\169es du client. Jetez un oeil au module "];os_user_proxy;[txt " pour comprendre son fonctionnement (et utilisez ce module pour obtenir des informations sur les utilisateurs d'Ocsigen Start)."]]
let demo_cache_3 ?(lang = get_language ()) () ~eliom_cscache () =
match lang with
| En -> List.flatten [[txt "When you get a piece of data through "];eliom_cscache;[txt " from client-side, the request to the server is done only if the data is not already in the client-side cache. On server-side, "];eliom_cscache;[txt " is using a temporary cache (with \\\"request\\\" scope) to avoid fetching the data several times from the database during the same request. This server-side cache is automatically sent to the client to fill the client-side cache. If you want to avoid too many requests from the client, prefill the server-side cache with the data the client program will need."]]
| Fr -> List.flatten [[txt "Quand une donn\195\169e du client est obtenue via "];eliom_cscache;[txt ", la requ\195\170te vers le serveur est faite uniquement si la donn\195\169e ne se trouve pas d\195\169j\195\160 dans le cache c\195\180t\195\169 client. Du c\195\180t\195\169 serveur, "];eliom_cscache;[txt " utilise un cache temporaire (avec une port\195\169e de type \"request\") afin d'\195\169viter de r\195\169cup\195\169rer les donn\195\169es de la base de donn\195\169es plusieurs fois sur une m\195\170me requ\195\170te. Ce cache c\195\180t\195\169 serveur est automatiquement envoy\195\169 au client pour remplir son cache. Pour \195\169viter un trop grand nombre de requ\195\170tes c\195\180t\195\169 client, pr\195\169-remplissez le cache c\195\180t\195\169 serveur avec les donn\195\169es dont le programme c\195\180t\195\169 client a besoin."]]
let demo_cache_4 ?(lang = get_language ()) () ~eliom_cscache () =
match lang with
| En -> List.flatten [[txt "In the near future, "];eliom_cscache;[txt " will enable saving persistent data, which is useful for implementing off-line applications."]]
| Fr -> List.flatten [[txt "Prochainement, "];eliom_cscache;[txt " permettra de sauvegarder des donn\195\169es de mani\195\168re persistante, ce qui peut \195\170tre utile pour impl\195\169menter des applications hors ligne."]]
let demo_calendar ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Calendar"]
| Fr -> [txt "Calendrier"]
let this_page_show_calendar ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This page shows Ocsigen Toolkit's date picker."]
| Fr -> [txt "Cette page montre le s\195\169lecteur de date d'Ocsigen Toolkit."]
let demo_carousel_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Carousel"]
| Fr -> [txt "Carousel"]
let ot_carousel_first_example_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This is a first example of Ocsigen Toolkit's carousel."]
| Fr -> [txt "Voici le premier exemple du carousel d'Ocsigen Toolkit."]
let ot_carousel_first_example_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "The carousel displays a number of blocks side-by-side (or vertically stacked)."]
| Fr -> [txt "Le carousel sert \195\160 afficher des blocs c\195\180te-\195\160-c\195\180te (ou empil\195\169s verticalement)."]
let ot_carousel_first_example_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "To switch to a different block, use the buttons in the carousel."]
| Fr -> [txt "Pour vous rendre sur un autre bloc, utilisez les boutons dans le carousel."]
let ot_carousel_first_example_4 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "On touch screens you can also swipe with your fingers."]
| Fr -> [txt "Sur les \195\169crans tactiles, swipez avec les doigts."]
let demo_carousel_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Carousel: page with tabs"]
| Fr -> [txt "Carousel\194\160: page avec onglets"]
let ot_carousel_second_example_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This page demonstrates how to use Ocsigen Toolkit's carousel to display a page with several tabs."]
| Fr -> [txt "Cette page montre comment utiliser le carousel d'Ocsigen Toolkit pour afficher une page avec des onglets."]
let ot_carousel_second_example_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Try to swipe on a touch screen."]
| Fr -> [txt "Sur \195\169cran tactile, glissez pour changer d'onglet."]
let ot_carousel_second_example_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Try on a small screen or browser window to see how the tabs stick on top while scrolling the page."]
| Fr -> [txt "Essayez sur un petit \195\169cran pour voir comment la barre d'onglets se fixe en haut de la page quand vous faites d\195\169filer la page vers le bas."]
let monday ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Monday"]
| Fr -> [txt "Lundi"]
let tuesday ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Tuesday"]
| Fr -> [txt "Mardi"]
let wednesday ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Wednesday"]
| Fr -> [txt "Mercredi"]
let thursday ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Thursday"]
| Fr -> [txt "Jeudi"]
let friday ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Friday"]
| Fr -> [txt "Vendredi"]
let saturday ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Saturday"]
| Fr -> [txt "Samedi"]
let sunday ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Sunday"]
| Fr -> [txt "Dimanche"]
let demo_carousel_wheel ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Wheel carousel"]
| Fr -> [txt "Carousel avec roulette"]
let demo_carousel_third_example_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Example of a vertical circular carousel (wheel). Try with a touch screen."]
| Fr -> [txt "Exemple de carousel vertical circulaire (wheel). Essayez avec un \195\169cran tactile."]
let demo_notification ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Notifications"]
| Fr -> [txt "Notifications"]
let demo_notification_got ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "got"]
| Fr -> [txt "Re\195\167u"]
let exchange_msg_between_users ?(lang = get_language ()) () ~os_notif () =
match lang with
| En -> List.flatten [[txt "Module "];os_notif;[txt " enables sending information to client applications (notifications, new messages ...)."]]
| Fr -> List.flatten [[txt "Le module "];os_notif;[txt " permet d'envoyer des donn\195\169es aux applications client (notifications, nouveaux messages,...)."]]
let open_multiple_tabs_browsers ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Open this page in multiple tabs or browsers."]
| Fr -> [txt "Ouvrez cette page dans plusieurs onglets et fen\195\170tres."]
let fill_input_form_send_message ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Fill in the input form to send a message to all other tabs."]
| Fr -> [txt "Remplissez le formulaire pour envoyer un message \195\160 tous vos autres onglets ouverts."]
let send_message ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "send message"]
| Fr -> [txt "envoyer le message"]
let demo_pgocaml ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Database request"]
| Fr -> [txt "Requ\195\170te \195\160 la base de donn\195\169es."]
let no_user_create_accounts ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "No user. Create some accounts to test."]
| Fr -> [txt "Aucun utilisateur. Cr\195\169ez quelques comptes pour tester."]
let demo_pgocaml_users ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Users:"]
| Fr -> [txt "Utilisateurs\194\160:"]
let demo_pgocaml_description_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This page shows signed-up users fetched from the database."]
| Fr -> [txt "Cette page montre tous les utilisateurs inscrits qui ont \195\169t\195\169 r\195\169cup\195\169r\195\169s de la base de donn\195\169es."]
let demo_pgocaml_description_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Have a look at the source code to see how to make a DB request with PGOCaml."]
| Fr -> [txt "Regardez dans le code source comment r\195\169aliser une requ\195\170te \195\160 la base de donn\195\169es en utilisant PGOcaml."]
let demo_pgocaml_description_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "We are using Ot_spinner to display the list, which means that, in the case the page is generated client-side, the page will be displayed immediately with a spinner, that will be replaced by the contents when ready. The code contains a 2s sleep to demonstrate the spinner."]
| Fr -> [txt "Nous utilisons Ot_spinner pour afficher la liste\194\160: dans le cas o\195\185 une page est g\195\169n\195\169r\195\169e c\195\180t\195\169 client, cette page est affich\195\169e imm\195\169diatement avec une ic\195\180ne de chargement qui sera remplac\195\169e par le contenu quand il sera pr\195\170t. Pour la d\195\169monstration, nous avons ajout\195\169 une pause de 2s pour laisser le temps de voir l'ic\195\180ne de chargement."]
let demo_popup ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Popup Button"]
| Fr -> [txt "Bouton popup"]
let demo_popup_click ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Click for a popup!"]
| Fr -> [txt "Cliquez pour afficher un popup\194\160!"]
let demo_popup_message ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Popup message"]
| Fr -> [txt "Message du popup"]
let demo_popup_content ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Here is a button showing a simple popup window when clicked:"]
| Fr -> [txt "Voici un bouton affichant une simple fen\195\170tre popup quand vous cliquez dessus\194\160:"]
let demo_reactive_programming ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Reactive pages"]
| Fr -> [txt "Pages r\195\169actives"]
let demo_reactive_programming_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This is an example of a page with reactive content. It is a very convenient solution to update pages when data changes."]
| Fr -> [txt "Ceci est un exemple d'une page avec du contenu r\195\169actif. C'est une solution extr\195\170ment simple pour mettre \195\160 jour une page quand les donn\195\169es changent."]
let demo_reactive_programming_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "It defines a (client-side) reactive OCaml list. You can add elements in this list via the input form. The page is updated automatically when the value of the reactive list changes."]
| Fr -> [txt "Il d\195\169finit une liste OCaml r\195\169active (c\195\180t\195\169 client). Vous pouvez ajouter des \195\169lements dans cette liste via le formulaire. Cette page sera automatique mise \195\160 jour quand la valeur de la liste r\195\169active changera."]
let demo_reactive_programming_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "The reactive page is generated either server-side (for example when you are using a Web browser and you reload this page) or client-side (in a mobile app or if you were already in the app before coming to this page)."]
| Fr -> [txt "La page r\195\169active est g\195\169n\195\169r\195\169e aussi bien du c\195\180t\195\169 serveur (par exemple quand vous utilisez le navigateur web et que vous rechargez cette page) que du c\195\180t\195\169 client (sur mobile ou si vous \195\169tiez d\195\169j\195\160 sur dans cette application avant de venir sur cette page)."]
let demo_reactive_programming_button ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "add"]
| Fr -> [txt "ajouter"]
let demo_eliom_ref ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Eliom references + OS dates"]
| Fr -> [txt "R\195\169f\195\169rences Eliom + dates OS"]
let demo_eliom_ref_first_visit ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This is your first visit."]
| Fr -> [txt "Ceci est votre premi\195\168re visite."]
let demo_eliom_ref_last_visit ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "The last time you visited was: "]
| Fr -> [txt " La derni\195\168re fois que vous avez visit\195\169 ce lien \195\169tait\194\160:"]
let demo_eliom_ref_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "We use an Eliom reference to record the last time you visited this page. Eliom references make it possible to save, server-side, data specific to one user, one browser, or one tab. "]
| Fr -> [txt "Nous utilisons les r\195\169f\195\169rences Eliom pour sauvegarder la date de la derni\195\168re fois que vous avez visit\195\169 cette page. Les r\195\169f\195\169rences Eliom permettent de sauvegarder c\195\180t\195\169 serveur des donn\195\169es propres \195\160 un utilisateur, \195\160 un navigateur ou \195\160 un onglet."]
let demo_eliom_ref_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "The value is different for each user."]
| Fr -> [txt "La valeur est diff\195\169rente pour chaque utilisateur."]
let demo_eliom_ref_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "The reference has been updated. Come back later!"]
| Fr -> [txt "La r\195\169f\195\169rence a \195\169t\195\169 mise \195\160 jour. Revenez plus tard\194\160!"]
let demo_rpc_button ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "RPC button"]
| Fr -> [txt "Bouton RPC"]
let demo_rpc_button_click_increase ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Click to increase server-side value"]
| Fr -> [txt "Cliquez pour augmenter la valeur c\195\180t\195\169 serveur."]
let demo_rpc_button_description ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This button performs an RPC to increase a server-side value."]
| Fr -> [txt "Ce bouton r\195\169alise un appel de fonction distante (RPC) pour augmenter une valeur c\195\180t\195\169 serveur."]
let demo_spinner ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Spinner"]
| Fr -> [txt "Ic\195\180ne de chargement"]
let demo_spinner_content_ready ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "The content is ready. "]
| Fr -> [txt "Le contenu est pr\195\170t."]
let demo_spinner_message_replace_spinner ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This message has replaced the spinner."]
| Fr -> [txt "Ce message a remplac\195\169 l'ic\195\180ne de chargement."]
let demo_spinner_description_ot ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This is a demo of the Ocsigen Toolkit spinner widget."]
| Fr -> [txt "Ceci est la d\195\169monstration de l'ic\195\180ne de chargement d'Ocsigen Toolkit."]
let demo_spinner_description_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "We use this widget to integrate into the page an HTML block that takes a long time to produce, e.g., because of a slow server call."]
| Fr -> [txt "Nous utilisons ce widget pour int\195\169grer dans notre page HTML un bloc qui prend un long moment \195\160 produire, par exemple, \195\160 cause d'un appel serveur lent."]
let demo_spinner_description_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "A spinner is displayed, which is then replaced with the actual content when this content is ready."]
| Fr -> [txt "Une ic\195\180ne de chargement est d'abord affich\195\169e, puis remplac\195\169e par le vrai contenu quand celui-ci est pr\195\170t."]
let demo_spinner_description_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "For the demo we just sleep for 5 seconds to simulate waiting for the content."]
| Fr -> [txt "Pour la d\195\169monstration, nous avons ajout\195\169 un d\195\169lai de 5 secondes pour simuler l'attente du contenu."]
let demo_spinner_generated_client_side ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "The spinner is generated client-side."]
| Fr -> [txt "L'ic\195\180ne de chargement est g\195\169n\195\169r\195\169e c\195\180t\195\169 client."]
let demo_timepicker ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Time picker"]
| Fr -> [txt "S\195\169lecteur d'heure"]
let demo_timepicker_back_to_hours ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Back to hours"]
| Fr -> [txt "Revenir aux heures"]
let demo_timepicker_description ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This page shows the Ocsigen Toolkit's time picker."]
| Fr -> [txt "Cette page montre le s\195\169lecteur d'heure d'Ocsigen Toolkit."]
let demo_tips ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Tips"]
| Fr -> [txt "Astuces"]
let change_profile_picture ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Change profile picture"]
| Fr -> [txt "Changer votre photo de profil."]
let demo_tongue_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Tongue"]
| Fr -> [txt "Languette"]
let ot_tongue_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This is an example of page with a tongue coming from the bottom of the screen. try to slide it with your finger on a mobile screen."]
| Fr -> [txt "Ceci est un exemple de page avec une languette partant du bas de l'\195\169cran. Essayez de la faire glisser vers le haut avec le doigt sur un t\195\169l\195\169phone mobile."]
let demo_widget_ot ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This app also contains demos for some widgets from Ocsigen Toolkit."]
| Fr -> [txt "Cette application contient \195\169galement des d\195\169monstrations de quelques widgets d'Ocsigen Toolkit."]
let demo_widget_see_drawer ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "The different demos are accessible through the drawer menu. To open it click the top left button on the screen."]
| Fr -> [txt "Les diff\195\169rentes d\195\169monstrations sont accessibles \195\160 travers le menu. Pour l'ouvrir, cliquez sur le bouton en haut \195\160 gauche de l'\195\169cran."]
let demo_widget_feel_free ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Feel free to modify the generated code and use it or redistribute it as you want."]
| Fr -> [txt "Vous \195\170tes libres de modifier le code g\195\169n\195\169r\195\169 et de l'utiliser ou de le redistribuer comme vous souhaitez."]
let users ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Users"]
| Fr -> [txt "Utilisateurs"]
let you_are_not_connected ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "You are not connected."]
| Fr -> [txt "Vous n'\195\170tes pas connect\195\169."]
let you_are ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "You are"]
| Fr -> [txt "Vous \195\170tes"]
let log_in_to_see_demo ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Log in to see the demo."]
| Fr -> [txt "Connectez-vous pour voir la d\195\169monstration."]
let your_user_id ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Your user id"]
| Fr -> [txt "Votre ID utilisateur"]
let the_module ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "The module"]
| Fr -> [txt "Le module"]
let allows_get_information_currently_connected_user ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "provides information about the currently connected user (server or client side)."]
| Fr -> [txt "vous autorise \195\160 obtenir les information de l'utilisateur courant connect\195\169 (c\195\180t\195\169 serveur ou c\195\180t\195\169 client)."]
let these_functions_called_server_or_client_side ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "These functions can be called from either server- or client-side."]
| Fr -> [txt "Ces fonctions peuvent \195\170tre appel\195\169es aussi bien c\195\180t\195\169 client que c\195\180t\195\169 serveur."]
let always_get_current_user_using_module ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Always get the current user id using module"]
| Fr -> [txt "R\195\169cup\195\169rez toujours l'ID de l'utilisateur courant en utilisant le module"]
let never_trust_client_pending_user_id ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Never trust a client sending its own user id!"]
| Fr -> [txt "Ne faites jamais confiance \195\160 un client envoyant son propre ID d'utilisateur\194\160!"]
let internationalization ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "I" else "i")];[txt "nternationalization"]]
| Fr -> List.flatten [[txt (if capitalize then "I" else "i")];[txt "nternationalisation"]]
let internationalization_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Ocsigen Start uses Ocsigen-i18n for internationalizing your app. Ocsigen-i18n defines a PPX syntax extension for automatically selecting language-dependent text for each user. The user can choose his preferred language from the settings page. By default the browser's language is used."]
| Fr -> [txt "Ocsigen Start utilise Ocsigen-i18n for internationaliser les applications. Ocsigen-i18n d\195\169finit une extension de syntaxe PPX qui s\195\169lectionne automatiquement les textes en fonction de la langue de l'utilisateur courant. L'utilisateur peut choisir sa langue pr\195\169f\195\169r\195\169e dans la page de param\195\168tres. Par d\195\169faut, la langue du navigateur est utilis\195\169e."]
let internationalization_2 ?(lang = get_language ()) () ~f1 ~f2 () =
match lang with
| En -> List.flatten [[txt "Write your translations (as tab-separated-values) in file "];f1;[txt ". File "];f2;[txt " is generated automatically from this file."]]
| Fr -> List.flatten [[txt "\195\137crivez vos traductions (au format \"tab-separated-values\") dans le fichier "];f1;[txt ". Le fichier "];f2;[txt " est g\195\169n\195\169r\195\169 automatiquement \195\160 partir de ce fichier."]]
let internationalization_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Have a look at the OCaml code of this page to discover some features of the module Ocsigen-i18n."]
| Fr -> [txt "Jetez un coup d'\197\147il au code OCaml de cette page pour d\195\169couvrir quelques astuces du module Ocsigen-i18n."]
let links_and_static_files ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Links, services and static files"]
| Fr -> [txt "Liens, services et fichiers statiques"]
let services ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Services"]
| Fr -> [txt "Services"]
let services_1 ?(lang = get_language ()) () ~f1 ~f2 ~f3 () =
match lang with
| En -> List.flatten [[txt "Have a look at file "];f1;[txt " to see some examples of service definitions. Most service handlers are defined in file "];f2;[txt ". Service registration is done in "];f3;[txt ". Have a look to see how to define a service returning an application page, an action or a redirection, etc. Read Ocsigen's tutorials and Eliom's manual for more information about services."]]
| Fr -> List.flatten [[txt "Vous trouverez des exemples de d\195\169finition de services dans le fichier "];f1;[txt ". La plupart des handlers de services sont d\195\169finis dans le fichier "];f2;[txt ". L'enregistrement des services est fait dans le fichier "];f3;[txt ". Jetez-y un \197\147il pour voir comment d\195\169finir une nouvelle page pour cette application, une action, une redirection, etc. Lisez les tutoriels d'Ocsigen et le manuel d'Eliom pour plus d'informations sur les services."]]
let links_and_forms ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Links and forms"]
| Fr -> [txt "Liens et formulaires"]
let links_and_forms_1 ?(lang = get_language ()) () ~t1 ~t2 () =
match lang with
| En -> List.flatten [[txt "Here is an example of an "];t1;[txt ", and an example of link towards an "];t2;[txt "."]]
| Fr -> List.flatten [[txt "Voici un exemple de "];t1;[txt ", et un exemple de lien vers un "];t2;[txt "."]]
let internal_link ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "internal link"]
| Fr -> [txt "lien interne"]
let external_service ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "external service"]
| Fr -> [txt "service externe"]
let static_files ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Static files"]
| Fr -> [txt "Fichiers statiques"]
let static_files_1 ?(lang = get_language ()) () ~static ~static_dir () =
match lang with
| En -> List.flatten [[txt "Use service "];static_dir;[txt " (predefined in Eliom) to create links towards static files (images, fonts, etc.). Put static files you want to include in the mobile app in directory "];static;[txt ". They will be stored locally on the mobile device. By default, links are relative on the Web app and absolute on the mobile app. For example, here is an example of an image stored locally in the mobile app:"]]
| Fr -> List.flatten [[txt "Utilisez le service "];static_dir;[txt " (pr\195\169d\195\169fini dans Eliom) pour faire des liens vers des fichiers statiques (images, fontes, etc.). Les fichiers statiques que vous voulez inclure dans l'application mobile doivent \195\170tre plac\195\169s dans le r\195\169pertoire "];static;[txt ". Ils seront stock\195\169s en local sur l'appareil mobile. Par d\195\169faut les liens sont relatifs dans l'application Web et absolus dans l'application mobile. Forcez les liens relatifs pour faire des liens vers des fichiers locaux dans l'application mobile. Par exemple voici une image stock\195\169e localement dans l'application mobile\194\160:"]]
let static_files_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "and a remote image:"]
| Fr -> [txt "et une image distante\194\160:"]
let change_language ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Change language"]
| Fr -> [txt "Changer la langue"]
let tips1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Tips for new users and new features"]
| Fr -> [txt "Astuces pour les nouveaux utilisateurs et nouvelles fonctionnalit\195\169s"]
let tips2 ?(lang = get_language ()) () ~os_tips () =
match lang with
| En -> List.flatten [[txt "Module "];os_tips;[txt " implements a way to display tips in the page to the users who haven't already seen them."]]
| Fr -> List.flatten [[txt "Le module "];os_tips;[txt " impl\195\169mente une fa\195\167on d'afficher des astuces dans la page aux utilisateurs qui ne les ont pas d\195\169j\195\160 vues."]]
let tips3 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This page contains a tip, that you will see only as connected user, until you close it."]
| Fr -> [txt "Cette page contient une astuce, que vous allez voir seulement en tant qu'utilisateur connect\195\169, jusqu'\195\160 ce que vous la fermiez."]
let tips4 ?(lang = get_language ()) () ~set_page () =
match lang with
| En -> List.flatten [[txt "It is possible to reset the set of already seen tips from the "];set_page;[txt "."]]
| Fr -> List.flatten [[txt "Il est possible de r\195\169initialiser l'ensemble des astuces d\195\169j\195\160 vues depuis la page "];set_page;[txt "."]]
let tips5 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "settings page"]
| Fr -> [txt "page Param\195\168tres"]
let demo_intro ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Demo: introduction"]
| Fr -> [txt "Demo\194\160: introduction"]
let general_principles ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "General principles"]
| Fr -> [txt "Principes g\195\169n\195\169raux"]
let demo_intro_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Ocsigen provides a set of largely independent tools for implementing Web and mobile applications (OCaml to JS compiler, Web server, typed HTML, etc.). Ocsigen can be used to implement, depending on your needs, either traditional Web sites (server-side), or client-side apps running in a browser, or full client-server apps, running both in a browser and as mobile apps. Ocsigen Start is a template for quickly writing such a client-server app."]
| Fr -> [txt "Ocsigen fournit un ensemble d'outils largement ind\195\169pendants pour programmer des applications Web et mobiles (compilateur OCaml vers Javascript, serveur Web, HTML typ\195\169, etc.). Cela vous permet d'\195\169crire, selon vos besoins, des sites Web traditionnels (c\195\180t\195\169 serveur), des applications clientes s'ex\195\169cutant dans une page Web, ou de v\195\169ritables applications client-serveur, pouvant s'ex\195\169cuter dans un navigateur ou comme application mobile. Ocsigen Start est un template pr\195\170t \195\160 utiliser pour ce type d'applications client-serveur."]
let demo_intro_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Ocsigen Eliom is a set of libraries for Web programming in OCaml: sessions, services, client-server communication, etc. It also contains an extension of the OCaml language to write a client-server program as a single app. Code annotations permit distinguishing between the code to be included in the server app, the code for the client app, and the code to be included in both of them. Have a look at the code of this app to learn how to generate typed HTML pages, how to call server function from client side, or how to send information to client applications (notifications)."]
| Fr -> [txt "Ocsigen Eliom est un ensemble de biblioth\195\168ques pour la programmation Web en OCaml : sessions, services, communication client-serveur, etc. Il contient aussi une extension du langage OCaml permettant d'\195\169crire des applications client-serveur. Des annotations du code permettent de distinguer le code devant \195\170tre inclus dans l'application serveur, du code qui doit \195\170tre inclus dans l'application cliente. Regardez le code source de cette application pour apprendre comment g\195\169n\195\169rer des pages HTML bien typ\195\169es, comment appeler une fonction serveur depuis un programme client, ou encore comment envoyer des informations aux clients connect\195\169s (notifications)."]
let demo_intro_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Read tutorials on Ocsigen's Web site for a more detailed introduction."]
| Fr -> [txt "Lisez les tutoriels du site d'Ocsigen pour une introduction plus d\195\169taill\195\169e."]
let demo_pagetransition ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Page transitions"]
| Fr -> [txt "Transition de pages"]
let demo_pagetransition_intro ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This demo illustrates smooth page transitions and the retention of a page's scroll position. To see the effects scroll a bit and click on one of the links. When you return to this page by hitting the back button the DOM of the page along with its scroll position will be restored from the cache without being charged from the server or generated on the client."]
| Fr -> [txt "Cette d\195\169mo pr\195\169sente des changement de page anim\195\169s et la m\195\169morisation des positions de scroll. Pour voir ces effets faites d\195\169filer la page un peu vers le bas et cliquez sur un des liens de la liste. Quand vous retournerez sur cette page en appuyant sur le bouton \194\171retour\194\187, le DOM de la page sera servi directement du cache sans \195\170tre g\195\169n\195\169r\195\169 une nouvelle fois. La position du d\195\169filement aura \195\169t\195\169 sauvegard\195\169."]
let demo_pagetransition_add_button ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Add"]
| Fr -> [txt "Ajouter"]
let demo_pagetransition_back_button ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Go back"]
| Fr -> [txt "Retourner"]
let demo_pagetransition_list_page ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "List Page"]
| Fr -> [txt "Page Liste"]
let demo_pagetransition_detail_page ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Detail Page"]
| Fr -> [txt "Page de D\195\169tails"]
let demo_pull_to_refresh ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Pull to refresh"]
| Fr -> [txt "Tirer pour rafra\195\174chir"]
let demo_pull_to_refresh_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This is an example of a page with refreshable content. It is a very common feature in mobile applications. You will need to view this page on your phone to see it work."]
| Fr -> [txt "Cette d\195\169mo pr\195\169sente une page avec du contenu actualisable. C'est une fonctionnalit\195\169 tr\195\168s pr\195\169sente dans les applications mobiles. Pour voir les effets de cette page, ouvrez-la dans l'application mobile."]
let demo_pull_to_refresh_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This page contains a counter that increases every time you \"refresh\" by pulling down the page. This feature is called \"pull to refresh\", but you give it your own action to be performed after the motion. Here, it updates a reactive signal after a second, but in your application, you will probably fetch data and update a more complicated signal than a number to rebuild a part of or the whole page, or do anything else you want."]
| Fr -> [txt "Cette page contient un compteur qui s'incr\195\169mente chaque fois que vous \"rechargez\" la page en tirant vers le bas avec votre doigt. Cette fonctionnalit\195\169 s'appelle \"Tirer pour rafra\195\174chir\", mais vous fournissez votre propre action \195\160 effectuer \195\160 la fin du geste. Ici, nous mettons un simple signal r\195\169actif \195\160 jour, mais dans votre application, vous r\195\169cup\195\169rerez probablement des donn\195\169es depuis le serveur pour mettre \195\160 jour un signal plus compliqu\195\169 qu'un nombre pour reconstruire toute ou une partie de la page, ou faire ce que vous voulez d'autre."]
let demo_pull_to_refresh_counter ?(lang = get_language ()) () ~n () =
match lang with
| En -> List.flatten [[txt "You refreshed the page "];n;[txt " times."]]
| Fr -> List.flatten [[txt "Vous avez rafra\195\174chi la page "];n;[txt " fois."]]
let disconnect_all ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Logout on all my devices"]
| Fr -> [txt "Me d\195\169connecter sur tous mes appareils"]
module S = struct
let welcome_text1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Welcome to Ocsigen Start. This is a template for applications based on Ocsigen (Eliom, Js_of_ocaml, etc.)."
| Fr -> "Bienvenue dans Ocsigen Start\194\160! Ceci est un template d'application \195\169crite avec Ocsigen (Eliom, Js_of_ocaml, etc.)."
let welcome_text2 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Use it:"
| Fr -> "Utilisez-le\194\160:"
let welcome_text3 ?(lang = get_language ()) ()  () =
match lang with
| En -> "As a basis for your own applications."
| Fr -> "Comme point de d\195\169part pour vos propres applications\194\160;"
let welcome_text4 ?(lang = get_language ()) ()  () =
match lang with
| En -> "To learn the most important concepts of client-server programming with Ocsigen."
| Fr -> "Pour apprendre les principaux concepts de la programmation client-serveur avec Ocsigen."
let welcome_text5 ?(lang = get_language ()) ()  () =
match lang with
| En -> "This application contains:"
| Fr -> "Cette application contient\194\160:"
let welcome_text6 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Features for user management (log-in form, user registration, activation links, password recovery, settings page, etc.)."
| Fr -> "Des fonctionnalit\195\169s de gestion des utilisateurs (connexion, cr\195\169ation d'utilisateur, liens d'activation, r\195\169cup\195\169ration de mot de passe, param\195\168tres de l'utilisateur,...)\194\160;"
let welcome_text7 ?(lang = get_language ()) ()  () =
match lang with
| En -> "An extensive demo of the most important features you need to implement your own app. Read the source code to learn! And remove the demo part when you're ready to start with your own app."
| Fr -> "Une d\195\169mo des plus importantes fonctionnalit\195\169s dont vous avez besoin pour \195\169crire votre propre application. Lisez le code source pour apprendre\194\160! Ensuite enlevez la partie demo quand vous \195\170tes pr\195\170ts \195\160 commencer votre propre application\194\160;"
let welcome_text8 ?(lang = get_language ()) ()  () =
match lang with
| En -> "A library with useful features (tips, notifications, etc.)."
| Fr -> "Une biblioth\195\168que avec de nombeuses fonctionnalit\195\169s utiles (tips, notifications, etc.)\194\160;"
let welcome_text9 ?(lang = get_language ()) ()  () =
match lang with
| En -> "All the features you need to create a multilingual app."
| Fr -> "Tous les outils pour cr\195\169er une application multilingue\194\160;"
let welcome_text10 ?(lang = get_language ()) ()  () =
match lang with
| En -> "A basic responsive CSS."
| Fr -> "Une feuille de style \"responsive\" basique."
let welcome_text11 ?(lang = get_language ()) ()  () =
match lang with
| En -> "This application is multi-platform: it can run as a client-server Web application (with server-side generated pages) and as a mobile app (with client-side generated pages) for Android, iOS or Windows. Have a look at the README file to learn how to generate the mobile apps, which you will be able to upload on Google Play or Apple App Store. "
| Fr -> "Cette application est multi-plateforme\194\160: elle peut tourner comme application Web client-serveur (avec des pages g\195\169n\195\169r\195\169es c\195\180t\195\169 serveur) ou bien comme application mobile pour iOS, Android ou Windows (avec des pages g\195\169n\195\169r\195\169es c\195\180t\195\169 client). Regardez le fichier README pour apprendre comment g\195\169n\195\169rer les applications mobiles que vous pourrez envoyer sur Google Play ou Apple App Store."
let about_handler_template ?(lang = get_language ()) ()  () =
match lang with
| En -> "This template provides a skeleton for an Ocsigen application."
| Fr -> "Ce template fournit une base pour une application Ocsigen."
let about_handler_license ?(lang = get_language ()) ()  () =
match lang with
| En -> "Feel free to modify the generated code and use it or redistribute it in any way you want."
| Fr -> "Vous \195\170tes libres de modifier le code g\195\169n\195\169r\195\169 et de l'utiliser ou le redistribuer comme vous le souhaitez."
let footer_generated ?(lang = get_language ()) ()  () =
match lang with
| En -> "This application has been generated using the"
| Fr -> "Cette application a \195\169t\195\169 g\195\169n\195\169r\195\169e en utilisant le template d'"
let footer_eliom_distillery ?(lang = get_language ()) ()  () =
match lang with
| En -> "template for Eliom-distillery and uses the"
| Fr -> "avec Eliom-distillery et utilise les technologies"
let footer_technology ?(lang = get_language ()) ()  () =
match lang with
| En -> " technology."
| Fr -> "."
let home ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "H" else "h");"ome"]
| Fr -> String.concat "" [(if capitalize then "H" else "h");"ome"]
let about ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "A" else "a");"bout"]
| Fr -> String.concat "" [(if capitalize then "\195\128" else "\195\160");" propos"]
let demo ?(lang = get_language ()) ()  () =
match lang with
| En -> "Demo"
| Fr -> "D\195\169mo"
let password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "P" else "p");"assword"]
| Fr -> String.concat "" [(if capitalize then "M" else "m");"ot de passe"]
let retype_password ?(lang = get_language ()) ()  () =
match lang with
| En -> "retype your password"
| Fr -> "retapez votre mot de passe"
let your_email ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "Y" else "y");"our email"]
| Fr -> String.concat "" [(if capitalize then "V" else "v");"otre e-mail"]
let your_password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "Y" else "y");"our password"]
| Fr -> String.concat "" [(if capitalize then "V" else "v");"otre mot de passe"]
let keep_logged_in ?(lang = get_language ()) ()  () =
match lang with
| En -> "keep me logged in"
| Fr -> "rester connect\195\169"
let sign_in ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "S" else "s");"ign in"]
| Fr -> String.concat "" [(if capitalize then "S" else "s");"e connecter"]
let forgot_your_password_q ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "F" else "f");"orgot your password?"]
| Fr -> String.concat "" [(if capitalize then "M" else "m");"ot de passe oubli\195\169\194\160?"]
let sign_up ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "S" else "s");"ign up"]
| Fr -> String.concat "" [(if capitalize then "S" else "s");"'enregistrer"]
let logout ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "L" else "l");"ogout"]
| Fr -> String.concat "" [(if capitalize then "S" else "s");"e d\195\169connecter"]
let set_as_main_email ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "S" else "s");"et as main email"]
| Fr -> String.concat "" [(if capitalize then "D" else "d");"\195\169finir comme e-mail principal"]
let validated ?(lang = get_language ()) () ?(capitalize=false) ?(f=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "V" else "v");"alidated"]
| Fr -> String.concat "" [(if capitalize then "V" else "v");"alid\195\169";(if f then "e" else "")]
let waiting_confirmation ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "W" else "w");"aiting for confirmation"]
| Fr -> String.concat "" [(if capitalize then "E" else "e");"n attente de confirmation"]
let main_email ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "M" else "m");"ain email"]
| Fr -> String.concat "" [(if capitalize then "E" else "e");"-mail principal"]
let change_password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "C" else "c");"hange your password:"]
| Fr -> String.concat "" [(if capitalize then "C" else "c");"hanger votre mot de passe\194\160:"]
let link_new_email ?(lang = get_language ()) ()  () =
match lang with
| En -> "Link a new email to your account:"
| Fr -> "Ajouter une adresse e-mail \195\160 votre compte\194\160:"
let currently_registered_emails ?(lang = get_language ()) ()  () =
match lang with
| En -> "Currently registered emails:"
| Fr -> "E-mails actuellement enregistr\195\169s\194\160:"
let settings ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "S" else "s");"ettings"]
| Fr -> String.concat "" [(if capitalize then "P" else "p");"aram\195\168tres"]
let error ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "E" else "e");"rror"]
| Fr -> String.concat "" [(if capitalize then "E" else "e");"rreur"]
let example_tip ?(lang = get_language ()) ()  () =
match lang with
| En -> "This is an example of tip."
| Fr -> "Ceci est un exemple de tip."
let look_module_tip ?(lang = get_language ()) ()  () =
match lang with
| En -> "Look at the code to see how it is defined."
| Fr -> "Regardez le code pour voir comment c'est d\195\169fini."
let passwords_do_not_match ?(lang = get_language ()) ()  () =
match lang with
| En -> "Passwords do not match"
| Fr -> "Les mots de passe ne correspondent pas"
let generate_action_link_key_subject_email ?(lang = get_language ()) ()  () =
match lang with
| En -> "creation"
| Fr -> "cr\195\169ation"
let sign_up_email_msg ?(lang = get_language ()) ()  () =
match lang with
| En -> "Welcome!\\r\\nTo confirm your email address, please click on this link:"
| Fr -> "Bienvenue\194\160!\\r\\nPour confirmer votre adresse e-mail, cliquer sur ce lien\194\160:"
let email_already_exists ?(lang = get_language ()) ()  () =
match lang with
| En -> "Email already exists"
| Fr -> "Cet e-mail existe d\195\169j\195\160"
let user_does_not_exist ?(lang = get_language ()) ()  () =
match lang with
| En -> "user does not exist"
| Fr -> "Cet utilisateur n'existe pas"
let account_not_activated ?(lang = get_language ()) ()  () =
match lang with
| En -> "Account not activated"
| Fr -> "Ce compte n'est pas activ\195\169"
let wrong_password ?(lang = get_language ()) ()  () =
match lang with
| En -> "Wrong password"
| Fr -> "Mauvais mot de passe"
let no_such_user ?(lang = get_language ()) ()  () =
match lang with
| En -> "No such user"
| Fr -> "Cet utilisateur n'existe pas"
let add_email_msg ?(lang = get_language ()) ()  () =
match lang with
| En -> "Welcome!\\r\\nTo confirm your email address, please click on this link:"
| Fr -> "Bienvenue\194\160!\\r\\nPour confirmer votre adresse e-mail, cliquez sur ce lien\194\160:"
let invalid_action_key ?(lang = get_language ()) ()  () =
match lang with
| En -> "Invalid action key, please ask for a new one."
| Fr -> "Clef d'action invalide. Demandez en une nouvelle svp."
let forgot_pwd_email_msg ?(lang = get_language ()) ()  () =
match lang with
| En -> "Hi,\\r\\nTo set a new password, please click on this link:"
| Fr -> "Bonjour,\\r\\nPour mettre \195\160 jour votre mot de passe, cliquez sur ce lien\194\160:"
let must_be_connected_to_see_page ?(lang = get_language ()) ()  () =
match lang with
| En -> "You must be connected to see this page."
| Fr -> "Vous devez \195\170tre connect\195\169 pour voir cette page."
let email_address ?(lang = get_language ()) ()  () =
match lang with
| En -> "Email address"
| Fr -> "Adresse e-mail"
let your_first_name ?(lang = get_language ()) ()  () =
match lang with
| En -> "Your first name"
| Fr -> "Votre pr\195\169nom"
let your_last_name ?(lang = get_language ()) ()  () =
match lang with
| En -> "Your last name"
| Fr -> "Votre nom"
let submit ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "S" else "s");"ubmit"]
| Fr -> String.concat "" [(if capitalize then "E" else "e");"nvoyer"]
let see_help_again_from_beginning ?(lang = get_language ()) ()  () =
match lang with
| En -> "See help again from beginning"
| Fr -> "Revoir l'aide depuis le d\195\169but"
let personal_information_not_set ?(lang = get_language ()) ()  () =
match lang with
| En -> "Your personal information has not been set yet."
| Fr -> "Vous n'avez pas encore entr\195\169 vos donn\195\169es personnelles."
let take_time_enter_name_password ?(lang = get_language ()) ()  () =
match lang with
| En -> "Please take time to enter your name and to set a password."
| Fr -> "Veuillez entrer votre nom et choisir un mot de passe svp."
let wrong_data_fix ?(lang = get_language ()) ()  () =
match lang with
| En -> "Wrong data. Please fix."
| Fr -> "Donn\195\169es incorrectes. Veuillez corriger."
let send ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "S" else "s");"end"]
| Fr -> String.concat "" [(if capitalize then "E" else "e");"nvoyer"]
let recover_password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "R" else "r");"ecover password"]
| Fr -> String.concat "" [(if capitalize then "R" else "r");"\195\169cup\195\169rer le mot de passe."]
let welcome ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "W" else "w");"elcome!"]
| Fr -> String.concat "" [(if capitalize then "B" else "b");"ienvenue\194\160!"]
let log_in_to_see_page ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "L" else "l");"og in to see this page."]
| Fr -> String.concat "" [(if capitalize then "C" else "c");"onnectez-vous pour voir cette page."]
let you_click_on_date ?(lang = get_language ()) () ~d ~m ~y () =
match lang with
| En -> String.concat "" ["You clicked on ";y;"/";m;"/";d]
| Fr -> String.concat "" ["Vous avez cliqu\195\169 sur ";d;"/";m;"/";y]
let you_click_on_time ?(lang = get_language ()) () ~h ~m () =
match lang with
| En -> String.concat "" ["You clicked on ";h;":";m]
| Fr -> String.concat "" ["Vous avez cliqu\195\169 sur ";h;":";m]
let demo_cache ?(lang = get_language ()) ()  () =
match lang with
| En -> "Cache"
| Fr -> "Cache"
let demo_cache_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Caching the data"
| Fr -> "Mise en cache des donn\195\169es"
let demo_cache_2 ?(lang = get_language ()) () ~eliom_cscache ~os_user_proxy () =
match lang with
| En -> String.concat "" ["Module ";eliom_cscache;" implements a cache of data that is designed for Eliom's client-server programming model. It permits saving a client-side copy of the data. Have a look at the module ";os_user_proxy;" to see how it works (and use this module for getting information about Ocsigen Start's users)."]
| Fr -> String.concat "" ["Le module ";eliom_cscache;" impl\195\169mente un cache de donn\195\169es construit pour le mod\195\168le client-serveur d'Eliom. Il permet de sauvegarder une copie des donn\195\169es du client. Jetez un oeil au module ";os_user_proxy;" pour comprendre son fonctionnement (et utilisez ce module pour obtenir des informations sur les utilisateurs d'Ocsigen Start)."]
let demo_cache_3 ?(lang = get_language ()) () ~eliom_cscache () =
match lang with
| En -> String.concat "" ["When you get a piece of data through ";eliom_cscache;" from client-side, the request to the server is done only if the data is not already in the client-side cache. On server-side, ";eliom_cscache;" is using a temporary cache (with \\\"request\\\" scope) to avoid fetching the data several times from the database during the same request. This server-side cache is automatically sent to the client to fill the client-side cache. If you want to avoid too many requests from the client, prefill the server-side cache with the data the client program will need."]
| Fr -> String.concat "" ["Quand une donn\195\169e du client est obtenue via ";eliom_cscache;", la requ\195\170te vers le serveur est faite uniquement si la donn\195\169e ne se trouve pas d\195\169j\195\160 dans le cache c\195\180t\195\169 client. Du c\195\180t\195\169 serveur, ";eliom_cscache;" utilise un cache temporaire (avec une port\195\169e de type \"request\") afin d'\195\169viter de r\195\169cup\195\169rer les donn\195\169es de la base de donn\195\169es plusieurs fois sur une m\195\170me requ\195\170te. Ce cache c\195\180t\195\169 serveur est automatiquement envoy\195\169 au client pour remplir son cache. Pour \195\169viter un trop grand nombre de requ\195\170tes c\195\180t\195\169 client, pr\195\169-remplissez le cache c\195\180t\195\169 serveur avec les donn\195\169es dont le programme c\195\180t\195\169 client a besoin."]
let demo_cache_4 ?(lang = get_language ()) () ~eliom_cscache () =
match lang with
| En -> String.concat "" ["In the near future, ";eliom_cscache;" will enable saving persistent data, which is useful for implementing off-line applications."]
| Fr -> String.concat "" ["Prochainement, ";eliom_cscache;" permettra de sauvegarder des donn\195\169es de mani\195\168re persistante, ce qui peut \195\170tre utile pour impl\195\169menter des applications hors ligne."]
let demo_calendar ?(lang = get_language ()) ()  () =
match lang with
| En -> "Calendar"
| Fr -> "Calendrier"
let this_page_show_calendar ?(lang = get_language ()) ()  () =
match lang with
| En -> "This page shows Ocsigen Toolkit's date picker."
| Fr -> "Cette page montre le s\195\169lecteur de date d'Ocsigen Toolkit."
let demo_carousel_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Carousel"
| Fr -> "Carousel"
let ot_carousel_first_example_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "This is a first example of Ocsigen Toolkit's carousel."
| Fr -> "Voici le premier exemple du carousel d'Ocsigen Toolkit."
let ot_carousel_first_example_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> "The carousel displays a number of blocks side-by-side (or vertically stacked)."
| Fr -> "Le carousel sert \195\160 afficher des blocs c\195\180te-\195\160-c\195\180te (ou empil\195\169s verticalement)."
let ot_carousel_first_example_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> "To switch to a different block, use the buttons in the carousel."
| Fr -> "Pour vous rendre sur un autre bloc, utilisez les boutons dans le carousel."
let ot_carousel_first_example_4 ?(lang = get_language ()) ()  () =
match lang with
| En -> "On touch screens you can also swipe with your fingers."
| Fr -> "Sur les \195\169crans tactiles, swipez avec les doigts."
let demo_carousel_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Carousel: page with tabs"
| Fr -> "Carousel\194\160: page avec onglets"
let ot_carousel_second_example_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "This page demonstrates how to use Ocsigen Toolkit's carousel to display a page with several tabs."
| Fr -> "Cette page montre comment utiliser le carousel d'Ocsigen Toolkit pour afficher une page avec des onglets."
let ot_carousel_second_example_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Try to swipe on a touch screen."
| Fr -> "Sur \195\169cran tactile, glissez pour changer d'onglet."
let ot_carousel_second_example_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Try on a small screen or browser window to see how the tabs stick on top while scrolling the page."
| Fr -> "Essayez sur un petit \195\169cran pour voir comment la barre d'onglets se fixe en haut de la page quand vous faites d\195\169filer la page vers le bas."
let monday ?(lang = get_language ()) ()  () =
match lang with
| En -> "Monday"
| Fr -> "Lundi"
let tuesday ?(lang = get_language ()) ()  () =
match lang with
| En -> "Tuesday"
| Fr -> "Mardi"
let wednesday ?(lang = get_language ()) ()  () =
match lang with
| En -> "Wednesday"
| Fr -> "Mercredi"
let thursday ?(lang = get_language ()) ()  () =
match lang with
| En -> "Thursday"
| Fr -> "Jeudi"
let friday ?(lang = get_language ()) ()  () =
match lang with
| En -> "Friday"
| Fr -> "Vendredi"
let saturday ?(lang = get_language ()) ()  () =
match lang with
| En -> "Saturday"
| Fr -> "Samedi"
let sunday ?(lang = get_language ()) ()  () =
match lang with
| En -> "Sunday"
| Fr -> "Dimanche"
let demo_carousel_wheel ?(lang = get_language ()) ()  () =
match lang with
| En -> "Wheel carousel"
| Fr -> "Carousel avec roulette"
let demo_carousel_third_example_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Example of a vertical circular carousel (wheel). Try with a touch screen."
| Fr -> "Exemple de carousel vertical circulaire (wheel). Essayez avec un \195\169cran tactile."
let demo_notification ?(lang = get_language ()) ()  () =
match lang with
| En -> "Notifications"
| Fr -> "Notifications"
let demo_notification_got ?(lang = get_language ()) ()  () =
match lang with
| En -> "got"
| Fr -> "Re\195\167u"
let exchange_msg_between_users ?(lang = get_language ()) () ~os_notif () =
match lang with
| En -> String.concat "" ["Module ";os_notif;" enables sending information to client applications (notifications, new messages ...)."]
| Fr -> String.concat "" ["Le module ";os_notif;" permet d'envoyer des donn\195\169es aux applications client (notifications, nouveaux messages,...)."]
let open_multiple_tabs_browsers ?(lang = get_language ()) ()  () =
match lang with
| En -> "Open this page in multiple tabs or browsers."
| Fr -> "Ouvrez cette page dans plusieurs onglets et fen\195\170tres."
let fill_input_form_send_message ?(lang = get_language ()) ()  () =
match lang with
| En -> "Fill in the input form to send a message to all other tabs."
| Fr -> "Remplissez le formulaire pour envoyer un message \195\160 tous vos autres onglets ouverts."
let send_message ?(lang = get_language ()) ()  () =
match lang with
| En -> "send message"
| Fr -> "envoyer le message"
let demo_pgocaml ?(lang = get_language ()) ()  () =
match lang with
| En -> "Database request"
| Fr -> "Requ\195\170te \195\160 la base de donn\195\169es."
let no_user_create_accounts ?(lang = get_language ()) ()  () =
match lang with
| En -> "No user. Create some accounts to test."
| Fr -> "Aucun utilisateur. Cr\195\169ez quelques comptes pour tester."
let demo_pgocaml_users ?(lang = get_language ()) ()  () =
match lang with
| En -> "Users:"
| Fr -> "Utilisateurs\194\160:"
let demo_pgocaml_description_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "This page shows signed-up users fetched from the database."
| Fr -> "Cette page montre tous les utilisateurs inscrits qui ont \195\169t\195\169 r\195\169cup\195\169r\195\169s de la base de donn\195\169es."
let demo_pgocaml_description_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Have a look at the source code to see how to make a DB request with PGOCaml."
| Fr -> "Regardez dans le code source comment r\195\169aliser une requ\195\170te \195\160 la base de donn\195\169es en utilisant PGOcaml."
let demo_pgocaml_description_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> "We are using Ot_spinner to display the list, which means that, in the case the page is generated client-side, the page will be displayed immediately with a spinner, that will be replaced by the contents when ready. The code contains a 2s sleep to demonstrate the spinner."
| Fr -> "Nous utilisons Ot_spinner pour afficher la liste\194\160: dans le cas o\195\185 une page est g\195\169n\195\169r\195\169e c\195\180t\195\169 client, cette page est affich\195\169e imm\195\169diatement avec une ic\195\180ne de chargement qui sera remplac\195\169e par le contenu quand il sera pr\195\170t. Pour la d\195\169monstration, nous avons ajout\195\169 une pause de 2s pour laisser le temps de voir l'ic\195\180ne de chargement."
let demo_popup ?(lang = get_language ()) ()  () =
match lang with
| En -> "Popup Button"
| Fr -> "Bouton popup"
let demo_popup_click ?(lang = get_language ()) ()  () =
match lang with
| En -> "Click for a popup!"
| Fr -> "Cliquez pour afficher un popup\194\160!"
let demo_popup_message ?(lang = get_language ()) ()  () =
match lang with
| En -> "Popup message"
| Fr -> "Message du popup"
let demo_popup_content ?(lang = get_language ()) ()  () =
match lang with
| En -> "Here is a button showing a simple popup window when clicked:"
| Fr -> "Voici un bouton affichant une simple fen\195\170tre popup quand vous cliquez dessus\194\160:"
let demo_reactive_programming ?(lang = get_language ()) ()  () =
match lang with
| En -> "Reactive pages"
| Fr -> "Pages r\195\169actives"
let demo_reactive_programming_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "This is an example of a page with reactive content. It is a very convenient solution to update pages when data changes."
| Fr -> "Ceci est un exemple d'une page avec du contenu r\195\169actif. C'est une solution extr\195\170ment simple pour mettre \195\160 jour une page quand les donn\195\169es changent."
let demo_reactive_programming_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> "It defines a (client-side) reactive OCaml list. You can add elements in this list via the input form. The page is updated automatically when the value of the reactive list changes."
| Fr -> "Il d\195\169finit une liste OCaml r\195\169active (c\195\180t\195\169 client). Vous pouvez ajouter des \195\169lements dans cette liste via le formulaire. Cette page sera automatique mise \195\160 jour quand la valeur de la liste r\195\169active changera."
let demo_reactive_programming_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> "The reactive page is generated either server-side (for example when you are using a Web browser and you reload this page) or client-side (in a mobile app or if you were already in the app before coming to this page)."
| Fr -> "La page r\195\169active est g\195\169n\195\169r\195\169e aussi bien du c\195\180t\195\169 serveur (par exemple quand vous utilisez le navigateur web et que vous rechargez cette page) que du c\195\180t\195\169 client (sur mobile ou si vous \195\169tiez d\195\169j\195\160 sur dans cette application avant de venir sur cette page)."
let demo_reactive_programming_button ?(lang = get_language ()) ()  () =
match lang with
| En -> "add"
| Fr -> "ajouter"
let demo_eliom_ref ?(lang = get_language ()) ()  () =
match lang with
| En -> "Eliom references + OS dates"
| Fr -> "R\195\169f\195\169rences Eliom + dates OS"
let demo_eliom_ref_first_visit ?(lang = get_language ()) ()  () =
match lang with
| En -> "This is your first visit."
| Fr -> "Ceci est votre premi\195\168re visite."
let demo_eliom_ref_last_visit ?(lang = get_language ()) ()  () =
match lang with
| En -> "The last time you visited was: "
| Fr -> " La derni\195\168re fois que vous avez visit\195\169 ce lien \195\169tait\194\160:"
let demo_eliom_ref_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "We use an Eliom reference to record the last time you visited this page. Eliom references make it possible to save, server-side, data specific to one user, one browser, or one tab. "
| Fr -> "Nous utilisons les r\195\169f\195\169rences Eliom pour sauvegarder la date de la derni\195\168re fois que vous avez visit\195\169 cette page. Les r\195\169f\195\169rences Eliom permettent de sauvegarder c\195\180t\195\169 serveur des donn\195\169es propres \195\160 un utilisateur, \195\160 un navigateur ou \195\160 un onglet."
let demo_eliom_ref_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> "The value is different for each user."
| Fr -> "La valeur est diff\195\169rente pour chaque utilisateur."
let demo_eliom_ref_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> "The reference has been updated. Come back later!"
| Fr -> "La r\195\169f\195\169rence a \195\169t\195\169 mise \195\160 jour. Revenez plus tard\194\160!"
let demo_rpc_button ?(lang = get_language ()) ()  () =
match lang with
| En -> "RPC button"
| Fr -> "Bouton RPC"
let demo_rpc_button_click_increase ?(lang = get_language ()) ()  () =
match lang with
| En -> "Click to increase server-side value"
| Fr -> "Cliquez pour augmenter la valeur c\195\180t\195\169 serveur."
let demo_rpc_button_description ?(lang = get_language ()) ()  () =
match lang with
| En -> "This button performs an RPC to increase a server-side value."
| Fr -> "Ce bouton r\195\169alise un appel de fonction distante (RPC) pour augmenter une valeur c\195\180t\195\169 serveur."
let demo_spinner ?(lang = get_language ()) ()  () =
match lang with
| En -> "Spinner"
| Fr -> "Ic\195\180ne de chargement"
let demo_spinner_content_ready ?(lang = get_language ()) ()  () =
match lang with
| En -> "The content is ready. "
| Fr -> "Le contenu est pr\195\170t."
let demo_spinner_message_replace_spinner ?(lang = get_language ()) ()  () =
match lang with
| En -> "This message has replaced the spinner."
| Fr -> "Ce message a remplac\195\169 l'ic\195\180ne de chargement."
let demo_spinner_description_ot ?(lang = get_language ()) ()  () =
match lang with
| En -> "This is a demo of the Ocsigen Toolkit spinner widget."
| Fr -> "Ceci est la d\195\169monstration de l'ic\195\180ne de chargement d'Ocsigen Toolkit."
let demo_spinner_description_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "We use this widget to integrate into the page an HTML block that takes a long time to produce, e.g., because of a slow server call."
| Fr -> "Nous utilisons ce widget pour int\195\169grer dans notre page HTML un bloc qui prend un long moment \195\160 produire, par exemple, \195\160 cause d'un appel serveur lent."
let demo_spinner_description_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> "A spinner is displayed, which is then replaced with the actual content when this content is ready."
| Fr -> "Une ic\195\180ne de chargement est d'abord affich\195\169e, puis remplac\195\169e par le vrai contenu quand celui-ci est pr\195\170t."
let demo_spinner_description_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> "For the demo we just sleep for 5 seconds to simulate waiting for the content."
| Fr -> "Pour la d\195\169monstration, nous avons ajout\195\169 un d\195\169lai de 5 secondes pour simuler l'attente du contenu."
let demo_spinner_generated_client_side ?(lang = get_language ()) ()  () =
match lang with
| En -> "The spinner is generated client-side."
| Fr -> "L'ic\195\180ne de chargement est g\195\169n\195\169r\195\169e c\195\180t\195\169 client."
let demo_timepicker ?(lang = get_language ()) ()  () =
match lang with
| En -> "Time picker"
| Fr -> "S\195\169lecteur d'heure"
let demo_timepicker_back_to_hours ?(lang = get_language ()) ()  () =
match lang with
| En -> "Back to hours"
| Fr -> "Revenir aux heures"
let demo_timepicker_description ?(lang = get_language ()) ()  () =
match lang with
| En -> "This page shows the Ocsigen Toolkit's time picker."
| Fr -> "Cette page montre le s\195\169lecteur d'heure d'Ocsigen Toolkit."
let demo_tips ?(lang = get_language ()) ()  () =
match lang with
| En -> "Tips"
| Fr -> "Astuces"
let change_profile_picture ?(lang = get_language ()) ()  () =
match lang with
| En -> "Change profile picture"
| Fr -> "Changer votre photo de profil."
let demo_tongue_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Tongue"
| Fr -> "Languette"
let ot_tongue_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "This is an example of page with a tongue coming from the bottom of the screen. try to slide it with your finger on a mobile screen."
| Fr -> "Ceci est un exemple de page avec une languette partant du bas de l'\195\169cran. Essayez de la faire glisser vers le haut avec le doigt sur un t\195\169l\195\169phone mobile."
let demo_widget_ot ?(lang = get_language ()) ()  () =
match lang with
| En -> "This app also contains demos for some widgets from Ocsigen Toolkit."
| Fr -> "Cette application contient \195\169galement des d\195\169monstrations de quelques widgets d'Ocsigen Toolkit."
let demo_widget_see_drawer ?(lang = get_language ()) ()  () =
match lang with
| En -> "The different demos are accessible through the drawer menu. To open it click the top left button on the screen."
| Fr -> "Les diff\195\169rentes d\195\169monstrations sont accessibles \195\160 travers le menu. Pour l'ouvrir, cliquez sur le bouton en haut \195\160 gauche de l'\195\169cran."
let demo_widget_feel_free ?(lang = get_language ()) ()  () =
match lang with
| En -> "Feel free to modify the generated code and use it or redistribute it as you want."
| Fr -> "Vous \195\170tes libres de modifier le code g\195\169n\195\169r\195\169 et de l'utiliser ou de le redistribuer comme vous souhaitez."
let users ?(lang = get_language ()) ()  () =
match lang with
| En -> "Users"
| Fr -> "Utilisateurs"
let you_are_not_connected ?(lang = get_language ()) ()  () =
match lang with
| En -> "You are not connected."
| Fr -> "Vous n'\195\170tes pas connect\195\169."
let you_are ?(lang = get_language ()) ()  () =
match lang with
| En -> "You are"
| Fr -> "Vous \195\170tes"
let log_in_to_see_demo ?(lang = get_language ()) ()  () =
match lang with
| En -> "Log in to see the demo."
| Fr -> "Connectez-vous pour voir la d\195\169monstration."
let your_user_id ?(lang = get_language ()) ()  () =
match lang with
| En -> "Your user id"
| Fr -> "Votre ID utilisateur"
let the_module ?(lang = get_language ()) ()  () =
match lang with
| En -> "The module"
| Fr -> "Le module"
let allows_get_information_currently_connected_user ?(lang = get_language ()) ()  () =
match lang with
| En -> "provides information about the currently connected user (server or client side)."
| Fr -> "vous autorise \195\160 obtenir les information de l'utilisateur courant connect\195\169 (c\195\180t\195\169 serveur ou c\195\180t\195\169 client)."
let these_functions_called_server_or_client_side ?(lang = get_language ()) ()  () =
match lang with
| En -> "These functions can be called from either server- or client-side."
| Fr -> "Ces fonctions peuvent \195\170tre appel\195\169es aussi bien c\195\180t\195\169 client que c\195\180t\195\169 serveur."
let always_get_current_user_using_module ?(lang = get_language ()) ()  () =
match lang with
| En -> "Always get the current user id using module"
| Fr -> "R\195\169cup\195\169rez toujours l'ID de l'utilisateur courant en utilisant le module"
let never_trust_client_pending_user_id ?(lang = get_language ()) ()  () =
match lang with
| En -> "Never trust a client sending its own user id!"
| Fr -> "Ne faites jamais confiance \195\160 un client envoyant son propre ID d'utilisateur\194\160!"
let internationalization ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "I" else "i");"nternationalization"]
| Fr -> String.concat "" [(if capitalize then "I" else "i");"nternationalisation"]
let internationalization_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Ocsigen Start uses Ocsigen-i18n for internationalizing your app. Ocsigen-i18n defines a PPX syntax extension for automatically selecting language-dependent text for each user. The user can choose his preferred language from the settings page. By default the browser's language is used."
| Fr -> "Ocsigen Start utilise Ocsigen-i18n for internationaliser les applications. Ocsigen-i18n d\195\169finit une extension de syntaxe PPX qui s\195\169lectionne automatiquement les textes en fonction de la langue de l'utilisateur courant. L'utilisateur peut choisir sa langue pr\195\169f\195\169r\195\169e dans la page de param\195\168tres. Par d\195\169faut, la langue du navigateur est utilis\195\169e."
let internationalization_2 ?(lang = get_language ()) () ~f1 ~f2 () =
match lang with
| En -> String.concat "" ["Write your translations (as tab-separated-values) in file ";f1;". File ";f2;" is generated automatically from this file."]
| Fr -> String.concat "" ["\195\137crivez vos traductions (au format \"tab-separated-values\") dans le fichier ";f1;". Le fichier ";f2;" est g\195\169n\195\169r\195\169 automatiquement \195\160 partir de ce fichier."]
let internationalization_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Have a look at the OCaml code of this page to discover some features of the module Ocsigen-i18n."
| Fr -> "Jetez un coup d'\197\147il au code OCaml de cette page pour d\195\169couvrir quelques astuces du module Ocsigen-i18n."
let links_and_static_files ?(lang = get_language ()) ()  () =
match lang with
| En -> "Links, services and static files"
| Fr -> "Liens, services et fichiers statiques"
let services ?(lang = get_language ()) ()  () =
match lang with
| En -> "Services"
| Fr -> "Services"
let services_1 ?(lang = get_language ()) () ~f1 ~f2 ~f3 () =
match lang with
| En -> String.concat "" ["Have a look at file ";f1;" to see some examples of service definitions. Most service handlers are defined in file ";f2;". Service registration is done in ";f3;". Have a look to see how to define a service returning an application page, an action or a redirection, etc. Read Ocsigen's tutorials and Eliom's manual for more information about services."]
| Fr -> String.concat "" ["Vous trouverez des exemples de d\195\169finition de services dans le fichier ";f1;". La plupart des handlers de services sont d\195\169finis dans le fichier ";f2;". L'enregistrement des services est fait dans le fichier ";f3;". Jetez-y un \197\147il pour voir comment d\195\169finir une nouvelle page pour cette application, une action, une redirection, etc. Lisez les tutoriels d'Ocsigen et le manuel d'Eliom pour plus d'informations sur les services."]
let links_and_forms ?(lang = get_language ()) ()  () =
match lang with
| En -> "Links and forms"
| Fr -> "Liens et formulaires"
let links_and_forms_1 ?(lang = get_language ()) () ~t1 ~t2 () =
match lang with
| En -> String.concat "" ["Here is an example of an ";t1;", and an example of link towards an ";t2;"."]
| Fr -> String.concat "" ["Voici un exemple de ";t1;", et un exemple de lien vers un ";t2;"."]
let internal_link ?(lang = get_language ()) ()  () =
match lang with
| En -> "internal link"
| Fr -> "lien interne"
let external_service ?(lang = get_language ()) ()  () =
match lang with
| En -> "external service"
| Fr -> "service externe"
let static_files ?(lang = get_language ()) ()  () =
match lang with
| En -> "Static files"
| Fr -> "Fichiers statiques"
let static_files_1 ?(lang = get_language ()) () ~static ~static_dir () =
match lang with
| En -> String.concat "" ["Use service ";static_dir;" (predefined in Eliom) to create links towards static files (images, fonts, etc.). Put static files you want to include in the mobile app in directory ";static;". They will be stored locally on the mobile device. By default, links are relative on the Web app and absolute on the mobile app. For example, here is an example of an image stored locally in the mobile app:"]
| Fr -> String.concat "" ["Utilisez le service ";static_dir;" (pr\195\169d\195\169fini dans Eliom) pour faire des liens vers des fichiers statiques (images, fontes, etc.). Les fichiers statiques que vous voulez inclure dans l'application mobile doivent \195\170tre plac\195\169s dans le r\195\169pertoire ";static;". Ils seront stock\195\169s en local sur l'appareil mobile. Par d\195\169faut les liens sont relatifs dans l'application Web et absolus dans l'application mobile. Forcez les liens relatifs pour faire des liens vers des fichiers locaux dans l'application mobile. Par exemple voici une image stock\195\169e localement dans l'application mobile\194\160:"]
let static_files_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> "and a remote image:"
| Fr -> "et une image distante\194\160:"
let change_language ?(lang = get_language ()) ()  () =
match lang with
| En -> "Change language"
| Fr -> "Changer la langue"
let tips1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Tips for new users and new features"
| Fr -> "Astuces pour les nouveaux utilisateurs et nouvelles fonctionnalit\195\169s"
let tips2 ?(lang = get_language ()) () ~os_tips () =
match lang with
| En -> String.concat "" ["Module ";os_tips;" implements a way to display tips in the page to the users who haven't already seen them."]
| Fr -> String.concat "" ["Le module ";os_tips;" impl\195\169mente une fa\195\167on d'afficher des astuces dans la page aux utilisateurs qui ne les ont pas d\195\169j\195\160 vues."]
let tips3 ?(lang = get_language ()) ()  () =
match lang with
| En -> "This page contains a tip, that you will see only as connected user, until you close it."
| Fr -> "Cette page contient une astuce, que vous allez voir seulement en tant qu'utilisateur connect\195\169, jusqu'\195\160 ce que vous la fermiez."
let tips4 ?(lang = get_language ()) () ~set_page () =
match lang with
| En -> String.concat "" ["It is possible to reset the set of already seen tips from the ";set_page;"."]
| Fr -> String.concat "" ["Il est possible de r\195\169initialiser l'ensemble des astuces d\195\169j\195\160 vues depuis la page ";set_page;"."]
let tips5 ?(lang = get_language ()) ()  () =
match lang with
| En -> "settings page"
| Fr -> "page Param\195\168tres"
let demo_intro ?(lang = get_language ()) ()  () =
match lang with
| En -> "Demo: introduction"
| Fr -> "Demo\194\160: introduction"
let general_principles ?(lang = get_language ()) ()  () =
match lang with
| En -> "General principles"
| Fr -> "Principes g\195\169n\195\169raux"
let demo_intro_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Ocsigen provides a set of largely independent tools for implementing Web and mobile applications (OCaml to JS compiler, Web server, typed HTML, etc.). Ocsigen can be used to implement, depending on your needs, either traditional Web sites (server-side), or client-side apps running in a browser, or full client-server apps, running both in a browser and as mobile apps. Ocsigen Start is a template for quickly writing such a client-server app."
| Fr -> "Ocsigen fournit un ensemble d'outils largement ind\195\169pendants pour programmer des applications Web et mobiles (compilateur OCaml vers Javascript, serveur Web, HTML typ\195\169, etc.). Cela vous permet d'\195\169crire, selon vos besoins, des sites Web traditionnels (c\195\180t\195\169 serveur), des applications clientes s'ex\195\169cutant dans une page Web, ou de v\195\169ritables applications client-serveur, pouvant s'ex\195\169cuter dans un navigateur ou comme application mobile. Ocsigen Start est un template pr\195\170t \195\160 utiliser pour ce type d'applications client-serveur."
let demo_intro_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Ocsigen Eliom is a set of libraries for Web programming in OCaml: sessions, services, client-server communication, etc. It also contains an extension of the OCaml language to write a client-server program as a single app. Code annotations permit distinguishing between the code to be included in the server app, the code for the client app, and the code to be included in both of them. Have a look at the code of this app to learn how to generate typed HTML pages, how to call server function from client side, or how to send information to client applications (notifications)."
| Fr -> "Ocsigen Eliom est un ensemble de biblioth\195\168ques pour la programmation Web en OCaml : sessions, services, communication client-serveur, etc. Il contient aussi une extension du langage OCaml permettant d'\195\169crire des applications client-serveur. Des annotations du code permettent de distinguer le code devant \195\170tre inclus dans l'application serveur, du code qui doit \195\170tre inclus dans l'application cliente. Regardez le code source de cette application pour apprendre comment g\195\169n\195\169rer des pages HTML bien typ\195\169es, comment appeler une fonction serveur depuis un programme client, ou encore comment envoyer des informations aux clients connect\195\169s (notifications)."
let demo_intro_3 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Read tutorials on Ocsigen's Web site for a more detailed introduction."
| Fr -> "Lisez les tutoriels du site d'Ocsigen pour une introduction plus d\195\169taill\195\169e."
let demo_pagetransition ?(lang = get_language ()) ()  () =
match lang with
| En -> "Page transitions"
| Fr -> "Transition de pages"
let demo_pagetransition_intro ?(lang = get_language ()) ()  () =
match lang with
| En -> "This demo illustrates smooth page transitions and the retention of a page's scroll position. To see the effects scroll a bit and click on one of the links. When you return to this page by hitting the back button the DOM of the page along with its scroll position will be restored from the cache without being charged from the server or generated on the client."
| Fr -> "Cette d\195\169mo pr\195\169sente des changement de page anim\195\169s et la m\195\169morisation des positions de scroll. Pour voir ces effets faites d\195\169filer la page un peu vers le bas et cliquez sur un des liens de la liste. Quand vous retournerez sur cette page en appuyant sur le bouton \194\171retour\194\187, le DOM de la page sera servi directement du cache sans \195\170tre g\195\169n\195\169r\195\169 une nouvelle fois. La position du d\195\169filement aura \195\169t\195\169 sauvegard\195\169."
let demo_pagetransition_add_button ?(lang = get_language ()) ()  () =
match lang with
| En -> "Add"
| Fr -> "Ajouter"
let demo_pagetransition_back_button ?(lang = get_language ()) ()  () =
match lang with
| En -> "Go back"
| Fr -> "Retourner"
let demo_pagetransition_list_page ?(lang = get_language ()) ()  () =
match lang with
| En -> "List Page"
| Fr -> "Page Liste"
let demo_pagetransition_detail_page ?(lang = get_language ()) ()  () =
match lang with
| En -> "Detail Page"
| Fr -> "Page de D\195\169tails"
let demo_pull_to_refresh ?(lang = get_language ()) ()  () =
match lang with
| En -> "Pull to refresh"
| Fr -> "Tirer pour rafra\195\174chir"
let demo_pull_to_refresh_1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "This is an example of a page with refreshable content. It is a very common feature in mobile applications. You will need to view this page on your phone to see it work."
| Fr -> "Cette d\195\169mo pr\195\169sente une page avec du contenu actualisable. C'est une fonctionnalit\195\169 tr\195\168s pr\195\169sente dans les applications mobiles. Pour voir les effets de cette page, ouvrez-la dans l'application mobile."
let demo_pull_to_refresh_2 ?(lang = get_language ()) ()  () =
match lang with
| En -> "This page contains a counter that increases every time you \"refresh\" by pulling down the page. This feature is called \"pull to refresh\", but you give it your own action to be performed after the motion. Here, it updates a reactive signal after a second, but in your application, you will probably fetch data and update a more complicated signal than a number to rebuild a part of or the whole page, or do anything else you want."
| Fr -> "Cette page contient un compteur qui s'incr\195\169mente chaque fois que vous \"rechargez\" la page en tirant vers le bas avec votre doigt. Cette fonctionnalit\195\169 s'appelle \"Tirer pour rafra\195\174chir\", mais vous fournissez votre propre action \195\160 effectuer \195\160 la fin du geste. Ici, nous mettons un simple signal r\195\169actif \195\160 jour, mais dans votre application, vous r\195\169cup\195\169rerez probablement des donn\195\169es depuis le serveur pour mettre \195\160 jour un signal plus compliqu\195\169 qu'un nombre pour reconstruire toute ou une partie de la page, ou faire ce que vous voulez d'autre."
let demo_pull_to_refresh_counter ?(lang = get_language ()) () ~n () =
match lang with
| En -> String.concat "" ["You refreshed the page ";n;" times."]
| Fr -> String.concat "" ["Vous avez rafra\195\174chi la page ";n;" fois."]
let disconnect_all ?(lang = get_language ()) ()  () =
match lang with
| En -> "Logout on all my devices"
| Fr -> "Me d\195\169connecter sur tous mes appareils"
end
end
]
