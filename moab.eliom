[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

module Moab_app =
  Eliom_registration.App (
    struct
      let application_name = "moab"
      let global_data_path = None
    end)

let main_service =
  Eliom_service.create
    ~id:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let () =
  Moab_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"moab"
           ~css:[["css";"moab.css"]]
           Html.F.(body [
             h1 [pcdata "Welcome from Eliom's distillery!"];
           ])))
