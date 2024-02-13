exception Error of string

let template : type a b. string option -> (unit -> a -> string) -> a -> (unit -> b -> string) -> b -> string =
  fun s f1 a1 f2 a2 ->
    let s = match s with
    | None -> ""
    | Some s ->
        Printf.sprintf {|<a class="w3-button w3-red w3-round" href="/update/%s">Edit</a>|} s
        in
        Printf.sprintf
  {|<!DOCTYPE html>
<html lang="en">
  <meta charset="UTF-8">
  <title>Page Title</title>
  <meta name="viewport" content="width=device-width,initial-scale=1">
   <link rel="stylesheet" href="https://www.w3schools.com/w3css/4/w3.css">
  <body>
    <div class="w3-cell-row" style="min-height: 100vh;">
      <div class="w3-container w3-cell" style="background: rgb(61, 79, 93);color: #fff;width:33%%;">
        <h1 class="w3-center">TIL</h1>
        <div class="w3-center">
          <a class="w3-button w3-green w3-round" href="/new">New</a>
          %s
        </div>
        <div>
          %a
        </div>
      </div>
      <div class="w3-container w3-cell">
        %a
      </div>
    </div>
  </body>
  </html>|} s f1 a1 f2 a2 
 
let time_to_string (f:float) : string =
  let tm = Unix.localtime f in
  Printf.sprintf "%02i/%02i/%i" tm.tm_mday (tm.tm_mon+1)
  (tm.tm_year+1900)

let with_date (fn:string) : string * float =
  try
    let full = Printf.sprintf "./posts/%s.md" fn in
    fn, (Unix.stat full).st_ctime
    with
  Unix.Unix_error _ ->
    fn, 0.0
 
let list_posts () : (string*float) list =
  Sys.readdir "./posts"
  |> Array.to_list
  |> List.filter (fun x -> Filename.extension x = ".md") 
  |> List.map Filename.remove_extension
  |> List.map with_date
  |> List.sort (fun (_,d1) (_,d2) -> compare d2 d1)

let rec pp_list (sep:string) (pp:unit -> 'a -> string) (():unit) (lst:'a list) : string =
  match lst with
    | [] -> ""
    | [hd] -> pp () hd
  | hd::tl -> Printf.sprintf "%a%s%a" pp hd sep (pp_list sep pp) tl

let pp_menu () (lst:(string*float) list) : string =
  pp_list "\n" (fun () (fn,da:string*float) ->
    Printf.sprintf
    {|<p><span style="color: rgb(176, 202, 219);">%s - </span><b><a href="/post/%s">%s</a></b></p>|}
    (time_to_string da) fn fn
  ) () lst


let pp_post () p =
  Printf.sprintf "%s" p
  
let add _ =
  let posts = list_posts () in
  Dream.html (template None pp_menu posts 
  (fun _ _ ->
    Printf.sprintf
     {|<h1>Add</h1>
       <form class="w3-container" action="/" method="post">
         <p>
         <label>Name</label>
         <input name="name" class="w3-input w3-border" type="text">
         </p>
         <p>
         <label>Content</label>
         <textarea name="content" class="w3-input w3-border" rows="20"></textarea>
         </p>
         <p>
          <input type="submit" class="w3-button w3-green w3-round" value="Send" />
         </p>
       </form>|}
  ) ())

let read_html fn : string option =
  let fn = Printf.sprintf "./posts/%s.md" fn in
  try
    let input = open_in fn in
    let doc = Omd.of_channel input in
    Some (Omd.to_html doc)
  with
  | Sys_error _ -> None

let read_txt fn : string option =
  let fn = Printf.sprintf "./posts/%s.md" fn in
  try
    let input = open_in fn in
    Some (really_input_string input (in_channel_length input))
  with
  | Sys_error _ -> None
 
let index _ =
  let posts = list_posts () in
  match posts with
  | [] -> 
    Dream.html (template None pp_menu posts (fun () () -> "") ())
  | (fn,_)::_ ->
    begin match read_html fn with
      | Some post -> Dream.html (template (Some fn) pp_menu posts pp_post post)
      | None -> raise (Error ("fail to read file '"^fn^"'"))
    end
 
let view request =
  let posts = list_posts () in
  let fn = Dream.param request "post" in
  match read_html fn with
  | None -> raise (Error ("fail to read file '"^fn^"'"))
  | Some post -> Dream.html (template (Some fn) pp_menu posts pp_post post)

let update request =
  let posts = list_posts () in
  let fn = Dream.param request "post" in
  match read_txt fn with
  | None -> raise (Error ("fail to read file '"^fn^"'"))
  | Some content ->
    Dream.html (template None pp_menu posts (
        fun _ _ ->
          Printf.sprintf
            {|<h1>Update</h1>
       <form class="w3-container" action="/post/%s" method="post">
         <p>
         <label>Name</label>
         <input name="name" class="w3-input w3-border" type="text" value="%s" readonly />
         </p>
         <p>
         <label>Content</label>
         <textarea name="content" class="w3-input w3-border" rows="20">%s</textarea>
         </p>
         <p>
          <input type="submit" class="w3-button w3-green w3-round" value="Send" />
         </p>
       </form>|} fn fn (Dream.html_escape content)
      ) ())

let write fn content =
  try
    let out = open_out fn in
    Printf.fprintf out "%s" content;
    close_out out;
    true
  with Sys_error _ -> false

let do_new request (lst:(string*string) list) =
  match lst with
  | [("content",content);("name",name)] ->
    if not (String.equal (Filename.dirname name) ".") then
        raise (Error ("Invalid name '"^name^"'"))
      else
        let fn = "./posts/" ^ name ^ ".md" in
        if Sys.file_exists fn then
          raise (Error ("File '"^fn^"' already exists"))
        else
          ( ignore (write fn content);
            index request )
  | _ ->
    raise (Error "post creation failed")

let do_update request (lst:(string*string) list) =
  match lst with
  | [("content",content);("name",name)] ->
      if not (String.equal (Filename.dirname name) ".") then
          raise (Error ("Invalid name '"^name^"'"))
      else
        let fn = "./posts/" ^ name ^ ".md" in
        if not (Sys.file_exists fn) then
          raise (Error ("File '"^fn^"' does not exist"))
        else
          ( ignore (write fn content);
            view request )
  | _ ->
    raise (Error "post update failed")

let () =
  Dream.run
  @@ Dream.router [
    Dream.get "/" index;
    Dream.get "/new" add;
    Dream.get "/update/:post" update;
    Dream.get "/post/:post" view;
    Dream.post "/" (fun request ->
      Lwt.bind (Dream.form ~csrf:false request)
      (fun form ->
        match form with
       | `Ok lst -> do_new request lst
       | _ -> raise (Error "post creation failed")));
    Dream.post "/post/:post" (fun request ->
      Lwt.bind (Dream.form ~csrf:false request)
      (fun form ->
        match form with
       | `Ok lst -> do_update request lst
       | _ -> raise (Error "post update failed")));
  ]
