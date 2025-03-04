external add_leading_dot_slash : string -> string = "addLeadingDotSlash" [@@mel.module "@11ty/eleventy-utils"] [@@mel.scope "TemplatePath"]

let input file_path = Js.String.split Node.Path.sep file_path |> Js.Array.joinWith "/"

let normalize_path file_path =
	if String.equal (Js.typeof file_path) "string" then
		add_leading_dot_slash (input file_path)
	else
		file_path

(* Can't remove it now because some JS code expects Path *)
module Path : sig
  val normalizePath : string -> string
end = struct 
	let normalizePath file_path = normalize_path file_path
end
