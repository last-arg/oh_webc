
type parse_options = {
	scriptingEnabled: bool;
	sourceCodeLocationInfo: bool;
}

type parse_result

external parse: string -> parse_options -> parse_result = "parse" [@@mel.module "parse5"]

module AstCache: sig
	type obj = {
		get: string -> parse_result;
	}
	val create: unit -> obj
end = struct
	type t = {
		(* TODO: not string, some parsed type *)
		ast: parse_result Js.Dict.t;
	}

	type obj = {
		get: string -> parse_result;
	}

	(* Immitate JS class *)
	let create () = 
		let ast_cache = {ast = Js.Dict.empty ();} in
		let get content =
			match Js.Dict.get ast_cache.ast content with
			| None -> begin 
				let parsed = parse content {scriptingEnabled = true; sourceCodeLocationInfo = true} in
				Js.Dict.set ast_cache.ast content parsed;
				parsed
			end
			| Some value -> value
		in
		{get = get}
end
