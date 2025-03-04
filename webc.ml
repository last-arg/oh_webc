[%%mel.raw {|
import { AstSerializer } from "./src/ast.js";
import { ModuleScript } from "./src/moduleScript.cjs";
import { ComponentManager } from "./src/componentManager.js";
|}]

let new_module_resolution = Webc_lib.ModuleResolution.create

external is_glob: string -> bool = "default" [@@mel.module "is-glob"]

type fastglob_opts = {
	ignore: string array;
	caseSensitiveMatch: bool;
	dot: bool;
}
type fastglob
external fastglob: fastglob = "default" [@@module "fast-glob"]
external fastglob_sync: fastglob -> string -> fastglob_opts -> string array = "sync" [@@mel.send]

module Path = Webc_lib.Path.Path

module AstCache = Webc_lib.AstCache
let localAstCache = AstCache.create ()

exception Webc_Error of string

module W = struct
  type ast_options = {
    mutable filePath: string option [@optional];
  }

  type t = {
    mutable customTransforms: <string: unit> Js.t; 
    mutable customHelpers: <string: unit> Js.t; 
    mutable customScopedHelpers: <string: unit> Js.t; 
    mutable globalComponents: <string: unit> Js.t;
    mutable astOptions: ast_options;
    mutable bundlerMode: bool;
    mutable ignores: string array;
    mutable rawInput: string option; [@optional]
    mutable _cachedContent: string option; [@optional]
    mutable filePath: string option; [@optional]
  } 

  type options = {
    file: string option; [@optional]
    input: string option; [@optional]
    ignores: string array option; [@optional]
  }

	let find_glob glob ?(ignores = [||]) () =
		fastglob_sync fastglob glob {
			ignore = ignores;
			caseSensitiveMatch = false;
			dot = false;
		}

	module SetString = Set.Make(String)

	(* Being naughty. Doing this because JS input can be string, array, or object *)
	let make_components_input input ignores=
		if [%mel.raw "typeof input === 'string'"] then
			`Arr (find_glob input ignores)
		else if [%mel.raw "Array.isArray(input)"] then
			`Arr [%mel.raw "input"]
		else
			`Obj [%mel.raw "input"]

	let get_components_map glob_or_object ignores = 
		match make_components_input glob_or_object ignores with
		| `Arr raw_files ->
			let obj = Js.Dict.empty () in
			if Js.Array.isArray raw_files then begin
				let module_resolver = new_module_resolution None in
				let resolved_files = ref SetString.empty in

				raw_files |> Array.iter (fun file ->
					let open Webc_lib.ModuleResolution in
					(* Resolve `npm:` aliases *)
					let has_valid_alias = has_valid_alias module_resolver file in
					let file = if has_valid_alias then resolve_aliases module_resolver file else file in
					(* Multiple glob searches *)
					if is_glob file then
						find_glob file ignores
						|> Array.iter (fun glob_file -> resolved_files := SetString.add glob_file !resolved_files)
					else
						resolved_files := SetString.add file !resolved_files;
				);

				SetString.iter (fun file ->
					let parsed_file = Node.Path.parse file in
					let name = parsed_file##name in 
					let obj_value = Js.Dict.get obj name in 
					if obj_value |> Option.is_some then begin
						let err = [%mel.raw {|
							new Error(`Global component name collision on "${name}" between: ${obj[name]} and ${file}`)
						|}] in
						raise err
				end; 
			
				Js.Dict.set obj name file;
				) !resolved_files;
			end;
			obj
		| `Obj obj -> obj

	let _get_raw_content this =
		if Option.is_some this.rawInput then
			this.rawInput
		else if Option.is_some this.filePath then
			let file_path = Option.get this.filePath in
			let cached_content = (Option.value ~default:"" this._cachedContent) in
			if (String.length cached_content) = 0 then begin
				this._cachedContent <- Some (Node.Fs.readFileAsUtf8Sync file_path)
			end;
			this._cachedContent
		else
			raise (Webc_Error "Missing a setInput or setInputPath method call to set the input.")
	
	let get_ast content =
		match Js.toOption content with 
		| None -> raise (Webc_Error "WebC.getAST() expects a content argument.")
		| Some content -> AstCache.get localAstCache content

	let get_rendering_mode content =
		if not (Js.String.startsWith "<!doctype" content) && 
			 not (Js.String.startsWith "<!DOCTYPE" content) &&
			 not (Js.String.startsWith "<html" content) 
		then
			"component"
		else
			"page"
	
	let set_input_path obj file =
		let file = Path.normalizePath file in
		obj.filePath <- Some file;
		obj.astOptions.filePath <- Some file

	let set_content obj input file_path = 
		obj.rawInput <- input;
		obj.astOptions.filePath <- (Js.toOption file_path)
		
  let create obj ~(opts: options) = 
    obj.customTransforms <- Js.Obj.empty ();
		obj.customHelpers <- Js.Obj.empty ();
		obj.customScopedHelpers <- Js.Obj.empty ();
		obj.globalComponents <- Js.Obj.empty ();
		obj.astOptions <- {filePath = None};
		obj.bundlerMode <- false;
		obj.ignores <- Option.value ~default:[||] opts.ignores;
		obj.rawInput <- opts.input;
		Option.iter (fun file -> set_input_path obj file) opts.file;
    ()

  let compile webc  = Js.log webc
end

let s1 = "world"
let s2 = {j|hello $s1|j}

[%%mel.raw {|

class WebC {
	constructor(options = {}) {
    W.create(this, options);
	}

	setInputPath(file) {
		W.set_input_path(this, file);
	}

	setContent(input, filePath) {
		set_content(this, input, filePath);
	}

	setGlobalComponentManager(manager) {
		this.globalComponentManager = manager;
	}

	getRenderingMode(content) {
		return get_rendering_mode(content)
	}

	_getRawContent() {
		return W._get_raw_content(this);
	}

	getContent() {
		let content = this._getRawContent();
		let mode = this.getRenderingMode(content.trimStart());

		// prepend for no-quirks mode on components or implicit page rendering modes (starts with <html>)
		if(mode === "component" || !content.startsWith("<!doctype ") && !content.startsWith("<!DOCTYPE")) {
			content = `<!doctype html>${content}`;
		}
		
		return {
			content,
			mode,
		};
	}

	static async getASTFromString(string) {
		let wc = new WebC({
			input: string
		});
		let { content } = wc.getContent();
		return wc.getAST(content);
	}

	// @deprecated for getFromFilePath
	static async getASTFromFilePath(filePath) {
		let wc = new WebC({
			file: filePath
		});
		let { content } = wc.getContent();
		return wc.getAST(content);
	}

	static async getFromFilePath(filePath) {
		let wc = new WebC({
			file: filePath
		});
		let { content, mode } = wc.getContent();

		return {
			content,
			ast: await wc.getAST(content),
			mode,
		};
	}

	getAST(content) {
		return get_ast(content);
	}

	setTransform(key, callback) {
		this.customTransforms[key] = callback;
	}

	setHelper(key, callback, isScoped = false) {
		if(isScoped) {
			this.customScopedHelpers[key] = callback;
		} else {
			this.customHelpers[key] = callback;
		}
	}

	setAlias(key, folder) {
		if(!this.aliases) {
			this.aliases = {};
		}

		this.aliases[key] = folder;
	}

	async _defineComponentsObject(obj = {}) {
		for(let name in obj) {
			let file = obj[name];
			if(this.globalComponents[name]) {
				throw new Error(`Global component name collision on "${name}" between: ${this.globalComponents[name]} and ${file}`)
			}
			this.globalComponents[name] = file;
		}
	}

	static findGlob(glob, ignores = []) {
		return find_glob(glob, ignores);
	}

	static getComponentsMap(globOrObject, ignores) {
		return get_components_map(globOrObject, ignores)
	}

	defineComponents(globOrObject) {
		this._defineComponentsObject(WebC.getComponentsMap(globOrObject, this.ignores));
	}

	setUidFunction(fn) {
		this.uidFn = fn;
	}

	async setup(options = {}) {
		let { content, mode } = this.getContent();
		let rawAst = this.getAST(content);

		let ast = new AstSerializer(this.astOptions);
		ast.setComponentManager(this.globalComponentManager);
		ast.setBundlerMode(this.bundlerMode);
		ast.setMode(mode);
		ast.setContent(content);
		ast.setData(options.data);

		if(this.aliases && Object.keys(this.aliases).length) {
			ast.setAliases(this.aliases);
		}

		if(this.uidFn) {
			ast.setUidFunction(this.uidFn);
		}

		for(let name in this.customTransforms) {
			ast.setTransform(name, this.customTransforms[name]);
		}

		for(let name in this.customHelpers) {
			ast.setHelper(name, this.customHelpers[name], false);
		}
		for(let name in this.customScopedHelpers) {
			ast.setHelper(name, this.customScopedHelpers[name], true);
		}

		await ast.setComponentsByFilePath(this.globalComponents);
		await ast.setComponentsByFilePath(options.components);

		return {
			ast: rawAst,
			serializer: ast,
		};
	}

	getComponents(setup) {
		let { ast, serializer } = setup;
		let obj = serializer.getComponentList(ast);
		return Object.keys(obj);
	}

	setBundlerMode(mode) {
		this.bundlerMode = !!mode;
	}

	async stream(options = {}) {
		let { ast, serializer } = await this.setup(options);

		serializer.streams.start();

		serializer.compile(ast, options.slots).catch(() => {
			// Node requires this to avoid unhandled rejection errors (yes, even with `finally`)
			serializer.streams.end();
		}).finally(() => {
			serializer.streams.end();
		});

		return serializer.streams.get();
	}

	async compile(options = {}) {
		let { ast, serializer } = await this.setup(options);

		return serializer.compile(ast, options.slots);
	}
}

export { WebC, ModuleScript, ComponentManager };
|}]

