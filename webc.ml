[%%mel.raw {|
import fs from "fs";
import fastglob from "fast-glob";
import isGlob from "is-glob";
import path from "path";

import { AstSerializer } from "./src/ast.js";
import { ModuleScript } from "./src/moduleScript.cjs";
import { ModuleResolution } from "./src/moduleResolution.js";
import { ComponentManager } from "./src/componentManager.js";
|}]

(*
external is_glob : string -> bool = "default" [@@mel.module "is-glob"]
[%%mel.raw "import './test.js'"]
let () = Js.log "webc_lib"
let () = "webc_lib" |> is_glob |> Js.log
*)

module Path = Webc_lib.Path.Path

module AstCache = Webc_lib.AstCache.AstCache
let localAstCache = AstCache.create ()

module W = struct
  type ast_options = {
    mutable filePath: string option [@optional];
  } [@@deriving abstract]

  type t = {
    mutable customTransforms: <string: unit> Js.t; 
    mutable customHelpers: <string: unit> Js.t; 
    mutable customScopedHelpers: <string: unit> Js.t; 
    mutable globalComponents: <string: unit> Js.t;
    mutable astOptions: ast_options;
    mutable bundlerMode: bool;
    mutable ignores: string array;
    mutable rawInput: string;
    mutable filePath: string option; [@optional]
  } 

  type options = {
    file: string option; [@optional]
    input: string option; [@optional]
    ignores: string array option; [@optional]
  }

(*
  external webc: t = "WebC"
  external create1: unit -> t = "WebC" [@@mel.new]  
  external compile1: t -> unit = "compile" [@@mel.send]
*)

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
		Option.iter (fun input -> obj.rawInput <- input) opts.input;
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
		if(!content.startsWith("<!doctype") && !content.startsWith("<!DOCTYPE") && !content.startsWith("<html")) {
			return "component";
		}

		return "page";
	}

	_getRawContent() {
		if(this.rawInput || this.rawInput === "") {
			return this.rawInput;
		} else if(this.filePath) {
			if(!this._cachedContent) {
				this._cachedContent = fs.readFileSync(this.filePath, {
					encoding: "utf8"
				});
			}

			return this._cachedContent;
		} else {
			throw new Error("Missing a setInput or setInputPath method call to set the input.");
		}
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
		if(!content) {
			throw new Error("WebC.getAST() expects a content argument.");
		}

		return localAstCache.get(content);
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
		return fastglob.sync(glob, {
			ignore: ignores,
			caseSensitiveMatch: false,
			dot: false,
		});
	}

	static getComponentsMap(globOrObject, ignores) {
		let rawFiles = globOrObject;

		// Passing in a single string is assumed to be a glob
		if(typeof rawFiles === "string") {
			rawFiles = WebC.findGlob(globOrObject, ignores);
		}

		if(Array.isArray(rawFiles)) {
			let moduleResolver = new ModuleResolution();
			let resolvedFiles = new Set();
			for(let file of rawFiles) {
				// Resolve `npm:` aliases
				let hasValidAlias = moduleResolver.hasValidAlias(file);
				if(hasValidAlias) {
					file = moduleResolver.resolveAliases(file);
				}

				// Multiple glob searches
				if(isGlob(file)) {
					let globResults = WebC.findGlob(file, ignores);
					for(let globFile of globResults) {
						resolvedFiles.add(globFile);
					}
				} else {
					resolvedFiles.add(file);
				}
			}

			let obj = {};
			for(let file of resolvedFiles) {
				let {name} = path.parse(file);
				if(obj[name]) {
					throw new Error(`Global component name collision on "${name}" between: ${obj[name]} and ${file}`)
				}
				obj[name] = file;
			}

			return obj;
		}

		return globOrObject;
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

var w = new WebC();

export { WebC, ModuleScript, ComponentManager };
|}]



