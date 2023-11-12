[%%mel.raw {|
import { TemplatePath } from "@11ty/eleventy-utils";
|}]

module P = Path.Path

type aliases = string Js.Dict.t

type obj = {
	mutable aliases: aliases;
	mutable tagName: string option; [@optional]
} 

type regex = {
		startsWithAlias: Js.Re.t;
}

let regex = {
		startsWithAlias = [%mel.re {|/^([^\:]+)\:/i|}];
}

let get_alias full_path =
	Js.String.match_ regex.startsWithAlias full_path
	|> Option.fold ~none:None ~some:(fun arr -> 
		if Option.is_some arr.(1) then arr.(1) else None
	)

let has_alias this alias = 
	Js.Dict.get this.aliases (Option.get alias) |> Option.is_some

let resolve_aliases this full_path =
	let alias = get_alias full_path in
	(* unaliased, relative from component path *)
	if Option.is_none alias then
		P.normalizePath full_path
	else if not (has_alias this alias) then
		let keys = Js.Dict.keys this.aliases |> Js.Array.joinWith ", " in 
		failwith {j|
			Invalid WebC aliased import path, requested: $full_path (known aliases: $keys})
		|j}
	else
		(* aliases, are relative from project root *)
		let alias = Option.get alias in
		let alias_value = Js.Dict.unsafeGet this.aliases alias in
		let unprefixed_path = Js.String.slice ~from:(String.length alias |> succ) ~to_:(String.length full_path) full_path in
		let path = Node.Path.join2 alias_value unprefixed_path in 
		P.normalizePath path

let set_aliases ?(aliases = Js.Dict.empty ()) this =
	if Option.is_none (Js.Dict.get aliases "npm") then
		Js.Dict.set aliases "npm" "./node_modules/";
	this.aliases <- aliases

let set_tag_name this tag_name = 
	this.tagName <- tag_name

[%%mel.raw {|
class ModuleResolution {
	constructor(aliases) {
		this.setAliases(aliases);
	}

	static REGEX = regex;

	setAliases(aliases = {}) {
		set_aliases(aliases, this);
	}

	setTagName(tagName) {
		set_tag_name(this, tagName);
	}

	checkLocalPath(resolvedPath) {
		let projectDir = TemplatePath.getWorkingDir();
		let modulePath = TemplatePath.absolutePath(projectDir, resolvedPath);

		// No references outside of the project are allowed
		if (!modulePath.startsWith(projectDir)) {
			throw new Error("Invalid import reference (must be in the project root), received: " + resolvedPath );
		}
	}

	hasValidAlias(fullPath) {
		let starts = Object.keys(this.aliases);
		for(let start of starts) {
			if(fullPath.startsWith(`${start}:`)) {
				return true;
			}
		}
		return false;
	}

	static getAlias(fullPath) {
		return get_alias(fullPath);
	}

	resolveAliases(fullPath) {
		return resolve_aliases(this, fullPath);
	}

	// npm:@11ty/eleventy is supported when tag name is supplied by WebC (returns `node_modules/@11ty/eleventy/tagName.webc`)
	// npm:@11ty/eleventy/folderName deep folder name is not supported
	// npm:@11ty/eleventy/module.webc direct reference is supported (with deep folder names too)
	resolve(fullPath) {
		// resolve aliases first
		let resolvedPath = this.resolveAliases(fullPath);

		// make sure file is local to the project
		this.checkLocalPath(resolvedPath);

		// direct link to a webc file
		if(resolvedPath.endsWith(".webc")) {
			return resolvedPath;
		}

		if(this.tagName) {
			// Add the tagName and webc suffix
			return `${resolvedPath}/${this.tagName}.webc`
		}

		return resolvedPath;
	}
}

export { ModuleResolution };
|}]

let [@warning "-27"] create: aliases option -> obj = [%mel.raw {|
	function(aliases) {
		return new ModuleResolution(aliases)
	}
|}]


