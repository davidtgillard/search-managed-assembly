open System.IO
open System.Reflection
open System.Reflection.Metadata;
open System.Reflection.PortableExecutable

/// Contains specs for a method attribute.
type MethodAttributeSpec =
  {
    /// The name of the method attribute type.
    Name: string
    /// The namespace of the method attribute type.
    Namespace: string   
  }
  
  /// Returns the fully qualified name of the method attribute.
  override this.ToString() = this.Namespace + "." + this.Name

/// Contains specs for a method.
type MethodSpec =
  {
    /// The name of the method.
    Name: string
    /// The fully qualified type name of the method.
    FullyQualifiedTypeName: string
    /// The specs of the attributes of the method.
    Attributes: MethodAttributeSpec list
  }
  /// Returns the fully qualified name of the method.
  override this.ToString() =
    let attrsStr = if List.isEmpty this.Attributes then
                     ""
                   else
                     let attrJoined = String.concat "; " (this.Attributes |> List.map (fun a -> a.ToString()))
                     $"[{attrJoined}] "
    attrsStr + this.FullyQualifiedTypeName + "." + this.Name

/// Retrieves all namespace referenced by a MetadataReader.
let private getNamespaces (reader: MetadataReader) =
  // internal recursive function that does a DFS of the nested namespaces referenced by the reader
  let rec getNamespacesInternal (toProcess: (NamespaceDefinition * string) list) (processed: (NamespaceDefinition * string) list) =
    if List.isEmpty toProcess then
      processed
    else
      let current = toProcess.Head
      let currentNS, nsPath = current
      let children = currentNS.NamespaceDefinitions
                     |> Seq.map (fun ndh ->
                                   let nsd = reader.GetNamespaceDefinition(ndh)
                                   let path = nsPath + (if nsPath = "" then "" else ".") + reader.GetString(nsd.Name)
                                   nsd, path)     
                     |> Seq.filter (fun (_, path) -> path.Length<>0 && not (path.Contains('$'))) 
                     |> Seq.toList
      let processed = if nsPath.Length > 0 then current::processed else processed 
      getNamespacesInternal (toProcess.Tail @ children) processed
  // kick off the recursion
  getNamespacesInternal [reader.GetNamespaceDefinitionRoot(), ""] []
  
/// Retrieves all the types (including nested types) belonging to a namespace referenced by a MetadataReader.
let private getAllTypesOfNamespace (reader: MetadataReader) (ns: NamespaceDefinition) (fqns: string) =
  // internal recursive function that does a DFS of the types within the namespace
  let rec getAllTypesInternal (toProcess: (TypeDefinition * string) list) (processed: (TypeDefinition * string) list) =
    if List.isEmpty toProcess then
      processed
    else
      let current = toProcess.Head
      let currentType, fqn = current
      let children = currentType.GetNestedTypes()
                     |> Seq.map (fun tdh ->
                                   let td = reader.GetTypeDefinition(tdh)
                                   let typeName = fqn + "." + reader.GetString(td.Name)
                                   td, typeName)
                     |> Seq.toList
      getAllTypesInternal (toProcess.Tail @ children) (current::processed)
  // kick off the recursion
  let types = ns.TypeDefinitions
              |> Seq.map (fun tdh ->
                            let td = reader.GetTypeDefinition(tdh)
                            let fqtn = fqns + "." + reader.GetString(td.Name)
                            td, fqtn)
              |> Seq.toList
  getAllTypesInternal types []

/// Retrieves all the types referenced by a MetadataReader, performing a DFS over first the namespaces and then the types of all the namespaces.
let private getAllTypes (reader: MetadataReader) =
  getNamespaces reader
  |> List.map (fun (nsd, fqns) -> getAllTypesOfNamespace reader nsd fqns)
  |> List.concat

/// Retrieves all the methods referenced by a MetadataReader.
let private getAllMethods (reader: MetadataReader) =
  /// internal function to convert an attribute handle to an attribute spec
  let attrHandleToAttrSpec (reader: MetadataReader) attrHandle =
    let attr = reader.GetCustomAttribute(attrHandle)
    let attrConstructor = attr.Constructor
    match attrConstructor.Kind with
    | HandleKind.MemberReference ->
      let container = reader.GetMemberReference(MemberReferenceHandle.op_Explicit(attrConstructor)).Parent
      let tr = reader.GetTypeReference(TypeReferenceHandle.op_Explicit(container))
      Some { Name = reader.GetString(tr.Name); Namespace = reader.GetString(tr.Namespace) }
    | _ -> None
  
  getAllTypes reader // find all types
  |> List.map (fun (td, fqtn) ->
                 td.GetMethods()
                 |> Seq.map (fun mdh -> // translate method definition handles to method specs
                                let mdef = reader.GetMethodDefinition(mdh)
                                let spec = { Name = reader.GetString(mdef.Name)
                                             FullyQualifiedTypeName = fqtn 
                                             Attributes = mdef.GetCustomAttributes() |> Seq.map (attrHandleToAttrSpec reader)
                                                                                     |> Seq.choose id
                                                                                     |> Seq.toList }
                                spec)
                |> Seq.toList)
  |> List.concat // flatten the list

/// loads methods from an assembly (.dll, .exe) file
let loadMethodsFromAssemblyFile filePath =
  try
    use fs = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
    use peReader = new PEReader(fs)
    let mr = peReader.GetMetadataReader()
    Ok (getAllMethods mr)
  with
  | ex -> Error ex  

// example of usage
[<EntryPoint>]
let main argv =
  let programName = Assembly.GetExecutingAssembly().GetName().Name
  let usage = $"Usage: {programName} ASSEMBLY_FILE METHOD_ATTR1 METHOD_ATTR2..."
  
  // report error message, usage message, and then exit with an error code of 1
  let exitWithError errMsg =
    printfn "Error: %s" errMsg
    printfn "%s" usage
    1
  
  if argv.Length < 2 then // bail out if not enough arguments provided
    exitWithError "invalid number of arguments provided"
  else
    let testAttributes = argv.[1..] |> Array.toList
    match loadMethodsFromAssemblyFile argv.[0] with
    | Ok methodSpecs ->
      methodSpecs 
      |> Seq.filter (fun mspec -> // filter 
                            mspec.Attributes
                            |> List.map (fun attr ->
                                               let attrStr = (attr.ToString())
                                               List.contains attrStr testAttributes)
                            |> List.fold (fun acc elem -> acc || elem) false)
      |> Seq.iter (fun mspec -> printfn "%s" (mspec.ToString())) // print out the fully qualified name of the methods that made it through the filter
      0 // no error
    | Error ex ->
      // just dump the stack trace
      exitWithError (ex.ToString())