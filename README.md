# search-managed-assembly
An example of how to search a managed assembly using System.Reflection.Metadata.

## Usage
The repository consistts of a command-line executable:

`search-managed-assembly ASSEMBLY_FILE METHOD_ATTR1 METHOD_ATTR2...`

which takes the first argument as a managed (.NET) assembly file, and a list of method attribute types. A list of methods within the assembly which have these attributes are output.

For instance, to find all test methods within an XUnit test assembly, run the application as follows:

`search-managed-assembly MY-DLL.dll Xunit.TheoryAttribute Xunit.FactAttribute `

## Purpose
At the moment, this repository serves as an example of how to extract information from an assembly in a cross-platform way. 

Maybe it'll be expanded into a more general-purpose command-line application, maybe not.
