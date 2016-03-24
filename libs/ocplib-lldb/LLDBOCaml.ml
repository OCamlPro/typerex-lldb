(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)


open LLDBEnums
open LLDBGenerated


external delete : 'a sb -> unit = "LLDBOCaml_Delete_ml"
external isNull : 'a sb -> bool = "lldb_is_NULL"

module FILE = struct
  external fopen : string -> string -> file = "lldb_fopen_ml"
  external fopen : file -> unit = "lldb_fclose_ml"
  external fopen : file -> unit = "lldb_fflush_ml"
end


let to_array getSize getItem s =
  let n = getSize s in
  Array.init n (fun i -> getItem s i)

let getDescrData1 f x =
  let stream = SBStream.create () in
  let (_ : bool) = f x stream in
  SBStream.getData stream

let getDescrData2 f x y =
  let stream = SBStream.create () in
  let (_ : bool) = f x stream y in
  SBStream.getData stream



(* Here, we refine the types of stubs that have been automatically
   generated, for example to replace integers by sum types. We also add
   manually written stubs. *)


(* If a module does not appear here, it is probably in LLDBGenerated. You might
choose either to open LLDBGenerated in your source files, or better, to
   make a redirection here (module SBXXX = SBXXX). *)

include TYPES

module SBValueList = struct
  include SBValueList
  let to_array = to_array getSize getValueAtIndex
end
module SBFileSpecList = struct
  include SBFileSpecList
  let to_array = to_array getSize getFileSpecAtIndex
end
module SBInstructionList = struct
  include SBInstructionList
  let to_array = to_array getSize getInstructionAtIndex
  let to_string x : string = getDescrData1 getDescription x
end
module SBModuleSpecList = struct
  include SBModuleSpecList
  let to_array = to_array getSize getSpecAtIndex
end
module SBStringList = struct
   include SBStringList
  let to_array = to_array getSize getStringAtIndex
end
module SBTypeList = struct
  include SBTypeList
  let to_array = to_array getSize getTypeAtIndex
end
module SBSymbolContextList = struct
  include SBSymbolContextList
  let to_array = to_array getSize getContextAtIndex
end

module SBAddress = struct
  include SBAddress
  let getAddressClass a = int_to_AddressClass (getAddressClass a)
  let to_string x : string = getDescrData1 getDescription x
end

module SBFrame = struct
  include SBFrame
  let to_string x : string = getDescrData1 getDescription x
end

module SBSymbol = struct
  include SBSymbol
  let getType s = int_to_SymbolType (getType s)
  let to_string x : string = getDescrData1 getDescription x
end

module SBThread = struct
  include SBThread
  let getBacktrace = to_array getNumFrames getFrameAtIndex
  let to_string x : string = getDescrData1 getDescription x
end

module SBDebugger = struct
  include SBDebugger
  external initialize : unit -> unit = "SBDebugger_Initialize_ml"
  external terminate : unit -> unit = "SBDebugger_Terminate_ml"
  external test : unit -> unit = "SBDebugger_Test_ml"
  external create : bool -> sbDebugger = "SBDebugger_Create_ml"
  external destroy : sbDebugger -> unit = "SBDebugger_Destroy_ml"
  let createTargetByName = createTarget1
  let to_string x : string = getDescrData1 getDescription x
end

module SBTarget = struct
  include SBTarget
  external launchVerySimple : sbTarget -> string array -> string -> sbProcess =
    "SBTarget_LaunchVerySimple_ml"
  let to_string x y : string = getDescrData2 getDescription x y
  let findSymbols target name stype =
    findSymbols target name (int_of_SymbolType stype)
end

module SBCommandInterpreter = struct
  include SBCommandInterpreter
  external addCommand : sbCommandInterpreter -> string ->
    (string array -> unit) -> string -> unit
      = "SBCommandInterpreter_AddCommand_ml"
end

module SBBreakpoint = struct
  include SBBreakpoint
  external setCallback : sbBreakpoint ->
    (sbProcess -> sbThread -> sbBreakpointLocation -> bool) -> unit
      = "SBBreakpoint_SetCallback_ml"
  let to_string x : string = getDescrData1 getDescription x
end

module SBProcess = struct
  include SBProcess
  let getState p = (Obj.magic (getState p) : state_type)
  let to_string x : string = getDescrData1 getDescription x
end

module SBEvent= struct
  include SBEvent
  let getType event =
    int_to_EventType ( SBEvent.getType event )
  let to_string x : string = getDescrData1 getDescription x
end

module SBModule = struct
  include SBModule
  external getVersion : int array -> int = "SBModule_GetVersion_ml"
  let to_string x : string = getDescrData1 getDescription x
end

module SBBreakpointLocation = struct
  include SBBreakpointLocation
  let to_string x y : string = getDescrData2 getDescription x y
end



module SBBlock = struct
  include SBBlock
  let to_string x : string = getDescrData1 getDescription x
end


module SBBroadcaster = struct
  include SBBroadcaster
end


module SBCommandPluginInterface = struct
  include SBCommandPluginInterface
end


module SBCommand = struct
  include SBCommand
end


module SBCommandReturnObject = struct
  include SBCommandReturnObject
  let to_string x : string = getDescrData1 getDescription x
end


module SBCommunication = struct
  include SBCommunication

end


module SBCompileUnit = struct
  include SBCompileUnit
  let to_string x : string = getDescrData1 getDescription x
end


module SBData = struct
  include SBData
  let to_string x y : string = getDescrData2 getDescription x y
end


module SBDeclaration = struct
  include SBDeclaration
  let to_string x : string = getDescrData1 getDescription x
end


module SBError = struct
  include SBError
  let to_string x : string = getDescrData1 getDescription x
end


module SBExpressionOptions = struct
  include SBExpressionOptions
end


module SBFileSpec = struct
  include SBFileSpec
  let to_string x : string = getDescrData1 getDescription x
  external createByName : string -> bool -> sbFileSpec =
    "SBFileSpec_new_CreateByName_ml"
end


module SBFunction = struct
  include SBFunction
  let to_string x : string = getDescrData1 getDescription x
end


module SBHostOS = struct
  include SBHostOS
end


module SBInputReader = struct
  include SBInputReader
end


module SBInstruction = struct
  include SBInstruction
  let to_string x : string = getDescrData1 getDescription x
end


module SBLineEntry = struct
  include SBLineEntry
  let to_string x : string = getDescrData1 getDescription x
end


module SBListener = struct
  include SBListener
end


module SBModuleSpec = struct
  include SBModuleSpec
  let to_string x : string = getDescrData1 getDescription x
end


module SBSection = struct
  include SBSection
  let to_string x : string = getDescrData1 getDescription x
end


module SBSourceManager = struct
  include SBSourceManager
end


module SBStream = struct
  include SBStream
end


module SBSymbolContext = struct
  include SBSymbolContext
  let to_string x : string = getDescrData1 getDescription x
end


module SBLaunchInfo = struct
  include SBLaunchInfo
end


module SBAttachInfo = struct
  include SBAttachInfo
end


module SBTypeMember = struct
  include SBTypeMember
  let to_string x y : string = getDescrData2 getDescription x y
end


module SBType = struct
  include SBType
  let to_string x y : string = getDescrData2 getDescription x y
end


module SBTypeCategory = struct
  include SBTypeCategory
  let to_string x y : string = getDescrData2 getDescription x y
end


module SBTypeFilter = struct
  include SBTypeFilter
  let to_string x y : string = getDescrData2 getDescription x y
end


module SBTypeFormat = struct
  include SBTypeFormat
  let to_string x y : string = getDescrData2 getDescription x y
end


module SBTypeNameSpecifier = struct
  include SBTypeNameSpecifier
  let to_string x y : string = getDescrData2 getDescription x y
end


module SBTypeSummary = struct
  include SBTypeSummary
  let to_string x y : string = getDescrData2 getDescription x y
end


module SBTypeSynthetic = struct
  include SBTypeSynthetic
  let to_string x y : string = getDescrData2 getDescription x y
end


module SBValue = struct
  include SBValue
  let to_string x : string = getDescrData1 getDescription x
end


module SBWatchpoint = struct
  include SBWatchpoint
  let to_string x y : string = getDescrData2 getDescription x y
end

