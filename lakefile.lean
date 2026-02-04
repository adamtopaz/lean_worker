import Lean
import Lake
open Lake DSL
open Lean

package "lean_worker" where
  version := v!"0.1.0"

@[default_target]
lean_lib LeanWorker

@[default_target]
lean_lib LeanWorkerTest where
  globs := #[Glob.submodules `LeanWorkerTest]

@[default_target]
lean_exe "lean_worker" where
  root := `Main

@[default_target]
lean_exe "test_server" where
  root := `LeanWorkerTest.TestServer

@[default_target]
lean_exe "test_client" where
  root := `LeanWorkerTest.TestClient

@[default_target]
lean_exe "run_tests" where
  root := `LeanWorkerTest.Main

@[default_target]
lean_exe "full_server" where
  root := `LeanWorkerTest.FullServerCli

module_facet module_metadata mod : Json := do
  let olean ← (← mod.olean.fetch).await
  let (data, _) ← Lean.readModuleData olean
  return pure <| json% {
    name : $(mod.name),
    path : $(mod.filePath mod.pkg.rootDir "lean"),
    consts : $(data.constNames),
    imports : $(data.imports.map Import.module)
  }

package_facet metadata pkg : Array Json := do
  let libs := pkg.leanLibs
  let exes := pkg.leanExes
  let mut out := #[]
  for lib in libs do
    let mods ← (← lib.modules.fetch).await
    for mod in mods do
      let mdata ← (← mod.facet `module_metadata |>.fetch).await
      out := out.push mdata
  for exe in exes do
    let mod := exe.root
    let mdata ← (← mod.facet `module_metadata |>.fetch).await
    out := out.push mdata
  return pure out
