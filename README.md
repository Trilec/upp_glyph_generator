# U++ Icon Builder — Modular Primitives (single-file)

An opinionated **U++ (Ultimate++)** demo app for drawing simple vector primitives
(**Rect, Circle, Line, Triangle, Curve, Text**) on a clipped inset canvas using
**normalized geometry** (0..1). The left pane hosts tools, style, and actions;
the right pane shows **live BufferPainter code** that reproduces your drawing.

> **Status:** first public release. Stable enough to play; export polish is next.

---

## Screenshot / Hero
<!-- Replace this with an actual image after the first GitHub Actions build -->
<p align="center">
  <img src="docs/app_screenshot.png" alt="U++ Icon Builder UI" width="720"/>
</p>

---

## Features

- **Per-primitive ops registry** (no virtuals): tiny function-pointer vtable per type.
- **Normalized coordinates** mapped to the inset, so shapes scale predictably.
- **Grid + snapping** (pixel-space snap; avoids drift).
- **Style system**: fill, stroke, stroke width, dash (custom or presets), even/odd fill.
- **Outline pass**: optional outer rim with its own style, opacity, and offset.
- **Text (top-aligned)** with font, bold/italic, and size relative to inset height.
- **JSON Save/Load**, **Undo/Redo** (history stores JSON snapshots).
- **Code export**: emits BufferPainter snippets for each shape.

---

## Build

**Requirements**
- U++ **2025.1+** (TheIDE)  
- Packages: `Core`, `CtrlLib`, `Draw`, `Painter`  
- Optional (future exporters): `plugin/png`, `plugin/jpg`

**Steps (TheIDE)**
1. Open `main.cpp` as a package (or create a package and add the file).
2. Ensure `Core`, `CtrlLib`, `Draw`, `Painter` are in **Package organizer → Uses**.
3. Set output type to GUI application.
4. Build & Run.

**Command-line (umk)**  
If you use `umk`, ensure the above packages are in the `.upp` and call `umk` per your toolchain.

---

## Run & Use

- **Tools row:** Cursor (select/move/resize) and buttons to create Rect/Circle/Line/Triangle/Text/Curve.
- **Ops row:**  
  - `Snap` (toggle pixel snap), `Clip` (clip to inset), `ClipAspect`, `SampleRes`, `Grid` + `Step`.
- **Actions row:** Duplicate, FlipX/Y, LayerUp/Dn, Undo/Redo, Reset (style), Clear, Delete, Save/Load JSON, BG toggle/color.
- **Style panel:** Fill/Stroke/Outline controls, per-channel opacity, dash strings/presets.
- **Text row:** Text string, char codes (comma-separated), Font list, Bold/Italic.

**Mouse & Keys**
- **Create:** choose a primitive tool, click-drag in the inset.
- **Select:** Cursor tool → click shape body/handles (topmost wins).
- **Move/Resize:** drag body to move; drag handles to resize (or edit control points).
- **Delete:** `Delete` key.
- **Undo/Redo:** buttons (history is per-document).
- **Duplicate:** copies selection and nudges it slightly.

---

## Data & Code

- **Normalized geometry:** all shape coordinates are stored in 0..1 relative to the **inset** rectangle.
- **JSON I/O:** `SaveJson()` and `LoadJson()` handle full canvas state (including styles).
- **Code Export:** right panel shows a function like:

```cpp
void DrawIcon(Draw& w, const Rect& inset)
{
    BufferPainter p(w, MODE_ANTIALIASED);
    // ...shape snippets here (Begin/Path/Fill/Stroke/End)...
}
```

## Known Issues / Next Steps

Pixel-centering of exported code: refine path/pen placements so the exported icon matches runtime more tightly at 1x.
Export bugs: edge cases when shapes fall below minimum size; outline-dash interplay in complex stacks.
Quality of life: keyboard nudges, alignment tools, symbol bank, PNG/ICO exporters.
Please see Issues for the current list and progress.

## Contributing

PRs and issues welcome!

Keep the single-file shape unless the change is clearly modular and mechanical.
Favor value semantics (Moveable<T>, Vector<T>) and scope-bound lifetimes.
GUI code must run on the main thread; balance painter Begin/End pairs carefully.
Avoid signature changes for existing public functions unless strictly necessary.

## Acknowledgements

Built on U++ (Ultimate++): Core, CtrlLib, Draw, Painter.
Thanks to the U++ community for headers, samples, and the excellent Painter/Draw stack.

## License

MIT