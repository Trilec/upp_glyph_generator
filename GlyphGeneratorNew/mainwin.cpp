/*
================================================================================
 mainwin.cpp: Main window UI and wiring, using FlowBoxLayout for layout.
================================================================================
*/

#include "canvas.h"
#include "FlowBoxLayout.h"  // From provided
#include "StageCard.h"  // From provided (assume available)
#include "AccordionCtrl.h"  // For collapsible sections if needed

struct MainWin : TopWindow {
    typedef MainWin CLASSNAME;

    bool ui_syncing = false;

    // Layout using FlowBoxLayout (programmatic, responsive).
    FlowBoxLayout rowTools { FlowBoxLayout::H };
    FlowBoxLayout rowOps { FlowBoxLayout::H };
    FlowBoxLayout rowActions { FlowBoxLayout::H };
    FlowBoxLayout rowStyle { FlowBoxLayout::V };  // V for stacked rows

    // Tools
    Button bCursor;

    // Ops row
    Option cbSnap, cbClip, cbBgEnable, cbShowGrid;
    Label  lblGrid, lblAspect, lblSample, lblExportSize, lblSel;
    EditInt edGrid;
    DropList dlAspect, dlSampleRes, dlExportSize;

    // Actions row
    Button bDup, bUndo, bRedo, bFlipX, bFlipY, bReset, bDelete, bLayerUp, bLayerDown, 
           bBg, bClear;
    ColorPusher cBg;

    // Style row widgets (use Accordion for sub-groups if complex).
    Option cbFill, cbStroke, cbEvenOdd, cbOutline;
    ColorPusher cFill, cStroke, cOutline;
    Label lblStrokeW, lblOutW, lblOpacity, lblDash, lblFillOp, lblStrokeOp, lblOutOp;
    EditIntSpin spinStrokeW, spinOutlineW;
    EditIntSpin esOutlineOffX, esOutlineOffY;
    EditString  edDash, edOutlineDash;
    DropList    dlStrokeType, dlOutlineType;
    
    Label lblRectRadius;
    EditIntSpin spinRectRx, spinRectRy;

    // Text subrow
    Label lblText, lblFont;
    EditString edText, edCodes;
    DropList  dlFont;
    Option    cbBold, cbItalic;

    // Labels (outline/stroke/text)
    Label lblOlType, lblOlOp, lblOlDash, lblOff, lblCodes;
    Label lblStType, lblStOp, lblStDash;

    // Per-channel opacity sliders
    SliderCtrl sFillOpacity, sStrokeOpacity, sOutlineOpacity;

    // Override
    Option cbOverrideFill, cbOverrideStroke, cbOverrideOutline;
    ColorPusher cOverrideFill, cOverrideStroke, cOverrideOutline;
    Label lblOverride;

    // Code panel (use StageCard for header/content).
    StageCard codeCard;
    Label codeTitle;
    Button bCopy, bLoad, bSave, bExportPNG, bExportJPG, bExportICO;
    DocEdit code;

    // Canvas
    Canvas canvas;

    Array<Button> toolBtns;  // For tools

    void UpdateCode() {
        // Full code generation as in original, with reserves.
        const Canvas::Aspect a = canvas.ASPECTS[canvas.aspect_ix];
        const int W = canvas.export_width;
        const int H = int(double(W) * a.h / a.w + 0.5);

        String out;
        StyleOverrides ov;

        ov.useFill = (bool)~cbOverrideFill;
        ov.useStroke = (bool)~cbOverrideStroke;
        ov.useOutline = (bool)~cbOverrideOutline; 
        ov.fillColor = (Color)~cOverrideFill;
        ov.strokeColor = (Color)~cOverrideStroke;
        ov.outlineColor = (Color)~cOverrideOutline;

        if(ov.useFill) out << Format("Color _FillColor    = Color(%d,%d,%d);\n", ov.fillColor.GetR(), ov.fillColor.GetG(), ov.fillColor.GetB());
        if(ov.useStroke) out << Format("Color _StrokeColor  = Color(%d,%d,%d);\n", ov.strokeColor.GetR(), ov.strokeColor.GetG(), ov.strokeColor.GetB());
        if(ov.useOutline) out << Format("Color _OutlineColor = Color(%d,%d,%d);\n\n", ov.outlineColor.GetR(), ov.outlineColor.GetG(), ov.outlineColor.GetB());

        out << "void DrawIcon(Draw& w, const Rect& inset";
        if(ov.useFill) out << ", Color& " << ov.fillName;
        if(ov.useStroke) out << ", Color& " << ov.strokeName;
        if(ov.useOutline) out << ", Color& " << ov.outlineName;
        out << ")\n\n{\n";
        out << "    BufferPainter p(w, MODE_ANTIALIASED);\n\n";

        if(canvas.bg_enabled) {
            const Color c = canvas.bg_color;
            out << "    p.Begin();\n";
            out << "    p.Move(Pointf(inset.left,  inset.top));\n";
            out << "    p.Line(Pointf(inset.right, inset.top));\n";
            out << "    p.Line(Pointf(inset.right, inset.bottom));\n";
            out << "    p.Line(Pointf(inset.left,  inset.bottom));\n";
            out << "    p.Close();\n";
            out << "    p.Fill(Color(" << c.GetR() << "," << c.GetG() << "," << c.GetB() << "));\n";
            out << "    p.End();\n\n";
        }

        for(const Shape& s : canvas.shapes)
            GetOps(s.type).EmitCode(out, s, ov);

        out << "}\n";
        code <<= out;
    }

    void PushStyleToUI() {
        if(ui_syncing) return;
        if(canvas.selected < 0 || canvas.selected >= canvas.shapes.GetCount()) return;
        ui_syncing = true;

        const Style& st = canvas.shapes[canvas.selected].style;
        const Shape& sh = canvas.shapes[canvas.selected];

        cbFill = st.enableFill;
        cbStroke = st.enableStroke;
        cbEvenOdd = st.evenOdd;
        cbOutline = st.outlineEnable;

        cFill <<= st.fill;
        cStroke <<= st.stroke;
        cOutline <<= st.outlineColor;

        spinStrokeW <<= st.strokeWidth;
        spinOutlineW <<= st.outlineWidth;

        sFillOpacity <<= Upp::clamp(int(st.fillOpacity * 100 + 0.5), 0, 100);
        sStrokeOpacity <<= Upp::clamp(int(st.strokeOpacity * 100 + 0.5), 0, 100);
        sOutlineOpacity <<= Upp::clamp(int(st.outlineOpacity * 100 + 0.5), 0, 100);

        dlStrokeType.SetIndex(ToI(st.strokeStyle));
        dlOutlineType.SetIndex(ToI(st.outlineStyle));

        edDash <<= DashFrom(st.strokeStyle, st.dash);
        edOutlineDash <<= DashFrom(st.outlineStyle, st.outlineDash);

        esOutlineOffX <<= st.outlineOffsetX;
        esOutlineOffY <<= st.outlineOffsetY;

        const Rect ir = canvas.GetInsetRect();
        const int base = min(ir.Width(), ir.Height());
        const bool isRect = (sh.type == PType::Rect);
        spinRectRx.MinMax(0, base / 2);
        spinRectRy.MinMax(0, base / 2);
        spinRectRx.Enable(isRect);
        spinRectRy.Enable(isRect);

        if(isRect) {
            spinRectRx <<= int(sh.rxN * base + 0.5);
            spinRectRy <<= int(sh.ryN * base + 0.5);
        }

        if(sh.type == PType::Text) {
            const TextData& td = sh.text;
            edText <<= td.text;
            edCodes <<= (td.text.IsEmpty() ? String() : AsString((int)(byte)td.text[0]));
            int face_ix = 0;
            for(int i = 0; i < dlFont.GetCount(); ++i) {
                if((String)dlFont.GetValue(i) == td.face) { face_ix = i; break; }
            }
            dlFont.SetIndex(face_ix);
            cbBold = td.bold;
            cbItalic = td.italic;
            MirrorTextUIFromSelection();
        }
        ui_syncing = false;
    }

    void PullStyleFromUI() {
        if(ui_syncing) return;
        if(canvas.selected < 0 || canvas.selected >= canvas.shapes.GetCount()) return;
        Style& st = canvas.shapes[canvas.selected].style;

        st.enableFill = (bool)~cbFill;
        st.enableStroke = (bool)~cbStroke;
        st.evenOdd = (bool)~cbEvenOdd;
        st.outlineEnable = (bool)~cbOutline;

        st.fill = (Color)~cFill;
        st.stroke = (Color)~cStroke;
        st.outlineColor = (Color)~cOutline;

        st.strokeWidth = (int)~spinStrokeW;
        st.outlineWidth = (int)~spinOutlineW;

        st.fillOpacity = Upp::clamp(((int)~sFillOpacity) / 100.0, 0.0, 1.0);
        st.strokeOpacity = Upp::clamp(((int)~sStrokeOpacity) / 100.0, 0.0, 1.0);
        st.outlineOpacity = Upp::clamp(((int)~sOutlineOpacity)/ 100.0, 0.0, 1.0);

        st.strokeStyle = FromI(dlStrokeType.GetIndex());
        st.outlineStyle = FromI(dlOutlineType.GetIndex());

        st.dash = ~edDash;
        st.strokeStyle = GuessStyleFromString(st.dash);
        dlStrokeType.SetIndex(ToI(st.strokeStyle));

        st.outlineDash = ~edOutlineDash;
        st.outlineStyle = GuessStyleFromString(st.outlineDash);
        dlOutlineType.SetIndex(ToI(st.outlineStyle));

        st.outlineOffsetX = (int)~esOutlineOffX;
        st.outlineOffsetY = (int)~esOutlineOffY;

        Shape& sh = canvas.shapes[canvas.selected];
        if(sh.type == PType::Rect) {
            const Rect ir = canvas.GetInsetRect();
            const int base = max(1, min(ir.Width(), ir.Height()));
            int rxpx = (int)~spinRectRx;
            int rypx = (int)~spinRectRy;

            Rect rpx(Canvas::X(ir, sh.x), Canvas::Y(ir, sh.y), Canvas::X(ir, sh.x + sh.w) - Canvas::X(ir, sh.x), Canvas::Y(ir, sh.y + sh.h) - Canvas::Y(ir, sh.y));
            rpx.Normalize();
            rxpx = min(rxpx, rpx.Width() / 2);
            rypx = min(rypx, rpx.Height() / 2);

            sh.rxN = Upp::clamp(rxpx / double(base), 0.0, 1.0);
            sh.ryN = Upp::clamp(rypx / double(base), 0.0, 1.0);
        }

        canvas.Refresh();
        UpdateCode();
    }

    void OnSnap() { canvas.snap = (bool)~cbSnap; canvas.Refresh(); }
    void OnClip() { canvas.clip = (bool)~cbClip; canvas.Refresh(); }
    void OnGrid() {
        int g = (int)~edGrid;
        g = Upp::clamp(g, 2, 64);
        canvas.grid = g;
        canvas.Refresh();
    }

    void OnSelectionChanged() { PushStyleToUI(); }
    void OnShapesChanged() {
        PushStyleToUI();
        UpdateCode();
        canvas.InvalidateCache();
    }

    void OnCopyCode() { WriteClipboardText(~code); PromptOK("Code copied to clipboard."); }

    void BuildToolButtons() {
        toolBtns.Clear();
        int x = 6;

        bCursor.SetLabel("Cursor");
        bCursor.WhenAction = [this] { canvas.tool = Tool::Cursor; };
        rowTools.AddFixed(bCursor, DPI(80));

        const Vector<ToolSpec>& specs = GetToolSpecs();
        toolBtns.SetCount(specs.GetCount());

        for(int i = 0; i < specs.GetCount(); ++i) {
            const ToolSpec& sp = specs[i];
            Button& b = toolBtns[i];

            b.SetLabel(sp.label);
            b.Tip(sp.tip);

            const PType t = sp.type;
            b.WhenAction = [this, t] {
                canvas.tool = Tool::CreateShape;
                canvas.creation_type = t;
            };

            rowTools.AddFixed(b, DPI(90));
        }
    }

    void LoadSystemFonts() {
        dlFont.Clear();
        int n = Font::GetFaceCount();
        for(int i = 0; i < n; ++i)
            dlFont.Add(Font::GetFaceName(i), Font::GetFaceName(i));
        if(dlFont.GetCount() == 0)
            dlFont.Add("Default", "");
        dlFont.SetIndex(0);
    }

    void MirrorTextUIFromSelection() {
        if(canvas.selected < 0 || canvas.selected >= canvas.shapes.GetCount()) return;
        if(canvas.shapes[canvas.selected].type != PType::Text) return;
        const TextData& td = canvas.shapes[canvas.selected].text;

        edText <<= td.text;

        String codes;
        for(int i = 0; i < td.text.GetCount(); ++i) {
            if(i) codes << ',';
            codes << AsString((int)(byte)td.text[i]);
        }
        edCodes <<= codes;

        int face_ix = 0;
        for(int i = 0; i < dlFont.GetCount(); ++i)
            if((String)dlFont.GetValue(i) == td.face) { face_ix = i; break; }
        dlFont.SetIndex(face_ix);
        cbBold = td.bold; cbItalic = td.italic;
    }

    void DoSave() {
        FileSel fs; fs.Type("JSON", "*.json");
        if(!fs.ExecuteSaveAs("Save icon JSON")) return;
        SaveFile(~fs, canvas.SaveJson());
    }
    void DoLoad() {
        FileSel fs; fs.Type("JSON", "*.json");
        if(!fs.ExecuteOpen("Load icon JSON")) return;
        canvas.LoadJson(LoadFile(~fs));
        UpdateCode();
    }

    MainWin() {
        Title("U++ Icon Builder — v0.8").Sizeable().Zoomable();

        Add(split.SizePos());
        split.Horz(left, right);
        split.SetPos(6000);

        // Use FlowBoxLayout for rows (responsive, wrap if needed).
        rowTools.SetGap(DPI(6)).SetInset(DPI(8), DPI(8)).SetWrap(true);
        rowOps.SetGap(DPI(6)).SetInset(DPI(8), DPI(8)).SetWrap(true);
        rowActions.SetGap(DPI(6)).SetInset(DPI(8), DPI(8)).SetWrap(true);
        rowStyle.SetGap(DPI(6)).SetInset(DPI(8), DPI(8)).SetWrap(true);

        left.Add(rowTools.TopPos(0, 40).HSizePos());
        left.Add(rowOps.TopPos(40, 28).HSizePos());
        left.Add(rowActions.TopPos(68, 32).HSizePos());
        left.Add(rowStyle.TopPos(100, 120).HSizePos());
        left.Add(canvas.VSizePos(220, 0).HSizePos());

        // Build tools row.
        BuildToolButtons();

        // Ops row widgets.
        cbSnap.SetLabel("Snap"); rowOps.Add(cbSnap);
        cbClip.SetLabel("Clip"); rowOps.Add(cbClip);
        cbBgEnable.SetLabel("Bg Color"); rowOps.Add(cbBgEnable);
        cBg <<= Color(250, 250, 250); rowOps.Add(cBg);
        lblAspect.SetText("Aspect Ratio"); rowOps.Add(lblAspect);
        for(int i = 0; i < ASPECT_COUNT; ++i) dlAspect.Add(i, ASPECTS[i].label);
        dlAspect.SetIndex(aspect_ix); rowOps.Add(dlAspect);
        lblSample.SetText("Preview"); rowOps.Add(lblSample);
        dlSampleRes.Add(0, "None"); dlSampleRes.Add(32, "32"); dlSampleRes.Add(64, "64"); dlSampleRes.Add(128, "128");
        dlSampleRes.SetData(sample_width); rowOps.Add(dlSampleRes);
        lblExportSize.SetText("Export"); rowOps.Add(lblExportSize);
        dlExportSize.Add(32, "32"); dlExportSize.Add(64, "64"); dlExportSize.Add(128, "128"); dlExportSize.Add(256, "256"); dlExportSize.Add(512, "512");
        dlExportSize.SetData(export_width); rowOps.Add(dlExportSize);
        cbShowGrid.SetLabel("Grid"); cbShowGrid <<= true; rowOps.Add(cbShowGrid);
        lblGrid.SetText("Step"); rowOps.Add(lblGrid);
        edGrid.MinMax(2, 64); edGrid <<= grid; rowOps.Add(edGrid);

        // Actions row.
        bDup.SetLabel("Duplicate"); rowActions.Add(bDup);
        bFlipX.SetLabel("FlipX"); rowActions.Add(bFlipX);
        bFlipY.SetLabel("FlipY"); rowActions.Add(bFlipY);
        bLayerUp.SetLabel("LayerUp"); rowActions.Add(bLayerUp);
        bLayerDown.SetLabel("LayerDn"); rowActions.Add(bLayerDown);
        bUndo.SetLabel("Undo"); rowActions.Add(bUndo);
        bRedo.SetLabel("Redo"); rowActions.Add(bRedo);
        bReset.SetLabel("Reset"); rowActions.Add(bReset);
        bDelete.SetLabel("Delete"); rowActions.Add(bDelete);

        // Style row (V stack with sub-H flows for rows).
        cbFill.SetLabel("Fill"); rowStyle.Add(cbFill);
        cFill <<= Style().fill; rowStyle.Add(cFill);
        lblFillOp.SetText("Opacity"); rowStyle.Add(lblFillOp);
        sFillOpacity.MinMax(0, 100); sFillOpacity <<= 100; rowStyle.Add(sFillOpacity);
        cbEvenOdd.SetLabel("EvenOdd"); rowStyle.Add(cbEvenOdd);

        cbStroke.SetLabel("Stroke"); rowStyle.Add(cbStroke);
        cStroke <<= Style().stroke; rowStyle.Add(cStroke);
        lblStOp.SetText("Opacity"); rowStyle.Add(lblStOp);
        sStrokeOpacity.MinMax(0, 100); sStrokeOpacity <<= 100; rowStyle.Add(sStrokeOpacity);
        lblStrokeW.SetText("Width"); rowStyle.Add(lblStrokeW);
        spinStrokeW.MinMax(0, 128); spinStrokeW <<= 2; rowStyle.Add(spinStrokeW);
        lblStType.SetText("Type"); rowStyle.Add(lblStType);
        dlStrokeType.Add("Solid"); dlStrokeType.Add("LongDash"); dlStrokeType.Add("ShortDash"); dlStrokeType.Add("Dotted"); dlStrokeType <<= 0; rowStyle.Add(dlStrokeType);
        lblStDash.SetText("Dash"); rowStyle.Add(lblStDash);
        rowStyle.Add(edDash);

        lblRectRadius.SetText("Radius X/Y"); rowStyle.Add(lblRectRadius);
        spinRectRx.MinMax(0, 100); rowStyle.Add(spinRectRx);
        spinRectRy.MinMax(0, 100); rowStyle.Add(spinRectRy);

        cbOutline.SetLabel("Outline"); rowStyle.Add(cbOutline);
        cOutline <<= Style().outlineColor; rowStyle.Add(cOutline);
        lblOlOp.SetText("Opacity"); rowStyle.Add(lblOlOp);
        sOutlineOpacity.MinMax(0, 100); sOutlineOpacity <<= 100; rowStyle.Add(sOutlineOpacity);
        lblOutW.SetText("Width"); rowStyle.Add(lblOutW);
        spinOutlineW.MinMax(0, 128); spinOutlineW <<= 0; rowStyle.Add(spinOutlineW);
        lblOlType.SetText("Type"); rowStyle.Add(lblOlType);
        dlOutlineType.Add("Solid"); dlOutlineType.Add("LongDash"); dlOutlineType.Add("ShortDash"); dlOutlineType.Add("Dotted"); dlOutlineType <<= 0; rowStyle.Add(dlOutlineType);
        lblOlDash.SetText("Dash"); rowStyle.Add(lblOlDash);
        rowStyle.Add(edOutlineDash);
        lblOff.SetText("Offset X/Y"); rowStyle.Add(lblOff);
        rowStyle.Add(esOutlineOffX);
        rowStyle.Add(esOutlineOffY);

        lblText.SetText("Text"); rowStyle.Add(lblText);
        rowStyle.Add(edText);
        lblCodes.SetText("Codes"); rowStyle.Add(lblCodes);
        rowStyle.Add(edCodes);
        lblFont.SetText("Font"); rowStyle.Add(lblFont);
        rowStyle.Add(dlFont);
        cbBold.SetLabel("Bold"); rowStyle.Add(cbBold);
        cbItalic.SetLabel("Italic"); rowStyle.Add(cbItalic);

        lblOverride.SetText("Override Colors"); rowStyle.Add(lblOverride);
        cbOverrideFill.SetLabel("Fill"); rowStyle.Add(cbOverrideFill);
        cOverrideFill <<= StyleOverrides().fillColor; rowStyle.Add(cOverrideFill);
        cbOverrideStroke.SetLabel("Stroke"); rowStyle.Add(cbOverrideStroke);
        cOverrideStroke <<= StyleOverrides().strokeColor; rowStyle.Add(cOverrideStroke);
        cbOverrideOutline.SetLabel("Outline"); rowStyle.Add(cbOverrideOutline);
        cOverrideOutline <<= StyleOverrides().outlineColor; rowStyle.Add(cOverrideOutline);

        // Code panel as StageCard.
        codeCard.SetTitle("Code Output").SetSubTitle("BufferPainter code");
        right.Add(codeCard.SizePos());
        codeCard.AddContent(code.SizePos());

        // File buttons in codeCard header or sub-flow.
        bSave.SetLabel("Save"); codeCard.AddContent(bSave);
        bLoad.SetLabel("Load"); codeCard.AddContent(bLoad);
        bClear.SetLabel("Clear"); codeCard.AddContent(bClear);
        bCopy.SetLabel("Clipboard"); codeCard.AddContent(bCopy);
        bExportPNG.SetLabel("Export PNG"); codeCard.AddContent(bExportPNG);
        bExportJPG.SetLabel("Export JPG"); codeCard.AddContent(bExportJPG);
        bExportICO.SetLabel("Export ICO"); codeCard.AddContent(bExportICO);

        // Wiring (WhenAction etc.).
        bDup.WhenAction = [this] { canvas.DuplicateSelected(); UpdateCode(); };
        // ... (wire all other buttons as in original).

        cbBgEnable.WhenAction = [this]{
            canvas.bg_enabled = (bool)~cbBgEnable;
            if(canvas.bg_enabled) canvas.bg_color = (Color)~cBg;
            canvas.Refresh();
            UpdateCode();
        };
        cBg.WhenAction = [this]{
            if(canvas.bg_enabled){
                canvas.bg_color = (Color)~cBg;
                canvas.Refresh();
                UpdateCode();
            }
        };

        cbSnap.WhenAction = THISBACK(OnSnap);
        cbClip.WhenAction = THISBACK(OnClip);
        edGrid.WhenAction = THISBACK(OnGrid);
        cbShowGrid.WhenAction = [this] { canvas.show_grid = (bool)~cbShowGrid; canvas.Refresh(); };
        dlAspect.WhenAction = [this] { canvas.aspect_ix = dlAspect.GetIndex(); canvas.Refresh(); };
        dlSampleRes.WhenAction = [this] { canvas.sample_width = (int)dlSampleRes.GetData(); canvas.Refresh(); };
        dlExportSize.WhenAction = [this] { canvas.export_width = (int)dlExportSize.GetData(); UpdateCode(); };

        cbFill.WhenAction = THISBACK(PullStyleFromUI);
        // ... (wire all style widgets as in original).

        canvas.WhenSelection = THISBACK(OnSelectionChanged);
        canvas.WhenShapesChanged = THISBACK(OnShapesChanged);

        bSave.WhenAction = [this] { DoSave(); };
        bLoad.WhenAction = [this] { DoLoad(); };

        LoadSystemFonts();
        UpdateCode();
    }
};