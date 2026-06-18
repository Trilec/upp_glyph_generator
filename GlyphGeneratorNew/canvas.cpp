/*
================================================================================
 canvas.cpp: Canvas implementations.
================================================================================
*/

#include "canvas.h"

// Canvas constructor - Sets defaults, seeds history.
Canvas::Canvas() {
    snap = true; clip = true; grid = 8;
    aspect_ix = 0; clip_scale_n = 0.80;
    bg_color = SColorFace();
    bg_enabled = false;
    sample_width = 128;
    export_width = 256;
    text_template = "Text";
    show_grid = true;
    PushHist();
}

// Computes clip rect based on aspect and scale.
Rect Canvas::GetInsetRect() const {
    Size sz = GetSize();
    int baseW = int(sz.cx * clip_scale_n + 0.5);
    int baseH = int(sz.cy * clip_scale_n + 0.5);
    const Aspect a = ASPECTS[Upp::clamp(aspect_ix, 0, ASPECT_COUNT - 1)];
    double ar = double(a.w) / double(a.h);
    int w = baseW;
    int h = int(w / ar + 0.5);
    if(h > baseH) { h = baseH; w = int(h * ar + 0.5); }
    int L = (sz.cx - w) / 2;
    int T = (sz.cy - h) / 2;
    return RectC(L, T, w, h);
}

// Pushes current JSON to history.
void Canvas::PushHist() {
    String s = SaveJson();
    if(hist_ix + 1 < hist.GetCount())
        hist.Trim(hist_ix + 1);
    hist.Add(s);
    hist_ix = hist.GetCount() - 1;
}

// Undoes to previous state.
bool Canvas::Undo() {
    if(hist_ix <= 0) return false;
    hist_ix--;
    LoadJson(hist[hist_ix]);
    InvalidateCache();
    return true;
}

// Redoes to next state.
bool Canvas::Redo() {
    if(hist_ix + 1 >= hist.GetCount()) return false;
    hist_ix++;
    LoadJson(hist[hist_ix]);
    InvalidateCache();
    return true;
}

// Paint - Renders with caching for efficiency.
void Canvas::Paint(Draw& w) override {
    Size sz = GetSize();
    Rect ir = GetInsetRect();
    w.DrawRect(sz, SColorFace());

    w.DrawRect(ir.left, ir.top, ir.Width(), 1, SColorMark());
    w.DrawRect(ir.left, ir.bottom, ir.Width(), 1, SColorMark());
    w.DrawRect(ir.left, ir.top, 1, ir.Height(), SColorMark());
    w.DrawRect(ir.right, ir.top, 1, ir.Height()+1, SColorMark());

    String excap = Format("Export [%d]", export_width);
    Font f = StdFont().Height(9);
    Size capsz = GetTextSize(excap, f);
    int tx = ir.left + (ir.Width() - capsz.cx) / 2;
    int ty = ir.top - capsz.cy - 2;
    if(ty < 0) ty = ir.bottom + 2;
    w.DrawText(tx, ty, excap, f, SColorText());

    if(IsNull(cached_inset_img)) {
        Size isz = ir.GetSize();
        ImageBuffer ib(isz);
        BufferPainter p(ib, MODE_ANTIALIASED);
        Rect lir = RectC(0, 0, isz.cx, isz.cy);
        p.Clear(bg_enabled ? bg_color : White());
        if(show_grid) {
            int g = clamp(grid, 4, 50);
            for(int x = g; x < isz.cx; x += g) p.DrawRect(x, 0, 1, isz.cy, Color(238, 238, 238));
            for(int y = g; y < isz.cy; y += g) p.DrawRect(0, y, isz.cx, 1, Color(238, 238, 238));
        }
        for(const Shape& s : shapes) GetOps(s.type).EmitPainter(p, lir, s);
        cached_inset_img = ib;
    }
    w.DrawImage(ir.left, ir.top, cached_inset_img);

    if(selected >= 0 && selected < shapes.GetCount()) GetOps(shapes[selected].type).DrawOverlay(w, ir, shapes[selected]);

    if(sample_width > 0) {
        const Aspect a = ASPECTS[Upp::clamp(aspect_ix, 0, ASPECT_COUNT - 1)];
        double ar = double(a.w) / double(a.h);
        int tw = sample_width;
        int th = max(1, int(tw / ar + 0.5));
        int pad = 8;
        Point pos(ir.left - pad - tw, ir.top);
        if(pos.x < pad) { pos.x = ir.left; pos.y = ir.top - pad - th; }
        if(pos.y < pad) { pos.x = pad; pos.y = pad; }
        Rect box = RectC(pos.x, pos.y, tw, th);
        Image preview = Rescale(cached_inset_img, Size(tw, th));
        String cap = Format("Preview [%d]", sample_width);
        capsz = GetTextSize(cap, f);
        tx = box.left + (box.Width() - capsz.cx) / 2;
        ty = box.top - capsz.cy - 2;
        if(ty < 0) ty = box.bottom + 2;
        w.DrawText(tx, ty, cap, f, SColorText());
        w.DrawRect(box, White());
        w.DrawRect(box.Inflated(1), SColorShadow());
        w.DrawImage(box.left, box.top, preview);
    }
}

// LeftDown - Starts selection or creation.
void Canvas::LeftDown(Point p, dword) override {
    SetFocus();
    SetCapture();
    Rect ir = GetInsetRect();
    creating = editing = moving = false;
    drag_vertex = -1;

    if(tool == Tool::Cursor) {
        // Vertex hit first, top-most.
        int vshape = -1, vindex = -1;
        for(int i = shapes.GetCount() - 1; i >= 0; --i) {
            int hv = GetOps(shapes[i].type).HitVertex(ir, shapes[i], p);
            if(hv >= 0) { vshape = i; vindex = hv; break; }
        }
        if(vshape >= 0) {
            if(selected != vshape) { selected = vshape; WhenSelection(); }
            Shape& s = shapes[selected];
            drag_vertex = vindex;
            GetOps(s.type).BeginEdit(s, ir, p, drag_vertex, grab_nx, grab_ny);
            editing = true; moving = false; Refresh(); return;
        }

        // Body hit.
        int pick = -1;
        for(int i = shapes.GetCount() - 1; i >= 0; --i)
            if(GetOps(shapes[i].type).HitBody(ir, shapes[i], p)) { pick = i; break; }

        if(selected != pick) { selected = pick; WhenSelection(); }
        if(selected >= 0) {
            Shape& s = shapes[selected];
            drag_vertex = -1;
            GetOps(s.type).BeginEdit(s, ir, p, drag_vertex, grab_nx, grab_ny);
            editing = true; moving = true;
        }
        Refresh(); return;
    }

    // Creation.
    if(tool == Tool::CreateShape && ir.Contains(p)) {
        start_px = snap ? Point(Snap1D(p.x, ir.left, grid), Snap1D(p.y, ir.top, grid)) : p;

        Shape s; s.type = creation_type;
        if(s.type == PType::Text)
            s.text.text = text_template.IsEmpty() ? "Text" : text_template;

        shapes.Add(s);
        selected = shapes.GetCount() - 1;

        GetOps(shapes[selected].type).BeginCreate(shapes[selected], ir, start_px);
        creating = true;
        WhenSelection();
        Refresh();
    }
}

// MouseMove - Handles drag updates.
void Canvas::MouseMove(Point p, dword) override {
    if(!HasCapture()) return;
    Rect ir = GetInsetRect();

    if(creating && selected >= 0) {
        GetOps(shapes[selected].type).DragCreate(shapes[selected], ir, start_px, p, snap, grid);
        Refresh();
    }
    else if(editing && selected >= 0) {
        if(!GetMouseLeft()) {
            ReleaseCapture();
            creating = editing = moving = false;
            drag_vertex = -1;
            WhenShapesChanged();
            Refresh(); return;
        }
        GetOps(shapes[selected].type).DragEdit(shapes[selected], ir, p, snap, grid, moving, drag_vertex, grab_nx, grab_ny);
        Refresh();
    }
}

// LeftUp - Ends drag, triggers changes if applicable.
void Canvas::LeftUp(Point, dword) override {
    if(!HasCapture()) return;
    ReleaseCapture();
    bool changed = creating || editing;
    creating = editing = moving = false;
    drag_vertex = -1;
    if(changed) WhenShapesChanged();
    PushHist();
    Refresh();
}

// Key - Handles delete for selected.
bool Canvas::Key(dword key, int) override {
    if(key == K_DELETE){ DeleteSelected(); return true; }
    return false;
}

// ClearAll - Clears shapes and resets selected.
void Canvas::ClearAll() {
    shapes.Clear(); selected = -1;
    WhenShapesChanged();
    PushHist(); Refresh();
}

// DeleteSelected - Removes selected shape if valid.
void Canvas::DeleteSelected() {
    if(selected >= 0 && selected < shapes.GetCount()) {
        shapes.Remove(selected);
        selected = -1;
        WhenSelection();
        WhenShapesChanged();
        PushHist(); Refresh();
    }
}

// ResetStyleSelectedOrDefaults - Resets style for selected to default.
void Canvas::ResetStyleSelectedOrDefaults() {
    if(selected >= 0 && selected < shapes.GetCount())
        shapes[selected].style = Style(); // default-constructed
    WhenSelection();
    WhenShapesChanged();
    PushHist(); Refresh();
}

// LayerUp - Moves selected shape up in layer order.
void Canvas::LayerUp() {
    if(selected < 0 || selected >= shapes.GetCount() - 1) return;
    Swap(shapes[selected], shapes[selected + 1]);
    selected++;
    WhenShapesChanged();
    PushHist(); Refresh();
}

// LayerDown - Moves selected shape down in layer order.
void Canvas::LayerDown() {
    if(selected <= 0 || selected >= shapes.GetCount()) return;
    Swap(shapes[selected], shapes[selected - 1]);
    selected--;
    WhenShapesChanged();
    PushHist(); Refresh();
}

// DuplicateSelected - Copies selected shape with small offset.
void Canvas::DuplicateSelected() {
    if(selected < 0 || selected >= shapes.GetCount()) return;
    Shape s = shapes[selected];
    Rect ir = GetInsetRect();
    double dx = 6.0 / max(1, ir.Width());
    double dy = 6.0 / max(1, ir.Height());
    switch(s.type) {
        case PType::Rect:     s.x += dx; s.y += dy; break;
        case PType::Circle:   s.cx += dx; s.cy += dy; break;
        case PType::Line:     s.p1 += Pointf(dx,dy); s.p2 += Pointf(dx,dy); break;
        case PType::Triangle: s.p1 += Pointf(dx,dy); s.p2 += Pointf(dx,dy); s.p3 += Pointf(dx,dy); break;
        case PType::Text:     s.x += dx; s.y += dy; break;
        case PType::Curve:    s.curve.a0 += Pointf(dx,dy); s.curve.a1 += Pointf(dx,dy);
                              s.curve.c0 += Pointf(dx,dy); s.curve.c1 += Pointf(dx,dy); break;
    }
    shapes.Insert(++selected, s);
    WhenSelection();
    WhenShapesChanged();
    PushHist(); Refresh();
}

// FlipXSelected - Flips selected shape horizontally around center.
void Canvas::FlipXSelected() {
    if(selected < 0 || selected >= shapes.GetCount()) return;
    double cx = 0.5; // normalized center
    Shape& s = shapes[selected];
    switch(s.type) {
        case PType::Rect:     s.x = 2 * cx - (s.x + s.w); break;
        case PType::Circle:   s.cx = 2 * cx - s.cx; break;
        case PType::Line:     s.p1.x = 2 * cx - s.p1.x; s.p2.x = 2 * cx - s.p2.x; break;
        case PType::Triangle: s.p1.x = 2 * cx - s.p1.x; s.p2.x = 2 * cx - s.p2.x; s.p3.x = 2 * cx - s.p3.x; break;
        case PType::Text:     s.x = 2 * cx - s.x; break;
        case PType::Curve:    s.curve.a0.x = 2 * cx - s.curve.a0.x;
                              s.curve.a1.x = 2 * cx - s.curve.a1.x;
                              s.curve.c0.x = 2 * cx - s.curve.c0.x;
                              s.curve.c1.x = 2 * cx - s.curve.c1.x; break;
    }
    WhenShapesChanged();
    PushHist(); Refresh();
}

// FlipYSelected - Flips selected shape vertically around center.
void Canvas::FlipYSelected() {
    if(selected < 0 || selected >= shapes.GetCount()) return;
    double cy = 0.5;
    Shape& s = shapes[selected];
    switch(s.type) {
        case PType::Rect:     s.y = 2 * cy - (s.y + s.h); break;
        case PType::Circle:   s.cy = 2 * cy - s.cy; break;
        case PType::Line:     s.p1.y = 2 * cy - s.p1.y; s.p2.y = 2 * cy - s.p2.y; break;
        case PType::Triangle: s.p1.y = 2 * cy - s.p1.y; s.p2.y = 2 * cy - s.p2.y; s.p3.y = 2 * cy - s.p3.y; break;
        case PType::Text:     s.y = 2 * cy - s.y; break;
        case PType::Curve:    s.curve.a0.y = 2 * cy - s.curve.a0.y;
                              s.curve.a1.y = 2 * cy - s.curve.a1.y;
                              s.curve.c0.y = 2 * cy - s.curve.c0.y;
                              s.curve.c1.y = 2 * cy - s.curve.c1.y; break;
    }
    WhenShapesChanged();
    PushHist(); Refresh();
}

// ResetDefaults - Resets canvas to initial state.
void Canvas::ResetDefaults() {
    shapes.Clear(); selected = -1;
    snap = true; clip = true; grid = 8;
    aspect_ix = 0; clip_scale_n = 0.80;
    text_template = "Text";
    WhenShapesChanged();
    PushHist(); Refresh();
}

// ShapeToVM - Converts shape to ValueMap for JSON.
ValueMap Canvas::ShapeToVM(const Shape& s) const {
    ValueMap m;
    m.Add("type", (int)s.type);
    m.Add("style", StyleToVM(s.style));

    switch(s.type) {
        case PType::Rect:
            m.Add("x", s.x);  m.Add("y", s.y);
            m.Add("w", s.w);  m.Add("h", s.h);
            m.Add("rxN", s.rxN);
            m.Add("ryN", s.ryN);
            break;

        case PType::Circle:
            m.Add("cx", s.cx); m.Add("cy", s.cy); m.Add("r", s.r);
            break;
        case PType::Line:
            m.Add("p1x", s.p1.x); m.Add("p1y", s.p1.y);
            m.Add("p2x", s.p2.x); m.Add("p2y", s.p2.y);
            break;
        case PType::Triangle:
            m.Add("p1x", s.p1.x); m.Add("p1y", s.p1.y);
            m.Add("p2x", s.p2.x); m.Add("p2y", s.p2.y);
            m.Add("p3x", s.p3.x); m.Add("p3y", s.p3.y);
            break;

        case PType::Text:
            m.Add("x", s.x); m.Add("y", s.y);
            m.Add("txt",   s.text.text);
            m.Add("face",  s.text.face);
            m.Add("sizeN", s.text.sizeN);
            m.Add("bold",  (int)s.text.bold);
            m.Add("italic",(int)s.text.italic);
            break;

        case PType::Curve:
            m.Add("cubic",  (int)s.curve.cubic);
            m.Add("closed", (int)s.curve.closed);
            m.Add("a0x", s.curve.a0.x); m.Add("a0y", s.curve.a0.y);
            m.Add("a1x", s.curve.a1.x); m.Add("a1y", s.curve.a1.y);
            m.Add("c0x", s.curve.c0.x); m.Add("c0y", s.curve.c0.y);
            m.Add("c1x", s.curve.c1.x); m.Add("c1y", s.curve.c1.y);
            break;
    }
    return m;
}

// StyleToVM - Converts style to ValueMap for JSON.
ValueMap Canvas::StyleToVM(const Style& s) const {
    ValueMap m;
    m("fill_r", s.fill.GetR())("fill_g", s.fill.GetG())("fill_b", s.fill.GetB())
     ("stroke_r", s.stroke.GetR())("stroke_g", s.stroke.GetG())("stroke_b", s.stroke.GetB())
     ("stroke_w", s.strokeWidth)("evenOdd", s.evenOdd)("dash", s.dash)
     ("enableFill", s.enableFill)("enableStroke", s.enableStroke)
     ("opacity", s.opacity)("fillOpacity", s.fillOpacity)("strokeOpacity", s.strokeOpacity)
     ("outlineEnable", s.outlineEnable)("outlineOutside", s.outlineOutside)
     ("outline_r", s.outlineColor.GetR())("outline_g", s.outlineColor.GetG())("outline_b", s.outlineColor.GetB())
     ("outline_w", s.outlineWidth)("outlineOpacity", s.outlineOpacity)
     ("strokeStyle", ToI(s.strokeStyle))("outlineStyle", ToI(s.outlineStyle))
     ("outlineDash", s.outlineDash)("outlineOffsetX", s.outlineOffsetX)("outlineOffsetY", s.outlineOffsetY);
    return m;
}

// StyleFrom - Loads style from ValueMap with defaults.
void Canvas::StyleFrom(Style& s, ValueMap m) {
    s.fill = Color(VI(m,"fill_r",120), VI(m,"fill_g",120), VI(m,"fill_b",120));
    s.stroke = Color(VI(m,"stroke_r",60), VI(m,"stroke_g",60), VI(m,"stroke_b",60));
    s.strokeWidth = VI(m,"stroke_w",2);
    s.evenOdd = VB(m,"evenOdd",false);
    s.dash = VS(m,"dash",String());
    s.enableFill = VB(m,"enableFill",true);
    s.enableStroke = VB(m,"enableStroke",true);

    s.opacity = VD(m,"opacity",1.0);
    s.fillOpacity = VD(m,"fillOpacity",1.0);
    s.strokeOpacity = VD(m,"strokeOpacity",1.0);

    s.outlineEnable = VB(m,"outlineEnable",false);
    s.outlineOutside = VB(m,"outlineOutside",true);
    s.outlineColor = Color(VI(m,"outline_r",255), VI(m,"outline_g",0), VI(m,"outline_b",0));
    s.outlineWidth = VI(m,"outline_w",0);
    s.outlineOpacity = VD(m,"outlineOpacity",1.0);
    s.strokeStyle = FromI(VI(m,"strokeStyle",0));
    s.outlineStyle = FromI(VI(m,"outlineStyle",0));
    s.outlineDash = VS(m,"outlineDash",String());
    s.outlineOffsetX = VI(m,"outlineOffsetX",0);
    s.outlineOffsetY = VI(m,"outlineOffsetY",0);
}

// SaveJson - Serializes canvas state to JSON.
String Canvas::SaveJson() const {
    ValueMap root;
    root.Add("snap", snap);
    root.Add("clip", clip);
    root.Add("grid", grid);
    root.Add("aspect_ix", aspect_ix);
    root.Add("clip_scale_n", clip_scale_n);
    root.Add("bg_enabled", bg_enabled);
    root.Add("bg_r", bg_color.GetR());
    root.Add("bg_g", bg_color.GetG());
    root.Add("bg_b", bg_color.GetB());
    root.Add("text_template", text_template);

    Vector<Value> arr;
    arr.Reserve(shapes.GetCount());
    for(const Shape& s : shapes)
        arr.Add(ShapeToVM(s));
    root.Add("shapes", ValueArray(pick(arr)));

    return AsJSON(root);
}

// LoadJson - Deserializes from JSON, updates state.
void Canvas::LoadJson(const String& js) {
    Value v = ParseJSON(~js);
    if(IsNull(v)) { Refresh(); return; }

    ValueMap root;
    if(IsValueMap(v))
        root = v;
    else if(IsPairArrayValueMap(v))
        root = PairArrayToMap(ValueArray(v));
    else {
        Refresh();
        return;
    }

    snap = VB(root, "snap", true);
    clip = VB(root, "clip", true);
    grid = VI(root, "grid", 8);
    aspect_ix = VI(root, "aspect_ix", 0);
    clip_scale_n = VD(root, "clip_scale_n", 0.80);
    bg_enabled = VB(root, "bg_enabled", false);
    bg_color = Color(VI(root,"bg_r", SColorFace().GetR()), VI(root,"bg_g", SColorFace().GetG()), VI(root,"bg_b", SColorFace().GetB()));
    text_template = VS(root, "text_template", "Text");

    shapes.Clear();
    ValueArray a = ValueArray(root["shapes"]);
    for(int i = 0; i < a.GetCount(); ++i) {
        if(!IsValueMap(a[i])) continue;
        ValueMap sh = a[i];
        Shape s;
        s.type = (PType)VI(sh, "type", (int)PType::Rect);

        Value stv = sh["style"];
        if(IsValueMap(stv)) StyleFrom(s.style, ValueMap(stv));

        switch(s.type) {
            case PType::Rect:
                s.x = VD(sh, "x", 0); s.y = VD(sh, "y", 0);
                s.w = VD(sh, "w", 0); s.h = VD(sh, "h", 0);
                s.rxN = VD(sh, "rxN", 0.0);
                s.ryN = VD(sh, "ryN", 0.0);
                break;
            case PType::Circle:
                s.cx = VD(sh, "cx", 0); s.cy = VD(sh, "cy", 0); s.r = VD(sh, "r", 0);
                break;
            case PType::Line:
                s.p1 = Pointf(VD(sh,"p1x",0), VD(sh,"p1y",0));
                s.p2 = Pointf(VD(sh,"p2x",0), VD(sh,"p2y",0));
                break;
            case PType::Triangle:
                s.p1 = Pointf(VD(sh,"p1x",0), VD(sh,"p1y",0));
                s.p2 = Pointf(VD(sh,"p2x",0), VD(sh,"p2y",0));
                s.p3 = Pointf(VD(sh,"p3x",0), VD(sh,"p3y",0));
                break;
            case PType::Text:
                s.x = VD(sh,"x",0); s.y = VD(sh,"y",0);
                s.text.text   = VS(sh,"txt","Text");
                s.text.face   = VS(sh,"face",String());
                s.text.sizeN  = VD(sh,"sizeN",0.18);
                s.text.bold   = VB(sh,"bold",false);
                s.text.italic = VB(sh,"italic",false);
                break;
            case PType::Curve:
                s.curve.cubic  = VB(sh,"cubic",true);
                s.curve.closed = VB(sh,"closed",false);
                s.curve.a0 = Pointf(VD(sh,"a0x",0), VD(sh,"a0y",0));
                s.curve.a1 = Pointf(VD(sh,"a1x",0), VD(sh,"a1y",0));
                s.curve.c0 = Pointf(VD(sh,"c0x",0), VD(sh,"c0y",0));
                s.curve.c1 = Pointf(VD(sh,"c1x",0), VD(sh,"c1y",0));
                break;
        }
        shapes.Add(s);
    }
    selected = shapes.IsEmpty() ? -1 : shapes.GetCount() - 1;
    WhenSelection();
    WhenShapesChanged();
    InvalidateCache();
    Refresh();
}

// RenderToImage - Renders current content to image of size W x H (no grid/frames).
Image Canvas::RenderToImage(int W, int H) {
    ImageBuffer ib(Size(W, H));
    ib.SetKind(IMAGE_OPAQUE);
    BufferPainter p(ib, MODE_ANTIALIASED);
    p.Clear(bg_enabled ? bg_color : White());
    Rect inset = RectC(0, 0, W, H);
    if(clip) {
        p.Begin();
        p.Move(Pointf(inset.left, inset.top));
        p.Line(Pointf(inset.right, inset.top));
        p.Line(Pointf(inset.right, inset.bottom));
        p.Line(Pointf(inset.left, inset.bottom));
        p.Close();
        p.Clip();
    }
    for(const Shape& s : shapes)
        GetOps(s.type).EmitPainter(p, inset, s);
    if(clip) p.End();
    return Image(ib);
}

// SaveSinglePngAsIco - Saves single PNG as ICO (Vista+ format).
bool Canvas::SaveSinglePngAsIco(const String& fn, const Image& img) {
    String png;
    {
        StringStream ss;
        PNGEncoder().Save(ss, img);
        if(ss.IsError()) return false;
        png = ss.GetResult();
    }
    FileOut out(fn);
    if(!out) return false;
    int w = img.GetWidth();
    int h = img.GetHeight();
    int count = 1;
    out.Put16le(0);      // reserved
    out.Put16le(1);      // type = 1 (icon)
    out.Put16le(count);  // count
    out.Put(w == 256 ? 0 : Upp::min(w,255));   // bWidth (0 for 256)
    out.Put(h == 256 ? 0 : Upp::min(h,255));   // bHeight (0 for 256)
    out.Put(0);                                // color count
    out.Put(0);                                // reserved
    out.Put16le(1);                            // planes (ignored for PNG)
    out.Put16le(32);                           // bitcount (hint)
    out.Put32le((int)png.GetCount());          // bytes in res
    out.Put32le(6 + 16);                       // image offset
    out.Put(png);
    out.Close();
    return true;
}