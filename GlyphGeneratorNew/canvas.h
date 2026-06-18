/*
================================================================================
 canvas.h: Canvas class for model/view/interactions.
================================================================================
*/

#ifndef CANVAS_H
#define CANVAS_H

#include "primitives.h"

struct Canvas : Ctrl {
    // Model.
    Vector<Shape> shapes;
    int selected = -1;

    // Tool state.
    Tool tool = Tool::Cursor;
    PType creation_type = PType::Rect;
    bool snap = true;
    bool clip = true;
    int grid = 8;

    // Aspect/clip.
    struct Aspect { int w, h; const char* label; };
    static constexpr Aspect ASPECTS[] = { 
        {1,1,"1:1"}, {4,3,"4:3"}, {3,2,"3:2"}, {16,9,"16:9"},
        {21,9,"21:9"}, {9,16,"9:16"}, {2,3,"2:3"}, {3,4,"3:4"}
    };
    static constexpr int ASPECT_COUNT = sizeof(ASPECTS) / sizeof(ASPECTS[0]);
    int aspect_ix = 0;
    double clip_scale_n = 0.80;
    Color bg_color = SColorFace();
    bool bg_enabled = false;
    int sample_width = 128;
    int export_width = 256;

    // Drag state.
    bool creating = false;
    bool editing = false;
    bool moving = false;
    int drag_vertex = -1;
    Point start_px;
    double grab_nx = 0.0, grab_ny = 0.0;

    // Text template.
    String text_template = "Text";

    // Callbacks.
    Callback WhenSelection;
    Callback WhenShapesChanged;

    // Grid visibility.
    bool show_grid = true;

    // History.
    Vector<String> hist;
    int hist_ix = -1;
    void PushHist();  // Pushes current JSON to history.
    bool Undo();      // Undoes to previous state, invalidates cache.
    bool Redo();      // Redoes to next state, invalidates cache.

    // Inset calculation.
    Rect GetInsetRect() const;  // Computes clip rect based on size/aspect/scale.

    Canvas();  // Default constructor with initial values.

    void Paint(Draw& w) override;  // Renders background, grid, shapes, overlay (with caching).

    // Mouse/key handlers.
    void LeftDown(Point p, dword) override;  // Starts selection/creation.
    void MouseMove(Point p, dword) override;  // Handles drag updates.
    void LeftUp(Point, dword) override;  // Ends drag, triggers changes.
    bool Key(dword key, int) override;  // Handles delete key.

    // Actions.
    void ClearAll();  // Clears shapes, resets selected.
    void DeleteSelected();  // Removes selected shape.
    void ResetStyleSelectedOrDefaults();  // Resets style for selected.
    void LayerUp();  // Moves selected up in layer.
    void LayerDown();  // Moves selected down in layer.
    void DuplicateSelected();  // Copies selected with offset.
    void FlipXSelected();  // Flips selected horizontally.
    void FlipYSelected();  // Flips selected vertically.
    void ResetDefaults();  // Resets to initial state.

    // JSON I/O.
    String SaveJson() const;  // Serializes state to JSON.
    void LoadJson(const String& js);  // Deserializes from JSON, invalidates cache.

    // Render.
    Image RenderToImage(int W, int H);  // Renders to image (no grid/overlay).
    bool SaveSinglePngAsIco(const String& fn, const Image& img);  // Saves PNG as ICO.

private:
    Image cached_inset_img;  // Cache for inset rendering to reduce per-frame work.
    void InvalidateCache() { cached_inset_img = Null; }  // Invalidates on changes (shapes/resized).

    ValueMap ShapeToVM(const Shape& s) const;  // Converts shape to ValueMap.
    ValueMap StyleToVM(const Style& s) const;  // Converts style to ValueMap.
    void StyleFrom(Style& s, ValueMap m);  // Loads style from ValueMap.
};

#endif // CANVAS_H