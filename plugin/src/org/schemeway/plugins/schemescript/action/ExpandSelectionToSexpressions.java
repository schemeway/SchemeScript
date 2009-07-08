/*
 * Copyright (c) 2005 SchemeWay.com
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.editor.autoedits.*;

/**
 * @author SchemeWay.com
 */
public class ExpandSelectionToSexpressions extends SchemeAction {

    public ExpandSelectionToSexpressions(SchemeEditor editor) {
        super(editor);
    }

    public void run() {
        SchemeEditor editor = getSchemeEditor();
        if (editor == null)
            return;

        IRegion selection = editor.getSelection();
        if (selection.getLength() > 1) {
            int start = selection.getOffset();
            int end = start + selection.getLength();
            IRegion expandedSelection = SexpUtils.findEnclosingSexpressions(editor.getDocument(), start, end);
            if (expandedSelection != null) {
                start = expandedSelection.getOffset();
                end = start + expandedSelection.getLength();
                editor.setSelection(start, end);
            }
        }
    }
}