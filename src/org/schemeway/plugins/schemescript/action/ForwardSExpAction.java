/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class ForwardSExpAction extends SchemeAction {
    private boolean mSelectExpression;

    public ForwardSExpAction(SchemeEditor editor, boolean selectExpression) {
        super(editor);
        setText("Select Forward Sexp");
        setToolTipText("Selects the next S-expression");
        mSelectExpression = selectExpression;
    }

    public void run() {
        final SchemeEditor editor = getSchemeEditor();
        if (editor == null) return;

        Region selection = editor.getSelection();

        SexpExplorer explorer = editor.getExplorer();
        if (explorer.forwardSexpression(selection.getOffset() + selection.getLength())) {
            int start = explorer.getSexpStart();
            int end = explorer.getSexpEnd();
            if (mSelectExpression) {
                if (selection.getLength() > 0) {
                    start = selection.getOffset();
                }
                else if (start > selection.getOffset()) {
                    start = selection.getOffset();
                }
                editor.setSelection(start, end);
            }
            else {
                editor.setPoint(end);
            }
        }
    }
}