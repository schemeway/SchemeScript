/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.util.Assert;

import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class ForwardSExpAction extends Action {
    private SchemeEditor mEditor;
    private boolean mSelectExpression;

    public ForwardSExpAction(SchemeEditor editor, boolean selectExpression) {
        Assert.isNotNull(editor);
        setText("Select Forward Sexp");
        setToolTipText("Selects the next S-expression");
        mEditor = editor;
        mSelectExpression = selectExpression;
    }

    public void run() {
        Region selection = mEditor.getSelection();

        SexpExplorer explorer = mEditor.getExplorer();
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
                mEditor.setSelection(start, end);
            }
            else {
                mEditor.setPoint(end);
            }
        }
    }
}