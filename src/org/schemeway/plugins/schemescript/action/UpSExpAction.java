/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;

import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class UpSExpAction extends Action {
    private SchemeEditor mEditor;
    private boolean mSelectExpression;

    public UpSExpAction(SchemeEditor editor, boolean selectExpression) {
        Assert.isNotNull(editor);
        setText("Moves to enclosing S-expression");
        setToolTipText("Moves to the enclosing S-expression");
        mEditor = editor;
        mSelectExpression = selectExpression;
    }

    public void run() {
        Region selection = mEditor.getSelection();

        SexpExplorer explorer = mEditor.getExplorer();
        if (explorer.upSexpression(selection.getOffset())) {
            if (mSelectExpression && explorer.forwardSexpression(explorer.getSexpStart())) {
                mEditor.setSelection(explorer.getSexpStart(), explorer.getSexpEnd());
            }
            else {
                mEditor.setPoint(explorer.getSexpStart());
            }
        }
    }
}