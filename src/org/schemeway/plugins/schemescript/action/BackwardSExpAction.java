/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class BackwardSExpAction extends SchemeAction implements ISchemeEditorAction {
    private boolean mSelectExpression;
    
    public BackwardSExpAction(SchemeEditor editor, boolean selectExpression) {
        super(editor);
        Assert.isNotNull(editor);
        setText("Select Backward Sexp");
        setToolTipText("Selects the previous S-expression");
        mSelectExpression = selectExpression;
    }
    
    public void run() {
        SchemeEditor editor = getSchemeEditor();
        if (editor == null) return;

        Region selection = editor.getSelection();
        int selectionEnd = selection.getOffset() + selection.getLength();
        SexpExplorer explorer = editor.getExplorer();
        if (explorer.backwardSexpression(selection.getOffset())) {
            int start = explorer.getSexpStart();
            if (mSelectExpression) {
                editor.setSelection(start, selectionEnd);
            }
            else {
                editor.setPoint(start);
            }
        }
    }
}
