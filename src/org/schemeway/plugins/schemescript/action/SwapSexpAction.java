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

public class SwapSexpAction extends Action {
    private SchemeEditor mEditor;

    public SwapSexpAction(SchemeEditor editor) {
        Assert.isNotNull(editor);
        setText("Swaps S-expressions");
        setToolTipText("Swaps the S-expressions before and after the cursor");
        mEditor = editor;
    }

    public void run() {
        int point = mEditor.getPoint();
        SexpExplorer explorer = mEditor.getExplorer();
        if (explorer.backwardSexpression(point)) {
            int previousStart = explorer.getSexpStart();
            int previousEnd = explorer.getSexpEnd();
            if (explorer.forwardSexpression(point)) {
                int nextStart = explorer.getSexpStart();
                int nextEnd = explorer.getSexpEnd();
                mEditor.swapText(previousStart, previousEnd - previousStart, nextStart, nextEnd - nextStart);
            }
        }
    }
}