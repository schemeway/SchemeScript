/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class SwapSexpAction extends SchemeAction {

    public SwapSexpAction(SchemeEditor editor) {
        super(editor);
        setText("Swaps S-expressions");
        setToolTipText("Swaps the S-expressions before and after the cursor");
    }

    public void run() {
        final SchemeEditor editor = getSchemeEditor();
        if (editor == null) return;

        int point = editor.getPoint();
        SexpNavigator explorer = editor.getExplorer();
        if (explorer.backwardSexpression(point)) {
            int previousStart = explorer.getSexpStart();
            int previousEnd = explorer.getSexpEnd();
            if (explorer.forwardSexpression(point)) {
                int nextStart = explorer.getSexpStart();
                int nextEnd = explorer.getSexpEnd();
                editor.swapText(previousStart, previousEnd - previousStart, nextStart, nextEnd - nextStart);
            }
        }
    }
}