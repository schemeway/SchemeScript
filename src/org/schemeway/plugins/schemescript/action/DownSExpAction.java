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

public class DownSExpAction extends Action {
    private SchemeEditor mEditor;

    public DownSExpAction(SchemeEditor editor) {
        Assert.isNotNull(editor);
        setText("Enters the next enclosed S-expression");
        setToolTipText("Enters the next enclosed  S-expression");
        mEditor = editor;
    }

    public void run() {
        Region selection = mEditor.getSelection();

        SexpExplorer explorer = mEditor.getExplorer();
        if (explorer.downSexpression(selection.getOffset()))
            mEditor.setPoint(explorer.getSexpStart());
    }

}