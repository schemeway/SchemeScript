/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;

public class RestoreSelectionAction extends Action
{
    private SchemeEditor mEditor;
    private SelectionStack mStack;

    public RestoreSelectionAction(SchemeEditor editor, SelectionStack stack) {
        Assert.isNotNull(editor);
        setText("Restore the previous selection");
        setToolTipText("Restore the previous selection");
        mEditor = editor;
        mStack = stack;
    }

    public void run() {
        Region selection = mStack.pop();
        if (selection != null) {
            mEditor.setSelection(selection.getOffset(), selection.getOffset() + selection.getLength());
        }
    }
}
