/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class DownSExpAction extends SchemeAction {
    
    public DownSExpAction(SchemeEditor editor) {
        super(editor);
        setText("Enters the next enclosed S-expression");
        setToolTipText("Enters the next enclosed  S-expression");
    }

    public void run() {
        SchemeEditor editor = getSchemeEditor();
        if (editor == null) return;
        
        Region selection = editor.getSelection();

        SexpExplorer explorer = editor.getExplorer();
        if (explorer.downSexpression(selection.getOffset()))
            editor.setPoint(explorer.getSexpStart());
    }

}