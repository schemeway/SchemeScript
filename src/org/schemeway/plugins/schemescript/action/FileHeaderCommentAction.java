/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.tools.*;

public class FileHeaderCommentAction extends SchemeAction {
    
    public FileHeaderCommentAction(SchemeEditor editor) {
        super(editor);
        setText("Insert header comment");
        setToolTipText("Insert a Scheme file header comment");
    }

    public void run() {
        SchemeEditor editor = getSchemeEditor();
        if (editor == null) return;
        
        String newline = TextUtilities.getDefaultLineDelimiter(editor.getDocument());

        editor.insertText(0, Comments.createHeaderComment(newline));
        editor.setPoint(8 + newline.length());
    }
}