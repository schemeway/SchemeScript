/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;

import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.tools.*;

public class FileHeaderCommentAction extends Action {
    private SchemeEditor mEditor;

    public FileHeaderCommentAction(SchemeEditor editor) {
        mEditor = editor;
        setText("Insert header comment");
        setToolTipText("Insert a Scheme file header comment");
    }

    public void run() {
        String newline = TextUtilities.getDefaultLineDelimiter(mEditor.getDocument());

        mEditor.insertText(0, Comments.createHeaderComment(newline));
        mEditor.setPoint(8 + newline.length());
    }
}