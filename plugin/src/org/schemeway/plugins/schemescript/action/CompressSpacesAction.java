/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;

public class CompressSpacesAction extends SchemeAction {

    public CompressSpacesAction(SchemeEditor editor) {
        super(editor);
        setText("Compress spaces");
        setToolTipText("Replaces all surrounding whitespaces by a single space");
    }

    public void run() {
        SchemeEditor editor = getSchemeEditor();
        if (editor == null)
            return;
        
        IDocument document = editor.getDocument();
        try {
            int currentPoint = editor.getPoint();
            int docLength = document.getLength();
            int start = currentPoint;
            int end = currentPoint;
            while (start > 0 && Character.isWhitespace(document.getChar(start - 1)))
                start--;
            while (end < docLength && Character.isWhitespace(document.getChar(end)))
                end++;
            editor.replaceText(start, end - start, " ");
        }
        catch (BadLocationException exception)
        {
        }
    }

}
