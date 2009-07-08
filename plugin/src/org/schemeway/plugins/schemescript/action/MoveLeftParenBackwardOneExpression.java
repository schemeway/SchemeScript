/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class MoveLeftParenBackwardOneExpression extends SchemeAction {

    public MoveLeftParenBackwardOneExpression(SchemeEditor editor) {
        super(editor);
    }
    
    public void run() {
        SchemeEditor editor = getSchemeEditor();
        if (editor == null) return;
        
        try {
            Region selection = editor.getSelection();
            if (selection.getLength() == 0) {
                IDocument document = editor.getDocument();
                char currentChar = document.getChar(selection.getOffset());
                if (currentChar == '(') {
                    SexpNavigator navigator = new SexpNavigator(document);
                    if (navigator.backwardSexpression(selection.getOffset())) {
                        try {
                            editor.startCompoundChange();
                            document.replace(selection.getOffset(), 1, "");
                            document.replace(navigator.getSexpStart(), 0, "(");
                            editor.setPoint(navigator.getSexpStart());
                        }
                        finally {
                            editor.endCompoundChange();
                        }
                    }
                }
            }
        }
        catch (BadLocationException e) {
        }
    }

}
