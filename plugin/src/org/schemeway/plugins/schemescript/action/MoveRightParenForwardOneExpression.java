/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class MoveRightParenForwardOneExpression extends SchemeAction {

    public MoveRightParenForwardOneExpression(SchemeEditor editor) {
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
                if (currentChar == ')') {
                    SexpNavigator navigator = new SexpNavigator(document);
                    if (navigator.forwardSexpression(selection.getOffset() + 1)) {
                        try {
                            editor.startCompoundChange();
                            document.replace(navigator.getSexpEnd(), 0, ")");
                            document.replace(selection.getOffset(), 1, "");
                            editor.setPoint(navigator.getSexpEnd() - 1);
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
